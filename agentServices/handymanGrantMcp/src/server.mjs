import { createServer } from 'node:http';
import { timingSafeEqual } from 'node:crypto';
import { pathToFileURL } from 'node:url';
import { business, catalog, createQuoteTools, intakeUrl, ToolError, tools } from './quoteTools.mjs';

const versions = ['2025-11-25', '2025-06-18', '2025-03-26'];
const serverInfo = { name: 'handymanGrant', title: 'Handyman Grant quote requests', version: '0.1.0', websiteUrl: business.website };
const resources = [
  { uri: 'handymangrant://business', name: 'businessInfo', title: 'Public business information', mimeType: 'application/json' },
  { uri: 'handymangrant://services', name: 'serviceCatalog', title: 'Public service catalog', mimeType: 'application/json' },
];

function sendJson(res, status, value, extraHeaders = {}) {
  res.writeHead(status, { 'Content-Type': 'application/json; charset=utf-8', 'Cache-Control': 'no-store', 'X-Content-Type-Options': 'nosniff', ...extraHeaders });
  res.end(JSON.stringify(value));
}
function rpcError(id, code, message) { return { jsonrpc: '2.0', id, error: { code, message } }; }
function tokenMatches(header, secret) {
  if (!secret) return true;
  const provided = Buffer.from(typeof header === 'string' ? header : '');
  const expected = Buffer.from(`Bearer ${secret}`);
  return provided.length === expected.length && timingSafeEqual(provided, expected);
}

export function createApp({ secret, enabled = false, endpoint = intakeUrl, accessToken = '', allowedHosts = ['127.0.0.1', 'localhost', '[::1]'], allowedOrigins = [], fetchImpl, now = Date.now, submissionLimit = 6, upstreamTimeoutMs, maxRequestsPerMinute = 300 }) {
  const quoteTools = createQuoteTools({ secret, enabled, endpoint, fetchImpl, now, submissionLimit, upstreamTimeoutMs });
  let requestWindowStart = now();
  let requestCount = 0;
  const server = createServer(async (req, res) => {
    // No request bodies, contact details, tokens, URLs with queries, or IPs are logged.
    try {
      const origin = req.headers.origin;
      if (origin !== undefined && !allowedOrigins.includes(origin)) return sendJson(res, 403, { error: 'Origin not allowed.' });
      if (origin) {
        res.setHeader('Access-Control-Allow-Origin', origin);
        res.setHeader('Vary', 'Origin');
      }
      const requestPath = req.url?.split('?')[0];
      if (requestPath === '/healthz' && req.method === 'GET') return sendJson(res, 200, { status: 'ok', version: serverInfo.version, submissionsEnabled: enabled });
      const host = req.headers.host;
      let hostname;
      try { hostname = new URL(`http://${host}`).hostname; } catch { /* invalid host */ }
      if (!host || !hostname || !allowedHosts.includes(hostname)) return sendJson(res, 403, { error: 'Host not allowed.' });
      if (requestPath === '/' && req.method === 'GET') return sendJson(res, 200, { name: serverInfo.title, endpoint: '/mcp', transport: 'Streamable HTTP (stateless JSON responses)', submissionsEnabled: enabled, publicWebsite: business.website });
      if (requestPath !== '/mcp') return sendJson(res, 404, { error: 'Not found.' });
      if (req.method === 'OPTIONS') {
        res.writeHead(204, { 'Access-Control-Allow-Methods': 'POST, GET, OPTIONS', 'Access-Control-Allow-Headers': 'Content-Type, Accept, Authorization, MCP-Protocol-Version', 'Cache-Control': 'no-store' });
        return res.end();
      }
      if (!tokenMatches(req.headers.authorization, accessToken)) return sendJson(res, 401, { error: 'Authentication required.' }, { 'WWW-Authenticate': 'Bearer realm="handyman-grant-mcp"' });
      // A stateless server with no SSE stream or sessions returns 405 for GET/DELETE.
      if (req.method !== 'POST') return sendJson(res, 405, { error: 'Use POST for MCP requests. No persistent stream is exposed.' }, { Allow: 'POST, OPTIONS' });
      if (now() - requestWindowStart >= 60000) { requestWindowStart = now(); requestCount = 0; }
      if (++requestCount > maxRequestsPerMinute) return sendJson(res, 429, { error: 'Server request limit reached.' }, { 'Retry-After': '60' });
      if (!(req.headers['content-type'] ?? '').toLowerCase().startsWith('application/json')) return sendJson(res, 415, { error: 'Expected application/json.' });
      const accept = req.headers.accept ?? '';
      if (!accept.includes('application/json') || !accept.includes('text/event-stream')) return sendJson(res, 406, { error: 'Accept must include application/json and text/event-stream.' });
      const protocolHeader = req.headers['mcp-protocol-version'];
      if (protocolHeader && !versions.includes(protocolHeader)) return sendJson(res, 400, { error: 'Unsupported MCP protocol version.' });
      const contentLength = Number(req.headers['content-length'] ?? 0);
      if (!Number.isFinite(contentLength) || contentLength > 65536) { req.resume(); return sendJson(res, 413, { error: 'Request too large.' }); }
      let size = 0;
      const chunks = [];
      for await (const chunk of req) {
        size += chunk.length;
        if (size > 65536) { sendJson(res, 413, { error: 'Request too large.' }); req.destroy(); return; }
        chunks.push(chunk);
      }
      let message;
      try { message = JSON.parse(Buffer.concat(chunks).toString('utf8')); }
      catch { return sendJson(res, 400, rpcError(null, -32700, 'Invalid JSON.')); }
      if (!message || typeof message !== 'object' || Array.isArray(message) || message.jsonrpc !== '2.0') return sendJson(res, 400, rpcError(null, -32600, 'Invalid JSON-RPC message.'));
      const hasId = Object.hasOwn(message, 'id');
      const validId = hasId && (typeof message.id === 'string' || (typeof message.id === 'number' && Number.isSafeInteger(message.id)));
      if (hasId && !validId) return sendJson(res, 400, rpcError(null, -32600, 'Invalid request id.'));
      if (message.params !== undefined && (!message.params || typeof message.params !== 'object' || Array.isArray(message.params))) return sendJson(res, 400, rpcError(validId ? message.id : null, -32602, 'Expected object parameters.'));
      if (!hasId && typeof message.method === 'string' && message.method.startsWith('notifications/')) { res.writeHead(202, { 'Cache-Control': 'no-store' }); return res.end(); }
      // This server never makes client requests; an incoming response is not expected.
      if (!validId || typeof message.method !== 'string') return sendJson(res, 400, rpcError(null, -32600, 'Expected a request.'));
      const params = message.params ?? {};
      let result;
      switch (message.method) {
        case 'initialize':
          if (typeof params.protocolVersion !== 'string' || !params.clientInfo || typeof params.clientInfo.name !== 'string' || typeof params.clientInfo.version !== 'string' || !params.capabilities || typeof params.capabilities !== 'object' || Array.isArray(params.capabilities)) return sendJson(res, 200, rpcError(message.id, -32602, 'Invalid initialization parameters.'));
          result = {
            protocolVersion: versions.includes(params.protocolVersion) ? params.protocolVersion : versions[0],
            capabilities: { tools: { listChanged: false }, resources: { subscribe: false, listChanged: false } },
            serverInfo,
            instructions: 'Use public information to help customers inquire about Handyman Grant. Draft first, show the exact details and privacy notice, and obtain customer permission before submission. Never invent customer contact details, availability, accepted prices, or Google advertising identifiers. This is not a booking service and cannot read existing business records.',
          };
          break;
        case 'ping': result = {}; break;
        case 'tools/list': result = { tools }; break;
        case 'resources/list': result = { resources }; break;
        case 'resources/templates/list': result = { resourceTemplates: [] }; break;
        case 'resources/read': {
          const resource = resources.find(item => item.uri === params.uri);
          if (!resource) return sendJson(res, 200, rpcError(message.id, -32602, 'Resource not found.'));
          result = { contents: [{ uri: resource.uri, mimeType: 'application/json', text: JSON.stringify(resource.uri.endsWith('services') ? catalog : business) }] };
          break;
        }
        case 'tools/call': {
          if (!tools.some(tool => tool.name === params.name)) return sendJson(res, 200, rpcError(message.id, -32602, 'Unknown tool.'));
          try {
            const value = await quoteTools.call(params.name, params.arguments ?? {});
            result = { content: [{ type: 'text', text: JSON.stringify(value) }], structuredContent: value, isError: false };
          } catch (error) {
            const value = { success: false, code: error instanceof ToolError ? error.code : 'internalError', message: error instanceof ToolError ? error.message : 'The operation could not be completed. No submission is confirmed.' };
            result = { content: [{ type: 'text', text: JSON.stringify(value) }], structuredContent: value, isError: true };
          }
          break;
        }
        default: return sendJson(res, 200, rpcError(message.id, -32601, 'Method not supported.'));
      }
      sendJson(res, 200, { jsonrpc: '2.0', id: message.id, result });
    } catch {
      if (!res.headersSent) sendJson(res, 500, { error: 'Internal server error.' });
      else res.end();
    }
  });
  server.requestTimeout = 20000;
  server.headersTimeout = 10000;
  server.keepAliveTimeout = 5000;
  server.maxHeadersCount = 40;
  return server;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const port = Number(process.env.PORT ?? 10000);
  if (!Number.isSafeInteger(port) || port < 1 || port > 65535) throw new Error('Invalid PORT.');
  const host = process.env.HOST ?? (process.env.RENDER ? '0.0.0.0' : '127.0.0.1');
  const allowedHosts = ['127.0.0.1', 'localhost', '[::1]', process.env.RENDER_EXTERNAL_HOSTNAME, ...(process.env.ALLOWED_HOSTS ?? '').split(',')].map(v => v?.trim()).filter(Boolean);
  const allowedOrigins = (process.env.ALLOWED_ORIGINS ?? 'https://handymangrant.com,https://www.handymangrant.com,https://chatgpt.com').split(',').map(v => v.trim()).filter(Boolean);
  const server = createApp({
    secret: process.env.DRAFT_ENCRYPTION_SECRET,
    enabled: process.env.QUOTE_SUBMISSIONS_ENABLED === 'true',
    endpoint: process.env.QUOTE_INTAKE_URL ?? intakeUrl,
    accessToken: process.env.MCP_ACCESS_TOKEN ?? '', allowedHosts, allowedOrigins,
  });
  server.listen(port, host, () => console.log(JSON.stringify({ event: 'mcpListening', port, submissionsEnabled: process.env.QUOTE_SUBMISSIONS_ENABLED === 'true' })));
  for (const signal of ['SIGTERM', 'SIGINT']) process.on(signal, () => { server.close(() => process.exit(0)); setTimeout(() => process.exit(1), 15000).unref(); });
}
