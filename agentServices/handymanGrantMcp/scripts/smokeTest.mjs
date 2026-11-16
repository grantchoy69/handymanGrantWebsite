// Read-only smoke test: never calls prepareQuoteRequest or submitQuoteRequest.
const endpoint = process.argv[2] ?? 'http://127.0.0.1:10000/mcp';
const parsed = new URL(endpoint);
if (!['https:', 'http:'].includes(parsed.protocol)) throw new Error('Use an HTTP(S) MCP URL.');
const headers = { 'Content-Type': 'application/json', Accept: 'application/json, text/event-stream' };
if (process.env.MCP_ACCESS_TOKEN) headers.Authorization = `Bearer ${process.env.MCP_ACCESS_TOKEN}`;
let id = 0;
async function rpc(method, params) {
  const response = await fetch(endpoint, { method: 'POST', headers, body: JSON.stringify({ jsonrpc: '2.0', id: ++id, method, params }), signal: AbortSignal.timeout(90000) });
  if (!response.ok) throw new Error(`HTTP ${response.status} for ${method}`);
  const reply = await response.json();
  if (reply.error) throw new Error(`${method}: ${reply.error.message}`);
  return reply.result;
}
const initialized = await rpc('initialize', { protocolVersion: '2025-11-25', clientInfo: { name: 'handymanGrantReadOnlySmoke', version: '0.1.0' }, capabilities: {} });
headers['MCP-Protocol-Version'] = initialized.protocolVersion;
const notification = await fetch(endpoint, { method: 'POST', headers, body: JSON.stringify({ jsonrpc: '2.0', method: 'notifications/initialized' }), signal: AbortSignal.timeout(90000) });
if (notification.status !== 202) throw new Error('Initialization notification not accepted.');
const listed = await rpc('tools/list', {});
const info = await rpc('tools/call', { name: 'getBusinessInfo', arguments: {} });
if (info.isError || info.structuredContent?.name !== 'Handyman Grant LLC') throw new Error('Business information tool failed.');
console.log(JSON.stringify({ server: initialized.serverInfo.name, protocol: initialized.protocolVersion, tools: listed.tools.map(tool => tool.name), submissionsEnabled: info.structuredContent.submissionsEnabled, existingCustomerRecordsAccessible: info.structuredContent.existingCustomerRecordsAccessible, liveLeadCreated: false }, null, 2));
