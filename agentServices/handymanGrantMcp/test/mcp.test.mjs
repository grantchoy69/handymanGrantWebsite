import { test } from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { request as httpRequest } from 'node:http';
import { createApp } from '../src/server.mjs';
import { createQuoteTools, intakeUrl, validateQuote } from '../src/quoteTools.mjs';

const secret = 'local-test-only-secret-not-for-deployment-12345';
const quote = { contactName: 'Example Customer', email: 'example@example.com', zipCode: '92104', serviceSlug: 'welding', projectDescription: 'The handrail support is rusted through near the base.', preferredContactMethod: 'email' };
const accepted = body => ({ success: true, requestId: '00000000-0000-4000-8000-000000000001', transactionId: body.transactionId });
const okFetch = async (_url, options) => new Response(JSON.stringify(accepted(JSON.parse(options.body))), { status: 201 });
const headers = { 'Content-Type': 'application/json', Accept: 'application/json, text/event-stream', 'MCP-Protocol-Version': '2025-11-25' };

async function withServer(options, run) {
  const server = createApp({ secret, fetchImpl: async () => { throw new Error('Unexpected upstream request'); }, ...options });
  server.listen(0, '127.0.0.1');
  await once(server, 'listening');
  const base = `http://127.0.0.1:${server.address().port}`;
  let id = 0;
  const rpc = async (method, params = {}, headerChanges = {}) => {
    const response = await fetch(`${base}/mcp`, { method: 'POST', headers: { ...headers, ...headerChanges }, body: JSON.stringify({ jsonrpc: '2.0', id: ++id, method, params }) });
    return { status: response.status, body: await response.json(), headers: response.headers };
  };
  try { await run({ base, rpc }); }
  finally { server.closeAllConnections(); await new Promise(resolve => server.close(resolve)); }
}

test('public business details disclose no customer records', async () => {
  const agent = createQuoteTools({ secret });
  const result = await agent.call('getBusinessInfo');
  assert.equal(result.name, 'Handyman Grant LLC');
  assert.equal(result.existingCustomerRecordsAccessible, false);
});
test('catalog has seven services and preserves owner scope decisions', async () => {
  const agent = createQuoteTools({ secret });
  const all = await agent.call('searchServices');
  assert.equal(all.services.length, 7);
  const result = await agent.call('searchServices', { query: 'rusted handrail welding' });
  assert.equal(result.services[0].slug, 'welding');
  assert.equal(result.unlistedProjectsCanBeSubmitted, true);
});
test('irrelevant query returns no results without blocking inquiry', async () => {
  const result = await createQuoteTools({ secret }).call('searchServices', { query: 'zzzznonmatching' });
  assert.equal(result.services.length, 0);
  assert.equal(result.unlistedProjectsCanBeSubmitted, true);
});
test('draft validates and does not submit to upstream', async () => {
  const result = await createQuoteTools({ secret, fetchImpl: () => { throw new Error('Must not call'); } }).call('prepareQuoteRequest', quote);
  assert.equal(result.status, 'draft');
  assert.equal(result.submitted, false);
  assert.equal(result.summary.email, quote.email);
  assert.ok(!result.confirmationToken.includes(quote.email));
});
test('preview refuses submission', async () => {
  const agent = createQuoteTools({ secret, fetchImpl: () => { throw new Error('Must not call'); } });
  const draft = await agent.call('prepareQuoteRequest', quote);
  await assert.rejects(agent.call('submitQuoteRequest', { confirmationToken: draft.confirmationToken, customerConfirmed: true }), { code: 'previewOnly' });
});
test('explicit confirmation is required', async () => {
  const agent = createQuoteTools({ secret, enabled: true });
  const draft = await agent.call('prepareQuoteRequest', quote);
  for (const customerConfirmed of [false, undefined, 'true']) {
    await assert.rejects(agent.call('submitQuoteRequest', { confirmationToken: draft.confirmationToken, customerConfirmed }), { code: 'confirmationRequired' });
  }
});
test('tampered token cannot change or submit a quote', async () => {
  const agent = createQuoteTools({ secret, enabled: true });
  const draft = await agent.call('prepareQuoteRequest', quote);
  const parts = draft.confirmationToken.split('.');
  parts[2] = (parts[2][0] === 'a' ? 'b' : 'a') + parts[2].slice(1);
  await assert.rejects(agent.call('submitQuoteRequest', { confirmationToken: parts.join('.'), customerConfirmed: true }), { code: 'invalidDraft' });
});
test('expired draft cannot submit', async () => {
  let time = Date.now();
  const agent = createQuoteTools({ secret, enabled: true, now: () => time });
  const draft = await agent.call('prepareQuoteRequest', quote);
  time += 24 * 3600000 + 1;
  await assert.rejects(agent.call('submitQuoteRequest', { confirmationToken: draft.confirmationToken, customerConfirmed: true }), { code: 'draftExpired' });
});
test('server forwards the exact reviewed contract with aiAgent channel', async () => {
  let body;
  let sentOptions;
  const agent = createQuoteTools({ secret, enabled: true, fetchImpl: async (url, options) => {
    assert.equal(url, intakeUrl); body = JSON.parse(options.body); sentOptions = options;
    return new Response(JSON.stringify(accepted(body)), { status: 201 });
  } });
  const draft = await agent.call('prepareQuoteRequest', quote);
  const receipt = await agent.call('submitQuoteRequest', { confirmationToken: draft.confirmationToken, customerConfirmed: true });
  assert.equal(receipt.success, true);
  assert.equal(body.contactMethod, 'aiAgent');
  assert.equal(body.projectDescription, quote.projectDescription);
  assert.equal(body.utmSource, 'mcp');
  assert.equal(body.sourceMetadata.preferredContactMethod, 'email');
  for (const field of ['gclid', 'gbraid', 'wbraid', 'formStartedAt', 'website', 'businessId']) assert.equal(field in body, false);
  for (const field of ['Authorization', 'Origin', 'x-forwarded-for']) assert.equal(field in sentOptions.headers, false);
  assert.match(receipt.transactionId, /^HG-LEAD-/);
});
test('duplicate submit uses cached receipt and same identity', async () => {
  let calls = 0;
  const agent = createQuoteTools({ secret, enabled: true, fetchImpl: async (...args) => { calls++; return okFetch(...args); } });
  const draft = await agent.call('prepareQuoteRequest', quote);
  const args = { confirmationToken: draft.confirmationToken, customerConfirmed: true };
  const first = await agent.call('submitQuoteRequest', args);
  const second = await agent.call('submitQuoteRequest', args);
  assert.equal(calls, 1);
  assert.equal(first.transactionId, second.transactionId);
  assert.equal(second.duplicateSubmission, true);
});
test('concurrent duplicate submits share one upstream operation', async () => {
  let calls = 0;
  const agent = createQuoteTools({ secret, enabled: true, fetchImpl: async (...args) => { calls++; await new Promise(resolve => setTimeout(resolve, 20)); return okFetch(...args); } });
  const draft = await agent.call('prepareQuoteRequest', quote);
  const args = { confirmationToken: draft.confirmationToken, customerConfirmed: true };
  const receipts = await Promise.all([agent.call('submitQuoteRequest', args), agent.call('submitQuoteRequest', args)]);
  assert.equal(calls, 1);
  assert.equal(receipts[0].transactionId, receipts[1].transactionId);
});
test('retry after restart retains the original transaction ID', async () => {
  const bodyIds = [];
  const fetchImpl = async (_url, options) => {
    const body = JSON.parse(options.body); bodyIds.push(body.transactionId);
    return new Response(JSON.stringify({ ...accepted(body), duplicateSubmission: bodyIds.length > 1 }), { status: 200 });
  };
  const first = createQuoteTools({ secret, enabled: true, fetchImpl });
  const draft = await first.call('prepareQuoteRequest', quote);
  const args = { confirmationToken: draft.confirmationToken, customerConfirmed: true };
  await first.call('submitQuoteRequest', args);
  const second = createQuoteTools({ secret, enabled: true, fetchImpl });
  assert.equal((await second.call('submitQuoteRequest', args)).duplicateSubmission, true);
  assert.equal(bodyIds[0], bodyIds[1]);
});
test('rate limit closes before shared intake quota and resets with time', async () => {
  let time = Date.now();
  const agent = createQuoteTools({ secret, enabled: true, fetchImpl: okFetch, submissionLimit: 1, now: () => time });
  const draft1 = await agent.call('prepareQuoteRequest', quote);
  await agent.call('submitQuoteRequest', { confirmationToken: draft1.confirmationToken, customerConfirmed: true });
  const draft2 = await agent.call('prepareQuoteRequest', quote);
  await assert.rejects(agent.call('submitQuoteRequest', { confirmationToken: draft2.confirmationToken, customerConfirmed: true }), { code: 'rateLimited' });
  time += 3600001;
  assert.equal((await agent.call('submitQuoteRequest', { confirmationToken: draft2.confirmationToken, customerConfirmed: true })).success, true);
});
test('upstream 429 is respected without automatically retrying', async () => {
  let calls = 0;
  const agent = createQuoteTools({ secret, enabled: true, fetchImpl: async () => { calls++; return new Response('{}', { status: 429 }); } });
  const draft = await agent.call('prepareQuoteRequest', quote);
  const args = { confirmationToken: draft.confirmationToken, customerConfirmed: true };
  await assert.rejects(agent.call('submitQuoteRequest', args), { code: 'rateLimited' });
  await assert.rejects(agent.call('submitQuoteRequest', args), { code: 'rateLimited' });
  assert.equal(calls, 1);
});
test('timeout does not claim success and retry keeps the same identity', async () => {
  const ids = [];
  const agent = createQuoteTools({ secret, enabled: true, fetchImpl: async (_url, options) => {
    const body = JSON.parse(options.body); ids.push(body.transactionId);
    if (ids.length === 1) throw new Error('timeout');
    return new Response(JSON.stringify(accepted(body)), { status: 201 });
  } });
  const draft = await agent.call('prepareQuoteRequest', quote);
  const args = { confirmationToken: draft.confirmationToken, customerConfirmed: true };
  await assert.rejects(agent.call('submitQuoteRequest', args), { code: 'submissionUnconfirmed' });
  assert.equal((await agent.call('submitQuoteRequest', args)).success, true);
  assert.equal(ids[0], ids[1]);
});
test('unconfirmed upstream responses are never reported as leads', async () => {
  for (const [status, body] of [[202, { accepted: true }], [200, {}], [503, { error: 'unavailable' }], [201, { success: true, requestId: 'x', transactionId: 'wrong' }]]) {
    const agent = createQuoteTools({ secret, enabled: true, fetchImpl: async () => new Response(JSON.stringify(body), { status }) });
    const draft = await agent.call('prepareQuoteRequest', quote);
    await assert.rejects(agent.call('submitQuoteRequest', { confirmationToken: draft.confirmationToken, customerConfirmed: true }), { code: 'submissionUnconfirmed' });
  }
});
test('customer-provided contact data is required and format checked', () => {
  for (const input of [
    { ...quote, email: undefined }, { ...quote, email: 'not-an-email' }, { ...quote, phone: '123' },
    { ...quote, preferredContactMethod: 'text' }, { ...quote, projectDescription: 'tiny' },
    { ...quote, zipCode: 'here' }, { ...quote, contactName: 'a'.repeat(121) },
    { ...quote, serviceSlug: 'unknown' }, { ...quote, projectDescription: 'a'.repeat(5001) },
    { ...quote, gclid: 'invented' }, { ...quote, businessId: 2 },
  ]) assert.throws(() => validateQuote(input), { code: 'invalidInput' });
});
test('unicode max-length inquiry round-trips without corrupting the token', async () => {
  const agent = createQuoteTools({ secret, enabled: true, fetchImpl: okFetch });
  const draft = await agent.call('prepareQuoteRequest', { ...quote, projectDescription: '\u754c'.repeat(5000) });
  assert.ok(draft.confirmationToken.length < 40000);
  assert.equal((await agent.call('submitQuoteRequest', { confirmationToken: draft.confirmationToken, customerConfirmed: true })).success, true);
});
test('unreviewed outbound endpoint and weak secret fail startup', () => {
  assert.throws(() => createQuoteTools({ secret, endpoint: 'http://127.0.0.1/internal' }));
  assert.throws(() => createQuoteTools({ secret: 'short' }));
  assert.throws(() => createQuoteTools({ secret: 'REPLACE_WITH_A_RANDOM_SECRET_OF_AT_LEAST_32_CHARACTERS' }));
});
test('MCP initialization, tool discovery, resource reading and ping work over HTTP', async () => {
  await withServer({}, async ({ rpc }) => {
    const init = await rpc('initialize', { protocolVersion: '2025-11-25', clientInfo: { name: 'test', version: '1' }, capabilities: {} });
    assert.equal(init.body.result.protocolVersion, '2025-11-25');
    assert.equal(init.body.result.capabilities.tools.listChanged, false);
    const listed = await rpc('tools/list');
    assert.deepEqual(listed.body.result.tools.map(tool => tool.name), ['getBusinessInfo', 'searchServices', 'prepareQuoteRequest', 'submitQuoteRequest']);
    assert.equal(listed.body.result.tools[3].annotations.readOnlyHint, false);
    const resource = await rpc('resources/read', { uri: 'handymangrant://business' });
    assert.equal(JSON.parse(resource.body.result.contents[0].text).name, 'Handyman Grant LLC');
    assert.equal((await rpc('resources/list')).body.result.resources.length, 2);
    assert.deepEqual((await rpc('resources/templates/list')).body.result.resourceTemplates, []);
    assert.deepEqual((await rpc('ping')).body.result, {});
  });
});
test('MCP version negotiation offers a supported version', async () => {
  await withServer({}, async ({ rpc }) => {
    const reply = await rpc('initialize', { protocolVersion: '2099-01-01', clientInfo: { name: 'test', version: '1' }, capabilities: {} }, { 'MCP-Protocol-Version': '2025-11-25' });
    assert.equal(reply.body.result.protocolVersion, '2025-11-25');
    assert.equal((await rpc('initialize')).body.error.code, -32602);
    assert.equal((await rpc('ping', {}, { 'MCP-Protocol-Version': 'not-supported' })).status, 400);
  });
});
test('MCP quote draft and enabled submit operate over HTTP with a mocked backend', async () => {
  await withServer({ enabled: true, fetchImpl: okFetch }, async ({ rpc }) => {
    const draft = (await rpc('tools/call', { name: 'prepareQuoteRequest', arguments: quote })).body.result.structuredContent;
    const receipt = (await rpc('tools/call', { name: 'submitQuoteRequest', arguments: { confirmationToken: draft.confirmationToken, customerConfirmed: true } })).body.result;
    assert.equal(receipt.isError, false);
    assert.equal(receipt.structuredContent.success, true);
  });
});
test('MCP preview errors are explicit tool errors', async () => {
  await withServer({}, async ({ rpc }) => {
    const draft = (await rpc('tools/call', { name: 'prepareQuoteRequest', arguments: quote })).body.result.structuredContent;
    const receipt = (await rpc('tools/call', { name: 'submitQuoteRequest', arguments: { confirmationToken: draft.confirmationToken, customerConfirmed: true } })).body.result;
    assert.equal(receipt.isError, true);
    assert.equal(receipt.structuredContent.code, 'previewOnly');
  });
});
test('no tools can read jobs, execute SQL, or access arbitrary resources', async () => {
  await withServer({}, async ({ rpc }) => {
    for (const name of ['getJobs', 'executeSql', 'getCustomers']) assert.equal((await rpc('tools/call', { name })).body.error.code, -32602);
    assert.equal((await rpc('resources/read', { uri: 'file:///etc/passwd' })).body.error.code, -32602);
    assert.equal((await rpc('invented/method')).body.error.code, -32601);
    assert.equal((await rpc('tools/call', { name: 'getBusinessInfo', arguments: { secret: 'x' } })).body.result.isError, true);
  });
});
test('Origin and Host validation reject unexpected browser origins', async () => {
  await withServer({ allowedOrigins: ['https://chatgpt.com'] }, async ({ rpc, base }) => {
    assert.equal((await rpc('ping', {}, { Origin: 'https://evil.example' })).status, 403);
    assert.equal((await rpc('ping', {}, { Origin: 'null' })).status, 403);
    const hostStatus = await new Promise((resolve, reject) => {
      const req = httpRequest(`${base}/mcp`, { method: 'POST', headers: { ...headers, Host: 'evil.example' } }, response => {
        response.resume(); response.on('end', () => resolve(response.statusCode));
      });
      req.on('error', reject);
      req.end(JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'ping', params: {} }));
    });
    assert.equal(hostStatus, 403);
    const good = await rpc('ping', {}, { Origin: 'https://chatgpt.com' });
    assert.equal(good.status, 200);
    assert.equal(good.headers.get('access-control-allow-origin'), 'https://chatgpt.com');
  });
});
test('optional bearer gate works without exposing the token', async () => {
  await withServer({ accessToken: 'example-private-client-secret' }, async ({ rpc }) => {
    assert.equal((await rpc('ping')).status, 401);
    assert.equal((await rpc('ping', {}, { Authorization: 'Bearer wrong' })).status, 401);
    assert.equal((await rpc('ping', {}, { Authorization: 'Bearer example-private-client-secret' })).status, 200);
  });
});
test('GET stream is 405, notification is 202, and health works', async () => {
  await withServer({}, async ({ base }) => {
    assert.equal((await fetch(`${base}/healthz`)).status, 200);
    assert.equal((await fetch(`${base}/mcp`)).status, 405);
    assert.equal((await fetch(`${base}/mcp`, { method: 'DELETE' })).status, 405);
    const notification = await fetch(`${base}/mcp`, { method: 'POST', headers, body: JSON.stringify({ jsonrpc: '2.0', method: 'notifications/initialized' }) });
    assert.equal(notification.status, 202);
    assert.equal(await notification.text(), '');
    assert.equal((await fetch(`${base}/mcp`, { method: 'OPTIONS' })).status, 204);
    assert.equal((await fetch(`${base}/database`)).status, 404);
  });
});
test('malformed JSON, batches, missing Accept and oversize bodies are rejected', async () => {
  await withServer({}, async ({ base, rpc }) => {
    const post = body => fetch(`${base}/mcp`, { method: 'POST', headers, body });
    assert.equal((await post('{')).status, 400);
    assert.equal((await post('[]')).status, 400);
    assert.equal((await post(JSON.stringify({ jsonrpc: '2.0', id: null, method: 'ping' }))).status, 400);
    assert.equal((await rpc('ping', {}, { Accept: 'application/json' })).status, 406);
    assert.equal((await rpc('ping', {}, { 'Content-Type': 'text/plain' })).status, 415);
    assert.equal((await post(JSON.stringify({ data: 'a'.repeat(70000) }))).status, 413);
  });
});
test('aggregate HTTP request throttling is enforced', async () => {
  await withServer({ maxRequestsPerMinute: 1 }, async ({ rpc }) => {
    assert.equal((await rpc('ping')).status, 200);
    assert.equal((await rpc('ping')).status, 429);
  });
});
