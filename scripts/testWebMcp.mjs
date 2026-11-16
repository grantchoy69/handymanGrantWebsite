import { test } from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import { createWebsiteTools, validateQuote, publicServices, searchCatalog, readAttribution, createSender, intakeEndpoint, draftLifetimeMs } from '../webMcpCore.mjs';

const quote = { contactName: 'Example Customer', email: 'test@example.com', zipCode: '92104', projectDescription: 'TEST ONLY: rusted handrail support repair.' };
const receiptId = '12345678-1234-4234-8234-123456789012';
const catalog = { services: [
  { slug: 'welding', serviceName: 'Welding & Metal Work', servicesIncluded: ['Handrail and railing repair', 'Gate hinge repair'], pricingNote: 'Indicative only.' },
  { slug: 'electrical', serviceName: 'Electrical Work', servicesIncluded: ['Light fixture repair'] },
  { slug: 'plumbing', serviceName: 'Plumbing', servicesIncluded: ['Faucet repair'] },
] };

test('all current pages and the shared generator template include one module with the correct path', () => {
  const root = new URL('../', import.meta.url);
  const current = JSON.parse(fs.readFileSync(new URL('services.json', root), 'utf8'));
  const pages = ['index.html', 'work.html', 'landing-page-template.html', ...current.services.map(service => `landing-pages/${service.slug}.html`)];
  for (const page of pages) {
    const html = fs.readFileSync(new URL(page, root), 'utf8');
    const scripts = html.match(/<script\b[^>]*\bdata-hg-webmcp[^>]*><\/script>/g) || [];
    const expectedPath = page.startsWith('landing-pages/') || page === 'landing-page-template.html' ? '../webMcp.js' : 'webMcp.js';
    assert.deepEqual(scripts, [`<script type="module" src="${expectedPath}" data-hg-webmcp></script>`], page);
  }
});
function harness(overrides = {}) {
  let clock = 10000;
  let sequence = 0;
  let approvals = 0;
  const sent = [], accepted = [];
  const tools = createWebsiteTools({
    config: { submissionsEnabled: true, business: { name: 'Handyman Grant LLC' } },
    loadCatalog: async () => catalog,
    review: async () => { approvals++; clock += 3000; return true; },
    send: async (p) => { sent.push(p); return { success: true, requestId: receiptId, transactionId: p.transactionId }; },
    onAccepted: (...args) => accepted.push(args),
    now: () => clock,
    uuid: () => `12345678-1234-4234-8234-${String(++sequence).padStart(12, '0')}`,
    ...overrides,
  });
  return { tools, call: (name, args, context) => tools.find((t) => t.name === name).execute(args, context), sent, accepted, advance: (ms) => { clock += ms; }, approvals: () => approvals };
}
test('exposes exactly four narrow public tools', () => {
  const h = harness();
  assert.deepEqual(h.tools.map((t) => t.name), ['getBusinessInfo', 'searchServices', 'prepareQuoteRequest', 'submitQuoteRequest']);
  assert.equal(h.tools[3].annotations.consequentialHint, true);
  assert.equal(h.tools[2].annotations.untrustedContentHint, true);
  assert.ok(h.tools.every((t) => t.inputSchema.additionalProperties === false));
});
test('business information discloses disabled submissions', async () => {
  const h = harness({ config: { submissionsEnabled: false, business: { name: 'Handyman Grant LLC' } } });
  const data = await h.call('getBusinessInfo');
  assert.equal(data.submissionsEnabled, false);
  assert.equal(data.existingCustomerRecordsAccessible, false);
});
test('rusted handrail search excludes unrelated repairs', async () => {
  const result = await harness().call('searchServices', { query: 'rusted handrail repair' });
  assert.deepEqual(result.services.map((s) => s.slug), ['welding']);
});
test('catalog only exposes public selected fields', () => {
  const services = publicServices({ services: [{ ...catalog.services[0], privateNote: 'not exported' }, { slug: '../../bad', serviceName: 'Bad' }] });
  assert.equal(services.length, 1);
  assert.equal('privateNote' in services[0], false);
});
test('unmatched search has no invented service', () => assert.deepEqual(searchCatalog(publicServices(catalog), 'spaceship'), []));
test('preparing is local and does not send or track', async () => {
  const h = harness(); const d = await h.call('prepareQuoteRequest', quote);
  assert.equal(d.submitted, false); assert.equal(d.request.email, quote.email);
  assert.match(d.privacyNotice, /Supabase/); assert.equal(h.sent.length, 0); assert.equal(h.accepted.length, 0);
});
test('identical preparation reuses same draft', async () => {
  const h = harness(); assert.equal((await h.call('prepareQuoteRequest', quote)).draftId, (await h.call('prepareQuoteRequest', quote)).draftId);
});
test('unknown fields and supplied attribution are rejected', () => {
  for (const key of ['gclid', 'endpoint', 'customerConfirmed', '__proto__', 'website']) {
    const q = JSON.parse(JSON.stringify(quote)); Object.defineProperty(q, key, { value: 'injected', enumerable: true });
    assert.throws(() => validateQuote(q), /Unexpected/);
  }
});
test('contact details are mandatory, not invented', () => {
  assert.throws(() => validateQuote({ ...quote, contactName: '' }), /name/);
  assert.throws(() => validateQuote({ ...quote, email: '' }), /phone number or email/);
});
test('bad or oversized fields fail rather than silently truncating', () => {
  assert.throws(() => validateQuote({ ...quote, email: 'not-email' }));
  assert.throws(() => validateQuote({ ...quote, phone: 'not a number' }));
  assert.throws(() => validateQuote({ ...quote, contactName: 'x'.repeat(121) }));
  assert.throws(() => validateQuote({ ...quote, projectDescription: 'x'.repeat(5001) }));
  assert.throws(() => validateQuote({ ...quote, projectDescription: '\u0000invalid control' }));
});
test('missing draft cannot submit', async () => {
  const h = harness(); assert.equal((await h.call('submitQuoteRequest', { draftId: 'fake' })).submitted, false); assert.equal(h.sent.length, 0);
});
test('changed draft invalidates prior approval target', async () => {
  const h = harness(); const d = await h.call('prepareQuoteRequest', quote);
  await h.call('prepareQuoteRequest', { ...quote, projectDescription: 'A different customer-approved project description.' });
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId })).status, 'invalidDraft');
});
test('expired draft never sends', async () => {
  const h = harness(); const d = await h.call('prepareQuoteRequest', quote); h.advance(draftLifetimeMs + 1);
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId })).status, 'expired'); assert.equal(h.sent.length, 0);
});
test('cancel never sends', async () => {
  const h = harness({ review: async () => false }); const d = await h.call('prepareQuoteRequest', quote);
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId })).status, 'canceled'); assert.equal(h.sent.length, 0);
});
test('disabled mode cannot be enabled by a model argument', async () => {
  const h = harness({ config: { submissionsEnabled: false, business: {} } }); const d = await h.call('prepareQuoteRequest', quote);
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId })).status, 'disabled');
  await assert.rejects(h.call('submitQuoteRequest', { draftId: d.draftId, submissionsEnabled: true })); assert.equal(h.sent.length, 0);
});
test('too-fast review is not disguised with a fabricated timestamp', async () => {
  const h = harness({ review: async () => true }); const d = await h.call('prepareQuoteRequest', quote);
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId })).status, 'reviewTooFast'); assert.equal(h.sent.length, 0);
});
test('accepted request uses exact reviewed data and server receipt', async () => {
  const h = harness(); const d = await h.call('prepareQuoteRequest', quote);
  const result = await h.call('submitQuoteRequest', { draftId: d.draftId });
  assert.equal(result.status, 'submitted'); assert.equal(result.requestId, receiptId);
  assert.equal(h.sent[0].contactMethod, 'aiAgent'); assert.equal(h.sent[0].projectDescription, quote.projectDescription);
  assert.equal(h.sent[0].sourceMetadata.intakeInterface, 'webmcp'); assert.equal(h.accepted.length, 1);
});
test('same draft retry does not resend or retrack', async () => {
  const h = harness(); const d = await h.call('prepareQuoteRequest', quote);
  await h.call('submitQuoteRequest', { draftId: d.draftId }); await h.call('submitQuoteRequest', { draftId: d.draftId });
  assert.equal(h.sent.length, 1); assert.equal(h.accepted.length, 1); assert.equal(h.approvals(), 1);
});
test('uncertain response retries with same transaction id and payload', async () => {
  const seen = [];
  const h = harness({ send: async (p) => { seen.push(p); if (seen.length === 1) throw new Error('lost response'); return { success: true, requestId: receiptId, transactionId: p.transactionId }; } });
  const d = await h.call('prepareQuoteRequest', quote);
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId })).status, 'unknown');
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId })).status, 'submitted');
  assert.deepEqual(seen[0], seen[1]); assert.equal(h.accepted.length, 1);
});

test('canceling a retry does not deny an earlier uncertain submission', async () => {
  let approve = true; let sends = 0;
  const h = harness({ review: async () => approve, send: async () => { sends++; throw new Error('accepted but response lost'); } });
  const draft = await h.call('prepareQuoteRequest', quote); h.advance(3000);
  assert.equal((await h.call('submitQuoteRequest', { draftId: draft.draftId })).status, 'unknown');
  approve = false;
  const result = await h.call('submitQuoteRequest', { draftId: draft.draftId });
  assert.equal(result.status, 'unknown'); assert.match(result.message, /earlier request may already have been received/i);
  assert.doesNotMatch(result.message, /nothing was sent/i); assert.equal(sends, 1);
});

test('uncertain delivery cannot be edited into a second request', async () => {
  const seen = [];
  const h = harness({ send: async payload => { seen.push(structuredClone(payload)); if (seen.length === 1) throw new Error('lost response'); return { success: true, requestId: receiptId, transactionId: payload.transactionId }; } });
  const draft = await h.call('prepareQuoteRequest', quote);
  await h.call('submitQuoteRequest', { draftId: draft.draftId });
  const changed = await h.call('prepareQuoteRequest', { ...quote, projectDescription: 'A changed project while delivery is uncertain.' });
  assert.equal(changed.status, 'unknown'); assert.equal(changed.draftId, undefined);
  assert.equal((await h.call('prepareQuoteRequest', quote)).draftId, draft.draftId);
  assert.equal((await h.call('submitQuoteRequest', { draftId: draft.draftId })).status, 'submitted');
  assert.deepEqual(seen[0], seen[1]);
});

test('an expired uncertain request retains its identity without returning contact details', async () => {
  let sends = 0;
  const h = harness({ send: async () => { sends++; throw new Error('accepted but response lost'); } });
  const draft = await h.call('prepareQuoteRequest', quote);
  await h.call('submitQuoteRequest', { draftId: draft.draftId }); h.advance(draftLifetimeMs + 1);
  for (const result of [await h.call('submitQuoteRequest', { draftId: draft.draftId }), await h.call('prepareQuoteRequest', quote), await h.call('prepareQuoteRequest', { ...quote, contactName: 'Changed' })]) {
    assert.equal(result.status, 'unknown'); assert.match(result.message, /contact Grant/i);
    assert.equal(result.request, undefined); assert.equal(result.draftId, undefined);
  }
  assert.equal(sends, 1);
});
test('honeypot-style accepted response is not claimed as a lead', async () => {
  const h = harness({ send: async () => ({ accepted: true }) }); const d = await h.call('prepareQuoteRequest', quote);
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId })).status, 'unknown'); assert.equal(h.accepted.length, 0);
});
test('wrong transaction receipt is rejected', async () => {
  const h = harness({ send: async () => ({ success: true, requestId: receiptId, transactionId: 'different' }) }); const d = await h.call('prepareQuoteRequest', quote);
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId })).submitted, false);
});
test('analytics exceptions do not lose a confirmed lead', async () => {
  const h = harness({ onAccepted: () => { throw new Error('blocked'); } }); const d = await h.call('prepareQuoteRequest', quote);
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId })).status, 'submitted');
});
test('already-aborted call never shows review or sends', async () => {
  const h = harness(); const d = await h.call('prepareQuoteRequest', quote); const c = new AbortController(); c.abort();
  assert.equal((await h.call('submitQuoteRequest', { draftId: d.draftId }, { signal: c.signal })).status, 'canceled'); assert.equal(h.approvals(), 0);
});
test('concurrent submission shares a single flight', async () => {
  let release; let reviews = 0;
  const h = harness({ review: async () => { reviews++; await new Promise((r) => { release = r; }); return true; } });
  const d = await h.call('prepareQuoteRequest', quote); h.advance(3000);
  const p1 = h.call('submitQuoteRequest', { draftId: d.draftId });
  const p2 = h.call('submitQuoteRequest', { draftId: d.draftId });
  await assert.rejects(h.call('prepareQuoteRequest', { ...quote, contactName: 'Other' }));
  release(); await Promise.all([p1, p2]); assert.equal(reviews, 1); assert.equal(h.sent.length, 1);
});
test('blocked storage falls back safely and strips arbitrary query data', () => {
  const a = readAttribution({ href: 'https://handymangrant.com/work.html?email=secret@example.com&gclid=real-click', storage: { getItem() { throw new Error('denied'); } } });
  assert.equal(a.gclid, 'real-click'); assert.equal(a.landingPage, 'https://handymangrant.com/work.html'); assert.equal(JSON.stringify(a).includes('secret@example.com'), false);
});
test('stored attribution survives ordinary page navigation', () => {
  const a = readAttribution({ href: 'https://handymangrant.com/work.html', now: 10000, storage: { getItem: () => JSON.stringify({ savedAt: new Date(9000), lastTouch: { values: { utm_source: 'chatgpt.com' } } }) } });
  assert.equal(a.utmSource, 'chatgpt.com'); assert.equal(a.gclid, '');
});
test('stale attribution ignored', () => {
  const a = readAttribution({ href: 'https://handymangrant.com/', now: 100 * 86400000, storage: { getItem: () => JSON.stringify({ savedAt: new Date(0), lastTouch: { values: { gclid: 'stale' } } }) } });
  assert.equal(a.gclid, '');
});
test('new actual campaign overrides prior stored data', () => {
  const a = readAttribution({ href: 'https://handymangrant.com/?gclid=new&utm_source=google', now: 10000, storage: { getItem: () => JSON.stringify({ savedAt: new Date(9000), lastTouch: { values: { utm_source: 'chatgpt.com', gclid: 'old' } } }) } });
  assert.equal(a.gclid, 'new'); assert.equal(a.utmSource, 'google');
});
test('sender rejects arbitrary exfiltration destination', () => assert.throws(() => createSender(() => {}, 'https://example.org/collect')));
test('sender uses fixed endpoint without cookies or redirects', async () => {
  let request;
  const send = createSender(async (...args) => { request = args; return { ok: true, json: async () => ({ success: true }) }; });
  await send({ contactName: 'Test' });
  assert.equal(request[0], intakeEndpoint); assert.equal(request[1].credentials, 'omit'); assert.equal(request[1].redirect, 'error');
});
test('HTTP rejection is not successful intake', async () => {
  const send = createSender(async () => ({ ok: false })); await assert.rejects(send({}));
});
