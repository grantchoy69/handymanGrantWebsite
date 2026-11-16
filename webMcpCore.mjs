/** Public website tools. No private-data reads or credentials. */
export const intakeEndpoint = 'https://habydccnoclsgoioabnp.supabase.co/functions/v1/submit-quote-request';
export const clientVersion = 'handymangrant-webmcp-v1';
export const noticeVersion = '2026-11-16-v1';
export const draftLifetimeMs = 30 * 60 * 1000;
export const minimumDraftAgeMs = 2500;
export const privacyNotice = 'Send the contact details and project description shown here to Handyman Grant LLC, stored in its Supabase lead inbox, so Grant can respond. This includes this site visit\'s referral/campaign information when available. Do not include passwords, payment details, or unrelated personal information. This is an inquiry, not a booking or price guarantee.';
const uuidPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
const own = (obj, key) => Object.prototype.hasOwnProperty.call(obj, key);
const fields = ['contactName', 'phone', 'email', 'zipCode', 'projectDescription'];
const limits = { contactName: 120, phone: 50, email: 320, zipCode: 20, projectDescription: 5000 };

function objectInput(input, allowed) {
  if (!input || typeof input !== 'object' || Array.isArray(input)) throw new Error('Expected an object.');
  if (Object.keys(input).some((key) => !allowed.includes(key))) throw new Error('Unexpected input field.');
}
export function validateQuote(input) {
  objectInput(input, fields);
  const result = {};
  for (const field of fields) {
    const raw = own(input, field) ? input[field] : '';
    if (typeof raw !== 'string') throw new Error(`${field} must be text.`);
    if (raw.length > limits[field]) throw new Error(`${field} is too long.`);
    if (/[\u0000-\u0008\u000b\u000c\u000e-\u001f\u007f]/.test(raw)) throw new Error('Unsupported control character.');
    result[field] = raw.trim();
  }
  result.email = result.email.toLowerCase();
  if (!result.contactName) throw new Error('Ask the customer for their name.');
  if (!result.phone && !result.email) throw new Error('Ask for a phone number or email address.');
  if (result.phone && !/^\+?[0-9().\s-]+$/.test(result.phone)) throw new Error('Please check the phone number.');
  if (result.phone && !/^[0-9]{7,15}$/.test(result.phone.replace(/\D/g, ''))) throw new Error('Please check the phone number.');
  if (result.email && !/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(result.email)) throw new Error('Please check the email address.');
  if (result.projectDescription.length < 10) throw new Error('Ask for a little more detail about the project.');
  return Object.freeze(result);
}

export function publicServices(data) {
  if (!data || !Array.isArray(data.services)) throw new Error('Service catalog is unavailable.');
  return data.services.filter((s) => /^[a-z0-9]+(?:-[a-z0-9]+)*$/.test(s.slug || '')).map((s) => ({
    slug: s.slug,
    name: String(s.serviceName || s.title || s.slug).slice(0, 200),
    description: String(s.metaDescription || s.hero?.subheadline || '').slice(0, 600),
    servicesIncluded: (Array.isArray(s.servicesIncluded) ? s.servicesIncluded : []).filter((v) => typeof v === 'string').slice(0, 20),
    pricingGuidance: String(s.pricingNote || '').slice(0, 1200),
    url: `https://handymangrant.com/landing-pages/${s.slug}.html`,
  }));
}
export function searchCatalog(services, query) {
  const noise = new Set(['a', 'an', 'and', 'the', 'for', 'in', 'of', 'my', 'near', 'me', 'please', 'need', 'repair', 'repairs', 'fix', 'work', 'san', 'diego']);
  const tokens = query.toLowerCase().match(/[a-z0-9]+/g) || [];
  const specific = tokens.filter((t) => !noise.has(t));
  const terms = specific.length ? specific : tokens;
  const aliases = { rusted: ['rust', 'welding'], rusty: ['rust', 'welding'], corroded: ['rust', 'welding'], corrosion: ['rust', 'welding'], handrails: ['handrail', 'railing'], handrail: ['handrail', 'railing'], bulb: ['light'], bulbs: ['light'] };
  const expanded = [...new Set(terms.flatMap((t) => aliases[t] || [t]))];
  return services.map((service, index) => {
    const title = `${service.name} ${service.slug}`.toLowerCase();
    const text = `${service.description} ${service.servicesIncluded.join(' ')}`.toLowerCase();
    const score = expanded.reduce((sum, word) => sum + (title.includes(word) ? 5 : text.includes(word) ? 2 : 0), 0);
    return { service, score, index };
  }).filter((x) => x.score > 0).sort((a, b) => b.score - a.score || a.index - b.index).slice(0, 3).map((x) => x.service);
}

const trim = (x, n = 500) => typeof x === 'string' ? x.slice(0, n) : '';
function cleanUrl(value) {
  try { const u = new URL(value); return ['https:', 'http:'].includes(u.protocol) ? `${u.origin}${u.pathname}`.slice(0, 1000) : ''; } catch { return ''; }
}
/** Read only approved attribution fields. Never take attribution from tool arguments. */
export function readAttribution({ href, referrer = '', title = '', language = '', storage, now = Date.now() }) {
  let stored;
  try {
    stored = JSON.parse(storage?.getItem('hgLeadAttributionV1') || 'null');
    const saved = Date.parse(stored?.savedAt || '');
    if (!Number.isFinite(saved) || now - saved > 90 * 86400000 || saved > now + 60000) stored = null;
  } catch { stored = null; }
  const keys = ['gclid', 'gbraid', 'wbraid', 'utm_source', 'utm_medium', 'utm_campaign', 'utm_term', 'utm_content'];
  const params = new URL(href).searchParams;
  const hasCampaign = keys.some((key) => Boolean(params.get(key)));
  const touch = hasCampaign ? null : stored?.lastTouch || stored?.firstTouch;
  const values = hasCampaign ? Object.fromEntries(keys.map((key) => [key, params.get(key)])) : touch?.values || {};
  const result = { landingPage: cleanUrl(href), pageTitle: trim(title, 300), referrer: cleanUrl(touch?.referrer || referrer), browserLanguage: trim(language, 50) };
  const mapping = { gclid: 'gclid', gbraid: 'gbraid', wbraid: 'wbraid', utm_source: 'utmSource', utm_medium: 'utmMedium', utm_campaign: 'utmCampaign', utm_term: 'utmTerm', utm_content: 'utmContent' };
  for (const [key, target] of Object.entries(mapping)) result[target] = trim(values[key]);
  return result;
}

export function createWebsiteTools(options) {
  const { config, loadCatalog, review, send, attribution = () => ({}), onAccepted = () => {}, now = () => Date.now(), uuid = () => crypto.randomUUID() } = options;
  const enabled = config.submissionsEnabled === true;
  let active = null;
  let pending = null;
  const completed = new Map();
  const newUuid = () => { const value = uuid(); if (!uuidPattern.test(value)) throw new Error('Secure request IDs unavailable.'); return value; };
  const preview = (draft) => ({
    status: 'draft', draftId: draft.draftId, request: { ...draft.request },
    recipient: 'Handyman Grant LLC', privacyNotice, expiresAt: new Date(draft.expiresAt).toISOString(),
    submissionsEnabled: enabled, submitted: false,
    nextStep: enabled ? 'Show these exact details and privacy notice to the customer. Only after approval, call submitQuoteRequest. A final on-page review dialog must also be approved.' : 'Submissions are disabled in this rollout. You may review the draft, but it cannot be sent through site tools.',
  });
  const success = (receipt) => ({ status: 'submitted', submitted: true, requestId: receipt.requestId, message: 'Quote request received by Handyman Grant. This is not a booking or a price guarantee.' });
  const fail = (message, status = 'notSubmitted') => ({ status, submitted: false, message });
  const expiredUnknown = () => fail('The earlier request may already have been received. Its draft has expired, so it cannot be resent or replaced here. Contact Grant to check before starting another request.', 'unknown');
  function expireActiveDraft() {
    if (!active || now() < active.expiresAt) return false;
    if (active.deliveryUncertain) {
      active.request = null;
      active.payload = null;
      active.expiredUncertain = true;
    } else active = null;
    return true;
  }
  const canceledResult = (draft, beforeSending = false) => draft.deliveryUncertain
    ? fail('This retry was canceled. The earlier request may already have been received. Retry the original draft before it expires, or contact Grant to check.', 'unknown')
    : fail(beforeSending ? 'Request canceled before sending.' : 'Canceled. Nothing was sent.', 'canceled');
  async function submit(args, { signal } = {}) {
    objectInput(args, ['draftId']);
    if (typeof args.draftId !== 'string') throw new Error('A draftId is required.');
    if (completed.has(args.draftId)) return success(completed.get(args.draftId));
    if (!enabled) return fail('Site-tool submissions are disabled. No request was sent.', 'disabled');
    if (pending && active?.draftId === args.draftId) return pending;
    const expired = expireActiveDraft();
    if (active?.expiredUncertain) return expiredUnknown();
    if (expired) return fail('Draft expired. Prepare and review a new draft.', 'expired');
    if (!active || active.draftId !== args.draftId) return fail('Prepare a new draft on this page and review it first.', 'invalidDraft');
    if (signal?.aborted) return canceledResult(active, true);
    const draft = active;
    pending = (async () => {
      try {
        const approved = await review({ ...preview(draft), noticeVersion }, signal);
        if (now() >= draft.expiresAt) {
          expireActiveDraft();
          return draft.deliveryUncertain ? expiredUnknown() : fail('Draft expired. Prepare and review a new draft.', 'expired');
        }
        if (!approved || signal?.aborted) return canceledResult(draft);
        if (active !== draft || now() >= draft.expiresAt) return fail('The draft changed or expired. Review a fresh draft.', 'expired');
        if (now() - draft.createdAt < minimumDraftAgeMs) return fail('Please allow a moment to review the details, then try again.', 'reviewTooFast');
        if (!draft.payload) {
          // Freeze attribution with the first confirmed attempt, so retries send the same request.
          draft.payload = Object.freeze({
            ...attribution(), ...draft.request,
            transactionId: draft.transactionId, contactMethod: 'aiAgent',
            formStartedAt: new Date(draft.createdAt).toISOString(), website: '',
            sourceMetadata: { clientVersion, intakeInterface: 'webmcp', consent: { noticeVersion, confirmedAt: new Date(now()).toISOString(), method: 'onPageReview' } },
          });
        }
        draft.deliveryUncertain = true;
        const receipt = await send(draft.payload, signal);
        if (!receipt || receipt.success !== true || typeof receipt.requestId !== 'string' || !uuidPattern.test(receipt.requestId) || receipt.transactionId !== draft.transactionId) {
          return fail('Receipt could not be verified. The request may have been received; retry this same draft rather than creating another.', 'unknown');
        }
        draft.deliveryUncertain = false;
        // Save the receipt before analytics. A blocked tag must not turn success into failure.
        completed.set(draft.draftId, receipt);
        if (completed.size > 20) completed.delete(completed.keys().next().value);
        try { onAccepted(receipt, draft.payload); } catch { /* Analytics is best effort. */ }
        return success(receipt);
      } catch (error) {
        return fail(error?.name === 'AbortError' ? 'Interrupted. The request may have been received; retry this same draft to avoid duplicates.' : 'Could not verify delivery. Retry this same draft, or contact Grant directly.', 'unknown');
      } finally { pending = null; }
    })();
    return pending;
  }
  const quoteSchema = {
    type: 'object', additionalProperties: false,
    properties: Object.fromEntries(fields.map((key) => [key, { type: 'string', maxLength: limits[key], description: ({ contactName: 'Customer-provided name. Do not invent it.', phone: 'Customer-provided phone, or omit.', email: 'Customer-provided email, or omit.', zipCode: 'Job ZIP code, if known. Street address is not needed.', projectDescription: 'Customer-approved project description. No unrelated personal or payment information.' })[key] }])),
    required: ['contactName', 'projectDescription'],
  };
  return [
    {
      name: 'getBusinessInfo', title: 'Handyman Grant business information',
      description: 'Read public contact details, service area and quote workflow. Does not access customer, job, or financial records.',
      inputSchema: { type: 'object', properties: {}, additionalProperties: false },
      annotations: { readOnlyHint: true, consequentialHint: false },
      execute: async (args = {}) => { objectInput(args, []); return { ...config.business, submissionsEnabled: enabled, existingCustomerRecordsAccessible: false }; },
    },
    {
      name: 'searchServices', title: 'Find a Handyman Grant service',
      description: 'Search the public services.json catalog. Prices are indicative, not a quote or booking. Grant decides scope and referral options.',
      inputSchema: { type: 'object', properties: { query: { type: 'string', minLength: 1, maxLength: 200 } }, required: ['query'], additionalProperties: false },
      annotations: { readOnlyHint: true, consequentialHint: false },
      execute: async (args) => {
        objectInput(args, ['query']);
        if (typeof args.query !== 'string' || !args.query.trim() || args.query.length > 200) throw new Error('Enter a short service query.');
        return { services: searchCatalog(publicServices(await loadCatalog()), args.query.trim()), pricingIsIndicative: true, unlistedProjectsCanBeReviewed: true };
      },
    },
    {
      name: 'prepareQuoteRequest', title: 'Draft a quote inquiry (does not send)',
      description: 'Draft locally using only details the customer supplied. Show the complete returned request and privacy notice for approval. Does not submit a lead, reserve an appointment, or send contact details to the server. Use the regular quote form to attach photos; site-tool requests contain text only.',
      inputSchema: quoteSchema,
      annotations: { readOnlyHint: false, consequentialHint: false, untrustedContentHint: true },
      execute: async (args) => {
        if (pending) throw new Error('A request is being reviewed or sent. Finish it before preparing another.');
        expireActiveDraft();
        if (active?.expiredUncertain) return expiredUnknown();
        const request = validateQuote(args);
        if (active?.deliveryUncertain && JSON.stringify(active.request) !== JSON.stringify(request)) {
          return fail('The earlier request may already have been received. Do not replace its details. Retry the original draft before it expires, or contact Grant to check.', 'unknown');
        }
        if (active && active.expiresAt > now() && JSON.stringify(active.request) === JSON.stringify(request)) {
          return completed.has(active.draftId) ? success(completed.get(active.draftId)) : preview(active);
        }
        active = { request, draftId: `HG-DRAFT-${newUuid()}`, transactionId: `HG-LEAD-${newUuid()}`, createdAt: now(), expiresAt: now() + draftLifetimeMs, payload: null, deliveryUncertain: false, expiredUncertain: false };
        return preview(active);
      },
    },
    {
      name: 'submitQuoteRequest', title: 'Submit quote request to Handyman Grant',
      description: 'Send the exact prepared quote inquiry and contact details after the customer approves them. Opens a final on-page review with Send and Cancel. Creates a lead in Handyman Grant\'s inbox; not a booking. Disabled during the review-only rollout.',
      inputSchema: { type: 'object', properties: { draftId: { type: 'string', maxLength: 60, description: 'ID returned by prepareQuoteRequest on this open page.' } }, required: ['draftId'], additionalProperties: false },
      annotations: { readOnlyHint: false, consequentialHint: true, untrustedContentHint: true },
      execute: submit,
    },
  ];
}

export function createSender(fetchFn, endpoint = intakeEndpoint) {
  if (endpoint !== intakeEndpoint) throw new Error('Unrecognized quote endpoint.');
  return async (payload, signal) => {
    const controller = new AbortController();
    const abort = () => controller.abort();
    if (signal?.aborted) controller.abort();
    signal?.addEventListener('abort', abort, { once: true });
    const timer = setTimeout(abort, 15000);
    try {
      const response = await fetchFn(endpoint, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(payload), signal: controller.signal, credentials: 'omit', redirect: 'error' });
      if (!response.ok) throw new Error('Quote intake rejected the request.');
      return await response.json();
    } finally { clearTimeout(timer); signal?.removeEventListener('abort', abort); }
  };
}
