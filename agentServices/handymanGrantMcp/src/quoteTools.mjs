import { createCipheriv, createDecipheriv, createHash, randomBytes, randomUUID } from 'node:crypto';
import { readFileSync } from 'node:fs';

export const business = JSON.parse(readFileSync(new URL('../data/business.json', import.meta.url), 'utf8'));
export const catalog = JSON.parse(readFileSync(new URL('../data/serviceCatalog.json', import.meta.url), 'utf8'));
export const intakeUrl = 'https://habydccnoclsgoioabnp.supabase.co/functions/v1/submit-quote-request';
const draftLifetimeMs = 24 * 60 * 60 * 1000;
const transactionPattern = /^HG-LEAD-[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
const serviceSlugs = catalog.services.map(service => service.slug);

export class ToolError extends Error {
  constructor(code, message) { super(message); this.code = code; }
}

const stringSchema = (maxLength, description, minLength = 1) => ({ type: 'string', minLength, maxLength, description });
const objectSchema = (properties, required = []) => ({ type: 'object', properties, required, additionalProperties: false });
const readAnnotations = { readOnlyHint: true, destructiveHint: false, idempotentHint: true, openWorldHint: false };

export const tools = [
  {
    name: 'getBusinessInfo', title: 'Get Handyman Grant contact and service-area details',
    description: 'Read public business contact details, service-area examples, and inquiry guidance. Does not access customers or provide live scheduling.',
    inputSchema: objectSchema({}), annotations: readAnnotations,
  },
  {
    name: 'searchServices', title: 'Search Handyman Grant services',
    description: 'Find published service descriptions, examples of scope, indicative pricing, and relevant pages. Unlisted or larger projects may still be worth asking Grant about; do not promise acceptance or reject them automatically.',
    inputSchema: objectSchema({ query: stringSchema(300, 'Optional service or problem, for example rusted handrail, gate, door or drywall.', 0) }), annotations: readAnnotations,
  },
  {
    name: 'prepareQuoteRequest', title: 'Draft a quote inquiry for customer review',
    description: 'Prepare a draft without contacting Grant or storing a lead. Use only customer-provided details. Show the returned summary to the customer, state that it will be shared with Handyman Grant, and ask permission before submitQuoteRequest. The opaque confirmation token must not be shown in a public link or log.',
    inputSchema: objectSchema({
      contactName: stringSchema(120, 'Customer-provided name.'),
      phone: stringSchema(50, 'Phone number supplied by the customer. Phone or email is required.'),
      email: stringSchema(320, 'Email address supplied by the customer. Phone or email is required.'),
      zipCode: stringSchema(10, 'Optional US project ZIP code; a street address is not needed.'),
      serviceSlug: { type: 'string', enum: [...serviceSlugs, 'other'], description: 'Optional service category; other preserves unusual inquiries for owner review.' },
      projectDescription: stringSchema(5000, 'Customer-approved project description. Do not include secrets or unrelated personal data.', 10),
      preferredContactMethod: { type: 'string', enum: ['phone', 'text', 'email', 'either'], description: 'How the customer would prefer Grant to respond. Defaults to either.' },
    }, ['contactName', 'projectDescription']),
    annotations: { readOnlyHint: true, destructiveHint: false, idempotentHint: false, openWorldHint: false },
  },
  {
    name: 'submitQuoteRequest', title: 'Send the reviewed quote request to Handyman Grant',
    description: 'WRITE ACTION. Only call after the customer explicitly approves the exact summary returned by prepareQuoteRequest. Submits their contact details and project description to Handyman Grant. This requests a quote; it does not send email, book a visit, accept a price, or promise a response time. For retries use the SAME confirmationToken. Never claim success unless success=true is returned. Preview deployments refuse writes.',
    inputSchema: objectSchema({
      confirmationToken: stringSchema(40000, 'Opaque, unmodified token from prepareQuoteRequest.'),
      customerConfirmed: { type: 'boolean', const: true, description: 'True only after the customer explicitly approved sharing this exact inquiry with Handyman Grant.' },
    }, ['confirmationToken', 'customerConfirmed']),
    annotations: { readOnlyHint: false, destructiveHint: false, idempotentHint: true, openWorldHint: true },
  },
];

function validateObject(value, allowed) {
  if (!value || typeof value !== 'object' || Array.isArray(value)) throw new ToolError('invalidInput', 'Expected a JSON object.');
  if (Object.keys(value).some(key => !allowed.includes(key))) throw new ToolError('invalidInput', 'The request contains unsupported fields.');
}
function text(value, field, max, min = 1, required = false) {
  if (value === undefined && !required) return undefined;
  if (typeof value !== 'string') throw new ToolError('invalidInput', `${field} must be text.`);
  const result = value.trim();
  if (result.length < min || result.length > max || /[\u0000-\u0008\u000b\u000c\u000e-\u001f]/.test(result)) {
    throw new ToolError('invalidInput', `${field} has an invalid length or control character.`);
  }
  return result;
}
export function validateQuote(input) {
  validateObject(input, ['contactName', 'phone', 'email', 'zipCode', 'serviceSlug', 'projectDescription', 'preferredContactMethod']);
  const quote = {
    contactName: text(input.contactName, 'contactName', 120, 1, true),
    phone: text(input.phone, 'phone', 50),
    email: text(input.email, 'email', 320)?.toLowerCase(),
    zipCode: text(input.zipCode, 'zipCode', 10),
    serviceSlug: input.serviceSlug ?? 'other',
    projectDescription: text(input.projectDescription, 'projectDescription', 5000, 10, true),
    preferredContactMethod: input.preferredContactMethod ?? 'either',
  };
  if (!quote.phone && !quote.email) throw new ToolError('invalidInput', 'Provide a phone number or email supplied by the customer.');
  if (quote.phone && !/^\+?[\d\s().-]+$/.test(quote.phone)) throw new ToolError('invalidInput', 'Please check the phone number.');
  if (quote.phone && !/^\d{7,15}$/.test(quote.phone.replace(/\D/g, ''))) throw new ToolError('invalidInput', 'Please check the phone number.');
  if (quote.email && !/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(quote.email)) throw new ToolError('invalidInput', 'Please check the email address.');
  if (quote.zipCode && !/^\d{5}(?:-\d{4})?$/.test(quote.zipCode)) throw new ToolError('invalidInput', 'Please use a five-digit ZIP or ZIP+4.');
  if (![...serviceSlugs, 'other'].includes(quote.serviceSlug)) throw new ToolError('invalidInput', 'Use a listed service category or other.');
  if (!['phone', 'text', 'email', 'either'].includes(quote.preferredContactMethod)) throw new ToolError('invalidInput', 'Invalid preferred contact method.');
  if (['phone', 'text'].includes(quote.preferredContactMethod) && !quote.phone) throw new ToolError('invalidInput', 'A phone number is required for phone or text contact.');
  if (quote.preferredContactMethod === 'email' && !quote.email) throw new ToolError('invalidInput', 'An email address is required for email contact.');
  return JSON.parse(JSON.stringify(quote));
}

export function createQuoteTools({ secret, enabled = false, endpoint = intakeUrl, fetchImpl = fetch, now = Date.now, submissionLimit = 6, upstreamTimeoutMs = 12000 }) {
  if (typeof secret !== 'string' || secret.length < 32 || secret.startsWith('REPLACE_')) throw new Error('Set DRAFT_ENCRYPTION_SECRET to a random secret of at least 32 characters.');
  // No caller can select a fetch target, preventing arbitrary outbound requests.
  if (endpoint !== intakeUrl) throw new Error('QUOTE_INTAKE_URL must be the reviewed Handyman Grant intake endpoint.');
  const key = createHash('sha256').update(secret).digest();
  const pending = new Map();
  const completed = new Map();
  let attempts = [];
  let upstreamCooldownUntil = 0;

  function seal(value) {
    const iv = randomBytes(12);
    const cipher = createCipheriv('aes-256-gcm', key, iv);
    cipher.setAAD(Buffer.from('handyman-grant-quote-v1'));
    const encrypted = Buffer.concat([cipher.update(JSON.stringify(value), 'utf8'), cipher.final()]);
    return ['v1', iv.toString('base64url'), encrypted.toString('base64url'), cipher.getAuthTag().toString('base64url')].join('.');
  }
  function open(token) {
    if (typeof token !== 'string' || token.length > 40000) throw new ToolError('invalidDraft', 'Prepare a new quote draft; this token is invalid.');
    try {
      const parts = token.split('.');
      if (parts.length !== 4 || parts[0] !== 'v1' || !parts.slice(1).every(p => /^[A-Za-z0-9_-]+$/.test(p))) throw new Error();
      const [, ivText, encryptedText, tagText] = parts;
      const iv = Buffer.from(ivText, 'base64url');
      const tag = Buffer.from(tagText, 'base64url');
      if (iv.length !== 12 || tag.length !== 16) throw new Error();
      const decipher = createDecipheriv('aes-256-gcm', key, iv);
      decipher.setAAD(Buffer.from('handyman-grant-quote-v1'));
      decipher.setAuthTag(tag);
      const decoded = Buffer.concat([decipher.update(Buffer.from(encryptedText, 'base64url')), decipher.final()]);
      const draft = JSON.parse(decoded.toString('utf8'));
      if (!transactionPattern.test(draft.transactionId) || !Number.isSafeInteger(draft.createdAt) || !Number.isSafeInteger(draft.expiresAt)) throw new Error();
      if (draft.createdAt > now() + 60000 || draft.expiresAt - draft.createdAt !== draftLifetimeMs) throw new Error();
      if (draft.expiresAt <= now()) throw new ToolError('draftExpired', 'This draft expired. Check whether the prior request arrived before creating a replacement.');
      draft.quote = validateQuote(draft.quote);
      return draft;
    } catch (error) {
      if (error instanceof ToolError) throw error;
      throw new ToolError('invalidDraft', 'This draft token is invalid or has been changed.');
    }
  }
  function prepare(input) {
    const quote = validateQuote(input);
    const createdAt = now();
    const draft = { transactionId: `HG-LEAD-${randomUUID()}`, createdAt, expiresAt: createdAt + draftLifetimeMs, quote };
    return {
      success: true, status: 'draft', submitted: false,
      summary: { recipient: business.name, ...quote },
      privacyNotice: 'Submission shares this contact information and project description with Handyman Grant for this inquiry. It does not subscribe the customer to marketing.',
      nextStep: 'Show this summary to the customer and get explicit permission before submitting.',
      confirmationToken: seal(draft), expiresAt: new Date(draft.expiresAt).toISOString(),
      submissionsEnabled: enabled,
    };
  }
  async function post(draft) {
    const currentTime = now();
    if (currentTime < upstreamCooldownUntil) throw new ToolError('rateLimited', 'Intake is temporarily rate-limited. Keep this same draft for a later retry or use the public contact details.');
    attempts = attempts.filter(at => currentTime - at < 3600000);
    if (attempts.length >= submissionLimit) throw new ToolError('rateLimited', 'The agent intake pilot has reached its hourly limit. Keep the same draft for a later retry.');
    attempts.push(currentTime);
    const q = draft.quote;
    const payload = {
      transactionId: draft.transactionId,
      contactName: q.contactName,
      ...(q.phone ? { phone: q.phone } : {}),
      ...(q.email ? { email: q.email } : {}),
      ...(q.zipCode ? { zipCode: q.zipCode } : {}),
      projectDescription: q.projectDescription,
      contactMethod: 'aiAgent',
      utmSource: 'mcp', utmMedium: 'agent',
      sourceMetadata: {
        interfaceSource: 'handymanGrantMcp', adapterVersion: '0.1.0',
        preferredContactMethod: q.preferredContactMethod, serviceSlug: q.serviceSlug,
        customerConfirmationAsserted: true,
        draftPreparedAt: new Date(draft.createdAt).toISOString(),
        submittedByAdapterAt: new Date(currentTime).toISOString(),
      },
    };
    let response;
    let result;
    try {
      response = await fetchImpl(endpoint, {
        method: 'POST', headers: { 'Content-Type': 'application/json', Accept: 'application/json' },
        body: JSON.stringify(payload), signal: AbortSignal.timeout(upstreamTimeoutMs), redirect: 'error',
      });
      // Only the fixed, reviewed endpoint is contacted. Do not forward arbitrary credentials,
      // Origin, x-forwarded-for, or other client headers to it.
      const body = await response.text();
      if (Buffer.byteLength(body) > 65536) throw new Error();
      try { result = JSON.parse(body); } catch { result = null; }
    } catch {
      throw new ToolError('submissionUnconfirmed', 'No confirmation was received. The request may have arrived. Retry only with this SAME confirmation token; do not create another draft automatically.');
    }
    if (response.status === 429) {
      upstreamCooldownUntil = now() + 3600000;
      throw new ToolError('rateLimited', 'The shared intake endpoint has reached its limit. Keep the same draft for a later retry.');
    }
    if (!response.ok || result?.success !== true || result?.transactionId !== draft.transactionId || typeof result?.requestId !== 'string' || !result.requestId || result.requestId.length > 200) {
      throw new ToolError('submissionUnconfirmed', 'The intake endpoint did not confirm this request. Do not claim it was received. Retain this same token for a retry.');
    }
    const receipt = {
      success: true, status: 'received', requestId: result.requestId,
      transactionId: draft.transactionId, duplicateSubmission: result.duplicateSubmission === true,
      message: 'Handyman Grant received the quote request. This is not a booking or a confirmed price. No email delivery is claimed.',
    };
    completed.set(draft.transactionId, { expiresAt: draft.expiresAt, receipt });
    return receipt;
  }
  async function submit(input) {
    validateObject(input, ['confirmationToken', 'customerConfirmed']);
    if (input.customerConfirmed !== true) throw new ToolError('confirmationRequired', 'The customer must explicitly approve the exact draft before it is submitted.');
    const draft = open(input.confirmationToken);
    if (!enabled) throw new ToolError('previewOnly', 'This MCP deployment is in preview mode. No quote was submitted. Use the public website or phone to contact Grant.');
    for (const [id, entry] of completed) if (entry.expiresAt <= now()) completed.delete(id);
    const cached = completed.get(draft.transactionId);
    if (cached) return { ...cached.receipt, duplicateSubmission: true };
    if (pending.has(draft.transactionId)) return pending.get(draft.transactionId);
    // Upstream durable transaction IDs make retries safe after a server restart too.
    const operation = post(draft).finally(() => pending.delete(draft.transactionId));
    pending.set(draft.transactionId, operation);
    return operation;
  }
  async function call(name, args = {}) {
    switch (name) {
      case 'getBusinessInfo':
        validateObject(args, []);
        return { ...business, submissionsEnabled: enabled, existingCustomerRecordsAccessible: false };
      case 'searchServices': {
        validateObject(args, ['query']);
        const query = text(args.query, 'query', 300, 0) ?? '';
        const terms = query.toLowerCase().match(/[a-z0-9]+/g) ?? [];
        const scored = catalog.services.map(service => ({ service, score: terms.reduce((n, term) => n + Number(JSON.stringify(service).toLowerCase().includes(term)), 0) }));
        const services = terms.length ? scored.filter(row => row.score > 0).sort((a, b) => b.score - a.score).map(row => row.service) : catalog.services;
        return { services, pricingIsIndicative: true, unlistedProjectsCanBeSubmitted: true, scopeDecision: 'Grant reviews the specific job and any referral options.', snapshotDate: catalog.snapshotDate };
      }
      case 'prepareQuoteRequest': return prepare(args);
      case 'submitQuoteRequest': return submit(args);
      default: throw new ToolError('unknownTool', 'Unknown tool.');
    }
  }
  return { call, enabled };
}
