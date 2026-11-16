import { createWebsiteTools, createSender, readAttribution, intakeEndpoint } from './webMcpCore.mjs';

const context = document.modelContext;
// Progressive enhancement: no DOM changes or extra data requests without support.
if (window.isSecureContext && window.top === window && typeof context?.registerTool === 'function') {
  start().catch(() => console.warn('Handyman Grant site tools unavailable; the regular quote form still works.'));
}

async function start() {
  if (window.__hgSiteToolsRegistered) return;
  window.__hgSiteToolsRegistered = true;
  const base = new URL('.', import.meta.url);
  const response = await fetch(new URL('webMcpConfig.json', base), { credentials: 'omit', cache: 'no-store' });
  if (!response.ok) throw new Error('Configuration unavailable.');
  const config = await response.json();
  if (config.enabled !== true || config.endpoint !== intakeEndpoint) return;
  const local = ['localhost', '127.0.0.1', '[::1]'].includes(location.hostname);
  if (!local && !['https://handymangrant.com', 'https://www.handymangrant.com'].includes(location.origin)) return;
  let catalog;
  const tools = createWebsiteTools({
    config,
    loadCatalog: async () => {
      if (catalog) return catalog;
      const result = await fetch(new URL('services.json', base), { credentials: 'omit' });
      if (!result.ok) throw new Error('Service catalog unavailable. Use the service pages.');
      catalog = await result.json();
      return catalog;
    },
    review: reviewQuote,
    send: createSender(window.fetch.bind(window), config.endpoint),
    attribution: () => {
      let storage;
      try { storage = window.localStorage; } catch { /* Storage may be blocked. */ }
      return readAttribution({ href: location.href, referrer: document.referrer, title: document.title, language: navigator.language, storage });
    },
    onAccepted: (receipt) => {
      if (typeof window.gtag !== 'function') return;
      // No contact data, description, ZIP, or draft is included in analytics.
      window.gtag('event', 'conversion', { send_to: 'AW-17749862714/nReaCO3k_8YbELrS5Y9C', transaction_id: receipt.transactionId, value: 1, currency: 'USD' });
      window.gtag('event', 'generate_lead', { send_to: 'G-65Z3SGSQP3', method: 'webmcp' });
    },
  });
  const lifetime = new AbortController();
  try {
    for (const tool of tools) await context.registerTool(tool, { signal: lifetime.signal });
  } catch (error) {
    lifetime.abort();
    throw error;
  }
}

function reviewQuote(draft, signal) {
  return new Promise((resolve, reject) => {
    if (signal?.aborted) { resolve(false); return; }
    const dialog = document.createElement('dialog');
    if (typeof dialog.showModal !== 'function') { reject(new Error('Review dialog unsupported. Use the normal form.')); return; }
    dialog.id = 'hgQuoteReview';
    dialog.setAttribute('aria-labelledby', 'hgQuoteReviewTitle');
    dialog.setAttribute('aria-describedby', 'hgQuoteReviewNotice');
    dialog.style.cssText = 'box-sizing:border-box;width:min(560px,92vw);max-height:88vh;overflow:auto;border:1px solid #666;border-radius:14px;padding:24px;background:#202020;color:#f5f5f5;font:16px/1.55 system-ui,sans-serif;';
    const previousFocus = document.activeElement;
    const addText = (tag, text, id) => {
      const element = document.createElement(tag);
      element.textContent = text;
      if (id) element.id = id;
      dialog.appendChild(element);
      return element;
    };
    addText('h2', 'Send this quote request?', 'hgQuoteReviewTitle').style.marginTop = '0';
    addText('p', 'Recipient: Handyman Grant LLC');
    addText('p', 'No photos are included in this request. To attach photos, use the regular quote form.');
    const labels = { contactName: 'Name', phone: 'Phone', email: 'Email', zipCode: 'ZIP code', projectDescription: 'Project' };
    for (const [key, value] of Object.entries(draft.request)) {
      if (!value) continue;
      addText('strong', labels[key] || key);
      const p = addText('p', value);
      p.style.cssText = 'margin:4px 0 16px;white-space:pre-wrap;overflow-wrap:anywhere;';
    }
    addText('p', draft.privacyNotice, 'hgQuoteReviewNotice').style.fontSize = '14px';
    const actions = document.createElement('div');
    actions.style.cssText = 'display:flex;gap:12px;flex-wrap:wrap;margin-top:20px;';
    const cancel = document.createElement('button');
    cancel.type = 'button'; cancel.textContent = 'Cancel';
    const send = document.createElement('button');
    send.type = 'button'; send.textContent = 'Send quote request';
    for (const button of [cancel, send]) {
      button.style.cssText = 'min-height:44px;border-radius:8px;padding:10px 16px;font:600 16px system-ui;cursor:pointer;border:1px solid #999;';
    }
    send.style.background = '#800020'; send.style.color = 'white';
    actions.append(cancel, send); dialog.appendChild(actions);
    let finished = false;
    const finish = (value) => {
      if (finished) return;
      finished = true;
      signal?.removeEventListener('abort', onAbort);
      observer.disconnect();
      if (dialog.open) dialog.close();
      dialog.remove();
      if (previousFocus?.isConnected && typeof previousFocus.focus === 'function') previousFocus.focus({ preventScroll: true });
      resolve(value);
    };
    const onAbort = () => finish(false);
    // A script-dispatched click must not silently authorize sending personal details.
    send.addEventListener('click', (event) => { if (event.isTrusted) finish(true); });
    cancel.addEventListener('click', () => finish(false));
    dialog.addEventListener('cancel', (event) => { event.preventDefault(); finish(false); });
    dialog.addEventListener('close', () => finish(false));
    const observer = new MutationObserver(() => { if (!dialog.isConnected) finish(false); });
    document.body.appendChild(dialog);
    observer.observe(document.body, { childList: true });
    signal?.addEventListener('abort', onAbort, { once: true });
    dialog.showModal();
    cancel.focus();
  });
}
