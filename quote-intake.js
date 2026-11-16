(() => {
  "use strict";

  const ENDPOINT =
    "https://habydccnoclsgoioabnp.supabase.co/functions/v1/submit-quote-request";
  const ATTRIBUTION_STORAGE_KEY = "hgLeadAttributionV1";
  const ATTRIBUTION_TTL_MS = 90 * 24 * 60 * 60 * 1000;
  const GOOGLE_ADS_CONVERSION =
    "AW-17749862714/nReaCO3k_8YbELrS5Y9C";
  const GA4_DESTINATION = "G-65Z3SGSQP3";
  const FORM_SELECTOR = "form[data-hg-quote-form], #contact form";
  const CLIENT_VERSION = "handymangrant-web-quote-v1";
  const MAX_PHOTOS = 5;
  const MAX_PHOTO_BYTES = 5 * 1024 * 1024;
  const MAX_TOTAL_PHOTO_BYTES = 20 * 1024 * 1024;
  const PHOTO_TYPES = new Set(["image/jpeg", "image/png", "image/webp", "image/heic", "image/heif"]);

  const ATTRIBUTION_KEYS = [
    "gclid",
    "gbraid",
    "wbraid",
    "utm_source",
    "utm_medium",
    "utm_campaign",
    "utm_term",
    "utm_content",
  ];

  function nowIso() {
    return new Date().toISOString();
  }

  function safeJsonParse(value) {
    try {
      return JSON.parse(value);
    } catch {
      return null;
    }
  }

  // Storage may be unavailable in private browsing or when browser policy blocks it.
  function readAttributionStorage() {
    try {
      return window.localStorage.getItem(ATTRIBUTION_STORAGE_KEY);
    } catch {
      return null;
    }
  }

  function saveAttributionStorage(value) {
    try {
      window.localStorage.setItem(ATTRIBUTION_STORAGE_KEY, JSON.stringify(value));
    } catch {
      // Attribution remains available in memory for this page.
    }
  }

  function getStoredAttribution() {
    const raw = readAttributionStorage();
    const parsed = raw ? safeJsonParse(raw) : null;

    if (!parsed || typeof parsed !== "object") {
      return null;
    }

    const savedAt = new Date(parsed.savedAt || 0).getTime();
    if (!Number.isFinite(savedAt) || Date.now() - savedAt > ATTRIBUTION_TTL_MS) {
      try {
        window.localStorage.removeItem(ATTRIBUTION_STORAGE_KEY);
      } catch {
        // Ignore blocked storage; the form must remain usable.
      }
      return null;
    }

    return parsed;
  }

  function readCurrentAttribution() {
    const params = new URLSearchParams(window.location.search);
    const values = {};

    for (const key of ATTRIBUTION_KEYS) {
      const value = params.get(key);
      if (value) {
        values[key] = value.slice(0, 500);
      }
    }

    return {
      capturedAt: nowIso(),
      landingPage: window.location.href.slice(0, 1000),
      path: `${window.location.pathname}${window.location.search}`.slice(0, 1000),
      referrer: document.referrer.slice(0, 1000),
      pageTitle: document.title.slice(0, 300),
      values,
    };
  }

  function hasCampaignData(touch) {
    if (!touch || !touch.values) {
      return false;
    }

    return ATTRIBUTION_KEYS.some((key) => Boolean(touch.values[key]));
  }

  function captureAttribution() {
    const current = readCurrentAttribution();
    const stored = getStoredAttribution();

    const next = stored || {
      savedAt: nowIso(),
      firstTouch: current,
      lastTouch: current,
    };

    if (!stored) {
      saveAttributionStorage(next);
      return next;
    }

    if (hasCampaignData(current)) {
      next.lastTouch = current;
      next.savedAt = nowIso();
      saveAttributionStorage(next);
    }

    return next;
  }

  function activeTouch(attribution) {
    if (!attribution) {
      return readCurrentAttribution();
    }

    return attribution.lastTouch || attribution.firstTouch || readCurrentAttribution();
  }

  function createUuid() {
    if (window.crypto && typeof window.crypto.randomUUID === "function") {
      return window.crypto.randomUUID();
    }

    const bytes = new Uint8Array(16);
    window.crypto.getRandomValues(bytes);
    bytes[6] = (bytes[6] & 0x0f) | 0x40;
    bytes[8] = (bytes[8] & 0x3f) | 0x80;

    const hex = Array.from(bytes, (byte) => byte.toString(16).padStart(2, "0"));
    return [
      hex.slice(0, 4).join(""),
      hex.slice(4, 6).join(""),
      hex.slice(6, 8).join(""),
      hex.slice(8, 10).join(""),
      hex.slice(10, 16).join(""),
    ].join("-");
  }

  function createTransactionId() {
    return `HG-LEAD-${createUuid()}`;
  }

  function ensureFormHelpers(form) {
    form.dataset.hgQuoteForm = "true";

    if (!form.dataset.hgFormStartedAt) {
      form.dataset.hgFormStartedAt = nowIso();
    }

    let honeypot = form.querySelector("[data-hg-honeypot]");
    if (!honeypot) {
      const wrapper = document.createElement("div");
      wrapper.setAttribute("aria-hidden", "true");
      wrapper.style.position = "absolute";
      wrapper.style.left = "-10000px";
      wrapper.style.width = "1px";
      wrapper.style.height = "1px";
      wrapper.style.overflow = "hidden";

      const label = document.createElement("label");
      label.textContent = "Website";

      honeypot = document.createElement("input");
      honeypot.type = "text";
      honeypot.name = "website";
      honeypot.tabIndex = -1;
      honeypot.autocomplete = "off";
      honeypot.dataset.hgHoneypot = "true";

      label.appendChild(honeypot);
      wrapper.appendChild(label);
      form.appendChild(wrapper);
    }

    let status = form.querySelector("[data-hg-form-status]");
    if (!status) {
      status = document.createElement("p");
      status.dataset.hgFormStatus = "true";
      status.setAttribute("role", "status");
      status.setAttribute("aria-live", "polite");
      status.style.minHeight = "1.5em";
      status.style.margin = ".25rem 0 0";
      form.appendChild(status);
    }

    return status;
  }

  function setStatus(statusElement, message, state) {
    statusElement.textContent = message;
    statusElement.dataset.state = state;
    statusElement.style.color = state === "success" ? "#8fe39a" : "#ffd0d0";
  }

  function clearStatus(statusElement) {
    statusElement.textContent = "";
    statusElement.dataset.state = "idle";
  }

  function initializePhotos(form, statusElement) {
    const input = form.querySelector("[data-hg-photo-input]");
    const list = form.querySelector("[data-hg-photo-list]");
    const count = form.querySelector("[data-hg-photo-count]");
    const photos = [];
    let busy = false;

    function revoke(photo) {
      if (photo.url) {
        window.URL.revokeObjectURL(photo.url);
        photo.url = null;
      }
    }

    function render() {
      if (!list) return;
      list.textContent = "";
      for (const photo of photos) {
        const item = document.createElement("li");
        item.className = "quote-photo";
        const fallback = document.createElement("span");
        fallback.className = "quote-photo-fallback";
        fallback.textContent = "Photo";
        item.appendChild(fallback);
        if (photo.url) {
          const image = document.createElement("img");
          image.src = photo.url;
          image.alt = "Preview of " + photo.file.name;
          image.loading = "lazy";
          fallback.hidden = true;
          image.addEventListener("error", () => {
            image.hidden = true;
            fallback.hidden = false;
            revoke(photo);
          }, { once: true });
          item.appendChild(image);
        }
        const name = document.createElement("span");
        name.className = "quote-photo-name";
        name.textContent = photo.file.name;
        item.appendChild(name);
        const remove = document.createElement("button");
        remove.type = "button";
        remove.className = "quote-photo-remove";
        remove.textContent = "Remove";
        remove.setAttribute("aria-label", "Remove " + photo.file.name);
        remove.disabled = busy;
        remove.addEventListener("click", () => {
          if (busy) return;
          const index = photos.indexOf(photo);
          if (index < 0) return;
          revoke(photo);
          photos.splice(index, 1);
          render();
        });
        item.appendChild(remove);
        list.appendChild(item);
      }
      if (count) count.textContent = photos.length ? `${photos.length} of ${MAX_PHOTOS} photos selected` : "No photos selected";
    }

    if (input) {
      input.disabled = false;
      input.addEventListener("change", () => {
        if (busy) return;
        const incoming = Array.from(input.files || []);
        input.value = "";
        const additions = incoming.filter(file => !photos.some(photo =>
          photo.file.name === file.name && photo.file.size === file.size && photo.file.lastModified === file.lastModified));
        let error = "";
        if (photos.length + additions.length > MAX_PHOTOS) error = "Please choose no more than 5 photos in total.";
        for (const file of additions) {
          const supported = file.type ? PHOTO_TYPES.has(file.type.toLowerCase()) : /\.(jpe?g|png|webp|heic|heif)$/i.test(file.name);
          if (!supported) error = "Please choose JPG, PNG, WebP, HEIC, or HEIF photos.";
          if (!file.size || file.size > MAX_PHOTO_BYTES) error = "Each photo must be between 1 byte and 5 MB.";
        }
        const total = [...photos.map(photo => photo.file), ...additions].reduce((sum, file) => sum + file.size, 0);
        if (total > MAX_TOTAL_PHOTO_BYTES) error = "Please keep the selected photos at 20 MB or less combined.";
        if (error) {
          setStatus(statusElement, error, "error");
          return;
        }
        for (const file of additions) {
          let url = null;
          try { url = window.URL.createObjectURL(file); } catch { /* A filename tile is sufficient when previewing is unavailable. */ }
          photos.push({ file, url });
        }
        clearStatus(statusElement);
        render();
      });
    }
    render();
    return {
      files: () => photos.map(photo => photo.file),
      setBusy(value) {
        busy = value;
        if (input) input.disabled = value;
        if (list) list.querySelectorAll("button").forEach(button => { button.disabled = value; });
      },
      clear() {
        photos.forEach(revoke);
        photos.length = 0;
        if (input) input.value = "";
        render();
      },
      dispose() { photos.forEach(revoke); },
    };
  }

  function getValue(formData, name) {
    const value = formData.get(name);
    return typeof value === "string" ? value.trim() : "";
  }

  function validatePayload(payload) {
    if (!payload.contactName) {
      return "Please enter your name.";
    }

    if (!payload.phone && !payload.email) {
      return "Please provide a phone number or email address.";
    }

    if (!payload.projectDescription || payload.projectDescription.length < 10) {
      return "Please include a little more detail about the project.";
    }

    return null;
  }

  function buildPayload(form, attribution) {
    const formData = new FormData(form);
    const touch = activeTouch(attribution);
    const values = touch.values || {};
    const transactionId =
      form.dataset.hgPendingTransactionId || createTransactionId();

    form.dataset.hgPendingTransactionId = transactionId;

    return {
      transactionId,
      contactName: getValue(formData, "name"),
      phone: getValue(formData, "phone"),
      email: getValue(formData, "email"),
      zipCode: getValue(formData, "zip"),
      projectDescription: getValue(formData, "message"),
      contactMethod: "webForm",
      landingPage: window.location.href,
      pageTitle: document.title,
      referrer: touch.referrer || document.referrer || "",
      utmSource: values.utm_source || "",
      utmMedium: values.utm_medium || "",
      utmCampaign: values.utm_campaign || "",
      utmTerm: values.utm_term || "",
      utmContent: values.utm_content || "",
      gclid: values.gclid || "",
      gbraid: values.gbraid || "",
      wbraid: values.wbraid || "",
      browserLanguage: navigator.language || "",
      formStartedAt: form.dataset.hgFormStartedAt,
      website: getValue(formData, "website"),
      sourceMetadata: {
        clientVersion: CLIENT_VERSION,
        firstTouch: attribution?.firstTouch || null,
        lastTouch: attribution?.lastTouch || null,
      },
    };
  }

  function fireConversion(transactionId, sourceType) {
    if (typeof window.gtag !== "function") {
      return;
    }

    window.gtag("event", "conversion", {
      send_to: GOOGLE_ADS_CONVERSION,
      transaction_id: transactionId,
      value: 1,
      currency: "USD",
    });

    window.gtag("event", "generate_lead", {
      send_to: GA4_DESTINATION,
      method: "web_form",
      lead_source: sourceType,
    });
  }

  function inferClientSource(payload) {
    const source = String(payload.utmSource || "").toLowerCase();
    const medium = String(payload.utmMedium || "").toLowerCase();
    const referrer = String(payload.referrer || "").toLowerCase();

    if (source.includes("chatgpt") || referrer.includes("chatgpt.com")) {
      return "chatgpt";
    }

    if (payload.gclid || payload.gbraid || payload.wbraid || source.includes("googleads") || source === "google_ads" ||
        (source === "google" && ["cpc", "ppc", "paidsearch", "paid_search", "paid-search"].includes(medium))) {
      return "googleAds";
    }

    if (medium === "organic" || source === "google" || referrer.includes("google.") ||
        referrer.includes("bing.com") || referrer.includes("duckduckgo.com")) {
      return "organic";
    }

    return "website";
  }

  async function submitQuoteForm(form, statusElement, attribution, photoPicker) {
    if (form.dataset.hgSubmitting === "true") {
      return;
    }
    clearStatus(statusElement);

    const payload = buildPayload(form, attribution);
    const photos = photoPicker.files();
    const validationError = validatePayload(payload);

    if (validationError) {
      setStatus(statusElement, validationError, "error");
      return;
    }

    form.dataset.hgSubmitting = "true";
    photoPicker.setBusy(true);
    const submitButton = form.querySelector('button[type="submit"], input[type="submit"]');
    const originalButtonText = submitButton?.textContent || "";

    if (submitButton) {
      submitButton.disabled = true;
      submitButton.setAttribute("aria-busy", "true");
      if ("textContent" in submitButton) {
        submitButton.textContent = photos.length ? "Sending request and photos..." : "Sending...";
      }
    }

    const controller = new AbortController();
    const timeoutId = window.setTimeout(() => controller.abort(), photos.length ? 90_000 : 15_000);

    try {
      let body = JSON.stringify(payload);
      let headers = { "Content-Type": "application/json" };
      if (photos.length) {
        body = new FormData();
        body.append("payload", JSON.stringify(payload));
        photos.forEach(file => body.append("photos", file, file.name));
        headers = {};
      }
      const response = await fetch(ENDPOINT, {
        method: "POST",
        headers,
        body,
        signal: controller.signal,
      });

      const result = await response.json().catch(() => ({}));

      if (!response.ok || !result.success) {
        throw new Error(
          typeof result.error === "string"
            ? result.error
            : "I couldn't send the request automatically.",
        );
      }

      if (photos.length && result.photoCount !== photos.length) {
        throw new Error("Your photos were not all confirmed. Your details and photos are still here so you can retry.");
      }

      try {
        fireConversion(result.transactionId || payload.transactionId, inferClientSource(payload));
      } catch {
        // Analytics failures must never turn an accepted lead into a submission error.
      }

      form.reset();
      photoPicker.clear();
      delete form.dataset.hgPendingTransactionId;
      form.dataset.hgFormStartedAt = nowIso();
      setStatus(
        statusElement,
        photos.length
          ? "Thanks — your quote request and photos were sent. Grant will follow up using the contact information you provided."
          : "Thanks — your quote request was sent. Grant will follow up using the contact information you provided.",
        "success",
      );
    } catch (error) {
      const message = error?.name === "AbortError"
        ? "The request timed out. Please try again, or call/text (619) 695-4334."
        : `${error?.message || "I couldn't send the request."} Please try again, or call/text (619) 695-4334.`;

      setStatus(statusElement, message, "error");
    } finally {
      delete form.dataset.hgSubmitting;
      photoPicker.setBusy(false);
      window.clearTimeout(timeoutId);

      if (submitButton) {
        submitButton.disabled = false;
        submitButton.removeAttribute("aria-busy");
        if ("textContent" in submitButton) {
          submitButton.textContent = originalButtonText || "Send Request";
        }
      }
    }
  }

  function removePrematureContactConversions() {
    document.querySelectorAll("a[href]").forEach((link) => {
      const href = link.getAttribute("href") || "";
      const isContactNavigation =
        href === "#contact" ||
        href.endsWith("index.html#contact") ||
        href.endsWith("/#contact");

      if (!isContactNavigation) {
        return;
      }

      const inlineHandler = link.getAttribute("onclick") || "";
      if (inlineHandler.includes("gtag_report_conversion")) {
        link.removeAttribute("onclick");
      }
    });
  }

  function initialize() {
    const attribution = captureAttribution();
    removePrematureContactConversions();

    document.querySelectorAll(FORM_SELECTOR).forEach((form) => {
      if (!(form instanceof HTMLFormElement)) {
        return;
      }

      const statusElement = ensureFormHelpers(form);
      const photoPicker = initializePhotos(form, statusElement);
      window.addEventListener("pagehide", (event) => {
        if (!event.persisted) photoPicker.dispose();
      });
      form.addEventListener("submit", (event) => {
        event.preventDefault();
        void submitQuoteForm(form, statusElement, attribution, photoPicker);
      });
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize, { once: true });
  } else {
    initialize();
  }
})();
