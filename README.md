# Handyman Grant Landing-Page Trust Sections

Replace these files in the repository:

- `landing-page-template.html`
- `generate_landing_pages.py`
- `landing-pages/`

## What changed

- The generator now extracts the canonical `Meet Grant`, `What to Expect When I Arrive`, and `Safety & Trust Guarantee` sections from `index.html` every time it runs.
- Those sections appear immediately after each service's example-work gallery.
- Photo links are rewritten automatically for files inside `landing-pages/`.
- Services with no gallery photos no longer render an empty gallery section.
- The service-specific process heading is now `How This Service Works` to avoid duplicating the arrival-expectations heading.
- The landing-page `Back to Home` button is larger and easier to tap.
- Existing Google Ads conversion tracking remains on all three phone CTAs.

## Regenerate the landing pages

From the repository root:

    python generate_landing_pages.py

The included `landing-pages` folder has already been regenerated and validated.

## Website quote intake

The homepage and all seven service pages submit quote requests to the Handyman Grant Supabase endpoint. A confirmed submission preserves referral/campaign attribution and sends a stable transaction ID with its Google Ads conversion. Opening the contact section alone does not count as a conversion. Phone tracking and the existing Google Analytics configuration remain in place.

Review incoming requests in the admin app under **Quote requests**. Requests do not automatically create customers or jobs, and this release does not send email notifications. The original email form remains available when JavaScript is disabled.

Run `node scripts/testQuoteIntake.cjs` to check browser submission behavior and the generated page wiring.

## Quote photos

The homepage and seven service forms accept up to five optional JPEG, PNG, WebP, HEIC, or HEIF originals, with a 5 MiB limit per photo and 20 MiB combined. Customers can add photos in several selections and remove individual previews. Formats the browser cannot preview still show a filename tile. Files remain only in page memory until submitted; leaving or refreshing the page discards the selection.

Requests without photos keep the existing JSON contract. Photo requests use multipart form data with a JSON `payload` field and repeated `photos` file fields. The browser supplies the multipart boundary. Only a successful response with `photoCount` equal to the selected count clears the form and records the conversion. Failed or incomplete uploads retain the fields, files, and transaction ID for retry. Photo requests allow 90 seconds, compared with 15 seconds for requests without photos.

The backend must validate all originals and confirm every photo before returning success. Deploy that support and the admin photo viewer before publishing this frontend. Keep the existing private attachment storage and access controls; this website never needs a storage key.
