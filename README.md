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
