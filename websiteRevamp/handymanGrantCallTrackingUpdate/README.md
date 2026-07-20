# Handyman Grant Landing-Page Call Tracking

Copy these files over the matching files in the website repository.

## What changed

- Added the Google Ads tag to the shared landing-page template.
- Added the existing homepage conversion action:
  `AW-17749862714/nReaCO3k_8YbELrS5Y9C`
- Added a floating **Call Grant** button to every generated service page.
- Added conversion tracking to all phone CTAs:
  - Hero call button
  - Bottom call button
  - Floating Call Grant button
- Added a 1.2-second navigation fallback so blocked tracking does not prevent a call.
- Updated the shared template, so future generated pages receive the same tracking.

## Regenerate pages

From the repository root:

    python generate_landing_pages.py

The included `landing-pages` folder has already been regenerated and validated.
