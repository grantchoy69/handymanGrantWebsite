# Handyman Grant Logo Update

Replace the corresponding files in the website repository with this folder's contents.

Included changes:

- Replaces the hammer emoji with the Handyman Grant LLC logo on index.html.
- Adds the same logo to the shared landing-page template.
- Corrects the invalid `<<div class="brand">` markup.
- Includes regenerated versions of all seven service landing pages.
- Includes a corrected Python generator that avoids nested UL and OL elements.
- Uses a cropped transparent `photos/logo.png` so the artwork displays at a useful size.
- Uses the logo as the homepage favicon.

After copying the files, future pages can be regenerated with:

    python generate_landing_pages.py
