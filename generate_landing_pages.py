#!/usr/bin/env python3
"""
Handyman Grant Landing Page Generator - Supports Gallery
"""

import json
from pathlib import Path

def build_list_html(items, wrapper="ul"):
    if not items:
        return ""
    lis = "\n".join([f"        <li>{item}</li>" for item in items])
    if wrapper == "ul":
        return f"<ul>\n{lis}\n      </ul>"
    else:
        return f"<ol>\n{lis}\n      </ol>"

def build_gallery_html(gallery):
    if not gallery:
        return ""
    items = []
    for photo in gallery:
        items.append(f'''        <div class="zoom-container">
          <img src="{photo.get('src', '')}" alt="{photo.get('alt', '')}">
        </div>''')
    return '\n'.join(items)

def generate_landing_pages():
    base_dir = Path(__file__).parent
    json_path = base_dir / "services.json"
    template_path = base_dir / "landing-page-template.html"
    output_dir = base_dir / "landing-pages"

    with open(json_path, 'r', encoding='utf-8') as f:
        data = json.load(f)

    with open(template_path, 'r', encoding='utf-8') as f:
        template = f.read()

    output_dir.mkdir(exist_ok=True)
    print(f"Generating landing pages into: {output_dir}")

    for service in data.get('services', []):
        slug = service.get('slug')
        if not slug:
            continue

        html = template

        # Simple replacements
        html = html.replace('{{ title }}', service.get('title', ''))
        html = html.replace('{{ metaDescription }}', service.get('metaDescription', ''))
        html = html.replace('{{ intro }}', service.get('intro', ''))
        html = html.replace('{{ pricingNote }}', service.get('pricingNote', ''))

        # Hero
        hero = service.get('hero', {})
        html = html.replace('{{ hero.headline }}', hero.get('headline', ''))
        html = html.replace('{{ hero.subheadline }}', hero.get('subheadline', ''))

        # CTA
        cta = service.get('cta', {})
        html = html.replace('{{ cta.primaryText }}', cta.get('primaryText', 'Get a Quote'))
        html = html.replace('{{ cta.secondaryText }}', cta.get('secondaryText', 'Call (619) 695-4334'))

        # Benefits
        benefits_html = ""
        for b in service.get('benefits', []):
            benefits_html += f'        <div class="card">{b}</div>\n'
        html = html.replace('{% for benefit in benefits %}\n        <div class="card">{{ benefit }}</div>\n        {% endfor %}', benefits_html.strip())

        # Gallery (new)
        gallery_html = build_gallery_html(service.get('gallery', []))
        html = html.replace('{% for photo in gallery %}\n        <div class="zoom-container">\n          <img src="{{ photo.src }}" alt="{{ photo.alt }}">\n        </div>\n        {% endfor %}', gallery_html)

        # Services Included
        services_html = build_list_html(service.get('servicesIncluded', []), "ul")
        html = html.replace(
            '{% for item in servicesIncluded %}\n          <li>{{ item }}</li>\n          {% endfor %}',
            services_html
        )

        # Process
        process_html = build_list_html(service.get('process', []), "ol")
        html = html.replace(
            '{% for step in process %}\n          <li>{{ step }}</li>\n          {% endfor %}',
            process_html
        )

        # Write file
        out_path = output_dir / f"{slug}.html"
        with open(out_path, 'w', encoding='utf-8') as f:
            f.write(html)

        print(f"  ✓ Generated: {out_path.name}")

    print("\nDone! Check the landing-pages/ folder.")


if __name__ == "__main__":
    generate_landing_pages()