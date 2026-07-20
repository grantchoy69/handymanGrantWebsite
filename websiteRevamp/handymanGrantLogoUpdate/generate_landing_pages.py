#!/usr/bin/env python3
"""Handyman Grant landing-page generator."""

import json
from pathlib import Path


def build_list_items_html(items):
    if not items:
        return ""
    return "\n".join(f"          <li>{item}</li>" for item in items)


def build_gallery_html(gallery):
    if not gallery:
        return ""
    items = []
    for photo in gallery:
        items.append(
            '        <div class="zoom-container">\n'
            f'          <img src="{photo.get("src", "")}" alt="{photo.get("alt", "")}">\n'
            '        </div>'
        )
    return "\n".join(items)


def generate_landing_pages():
    base_dir = Path(__file__).parent
    json_path = base_dir / "services.json"
    template_path = base_dir / "landing-page-template.html"
    output_dir = base_dir / "landing-pages"

    with json_path.open("r", encoding="utf-8") as file:
        data = json.load(file)

    template = template_path.read_text(encoding="utf-8")
    output_dir.mkdir(exist_ok=True)
    print(f"Generating landing pages into: {output_dir}")

    for service in data.get("services", []):
        slug = service.get("slug")
        if not slug:
            continue

        html = template
        html = html.replace("{{ title }}", service.get("title", ""))
        html = html.replace("{{ metaDescription }}", service.get("metaDescription", ""))
        html = html.replace("{{ intro }}", service.get("intro", ""))
        html = html.replace("{{ pricingNote }}", service.get("pricingNote", ""))

        hero = service.get("hero", {})
        html = html.replace("{{ hero.headline }}", hero.get("headline", ""))
        html = html.replace("{{ hero.subheadline }}", hero.get("subheadline", ""))

        cta = service.get("cta", {})
        html = html.replace("{{ cta.primaryText }}", cta.get("primaryText", "Get a Quote"))
        html = html.replace("{{ cta.secondaryText }}", cta.get("secondaryText", "Call (619) 695-4334"))

        benefits_html = "\n".join(
            f'        <div class="card">{benefit}</div>'
            for benefit in service.get("benefits", [])
        )
        html = html.replace(
            "{% for benefit in benefits %}\n        <div class=\"card\">{{ benefit }}</div>\n        {% endfor %}",
            benefits_html,
        )

        html = html.replace(
            "{% for photo in gallery %}\n        <div class=\"zoom-container\">\n          <img src=\"{{ photo.src }}\" alt=\"{{ photo.alt }}\">\n        </div>\n        {% endfor %}",
            build_gallery_html(service.get("gallery", [])),
        )

        html = html.replace(
            "{% for item in servicesIncluded %}\n          <li>{{ item }}</li>\n          {% endfor %}",
            build_list_items_html(service.get("servicesIncluded", [])),
        )

        html = html.replace(
            "{% for step in process %}\n          <li>{{ step }}</li>\n          {% endfor %}",
            build_list_items_html(service.get("process", [])),
        )

        output_path = output_dir / f"{slug}.html"
        output_path.write_text(html, encoding="utf-8")
        print(f"  Generated: {output_path.name}")

    print("\nDone. Check the landing-pages folder.")


if __name__ == "__main__":
    generate_landing_pages()
