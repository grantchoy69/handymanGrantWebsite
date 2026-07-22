#!/usr/bin/env python3
"""Handyman Grant landing-page generator."""

import json
import re
from pathlib import Path


def build_list_items_html(items):
    if not items:
        return ""
    return "\n".join(f"          <li>{item}</li>" for item in items)


def build_gallery_items_html(gallery):
    items = []
    for photo in gallery:
        items.append(
            '        <div class="zoom-container">\n'
            f'          <img src="{photo.get("src", "")}" '
            f'alt="{photo.get("alt", "")}" loading="lazy" decoding="async">\n'
            '        </div>'
        )
    return "\n".join(items)


def build_gallery_section_html(gallery):
    """Return no gallery section when a service has no photos."""
    if not gallery:
        return ""

    return (
        '    <section id="gallery">\n'
        '      <h2>Examples of My Work</h2>\n'
        '      <div class="random-gallery">\n'
        f'{build_gallery_items_html(gallery)}\n'
        '      </div>\n'
        '    </section>'
    )


def extract_section(index_html, section_id):
    """Extract a canonical top-level section from index.html by id."""
    pattern = re.compile(
        rf'<section\b[^>]*\bid=["\']{re.escape(section_id)}["\'][^>]*>'
        rf'[\s\S]*?</section>',
        re.IGNORECASE,
    )
    match = pattern.search(index_html)

    if not match:
        raise ValueError(
            f'Could not find <section id="{section_id}"> in index.html'
        )

    return match.group(0)


def make_paths_relative_to_landing_page(section_html):
    """Rewrite root-page links for pages stored inside landing-pages/."""
    replacements = (
        ('src="photos/', 'src="../photos/'),
        ("src='photos/", "src='../photos/"),
        ('href="photos/', 'href="../photos/'),
        ("href='photos/", "href='../photos/"),
        ('href="index.html', 'href="../index.html'),
        ("href='index.html", "href='../index.html"),
        ('href="work.html', 'href="../work.html'),
        ("href='work.html", "href='../work.html"),
    )

    for old, new in replacements:
        section_html = section_html.replace(old, new)

    return section_html


def clean_shared_section_copy(section_html):
    """Remove one homepage-only sentence from generated landing pages."""
    section_html = section_html.replace(
        "My goal is for you to feel confident you brought in someone who can "
        "actually handle the job — without creating new problems or drama "
        "with your wife.",
        "My goal is for you to feel confident you brought in someone who can "
        "actually handle the job without creating new problems or surprises.",
    )
    return section_html


def build_shared_section_html(index_html, section_id):
    """Extract and prepare one shared section for a landing page."""
    section_html = extract_section(index_html, section_id)
    section_html = make_paths_relative_to_landing_page(section_html)
    section_html = clean_shared_section_copy(section_html)
    return "    " + section_html.replace("\n", "\n    ")


def generate_landing_pages():
    base_dir = Path(__file__).parent
    json_path = base_dir / "services.json"
    index_path = base_dir / "index.html"
    template_path = base_dir / "landing-page-template.html"
    output_dir = base_dir / "landing-pages"

    with json_path.open("r", encoding="utf-8") as file:
        data = json.load(file)

    index_html = index_path.read_text(encoding="utf-8")
    template = template_path.read_text(encoding="utf-8")

    meet_grant_html = build_shared_section_html(index_html, "meet-grant")
    what_to_expect_html = build_shared_section_html(
        index_html,
        "what-to-expect",
    )
    trust_html = build_shared_section_html(index_html, "trust")

    output_dir.mkdir(exist_ok=True)
    print(f"Generating landing pages into: {output_dir}")

    for service in data.get("services", []):
        slug = service.get("slug")
        if not slug:
            continue

        html = template
        html = html.replace("{{ title }}", service.get("title", ""))
        html = html.replace(
            "{{ metaDescription }}",
            service.get("metaDescription", ""),
        )
        html = html.replace("{{ intro }}", service.get("intro", ""))
        html = html.replace(
            "{{ pricingNote }}",
            service.get("pricingNote", ""),
        )

        hero = service.get("hero", {})
        html = html.replace(
            "{{ hero.headline }}",
            hero.get("headline", ""),
        )
        html = html.replace(
            "{{ hero.subheadline }}",
            hero.get("subheadline", ""),
        )

        cta = service.get("cta", {})
        html = html.replace(
            "{{ cta.primaryText }}",
            cta.get("primaryText", "Get a Quote"),
        )
        html = html.replace(
            "{{ cta.secondaryText }}",
            cta.get("secondaryText", "Call (619) 695-4334"),
        )

        html = html.replace(
            "{{ meetGrantSection }}",
            meet_grant_html,
        )
        html = html.replace(
            "{{ gallerySection }}",
            build_gallery_section_html(service.get("gallery", [])),
        )
        html = html.replace(
            "{{ whatToExpectSection }}",
            what_to_expect_html,
        )
        html = html.replace(
            "{{ trustSection }}",
            trust_html,
        )

        html = html.replace(
            "{% for item in servicesIncluded %}\n"
            "          <li>{{ item }}</li>\n"
            "          {% endfor %}",
            build_list_items_html(service.get("servicesIncluded", [])),
        )
        html = html.replace(
            "{% for step in process %}\n"
            "          <li>{{ step }}</li>\n"
            "          {% endfor %}",
            build_list_items_html(service.get("process", [])),
        )

        output_path = output_dir / f"{slug}.html"
        output_path.write_text(html, encoding="utf-8")
        print(f"  Generated: {output_path.name}")

    print("\nDone. Check the landing-pages folder.")


if __name__ == "__main__":
    generate_landing_pages()
