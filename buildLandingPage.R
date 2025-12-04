generateLandingPage <- function(indexPath, pageSlug, jsonPath, aboutMe = FALSE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install jsonlite: install.packages('jsonlite')")
  }
  library(jsonlite)
  
  # ----- read index.html -----
  indexLines <- readLines(indexPath, warn = FALSE)
  indexText  <- paste(indexLines, collapse = "\n")
  # ----- robust Meet Grant section extraction -----
  meetGrantMatch <- regexpr(
    "<section id=\"meet-grant\"[\\s\\S]*?</section>",
    indexText,
    perl = TRUE
  )
  
  if (!aboutMe) {
    # feature disabled for this page
    meetGrantHtml <- ""
  } else if (meetGrantMatch[1] == -1) {
    # enabled, but section missing in index.html
    meetGrantHtml <- ""
  } else {
    meetGrantHtml <- substr(
      indexText,
      meetGrantMatch[1],
      meetGrantMatch[1] + attr(meetGrantMatch, "match.length") - 1
    )
  }
  
  
  # ----- load config JSON -----
  cfg <- jsonlite::fromJSON(jsonPath)
  
  # ----- find structural markers in index.html -----
  mainStart    <- regexpr("<main class=\"container\">", indexText)
  footerStart  <- regexpr("<footer class=\"foot\">", indexText) - 2
  contactStart <- regexpr("<section id=\"contact\"", indexText) - 2
  mainEnd      <- regexpr("</main>", indexText) 
  
  if (mainStart == -1 || footerStart == -1 ||
      contactStart == -1 || mainEnd == -1) {
    stop("Could not find expected <main>, <footer>, or contact section markers in index.html")
  }
  
  # everything up to just before <main class="container">
  headHtml    <- substr(indexText, 1, mainStart - 1)
  # contact section (we'll reuse this as-is)
  contactHtml <- substr(indexText, contactStart, mainEnd - 1)
  # footer and closing tags
  footerHtml  <- substr(indexText, footerStart, nchar(indexText))
  
  # ----- update <title> and meta tags based on JSON -----
  if (!is.null(cfg$seoTitle)) {
    headHtml <- sub("(<title>)(.*?)(</title>)",
                    paste0("\\1", cfg$seoTitle, "\\3"),
                    headHtml)
    headHtml <- sub(
      '(<meta property="og:title" content=")[^"]*(" */*>)',
      paste0("\\1", cfg$seoTitle, "\\2"),
      headHtml
    )
  }
  
  if (!is.null(cfg$metaDescription)) {
    headHtml <- sub(
      '(<meta name="description" content=")[^"]*(" */*>)',
      paste0("\\1", cfg$metaDescription, "\\2"),
      headHtml
    )
    headHtml <- sub(
      '(<meta property="og:description" content=")[^"]*(" */*>)',
      paste0("\\1", cfg$metaDescription, "\\2"),
      headHtml
    )
  }
  
  # ----- hero section -----
  heroId <- if (!is.null(cfg$heroId)) cfg$heroId else pageSlug
  
  # badges
  badgesHtml <- ""
  if (!is.null(cfg$badges) && length(cfg$badges) > 0) {
    badgeLines <- paste0(
      '          <span class="badge">',
      cfg$badges,
      "</span>"
    )
    badgesHtml <- paste(badgeLines, collapse = "\n")
  }
  
  primaryHref   <- cfg$primaryCtaHref
  secondaryHref <- cfg$secondaryCtaHref
  
  heroHtml <- paste0(
    '<main class="container">', "\n",
    '    <section class="hero" id="', heroId, '">', "\n",
    "      <div>\n",
    "        <h1>", cfg$heroHeading, "</h1>\n",
    "        <p>", cfg$heroSubheading, "</p>\n",
    "        <div class=\"cta-row\">\n",
    "          <a class=\"cta\" href=\"", primaryHref,
    "\" onclick=\"return gtag_report_conversion('", primaryHref, "');\">",
    cfg$primaryCtaText, "</a>\n",
    "          <a class=\"cta\" href=\"", secondaryHref,
    "\" onclick=\"return gtag_report_conversion('", secondaryHref, "');\">",
    cfg$secondaryCtaText, "</a>\n",
    "        </div>\n",
    "        <div class=\"badges\">\n",
    badgesHtml, "\n",
    "        </div>\n",
    "      </div>\n",
    "    </section>\n"
  )
  
  # ----- services section -----
  servicesListHtml <- ""
  if (!is.null(cfg$serviceBullets) && length(cfg$serviceBullets) > 0) {
    li <- paste0("          <li>", cfg$serviceBullets, "</li>")
    servicesListHtml <- paste(li, collapse = "\n")
  }
  
  servicesHtml <- paste0(
    "    <section id=\"services\">\n",
    "      <h2>", cfg$servicesHeading, "</h2>\n",
    "      <div class=\"card\">\n",
    "        <ul class=\"muted\" style=\"margin:.2rem 0 0 1rem; line-height:1.7\">\n",
    servicesListHtml, "\n",
    "        </ul>\n",
    "      </div>\n",
    "    </section>\n"
  )
  
  # ----- why section -----
  whyListHtml <- ""
  if (!is.null(cfg$whyBullets) && length(cfg$whyBullets) > 0) {
    li <- paste0("        <li>", cfg$whyBullets, "</li>")
    whyListHtml <- paste(li, collapse = "\n")
  }
  
  whyHtml <- paste0(
    "    <section id=\"why\">\n",
    "      <h2>", cfg$whyHeading, "</h2>\n",
    "      <ul>\n",
    whyListHtml, "\n",
    "      </ul>\n",
    "    </section>\n"
  )
  
  # ----- assemble new <main> -----
  mainHtml <- paste0(
    heroHtml, "\n",
    meetGrantHtml, "\n",
    servicesHtml, "\n",
    whyHtml, "\n",
    contactHtml, "\n",
    "  </main>\n"
  )
  
  # ----- stitch everything back together -----
  pageHtml <- paste0(headHtml, mainHtml, footerHtml)
  
  outPath <- file.path(dirname(indexPath), paste0(pageSlug, ".html"))
  writeLines(pageHtml, outPath, useBytes = TRUE)
  invisible(outPath)
}
