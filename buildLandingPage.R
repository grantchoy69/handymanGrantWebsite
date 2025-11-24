generateLandingPage <- function(indexPath, pageSlug, jsonPath) {
  # Dependencies
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Please install jsonlite: install.packages('jsonlite')")
  }
  library(jsonlite)
  
  # Read index.html as one big string
  indexLines <- readLines(indexPath, warn = FALSE)
  indexText  <- paste(indexLines, collapse = "\n")
  
  # Load config
  config <- jsonlite::fromJSON(jsonPath)
  
  # Locate structural markers in index.html
  mainStart    <- regexpr("<main class=\"container\">", indexText)
  footerStart  <- regexpr("<footer class=\"foot\">", indexText)
  contactStart <- regexpr("<section id=\"contact\"", indexText)
  mainEnd      <- regexpr("</main>", indexText)
  
  if (mainStart == -1 || footerStart == -1 || contactStart == -1 || mainEnd == -1) {
    stop("Could not find expected <main>, <footer>, or contact section markers in index.html")
  }
  
  # Split head, contact, footer
  headHtml    <- substr(indexText, 1, mainStart - 1)
  contactHtml <- substr(indexText, contactStart, mainEnd - 1)
  footerHtml  <- substr(indexText, footerStart, nchar(indexText))
  
  # ---- Update <title> and meta tags in head ----
  if (!is.null(config$seoTitle)) {
    headHtml <- sub("(<title>)(.*?)(</title>)",
                    paste0("\\1", config$seoTitle, "\\3"),
                    headHtml)
    headHtml <- sub("(<meta property=\"og:title\" content=\")[^\"]*(\" />)",
                    paste0("\\1", config$seoTitle, "\\2"),
                    headHtml)
  }
  
  if (!is.null(config$metaDescription)) {
    headHtml <- sub("(<meta name=\"description\" content=\")[^\"]*(\"/>)",
                    paste0("\\1", config$metaDescription, "\\2"),
                    headHtml)
    headHtml <- sub("(<meta property=\"og:description\" content=\")[^\"]*(\" />)",
                    paste0("\\1", config$metaDescription, "\\2"),
                    headHtml)
  }
  
  # ---- Build hero section from JSON ----
  heroId <- if (!is.null(config$heroId)) config$heroId else pageSlug
  
  badgesHtml <- ""
  if (!is.null(config$badges) && length(config$badges) > 0) {
    badgeLines <- paste(
      "          <span class=\"badge\">",
      config$badges,
      "</span>",
      sep = ""
    )
    badgesHtml <- paste(badgeLines, collapse = "\n")
  }
  
  heroHtml <- paste0(
    "    <section class=\"hero\" id=\"", heroId, "\">\n",
    "      <div>\n",
    "        <h1>", config$heroHeading, "</h1>\n",
    "        <p>", config$heroSubheading, "</p>\n",
    "        <div class=\"cta-row\">\n",
    "          <a class=\"cta\" href=\"", config$primaryCtaHref, "\">",
    config$primaryCtaText, "</a>\n",
    "          <a class=\"cta secondary\" href=\"", config$secondaryCtaHref, "\">",
    config$secondaryCtaText, "</a>\n",
    "        </div>\n",
    "        <div class=\"badges\">\n",
    badgesHtml, "\n",
    "        </div>\n",
    "      </div>\n",
    "    </section>\n"
  )
  
  # ---- Services section ----
  servicesListHtml <- ""
  if (!is.null(config$serviceBullets) && length(config$serviceBullets) > 0) {
    liLines <- paste(
      "          <li>",
      config$serviceBullets,
      "</li>",
      sep = ""
    )
    servicesListHtml <- paste(liLines, collapse = "\n")
  }
  
  servicesHtml <- paste0(
    "    <section id=\"services\">\n",
    "      <h2>", config$servicesHeading, "</h2>\n",
    "      <div class=\"card\">\n",
    "        <ul class=\"muted\" style=\"margin:.2rem 0 0 1rem; line-height:1.7\">\n",
    servicesListHtml, "\n",
    "        </ul>\n",
    "      </div>\n",
    "    </section>\n"
  )
  
  # ---- Why section ----
  whyListHtml <- ""
  if (!is.null(config$whyBullets) && length(config$whyBullets) > 0) {
    liLines <- paste(
      "        <li>",
      config$whyBullets,
      "</li>",
      sep = ""
    )
    whyListHtml <- paste(liLines, collapse = "\n")
  }
  
  whyHtml <- paste0(
    "    <section id=\"why\">\n",
    "      <h2>", config$whyHeading, "</h2>\n",
    "      <ul>\n",
    whyListHtml, "\n",
    "      </ul>\n",
    "    </section>\n"
  )
  
  # ---- Assemble new <main> ----
  mainHtml <- paste0(
    "<main class=\"container\">\n",
    heroHtml, "\n",
    servicesHtml, "\n",
    whyHtml, "\n",
    contactHtml, "\n",
    "  </main>\n"
  )
  
  # ---- Combine everything and write file ----
  pageHtml <- paste0(headHtml, mainHtml, footerHtml)
  
  outputPath <- file.path(dirname(indexPath), paste0(pageSlug, ".html"))
  writeLines(pageHtml, outputPath, useBytes = TRUE)
  
  return(invisible(outputPath))
}
