setwd("C:/HandyManGrant/handymanGrantWebsite")
source("buildLandingPage.R")  # if you save the function in this file

generateLandingPage(
  indexPath  = "index.html",
  pageSlug   = "drywall-repair",
  jsonPath   = "drywallRepair.json"
)
