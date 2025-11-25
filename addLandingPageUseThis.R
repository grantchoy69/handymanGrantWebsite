setwd("C:/HandyManGrant/handymanGrantWebsite")
source("buildLandingPage.R")  # if you save the function in this file

generateLandingPage(
  indexPath  = "index.html",
  pageSlug   = "drywall-repair",
  jsonPath   = "drywallRepair.json"
)


generateLandingPage("index.html", "drywall-repair", "drywallRepair.json")
generateLandingPage("index.html", "faucet-installation", "faucetInstallation.json")
generateLandingPage("index.html", "toilet-installation", "toiletInstallation.json")
generateLandingPage("index.html", "light-fixture-installation", "lightFixtureInstallation.json")
generateLandingPage("index.html", "ceiling-fan-installation", "ceilingFanInstallation.json")
generateLandingPage("index.html", "door-repair", "doorRepair.json")
generateLandingPage("index.html", "fence-repair", "fenceRepair.json")
