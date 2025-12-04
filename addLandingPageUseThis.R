setwd("C:/HandyManGrant/handymanGrantWebsite")
source("buildLandingPage.R")  # if you save the function in this file


generateLandingPage(
  indexPath = "index.html",
  pageSlug  = "handyman-near-me",
  jsonPath  = "handymanNearMe.json"
)

generateLandingPage("index.html", "drywall-repair", "drywallRepair.json")
generateLandingPage("index.html", "faucet-installation", "faucetInstallation.json")
generateLandingPage("index.html", "toilet-installation", "toiletInstallation.json")
generateLandingPage("index.html", "light-fixture-installation", "lightFixtureInstallation.json")
generateLandingPage("index.html", "ceiling-fan-installation", "ceilingFanInstallation.json")
generateLandingPage("index.html", "door-repair", "doorRepair.json")
generateLandingPage("index.html", "fence-repair", "fenceRepair.json")

generateLandingPage("index.html", "handyman-for-women-and-seniors", "handymanWomenSeniors.json")
generateLandingPage("index.html", "senior-home-repairs", "seniorHomeRepairs.json")
generateLandingPage("index.html", "aging-in-place-home-help", "agingInPlace.json")
