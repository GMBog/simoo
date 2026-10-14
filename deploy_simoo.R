# ── Deploy Simoo to shinyapps.io ──────────────────────────────────────────────
# Run this script once from RStudio Console to publish the app.
# If rsconnect is not installed yet: install.packages("rsconnect")

library(rsconnect)

# ① If you have NOT yet connected your shinyapps.io account, run this first:
#    rsconnect::setAccountInfo(name="<YOUR-ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
#    (Get these from shinyapps.io → Account → Tokens → Show)

# ② Deploy
rsconnect::deployApp(
  appDir  = "/Users/GuillermoMartinez/Documents/MyScripts/MyPackages/simoo",
  appName = "Simoo",
  appTitle = "Simoo — Dairy Herd Genetics Simulator"
)
