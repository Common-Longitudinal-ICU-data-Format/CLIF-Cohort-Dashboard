# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install required packages
required_packages <- c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "leaflet",
  "plotly",
  "DT",
  "htmltools"
)

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
lapply(required_packages, library, character.only = TRUE) 