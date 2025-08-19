# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install required packages if not already installed
packages <- c(
  "tidyverse",
  "leaflet",
  "htmltools",
  "bslib",
  "plotly",
  "DT",
  "readxl",
  "scales",  # For number formatting
  "jsonlite", # For JSON parsing
  "knitr",    # For table formatting
  "kableExtra" # For advanced table styling
)

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load all packages
lapply(packages, library, character.only = TRUE) 