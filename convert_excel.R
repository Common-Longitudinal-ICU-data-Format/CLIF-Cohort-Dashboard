# Load required packages
library(readxl)

# Convert final_website_data.xlsx to CSV
website_data <- read_excel("data/final_website_data.xlsx")
write.csv(website_data, "data/final_website_data.csv", row.names = FALSE)

# Convert CLIF_cohort_website_table.xlsx to CSV
cohort_table <- read_excel("data/CLIF_cohort_website_table.xlsx")
write.csv(cohort_table, "data/CLIF_cohort_website_table.csv", row.names = FALSE) 