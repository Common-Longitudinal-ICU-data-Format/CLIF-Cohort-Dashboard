# CLIF Table Status Checker
# Analyzes completion status of CLIF tables based on requirements from generate_table_one.ipynb

library(tidyverse)
library(jsonlite)

# Define required tables and their critical columns based on notebook requirements
required_tables <- list(
  "patient" = c("patient_id", "race_category", "ethnicity_category", "sex_category", "death_dttm"),
  "hospitalization" = c("patient_id", "hospitalization_id", "admission_dttm", "discharge_dttm", "age_at_admission"),
  "adt" = c("hospitalization_id", "hospital_id", "in_dttm", "out_dttm", "location_category"),
  "vitals" = c("hospitalization_id", "recorded_dttm", "vital_category", "vital_value"),
  "labs" = c("hospitalization_id", "lab_result_dttm", "lab_order_dttm", "lab_category", "lab_value_numeric"),
  "medication_admin_continuous" = c("hospitalization_id", "admin_dttm", "med_name", "med_category", "med_dose", "med_dose_unit"),
  "respiratory_support" = c("hospitalization_id", "recorded_dttm", "device_category", "mode_category", "fio2_set", "lpm_set", "resp_rate_set", "peep_set", "resp_rate_obs", "tidal_volume_set", "pressure_control_set", "pressure_support_set"),
  "patient_assessments" = c("hospitalization_id", "recorded_dttm", "assessment_category", "numerical_value"),
  "microbiology_culture" = c("hospitalization_id", "recorded_dttm"),
  "microbiology_nonculture" = c("hospitalization_id", "recorded_dttm"),
  "position" = c("hospitalization_id", "recorded_dttm"),
  "sensitivity" = c("hospitalization_id", "recorded_dttm"),
  "medication_orders" = c("hospitalization_id", "recorded_dttm"),
  "medication_admin_intermittent" = c("hospitalization_id", "recorded_dttm"),
  "hospital_diagnosis" = c("hospitalization_id", "recorded_dttm"),
  "provider" = c("hospitalization_id", "recorded_dttm"),
  "intake_output" = c("hospitalization_id", "recorded_dttm"),
  "patient_procedures" = c("hospitalization_id", "recorded_dttm"),
  "crrt_therapy" = c("hospitalization_id", "recorded_dttm"),
  "ecmo_mcs" = c("hospitalization_id", "recorded_dttm"),
  "transfusion" = c("hospitalization_id", "recorded_dttm"),
  "code_status" = c("hospitalization_id", "recorded_dttm")
)

# Required data categories
required_categories <- list(
  "vitals" = c("weight_kg"),
  "labs" = c("creatinine", "bilirubin_total", "po2_arterial", "platelet_count"),
  "medication_admin_continuous" = c("norepinephrine", "epinephrine", "phenylephrine", "vasopressin", "dopamine"),
  "patient_assessments" = c("gcs_total")
)

#' Check if a table file exists and get basic info
#' @param table_name Name of the CLIF table
#' @param tables_path Path to CLIF tables directory
check_table_existence <- function(table_name, tables_path) {
  # Look for common file formats
  possible_files <- file.path(tables_path, paste0("clif_", table_name, c(".csv", ".parquet", ".arrow", ".feather")))
  existing_files <- possible_files[file.exists(possible_files)]
  
  if (length(existing_files) == 0) {
    return(list(exists = FALSE, file_path = NA, rows = 0, cols = 0))
  }
  
  file_path <- existing_files[1]
  
  # Try to read file info without loading entire file
  tryCatch({
    if (grepl("\\.csv$", file_path)) {
      # For CSV, read just the header to get column count
      header <- read.csv(file_path, nrows = 1)
      cols <- ncol(header)
      # Get row count efficiently
      rows <- length(readLines(file_path)) - 1  # subtract header
    } else if (grepl("\\.parquet$", file_path)) {
      if (require(arrow, quietly = TRUE)) {
        df <- arrow::read_parquet(file_path)
        rows <- nrow(df)
        cols <- ncol(df)
      } else {
        # Fallback if arrow not available
        rows <- NA
        cols <- NA
      }
    } else {
      rows <- NA
      cols <- NA
    }
    
    return(list(exists = TRUE, file_path = file_path, rows = rows, cols = cols))
  }, error = function(e) {
    return(list(exists = TRUE, file_path = file_path, rows = NA, cols = NA, error = e$message))
  })
}

#' Check table completeness based on required columns and categories
#' @param table_name Name of the CLIF table
#' @param tables_path Path to CLIF tables directory
check_table_completeness <- function(table_name, tables_path) {
  
  table_info <- check_table_existence(table_name, tables_path)
  
  if (!table_info$exists) {
    return(list(
      table = table_name,
      status = "Not started",
      exists = FALSE,
      required_cols = length(required_tables[[table_name]]),
      missing_cols = length(required_tables[[table_name]]),
      rows = 0,
      completeness_score = 0,
      details = "Table file not found"
    ))
  }
  
  # Try to read the actual table to check columns
  tryCatch({
    if (grepl("\\.csv$", table_info$file_path)) {
      df <- read.csv(table_info$file_path, nrows = 100)  # Sample for column check
    } else if (grepl("\\.parquet$", table_info$file_path) && require(arrow, quietly = TRUE)) {
      df <- arrow::read_parquet(table_info$file_path) %>% slice_head(n = 100)
    } else {
      # Can't read file format
      return(list(
        table = table_name,
        status = "In progress",
        exists = TRUE,
        required_cols = length(required_tables[[table_name]]),
        missing_cols = NA,
        rows = table_info$rows,
        completeness_score = 0.5,
        details = "File exists but format not supported for column checking"
      ))
    }
    
    # Check required columns
    required_cols <- required_tables[[table_name]]
    available_cols <- names(df)
    missing_cols <- setdiff(required_cols, available_cols)
    
    # Check required categories if applicable
    category_completeness <- 1
    missing_categories <- character(0)
    
    if (table_name %in% names(required_categories)) {
      if ("vital_category" %in% available_cols) {
        available_categories <- unique(df$vital_category)
        missing_categories <- setdiff(required_categories[[table_name]], available_categories)
      } else if ("lab_category" %in% available_cols) {
        available_categories <- unique(df$lab_category)
        missing_categories <- setdiff(required_categories[[table_name]], available_categories)
      } else if ("med_category" %in% available_cols) {
        available_categories <- unique(df$med_category)
        missing_categories <- setdiff(required_categories[[table_name]], available_categories)
      } else if ("assessment_category" %in% available_cols) {
        available_categories <- unique(df$assessment_category)
        missing_categories <- setdiff(required_categories[[table_name]], available_categories)
      }
      
      if (length(missing_categories) > 0) {
        category_completeness <- 1 - (length(missing_categories) / length(required_categories[[table_name]]))
      }
    }
    
    # Calculate completeness score
    col_completeness <- 1 - (length(missing_cols) / length(required_cols))
    overall_completeness <- (col_completeness + category_completeness) / 2
    
    # Determine status
    if (overall_completeness >= 0.9 && table_info$rows > 0) {
      status <- "Complete"
    } else if (overall_completeness >= 0.5 || table_info$rows > 0) {
      status <- "In progress"
    } else {
      status <- "Not started"
    }
    
    # Create details
    details <- ""
    if (length(missing_cols) > 0) {
      details <- paste("Missing columns:", paste(missing_cols, collapse = ", "))
    }
    if (length(missing_categories) > 0) {
      if (details != "") details <- paste(details, "; ")
      details <- paste0(details, "Missing categories: ", paste(missing_categories, collapse = ", "))
    }
    if (details == "") details <- "All requirements met"
    
    return(list(
      table = table_name,
      status = status,
      exists = TRUE,
      required_cols = length(required_cols),
      missing_cols = length(missing_cols),
      rows = table_info$rows,
      completeness_score = round(overall_completeness, 3),
      details = details
    ))
    
  }, error = function(e) {
    return(list(
      table = table_name,
      status = "In progress",
      exists = TRUE,
      required_cols = length(required_tables[[table_name]]),
      missing_cols = NA,
      rows = table_info$rows,
      completeness_score = 0.5,
      details = paste("Error reading table:", e$message)
    ))
  })
}

#' Generate status report for all tables
#' @param tables_path Path to CLIF tables directory
#' @param output_path Path to save status report
generate_status_report <- function(tables_path, output_path = "data/table_status.csv") {
  
  # Check each required table
  results <- map_dfr(names(required_tables), function(table_name) {
    result <- check_table_completeness(table_name, tables_path)
    as.data.frame(result, stringsAsFactors = FALSE)
  })
  
  # Add summary statistics
  summary_stats <- list(
    total_tables = nrow(results),
    complete_tables = sum(results$status == "Complete"),
    in_progress_tables = sum(results$status == "In progress"),
    not_started_tables = sum(results$status == "Not started"),
    overall_completeness = round(mean(results$completeness_score, na.rm = TRUE), 3),
    last_updated = Sys.time()
  )
  
  # Save results
  write.csv(results, output_path, row.names = FALSE)
  
  # Save summary as JSON for easy dashboard integration
  summary_path <- gsub("\\.csv$", "_summary.json", output_path)
  write_json(summary_stats, summary_path, pretty = TRUE)
  
  cat("Status report generated:\n")
  cat("- Detailed report:", output_path, "\n")
  cat("- Summary:", summary_path, "\n")
  cat("- Complete tables:", summary_stats$complete_tables, "/", summary_stats$total_tables, "\n")
  cat("- Overall completeness:", scales::percent(summary_stats$overall_completeness), "\n")
  
  return(results)
}

# Main execution function
main <- function() {
  # Default path - adjust as needed
  tables_path <- "/Users/dema/WD/CLIF-TableOne/tables"
  
  # Check if tables path exists, if not use current directory
  if (!dir.exists(tables_path)) {
    cat("Tables path not found:", tables_path, "\n")
    cat("Please specify the correct path to CLIF tables\n")
    return(NULL)
  }
  
  # Generate the status report
  results <- generate_status_report(tables_path)
  
  return(results)
}

# Run if called directly
if (!interactive()) {
  main()
}