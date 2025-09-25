#!/usr/bin/env python3

"""
Script to extract table status from site CSV files and create a status matrix
Output: Table_Name, Overall, [site_name1], [site_name2], ... format
"""

import pandas as pd
import os
import glob
from collections import Counter

def extract_site_name(filename):
    """Extract site name from filename"""
    base_name = os.path.basename(filename)
    # Extract everything before "_consolidated_validation_report.csv"
    site_name = base_name.replace("_consolidated_validation_report.csv", "")
    # Replace underscores with spaces for cleaner display
    site_name = site_name.replace("_", " ")
    return site_name

def calculate_overall_status(row):
    """Calculate overall status for a table across all sites"""
    statuses = [status for status in row if status != "Not Available"]

    if not statuses:
        return "Not Available"

    # Logic: If all sites are complete, overall is Complete
    # If any site has complete and others are partial, overall is Partial
    # Otherwise, overall is Incomplete
    if all(status == "complete" for status in statuses):
        return "Complete"
    elif any(status == "complete" for status in statuses) and any(status == "partial" for status in statuses):
        return "Partial"
    elif all(status == "partial" for status in statuses):
        return "Partial"
    else:
        return "Incomplete"

def main():
    # Directory containing site status CSV files
    site_status_dir = "/Users/dema/WD/CLIF-Cohort-Dashboard/data/site_status"

    # Get all CSV files matching the pattern
    csv_pattern = os.path.join(site_status_dir, "*_consolidated_validation_report.csv")
    csv_files = glob.glob(csv_pattern)

    print(f"Found {len(csv_files)} CSV files:")
    for f in csv_files:
        print(f"  {os.path.basename(f)}")

    # Initialize list to store data from each site
    all_site_data = []

    # Process each CSV file
    for csv_file in csv_files:
        site_name = extract_site_name(csv_file)
        print(f"Processing {site_name}...")

        try:
            # Read the CSV file
            site_data = pd.read_csv(csv_file)

            # Extract unique table status for each table
            table_status = site_data.groupby('table_name')['status'].first().reset_index()
            table_status['site'] = site_name

            all_site_data.append(table_status)

        except Exception as e:
            print(f"Error processing {site_name}: {e}")
            continue

    if not all_site_data:
        print("No data processed successfully!")
        return

    # Combine all site data
    combined_data = pd.concat(all_site_data, ignore_index=True)

    # Create the status matrix
    status_matrix = combined_data.pivot_table(
        index='table_name',
        columns='site',
        values='status',
        aggfunc='first',
        fill_value='Not Available'
    ).reset_index()

    # Calculate overall status for each table
    site_columns = [col for col in status_matrix.columns if col != 'table_name']
    status_matrix['Overall'] = status_matrix[site_columns].apply(
        lambda row: calculate_overall_status(row.tolist()),
        axis=1
    )

    # Reorder columns to put Overall first
    columns = ['table_name', 'Overall'] + site_columns
    status_matrix = status_matrix[columns]

    # Rename table_name column
    status_matrix.rename(columns={'table_name': 'Table_Name'}, inplace=True)

    # Sort by table name
    status_matrix = status_matrix.sort_values('Table_Name').reset_index(drop=True)

    # Write the output
    output_file = "data/site_status_matrix.csv"
    status_matrix.to_csv(output_file, index=False)

    print(f"\nStatus matrix created successfully!")
    print(f"Output saved to: {output_file}")
    print(f"Matrix dimensions: {len(status_matrix)} tables x {len(site_columns)} sites")

    # Display the matrix
    print(f"\nStatus Matrix Preview:")
    print(status_matrix)

    # Summary statistics
    print(f"\nSummary by site:")
    for site_col in site_columns:
        if site_col == 'Overall':
            continue
        status_counts = status_matrix[site_col].value_counts()
        print(f"\n{site_col}:")
        print(status_counts)

if __name__ == "__main__":
    main()