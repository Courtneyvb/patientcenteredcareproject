#!/bin/bash
# This script generates a figure and saves it to the Hugo static folder

echo "Generating figure..."

# Run the R script to generate the figure
Rscript survey_likert.R  # Replace with the actual path to your R script

echo "Figure generated and saved."
