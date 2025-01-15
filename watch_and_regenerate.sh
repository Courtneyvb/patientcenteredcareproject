#!/bin/bash
# Watch the source directory and regenerate the figure when data changes

# Define the path to the R script or data file to monitor
WATCH_PATH='https://docs.google.com/spreadsheets/d/1V1b6-7YElXFuvsTR5kf58gtDCCXNNy0b-OII86-bs50/edit?gid=0#gid=0'  # Change this to the actual path

# Start watching for file changes using fswatch
fswatch -o $WATCH_PATH | while read f; do
    echo "Data or script changed. Generating figure..."
    ./generate_figure.sh  # Regenerate the figure by running the shell script
done
