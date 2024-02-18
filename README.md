# MLB Prospect Model

This repo contains the R and CSV files for my M-SABR MLB Prospect Model. The article I wrote detailing the model and its results is linked here:

Contents of the repo:

combined.R - This is the main file you need to run the model. It runs the model and compiles the final list. None of the other files require edits.

scrapeFGhitters.R, scrapeFGpitchers.R - Files used to web scrape various statistics from FanGraphs for the model

mlH.R, mlP.R - Files containing the Machine Learning methods used to create the model.

finalHitters2024.csv, finalPitchers2024.csv - Output CSV files created from the Machine Learning files to list the top hitters and pitchers, respectfully

final2024.csv - Output CSV file containing the combined list. This holds the Top 100 list used in the article.
