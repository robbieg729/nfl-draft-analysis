# NFL Draft Analysis Project - Overview
This was my main project during my summer internship at the NFL with the Football Data & Analytics department. The goals were to
- use historical data from 2012 onwards to determine which positions have been historically drafted worse than others in the NFL Draft, and 
- analyze what, if anything, could be improved about the NFL Combine process.
This repository contains:
- a selection of code scripts used during the analysis,
- a selection of charts created as part of the analysis,
- a PowerPoint summarizing the findings.

## Data
There were a large number of different CSV files used in this analysis, ranging from basic player information to PFF Grades and Madden Ratings. None of the CSV files could be uploaded as they contain sensitive information.

## Code
### load_data.R
Loads CSV files from Data folder.
### clean_data.R
Cleans relevant data for use in EDA/models.
### load_average_madden_ratings.R
Loads dataset containing Average Madden Ratings in Years 3-5 for all draft picks. This was the metric used to define success for a player in the NFL.
### draft_curve_functions.R
Helper functions for computing and plotting correlations and draft curve models.
### position/gm/college/classman_draft_curves.R
Produces draft curve plots by relevant group (gm also includes team), computes and plots correlation metrics (R-Squared, Kendall's Tau, etc.) that were the focal point of presentations.
### draft_round_analysis.R
Plots analysis of best available analysis by draft round.
### logistic_model.R
Logistic models for "probability of getting a player with Madden Rating above X". Also computes pick values for each pick by position using exponential models.
### trade_analysis.R
Analysis on draft-day trades in the NFL. Main results are efficiency of trading by position traded up for and round of pick traded up for.
### position_deep_dives.R
Look into if specific positions within a position (e.g. Free Safety within Safety) are better/worse drafted than others with respect to Madden Rating. Also looks at if players with certain attributes are better/worse drafted than others (e.g. Linebackers that pass rush more than cover).
### position_role_changes.R
Look into how often positions perform certain roles/line up at certain positions and how this could help adding Combine drills.
### draft_sim.R
Resampling of 2012-2021 NFL Drafts to determine significance of difference in correlation metrics.

## Charts
Organized into two main folders: 
- "Madden Ratings" contains some EDA on player Madden Ratings, including year-over-year differences and distributions depending on roster type (Active/Inactive),
- "Draft Curves/Average Madden Ratings" containing most of the main results from the analysis.