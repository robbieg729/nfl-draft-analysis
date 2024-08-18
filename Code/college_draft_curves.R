library(dplyr)
library(readr)
library(stringr)
library(reshape)
library(ggplot2)
library(tidyverse)
library(stats)
library(xgxr)
library(common)
library(forcats)
library(ggrepel)
library(gghighlight)

# Get required functions
source("draft_curve_functions.R")

# Store Group of 5 conferences
group_of_five <- c("Mid-American Conference", "Independent", "Conference USA", "Mountain West Conference",
                   "American Athletic Conference", "Sun Belt Conference")

# Read in data on NCAA clubs and add variable stating which conference grouping to put them in
college_clubs <- read.csv("../Data/NonProDB.dbo.Club.csv") %>%
  filter(season >= 2011) %>%
  mutate(division=ifelse(conference %in% group_of_five, "Group of 5", 
                         ifelse(division=="1A" | division=="1AA", "FCS", "D-II and Below")),
         season=season + 1) %>%
  distinct(season, conference, division) %>%
  add_row(season=c(2012, 2013), conference=rep("American Athletic Conference", 2),
          division=rep("Group of 5", 2)) %>%
  filter(!is.na(conference), conference!="")
colnames(college_clubs)[3] <- "college_division"
  
# Read in draft picks-Madden Ratings dataset
draft_picks_dup_summary <- read.csv("../Data/draft_picks_madden.csv") %>%
  select(-X)

# Put Notre Dame into ACC, and merge above data with NCAA club data
draft_picks_for_college <- draft_picks_dup_summary %>%
  mutate(conference=ifelse(conference=="Independent" & college == "Notre Dame", "Atlantic Coast Conference", 
                             conference)) %>%
  left_join(college_clubs,  by=c("draft_year"="season", "conference"),
            relationship = "many-to-one", multiple="first") %>%
  mutate(conference_abbr=ifelse(conference_abbr=="IND." & college == "Notre Dame", "ACC", 
                                ifelse(conference_abbr %in% conferences_to_keep, conference_abbr, college_division))) %>%
  filter(!is.na(conference_abbr))

# Initialize empty data frame for correlation results
college_correlations <- data.frame()

# For each conference grouping, generate model, plot draft curve, and get correlations
for (conf in unique(draft_picks_for_college$conference_abbr)){
  
  # Subset data for current conference grouping
  df <- draft_picks_for_college %>%
    filter(conference_abbr==conf)
  
  # Generate model and get parameters
  m <- fit_exp_model(df %>%
                       mutate(x=draft_number), "draft_number")
  params <- as.vector(coef(m))
  
  # Plot draft curve
  plot_draft_curve(df %>%
                     mutate(x=draft_number), "draft_number", "mean_overall", "Draft Pick",
                   "Average Madden Rating",
                   "Draft Curve for Average Madden Rating in Years 3-5 vs. Draft Pick",
                   subtitle=paste("All ", conf, " Offensive/Defensive Players Drafted 2012-21", sep=""),
                   path=paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/College Conference/",
                              conf, ".pdf", sep=""),
                   r2=m$r2, expn=TRUE, method_str="Exponential", method="exponential",
                   expn_params=list(alpha=params[1],
                                    beta=params[2],
                                    theta=params[3]))
  
  # Get correlations for conference grouping and add to existing data frame
  college_correlations <- rbind(college_correlations, 
                                data.frame(conference_abbr=conf,
                                           r2=m$r2,
                                           pearsons=cor(log(df$draft_number), df$mean_overall,
                                                        method="pearson")))
}

# Arrange correlations in descending order of R-Squared
college_correlations <- college_correlations %>% arrange(desc(r2)) 

# Fit model for all players and get parameters
m <- fit_exp_model(draft_picks_for_college %>%
                     mutate(x=draft_number), "draft_number")
params <- as.vector(coef(m))

# Get conference groupings included in data
valid_conferences <- unique(draft_picks_for_college[!is.na(draft_picks_for_college$conference_abbr),]$conference_abbr)

# Prepare data for faceted spaghetti plot
draft_picks_for_college_facet <- draft_picks_for_college %>%
  select(player_id, conference_abbr, draft_number, mean_overall) %>%
  uncount(length(unique(conference_abbr))) %>%
  mutate(fct=conference_abbr) %>%
  group_by(player_id) %>%
  mutate(fct=valid_conferences) %>%
  ungroup() %>%
  mutate(is_fct=conference_abbr==fct,
         grp=conference_abbr)

# Plot faceted spaghetti of draft curves by conference grouping
plot_faceted_spaghetti(draft_picks_for_college_facet, college_correlations$conference_abbr,
                       3, 3, params, "College Conference",
                       college_correlations %>%
                         mutate(fct=conference_abbr, is_fct=TRUE, grp=conference_abbr))

# Get info on best/top 3 available by conference grouping and plot
college_available_rank_data <- plot_best_available_plots(draft_picks_for_college %>%
                                   mutate(fct=conference_abbr),
                                 3, 3, "College Conference")