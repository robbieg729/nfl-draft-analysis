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
library(zoo)

# Get required functions
source("draft_curve_functions.R")

# Read in Madden Ratings-Draft Pick dataset
draft_picks_dup_summary <- read.csv("../Data/draft_picks_madden.csv") %>%
  select(-X)


# Fit model for every position and get parameters
model_all <- fit_exp_model(draft_picks_dup_summary %>% 
                             mutate(x=draft_number), "draft_number")
params_model_all <- as.vector(coef(model_all))

# Use model to make predictions and calculate error
draft_picks_dup_summary$pred_mean_overall <- model_all$fit
draft_picks_dup_summary$pred_diff <- draft_picks_dup_summary$mean_overall - draft_picks_dup_summary$pred_mean_overall

# Get model R-Squareds between Draft Pick and Average Madden Rating
corrs_by_pick <- get_model_correlations(draft_picks_dup_summary, "draft_number",
                                        "Draft Pick")

# Get Kendalls Tau for each positional draft class
corrs_by_position_rank <- get_model_correlations(draft_picks_dup_summary,
                                                 "position_rank",
                                                 "Draft Positional Rank")

# Plot R-Squareds and Kendalls Taus
plot_model_correlations(corrs_by_pick, "Draft Pick")
plot_model_correlations(corrs_by_position_rank %>%
                          filter(draft_year != "All") %>%
                          mutate(draft_year=as.numeric(draft_year)), "Draft Positional Rank")


# Get positions in data
valid_positions <- unique(draft_picks_dup_summary$position)

# Prepare draft picks data for faceted spaghetti plot
draft_picks_for_position_facet <- draft_picks_dup_summary %>%
  select(player_id, position, draft_number, mean_overall) %>%
  uncount(length(valid_positions)) %>%
  mutate(fct=position) %>%
  group_by(player_id) %>%
  mutate(fct=valid_positions) %>%
  ungroup() %>%
  mutate(is_fct=position==fct,
         grp=position)

# Get order of positions, sorting by descending R-Squared
corrs_for_position_facet <- get_yearly_r2s(draft_picks_dup_summary, "draft_number", "Draft Pick",
                                           each=FALSE) %>%
  mutate(fct=position, is_fct=TRUE, grp=position) %>%
  arrange(desc(r2))

# Plot faceted spaghetti plot of draft curves for Average Madden Rating vs. Draft Pick
plot_faceted_spaghetti(draft_picks_for_position_facet, corrs_for_position_facet$position,
                       3, 4, params_model_all, "Position",
                       corrs_for_position_facet)

# Plot how often best available player is drafted out of all positions
ggplot(data=draft_picks_dup_summary, aes(x=madden_pos_rank_available)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=1, fill="darkblue") + xlab("Rank") + 
  ylab("Density") +
  labs(title="Ranking of Drafted Player out of Available Players at Position",
       subtitle="All Offensive/Defensive Players Drafted 2012-21")
ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Position/Rank Available All.pdf",
       width=7, height=7)

# Get info and plot how often best/top 3 available player is drafted for each position
pos_available_rank_data <- plot_best_available_plots(draft_picks_dup_summary %>%
                                   mutate(fct=position),
                                 3, 4, "Position")