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

# Load required functions
source("draft_curve_functions.R")

# Helper function to plot faceted spaghetti plot for each NFL division
plot_spaghetti_for_divison <- function(draft_picks_for_division, corrs, div){
  div_teams <- unique(draft_picks_for_division$team)
  print(div)
  n_dim <- ifelse(div %in% c("AFC", "NFC"), 4, 2)
  draft_picks_for_division <- draft_picks_for_division %>%
    select(player_id, team, draft_number, mean_overall) %>%
    uncount(n_dim^2) %>%
    mutate(fct=team) %>%
    group_by(player_id) %>%
    mutate(fct=div_teams) %>%
    ungroup() %>%
    mutate(is_fct=team==fct,
           grp=team)
  
  if (div %in% c("AFC", "NFC")){
    corrs <- corrs %>%
      filter(grepl(div, division))
  }
  else{
    corrs <- corrs %>%
      filter(division==div)
  }
  corrs <- corrs %>%
    mutate(fct=team, is_fct=TRUE, grp=team)
  
  
  plot_faceted_spaghetti(draft_picks_for_division, corrs$team, n_dim, n_dim, params_model_all, 
                         "Team", corrs, add_label=div)
}

# Initialize team divisions 
team_divisions <- data.frame(
  team=c("BUF", "NYJ", "NWE", "MIA",
         "CIN", "CLE", "BAL", "PIT",
         "JAX", "IND", "HOU", "TEN",
         "KAN", "LAC", "LVR", "DEN",
         "DAL", "PHI", "NYG", "WAS",
         "GNB", "DET", "CHI", "MIN",
         "TAM", "NOR", "ATL", "CAR",
         "ARI", "LAR", "SFO", "SEA"),
  division=c(rep("AFC East", 4), rep("AFC North", 4), rep("AFC South", 4), rep("AFC West", 4),
             rep("NFC East", 4), rep("NFC North", 4), rep("NFC South", 4), rep("NFC West", 4))
)

# Load draft picks-Madden Ratings data and join with team divisions, while adding column that
# includes info on GM and the teams they were with
draft_picks_dup_summary <- read.csv("../Data/draft_picks_madden.csv") %>%
  left_join(team_divisions, by="team") %>%
  group_by(gm) %>%
  mutate(gm_with_teams=paste(gm, " (", paste(unique(team), collapse=", "), ")", sep="")) %>%
  ungroup()

# Fit model for all players and get parameters
model_all <- fit_exp_model(draft_picks_dup_summary %>% 
                             mutate(x=draft_number), "draft_number")
params_model_all <- as.vector(coef(model_all))

# Generate curve for all players
curve_all <- plot_draft_curve(draft_picks_dup_summary %>%
                                mutate(x=draft_number), "draft_number", "mean_overall", "Draft Pick",
                              "Average Madden Rating",
                              "Draft Curve for Average Madden Rating in Years 3-5 vs. Draft Pick", "",
                              "../Charts/Draft Curves/Average Madden Ratings/Draft Pick/All.pdf",
                              method="exponential", method_str="Exponential", expn=TRUE, 
                              expn_params=list(alpha=params_model_all[1], beta=params_model_all[2],
                                               theta=params_model_all[3]),
                              add_points=FALSE)

# Predict and compute errors
draft_picks_dup_summary$pred_mean_overall <- model_all$fit
draft_picks_dup_summary$pred_diff <- draft_picks_dup_summary$mean_overall - draft_picks_dup_summary$pred_mean_overall

# Compute various correlation metrics for each relevant GM
gm_corrs <- draft_picks_dup_summary %>% 
  filter(!is.na(gm)) %>%
  group_by(gm) %>%
  summarize(gm_with_teams=gm_with_teams[1], n_picks=n(), pearsons=cor(log(draft_number), mean_overall, method="pearson"),
            kendalls=cor(position_rank, -1*mean_overall, method="kendall"),
            r2=1 - (sum((mean_overall - pred_mean_overall)^2))/(sum((mean_overall - mean(draft_picks_dup_summary$mean_overall))^2)),
            avg_mean_overall=mean(mean_overall), median_mean_overall=median(mean_overall),
            picks_under_line=sum(mean_overall + 5 < pred_mean_overall)/n(),
            picks_over_line=1-picks_under_line,
            total_diff=sum(mean_overall - pred_mean_overall),
            avg_diff=total_diff/n()) %>%
  arrange(desc(r2))

# Generate draft curve for each GM
for (gm_ in gm_corrs$gm){
  gm_draftees <- draft_picks_dup_summary %>% filter(gm==gm_)
  gm_plot <- curve_all + geom_point(data=gm_draftees, aes(x=draft_number, y=mean_overall)) +
    geom_text_repel(data=gm_draftees, aes(x=draft_number, y=mean_overall, 
                                          label=ifelse(abs(pred_diff) > 9, name, ""))) + 
    labs(title=paste("Draft Curve for Average Madden Rating in Years 3-5 vs. Draft Pick by", gm_),
         subtitle=paste("Curve: All Offensive/Defensive Players Drafted 2012-21, Points: ", gm_, " Draftees", sep=""))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/GM/", gm_, ".pdf", sep=""),
         width=12, height=7)
}

# Remove some GMs as they don't have enough data to generate a draft curve
gms_to_remove <- c("Jon Robinson", "Steve Keim", "Bruce Allen", "Ted Thompson", "Ryan Pace")
valid_gms <- gm_corrs[!(gm_corrs$gm %in% gms_to_remove),]$gm
valid_gms_with_team <- unique((draft_picks_dup_summary %>% filter(gm %in% valid_gms))$gm_with_teams)

# Prepare data for faceted spaghetti plot by GM
draft_picks_for_gm <- draft_picks_dup_summary %>%
  filter(gm %in% valid_gms) %>%
  select(player_id, gm_with_teams, draft_number, mean_overall) %>%
  uncount(length(valid_gms)) %>%
  mutate(fct=gm_with_teams) %>%
  group_by(player_id) %>%
  mutate(fct=valid_gms_with_team) %>%
  ungroup() %>%
  mutate(is_fct=gm_with_teams==fct,
         grp=gm_with_teams)

# Plot faceted spaghetti by GM
plot_faceted_spaghetti(draft_picks_for_gm, (gm_corrs %>% filter(gm %in% valid_gms))$gm_with_teams,
                       6, 5, params_model_all, "GM", (gm_corrs %>%
                                                        filter(gm %in% valid_gms) %>%
                                                        mutate(fct=gm_with_teams,
                                                               is_fct=TRUE,
                                                               grp=gm)))

# Get info on best/top 3 available by GM and plot
gm_available_rank_data <- plot_best_available_plots(draft_picks_dup_summary %>%
                                   filter(!is.na(gm)) %>%
                                   mutate(fct=gm_with_teams),
                                 6, 5, "GM")


# Compute correlations by team
team_corrs <- draft_picks_dup_summary %>% 
  group_by(team) %>%
  summarize(division=division[1], n_picks=n(), 
            pearsons=cor(log(draft_number), mean_overall, method="pearson"),
            kendalls=cor(position_rank, -1*mean_overall, method="kendall"),
            r2=1 - (sum((mean_overall - pred_mean_overall)^2))/(sum((mean_overall - mean(draft_picks_dup_summary$mean_overall))^2)),
            avg_mean_overall=mean(mean_overall), median_mean_overall=median(mean_overall),
            picks_under_line=sum(mean_overall + 5 < pred_mean_overall)/n(),
            picks_over_line=1-picks_under_line,
            total_diff=sum(mean_overall - pred_mean_overall),
            avg_diff=total_diff/n()) %>%
  arrange(desc(r2))

# Plot faceted spaghetti for each NFL division, removing Josh Rosen to ensure plotability
for (div in unique(team_divisions$division)){
  plot_spaghetti_for_divison(draft_picks_dup_summary %>%
                               filter(division==div,
                                      !(name=="Josh Rosen")),
                             team_corrs, div)
}

# Plot faceted spaghetti for each conference
for (conf in c("AFC", "NFC")){
  plot_spaghetti_for_divison(draft_picks_dup_summary %>%
                               filter(grepl(conf, division),
                                      !(name=="Josh Rosen")),
                             team_corrs, conf)
}

# Get all teams
teams <- unique(draft_picks_dup_summary$team)

# Prepare data for faceted spaghetti plot by team
draft_picks_for_team <- draft_picks_dup_summary %>%
  select(player_id, name, team, draft_number, mean_overall) %>%
  filter(!(name=="Josh Rosen")) %>%
  uncount(32) %>%
  mutate(fct=team) %>%
  group_by(player_id) %>%
  mutate(fct=teams) %>%
  ungroup() %>%
  mutate(is_fct=team==fct,
         grp=team)

# Ensure required variables are in team_corrs
team_corrs <- team_corrs %>%
  mutate(fct=team, is_fct=TRUE, grp=team)

# Plot faceted spaghetti by team
plot_faceted_spaghetti(draft_picks_for_team, team_corrs$team, 6, 6, params_model_all, 
                       "Team", team_corrs)

# Get best/top 3 available info by team and plot
team_available_rank_info <- plot_best_available_plots(draft_picks_dup_summary %>%
                                   mutate(fct=team),
                                 6, 6, "Team")