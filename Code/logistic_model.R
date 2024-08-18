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
library(import)

# Import function to fit exponential model
import::from("draft_curve_functions.R", fit_exp_model)

# Set seed for reproducibility
set.seed(101)

# Load draft picks-Madden Ratings data
draft_picks_dup_summary <- read.csv("../Data/draft_picks_madden.csv") %>%
  select(-X)

# Get the latest pick used across 2012-2021 drafts
max_pick <- max(draft_picks_dup_summary$draft_number)

# Helper function to get logistic models for Madden Rating over 70, 75, and 80
get_madden_prob_models <- function(draft_picks_dup_summary, pos){
  df_pos <- draft_picks_dup_summary %>%
    filter(position %in% c(pos)) %>%
    mutate(mean_over_70=mean_overall >= 70,
           mean_over_75=mean_overall >= 75,
           mean_over_80=mean_overall >= 80)
  
  sample <- sample.int(nrow(df_pos), size=floor(0.8*nrow(df_pos)), replace=FALSE)
  train <- df_pos
  test <- df_pos
  
  model_70 <- get_model_predictions(glm(mean_over_70 ~ draft_number, family=binomial(link="logit"),
                  data=train), test$draft_number, test$mean_over_70)
  
  model_75 <- get_model_predictions(glm(mean_over_75 ~ draft_number, family=binomial(link="logit"),
                  data=train), test$draft_number, test$mean_over_75)
  
  model_80 <- get_model_predictions(glm(mean_over_80 ~ draft_number, family=binomial(link="logit"),
                  data=train), test$draft_number, test$mean_over_80)
  
  return (list(model_70=model_70, model_75=model_75, model_80=model_80))
}

# Helper function to get value in [0, 1] of each specific draft pick for each specific position
get_pick_values_df <- function(draft_picks_dup_summary){
  pick_values <- data.frame()
  for (pos in append("All", unique(draft_picks_dup_summary$position))){
    df_pos <- draft_picks_dup_summary %>%
      filter(position %in% ifelse(pos=="All", unique(draft_picks_dup_summary$position), c(pos)))
    
    exp_model <- fit_exp_model(df_pos %>% mutate(x=draft_number), "draft_number")
    vals <- predict.nls(exp_model, newdata=data.frame(x=1:max_pick))
    
    pred_max <- max(vals)
    pred_min <- min(vals)
    
    vals <- (vals - pred_min + 0.3) / (pred_max - pred_min + 0.3)
    
    pick_values <- rbind(pick_values, data.frame(
      position=rep(pos, max_pick), pick=1:max_pick, value=vals
    ))
  }
  
  return (pick_values)
}

# Helper function to get model predictions and change in odds after waiting one round
get_model_predictions <- function(model, test_x, test_y){
  model$pick_probs <- predict(model, data.frame(draft_number=1:max_pick), type="response")
  model$wait_round_odds <- exp(32 * model$coefficients[2])
  
  test_pred <- data.frame(pred=predict(model, data.frame(draft_number=test_x), type="response"))
  test_pred <- test_pred %>%
    mutate(pred=ifelse(pred >= 0.5, 1, 0))
  
  model$accuracy <- sum(test_pred$pred == test_y)/length(test_x)
  return (model)
}

# Helper function to plot curves for each model at each position, grouped into one plot
plot_madden_prob_curves <- function(models, pos){
  model_70 <- models$model_70
  model_75 <- models$model_75
  model_80 <- models$model_80
  
  df_plot <- data.frame(pick=rep(1:max_pick, 3), pick_probs=c(model_70$pick_probs, model_75$pick_probs,
                                                         model_80$pick_probs),
                        model=c(rep("Over 70", max_pick), rep("Over 75", max_pick), rep("Over 80", max_pick)),
                        position=rep(pos, max_pick*3))
  ggplot(data=df_plot, aes(x=pick, y=pick_probs, group=model, color=model)) +
    geom_line(lwd=1) + xlab("Pick") + ylab("Probability") + ylim(c(0, 1)) +
    labs(title=paste("Probability of Drafting ", pos, " with Average Madden Rating in Years 3-5 Above 70, 75, and 80", sep=""),
         color="Average Madden Rating in Years 3-5") +
    scale_color_discrete(type=c("darkblue", "red", "lightgreen")) +
    theme(legend.position="inside", legend.position.inside=c(0.8, 0.8), 
          legend.text=element_text(size=15), legend.title = element_text(size=15))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Probability Curves/", pos, ".pdf", sep=""),
         width=12, height=7)
  
  return (df_plot)
}

# Helper function to help make spaghetti plots for each model
plot_madden_prob_spaghetti_plots <- function(df){
  model_names <- c("Over 70", "Over 75", "Over 80")
  for (model_name in model_names){
    df_model <- df %>% filter(model==model_name)
    
    ggplot(data=df_model, aes(x=pick, y=pick_probs, group=position, color=is_fct)) +
      geom_line(lwd=1) + xlab("Pick") + ylab("Probability") + ylim(c(0, 1)) +
      labs(title=paste("Probability of Drafting Player with Average Madden Rating in Years 3-5 Above",
                       parse_number(model_name), "by Position")) +
      gghighlight(is_fct, use_direct_label=FALSE) + scale_color_discrete(type=c("darkblue")) +
      facet_wrap(~factor(fct, levels=c("QB", "RB", "WR", "TE", "OT", "IOL", "DL", "EDGE", "LB", "CB", "S")),
                 nrow=3, ncol=4) +
      theme(legend.position="None")
    
    ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Probability Curves/Spaghetti ", 
                 parse_number(model_name), ".pdf", sep=""),
           width=10, height=7)
  }
}

# Get three models for every position
prob_models_all <- get_madden_prob_models(draft_picks_dup_summary, unique(draft_picks_dup_summary$position))

# Plot curves and get data frame with data in plotting format
all_df <- plot_madden_prob_curves(prob_models_all, "Any Player")

# Get [0, 1] value of each pick
pick_values <- get_pick_values_df(draft_picks_dup_summary)

# Initialize empty data frames to store data
spaghetti_df <- data.frame()
prob_model_summaries <- data.frame()

# Loop through each position and add data to empty data frames
for (pos in append("All", unique(draft_picks_dup_summary$position))){
  pos_prob_models <- prob_models_all
  if (pos != "All"){
    pos_prob_models <- get_madden_prob_models(draft_picks_dup_summary, pos)
    pos_prob_df <- plot_madden_prob_curves(pos_models, pos)
    
    spaghetti_df <- rbind(spaghetti_df, pos_df)
  }
  
  prob_model_summaries <- rbind(prob_model_summaries, 
                                data.frame(
                                  position=rep(pos, 3),
                                  model=c("Over 70", "Over 75", "Over 80"),
                                  accuracy=c(pos_prob_models$model_70$accuracy,
                                             pos_prob_models$model_75$accuracy,
                                             pos_prob_models$model_80$accuracy),
                                  odds_effect=c(pos_prob_models$model_70$wait_round_odds,
                                                pos_prob_models$model_75$wait_round_odds,
                                                pos_prob_models$model_80$wait_round_odds)
                                ))
  
  
}

# Prepare data for spaghetti plot
spaghetti_df <- spaghetti_df %>%
  mutate(pick_id=1:nrow(spaghetti_df)) %>%
  uncount(length(unique(position))) %>%
  group_by(pick_id) %>%
  mutate(fct=unique(draft_picks_dup_summary$position),
         is_fct=fct==position) 

# Plot spaghetti plots
plot_madden_prob_spaghetti_plots(spaghetti_df)

# Store pick values in data frame for trade analysis
write.csv(pick_values, "../Data/pick_values.csv")