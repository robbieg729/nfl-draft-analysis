library(dplyr)
library(readr)
library(stringr)
library(reshape)
library(ggplot2)
library(tidyverse)
library(stats)
library(xgxr)
library(ggrepel)

# helper function to get Madden rank of drafted player out of available players at position
ranks_available <- function(v){
  ranks <- c()
  for (i in 1:length(v)){
    v_i <- v[i:length(v)]
    v_i[2:length(v_i)] <- v_i[2:length(v_i)] - 0.99
    ranks <- append(ranks, rank(-v_i, ties.method="min")[1])
  }
  return (ranks)
}

# helper function to get R-Squared of exponential decay model
get_exp_model_r2 <- function(draft_number, mean_overall, start_=c(25, -0.1, 50), exp_model_all=NULL){
  df <- data.frame(draft_number=draft_number, mean_overall=mean_overall)
  tryCatch(
    {
      alpha.0 <- start_[1]
      beta.0 <- start_[2]
      theta.0 <- start_[3]
      
      # Starting parameters
      start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
      
      model <- nls(mean_overall ~ alpha * exp(beta * draft_number) + theta, data=df,
                   start=start)
      
      pred <- model$m$fitted()
      
      ss_res <- sum((df$mean_overall - pred)^2)
      ss_tot <- sum((df$mean_overall - mean(df$mean_overall))^2)
      r2 <- 1 - ss_res/ss_tot
      
      return (r2)
    },
    error=function(cond){
      if (!is.null(exp_model_all)){
        pred <- predict.nls(exp_model_all, newdata=data.frame(draft_number=df$draft_number))
        
        ss_res <- sum((df$mean_overall - pred)^2)
        ss_tot <- sum((df$mean_overall - mean(df$mean_overall))^2)
        r2 <- 1 - ss_res/ss_tot
        return (r2)
      }
      else{
        return (0.15)
      }
    }
  )
}

# Helper function to get R-Squared of exponential model from simulated Draft
get_exp_model_simmed <- function(df_simmed, start_params){
  tryCatch(
    {
      return (nls(mean_overall ~ alpha * exp(beta * draft_number) + theta,
                  start=list(alpha=start_params[1], beta=start_params[2], theta=start_params[3]),
                  data=df_simmed))
    },
    error=function(cond){
      return (NULL)
    }
  )
}

# Helper function to plot boxplots of correlations at each position for first 20 simulated 
# Drafts and observed Drafts
plot_sample_boxplots <- function(simmed_corrs, corr="r2", corr_label="R-Squared", bucket_size=5){
  simmed_corrs_for_plot <- simmed_corrs %>%
    filter(n_sim %in% 0:20) %>%
    mutate(observed=n_sim==0)
  
  sim_orders <- simmed_corrs_for_plot %>%
    group_by(n_sim) %>%
    summarize(med=median((!!sym(corr)))) %>%
    arrange(med)
  
  ylabels <- unique(paste("Sim", sim_orders$n_sim))
  ylabels[ylabels=="Sim 0"] <- "Observed Values"
  
  label_var <- paste(corr, "label", sep="_")
  
  file_corr_label <- ifelse(grepl("%", corr_label), substr(corr_label, 3, nchar(corr_label)), corr_label)
  
  ggplot(data=simmed_corrs_for_plot, aes(x=(!!sym(corr)), 
                                         y=reorder(as.factor(n_sim), (!!sym(corr)), median),
                                         color=observed)) +
    geom_boxplot() + geom_point() + geom_text_repel(aes(label=(!!sym(label_var)))) +
    scale_y_discrete(labels=ylabels) + 
    scale_color_discrete(type=c("black", "red")) + 
    ylab("Simulation") + xlab(corr_label) +
    labs(title=paste("Boxplot comparing Observed vs. Simulated", corr_label, "by Position"),
         subtitle=paste("Sampling by size", bucket_size, "buckets")) +
    theme(legend.position = "None")
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Resample/Sim Boxplot ", file_corr_label, 
               " ", player_bucket_size, ".pdf", sep=""),
         width=12, height=7)
}

# Helper function to plot faceted histograms of simulated correlations by position
plot_corr_histograms_by_position <- function(simmed_corrs, observed_correlations, corr="r2", bucket_size=5){
  fct_order <- (observed_correlations %>% filter(correlation==corr) %>%
                  arrange(desc(value)))$position
  ggplot(simmed_corrs, aes(x=(!!sym(corr)))) + 
    geom_histogram(aes(y=after_stat(density)), fill="darkblue") +
    facet_wrap(~factor(position, levels=fct_order), nrow=4) +
    geom_vline(data=observed_correlations %>% filter(correlation==corr), 
               aes(xintercept=value), col="red") +
    ylab("Density") + xlab(corr) + labs(title=paste("Histogram of Simulated", corr, "by Position"),
                                        subtitle=paste("2012-21 Drafts Simulated N = 1000 times using size", bucket_size, "buckets"))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Resample/Sim Position Facet ", corr, " ", bucket_size, ".pdf", sep=""),
         width=12, height=7)
}

# reading in data
draft_picks_dup_summary <- read.csv("../Data/draft_picks_madden.csv") %>%
  select(-X)

udfas <- read.csv("../Data/udfas_madden.csv") %>%
  select(-X)

# start parameters for an exponential model
exp_model_params <- c(25, -0.1, 50)

# observed ranges/correlation values
observed_ranges <- data.frame(correlation=c("r2", "kendall", "best_available", "top_3_available"),
                              obs=c(0.197, 0.103, 0.194, 0.3859125))

observed_correlations <- data.frame(position=c(rep("QB", 4), rep("LB", 4), rep("OT", 4), rep("DL", 4),
                                               rep("EDGE", 4), rep("TE", 4), rep("RB", 4), rep("WR", 4),
                                               rep("CB", 4), rep("IOL", 4), rep("S", 4)),
                                    correlation=rep(c("r2", "kendall", "best_available", "top_3_available"),
                                                    11),
                                    value=c(0.534, 0.494, 0.291, 0.645, 0.498, 0.486, 0.097, 0.260,
                                            0.460, 0.487, 0.185, 0.480, 0.459, 0.478, 0.176, 0.430, 
                                            0.456, 0.436, 0.228, 0.549, 0.41, 0.510, 0.207, 0.533,
                                            0.404, 0.509, 0.194, 0.357, 0.400, 0.431, 0.131, 0.304,
                                            0.391, 0.454, 0.116, 0.278, 0.367, 0.407, 0.116, 0.287,
                                            0.337, 0.428, 0.168, 0.416),
                                    n_sim=rep(0, 44))

# data frames for storing simmed correlations
r2s <- data.frame()
kendalls <- data.frame()
best_availables <- data.frame()
top_3_availables <- data.frame()

# parameters for simulation
n_sims <- 1000
player_bucket_size <- 5
n_buckets <- ceiling(max(draft_picks_dup_summary$draft_number) / player_bucket_size)

for (i in 1:n_sims){
  
  # split player pool into first-round QBs and non-first-round QBs
  player_pool <- draft_picks_dup_summary %>%
    filter(!(position=="QB")) %>%
    select(draft_year, draft_round, draft_number, position, position_rank, mean_overall)
  
  qbs_pool <- draft_picks_dup_summary %>%
    filter(position=="QB") %>%
    select(draft_year, draft_round, draft_number, position, position_rank, mean_overall)
  
  # Sim QBs by draft rounds (1st, 2nd-3rd, 4th-5th, 6th-7th)
  qbs_pool_simmed <- data.frame()
  qbs_rounds_bins <- data.frame(bin=c(1, 2, 2, 3, 3, 4, 4),
                                round=1:7)
  for (b in unique(qbs_rounds_bins$bin)){
    rounds <- (qbs_rounds_bins %>% filter(bin==b))$round
    qbs_pool_round <- qbs_pool %>% filter(draft_round %in% rounds)
    qbs_pool_round$mean_overall <- sample(qbs_pool_round$mean_overall, length(qbs_pool_round$mean_overall), replace=FALSE)
    qbs_pool_simmed <- rbind(qbs_pool_simmed, qbs_pool_round)
  }
  
  # resample all other player Madden Ratings, based on size 5 buckets (e.g. picks 1-5, picks 6-10)
  player_pool_simmed <- data.frame()
  min_pick <- 1
  max_pick <- player_bucket_size
  for (j in 1:n_buckets){
    madden_ratings_pool <- (player_pool %>% filter(draft_number %in% min_pick:max_pick))$mean_overall
    round_pool <- player_pool %>% filter(draft_number %in% min_pick:max_pick)
    round_pool$mean_overall <- sample(madden_ratings_pool, length(madden_ratings_pool), replace=FALSE)
    
    player_pool_simmed <- rbind(player_pool_simmed, round_pool)
    min_pick <- min_pick + player_bucket_size
    max_pick <- max_pick + player_bucket_size
  }
  
  # join first-round QBs and other players
  player_pool_simmed <- rbind(player_pool_simmed, qbs_pool_simmed) %>%
    arrange(draft_year, draft_number)
  
  # resample UDFAs
  udfas_pool <- udfas %>%
    select(draft_year, draft_number, position)
  udfas_ratings_pool <- udfas$mean_overall
  udfas_pool$mean_overall <- sample(udfas_ratings_pool, length(udfas_ratings_pool), replace=FALSE)
  
  # calculate best available
  player_pool_simmed_with_udfas <- bind_rows(player_pool_simmed, udfas_pool) %>%
    group_by(draft_year, position) %>%
    arrange(draft_number) %>%
    mutate(madden_pos_rank_available=ranks_available(mean_overall))
  
  # fit exponential curve for all simulated players
  exp_model_all <- get_exp_model_simmed(player_pool_simmed, exp_model_params)
  
  # get correlations for each position
  pos_r2s <- player_pool_simmed %>%
    group_by(position) %>%
    summarize(r2=get_exp_model_r2(draft_number, mean_overall, start_=exp_model_params,
                                  exp_model_all=exp_model_all))
  
  pos_kendalls <- player_pool_simmed %>%
    group_by(draft_year, position) %>%
    summarize(kendall=cor(position_rank, -mean_overall, method="kendall")) %>%
    group_by(position) %>%
    summarize(kendall=median(kendall))
  
  pos_best_available <- player_pool_simmed_with_udfas %>%
    group_by(position) %>%
    summarize(best_available=sum(madden_pos_rank_available==1)/n())
  
  pos_top_3_available <- player_pool_simmed_with_udfas %>%
    group_by(position) %>%
    summarize(top_3_available=sum(madden_pos_rank_available<=3)/n())
  
  # add simmed correlations to data frames
  r2s <- rbind(r2s, data.frame(n_sim=i, position=pos_r2s$position, r2=pos_r2s$r2))
  kendalls <- rbind(kendalls, data.frame(n_sim=i, position=pos_kendalls$position, 
                                         kendall=pos_kendalls$kendall))
  best_availables <- rbind(best_availables, data.frame(n_sim=i, position=pos_best_available$position,
                                                       best_available=pos_best_available$best_available))
  top_3_availables <- rbind(top_3_availables, data.frame(n_sim=i, position=pos_top_3_available$position,
                                                         top_3_available=pos_top_3_available$top_3_available))
}

# Create data frame to store all simulated correlations along with observed correlations
simmed_corrs <- left_join(r2s, left_join(kendalls, best_availables, by=c("n_sim", "position")),
                          by=c("n_sim", "position")) %>%
  left_join(top_3_availables, by=c("n_sim", "position")) %>%
  bind_rows(observed_correlations %>% pivot_wider(names_from="correlation", values_from="value")) %>%
  group_by(n_sim) %>%
  mutate(r2_label=ifelse(!(rank(r2, ties.method="min") %in% 3:9), position, ""),
         kendall_label=ifelse(!(rank(kendall, ties.method="min") %in% 3:9), position, ""),
         best_available_label=ifelse(!(rank(best_available, ties.method="min") %in% 3:9), position, ""),
         top_3_available_label=ifelse(!(rank(top_3_available, ties.method="min") %in% 3:9), position, ""))

# Plot correlation boxplots
plot_sample_boxplots(simmed_corrs, bucket_size = player_bucket_size)
plot_sample_boxplots(simmed_corrs, corr="kendall", corr_label="Kendall's Tau",
                     bucket_size = player_bucket_size)
plot_sample_boxplots(simmed_corrs, corr="best_available", corr_label="% Best Available",
                     bucket_size = player_bucket_size)
plot_sample_boxplots(simmed_corrs, corr="top_3_available", corr_label="% Top 3 Available",
                     bucket_size = player_bucket_size)

# Plot correlation histograms
plot_corr_histograms_by_position(simmed_corrs, observed_correlations, corr="r2", 
                                 bucket_size=player_bucket_size)
plot_corr_histograms_by_position(simmed_corrs, observed_correlations, corr="kendall", 
                                 bucket_size=player_bucket_size)
plot_corr_histograms_by_position(simmed_corrs, observed_correlations, corr="best_available",
                                 bucket_size=player_bucket_size)
plot_corr_histograms_by_position(simmed_corrs, observed_correlations, corr="top_3_available",
                                 bucket_size=player_bucket_size)

# Get data to plot histograms faceted by correlation
sim_summaries <- simmed_corrs %>%
  filter(n_sim > 0) %>%
  group_by(n_sim) %>%
  summarize(r2=max(r2) - min(r2), kendall=max(kendall) - min(kendall),
            best_available=max(best_available) - min(best_available),
            top_3_available=max(top_3_available) - min(top_3_available)) 

sim_summaries_long <- sim_summaries %>% pivot_longer(cols=c("r2", "kendall", "best_available", "top_3_available"),
                                                      names_to="correlation", values_to="range")

# Make faceted histograms by correlation plot
ggplot(sim_summaries_long, aes(x=range)) +
  geom_histogram(fill="darkblue") +
  facet_wrap(~factor(correlation, levels=c("r2", "kendall", "best_available", "top_3_available"),
                     labels=c("R-Squared", "Median Kendall's Tau", "% Best Available", "% Top 3 Available")),
             scales="free") +
  geom_vline(data=observed_ranges, aes(xintercept=obs, color="Observed"), col="red") +
  ylab("Count") + xlab("Range") + labs(title="Histogram of Simulated Differences between Best and Worst Score by Position",
                                         subtitle=paste("2012-21 Drafts Simulated N = 1000 times using size", player_bucket_size, "buckets; Red lines represent observed differences")) +
  theme(plot.title=element_text(size=20), plot.subtitle=element_text(size=15),
        axis.title = element_text(size=15), axis.text=element_text(size=12),
        strip.text=element_text(size=15))
ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Resample/Sim Results ", player_bucket_size, ".pdf", sep=""), width=12, height=7)