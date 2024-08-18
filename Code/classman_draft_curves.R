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

# Load required functions
source("draft_curve_functions.R")

# Load draft picks-Madden Ratings data
draft_picks_dup_summary <- read.csv("../Data/draft_picks_madden.csv") %>%
  select(-X)

# Fit models for upperclassmen and underclassmen
m_yes <- fit_exp_model(draft_picks_dup_summary %>%
                         mutate(x=draft_number) %>%
                         filter(underclassman=="Yes"), "draft_number")
m_no <- fit_exp_model(draft_picks_dup_summary %>%
                        mutate(x=draft_number) %>%
                        filter(underclassman=="No"), "draft_number")

# Get R-Squareds for both models and arrange in descending order
corrs_for_classman <- data.frame(fct=c("Underclassman", "Upperclassman"), is_fct=TRUE,
                                 grp=c("Underclassman", "Upperclassman"),
                                 r2=c(m_yes$r2, m_no$r2)) %>%
  arrange(desc(r2))

# Plot grouped draft curves for upper/underclassmen
ggplot(data=draft_picks_dup_summary, aes(x=draft_number, y=mean_overall, color=underclassman)) +
  stat_smooth(method="nls", formula=(y ~ alpha * exp(beta * x) + theta),
              method.args=list(start=list(alpha=as.vector(coef(m_yes))[1], 
                                          beta=as.vector(coef(m_yes))[2],
                                          theta=as.vector(coef(m_yes))[3]),
                               control=nls.control(maxiter=200)), se=FALSE) +
  xlab("Draft Pick") + ylab("Average Madden Rating") +
  labs(title="Draft Curves for Average Madden Rating in Years 3-5 vs. Draft Pick by Classman",
       subtitle="All Offensive/Defensive Players Drafted 2012-21",
       colour="Class") +
  scale_color_manual(labels=c("Upper", "Under"), values=c("darkblue", "red")) +
  theme(legend.position="inside", legend.position.inside=c(0.8, 0.8),
        legend.title = element_text(size=15), legend.text=element_text(size=12))
ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Classman/Grouped Classman.pdf",
       width=12, height=7)
  

# Plot faceted spaghetti by position
ggplot(data=draft_picks_dup_summary, aes(x=draft_number, y=mean_overall, group=underclassman,
                                         colour = underclassman)) +
  stat_smooth(method="nls", formula=(y ~ alpha * exp(beta * x) + theta),
              method.args=list(start=list(alpha=as.vector(coef(m_yes))[1], 
                                          beta=as.vector(coef(m_yes))[2],
                                          theta=as.vector(coef(m_yes))[3]),
                               control=nls.control(maxiter=200)), se=FALSE) +
  facet_wrap(~factor(position), nrow=3, ncol=4) +
  xlab("Draft Pick") + ylab("Average Madden Rating") +
  labs(title="Draft Curves for Average Madden Rating in Years 3-5 vs. Draft Pick by Classman and Position",
       subtitle="All Offensive/Defensive Players Drafted 2012-21",
       colour="Class") +
  scale_color_manual(labels=c("Upper", "Under"), values=c("darkblue", "red"))
ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Classman/Spaghetti Classman Position.pdf",
       width=10, height=7)


# Helper function to plot faceted spaghetti plots by position at one class
make_classmen_spaghetti_plots <- function(draft_picks_dup_summary, start_params, class="Under"){
  classmen <- draft_picks_dup_summary %>%
    filter(underclassman==ifelse(class=="Under", "Yes", "No"))
  
  classmen_r2s <- get_yearly_r2s(classmen, "draft_number", "Draft Pick", each=FALSE) %>%
    arrange(desc(r2))
  
  draft_picks_for_classman_facet <- classmen %>%
    select(player_id, position, draft_number, mean_overall) %>%
    uncount(nrow(classmen_r2s)) %>%
    mutate(fct=position) %>%
    group_by(player_id) %>%
    mutate(fct=classmen_r2s$position) %>%
    ungroup() %>%
    mutate(is_fct=position==fct,
           grp=position)
  
  plot_faceted_spaghetti(draft_picks_for_classman_facet, classmen_r2s$position,
                         3, 4, start_params, "Classman",
                         classmen_r2s %>%
                           mutate(is_fct=TRUE, fct=position, grp=position), add_label=class)
}

# Plot faceted spaghetti plots by position at each class
make_classmen_spaghetti_plots(draft_picks_dup_summary, as.vector(coef(m_yes)), "Upper")
make_classmen_spaghetti_plots(draft_picks_dup_summary, as.vector(coef(m_no)), "Under")


# Split data into upper/underclassmen
upperclassmen <- draft_picks_dup_summary %>% filter(underclassman=="No")
underclassmen <- draft_picks_dup_summary %>% filter(underclassman=="Yes")

# Get best/top 3 available info by position for each class and plot
upperclassmen_pos_available_data <- plot_best_available_plots(
  upperclassmen %>% mutate(fct=position), 3, 4, " - Upperclassmen"
)

underclassmen_pos_available_data <- plot_best_available_plots(
  underclassmen %>% mutate(fct=position), 3, 4, " - Underclassmen"
)