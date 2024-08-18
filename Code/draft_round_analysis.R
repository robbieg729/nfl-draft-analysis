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
source("draft_curve_functions.R")

draft_picks_dup_summary <- read.csv("../Data/draft_picks_madden.csv") %>%
  select(-X)

## Best available analysis
draft_round_available_rank_info <- plot_best_available_plots(draft_picks_dup_summary %>%
                                   mutate(fct=draft_round),
                                 3, 3, "Draft Round")