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
library(import)

# import function to fit exponential model
import::from("draft_curve_functions.R", fit_exp_model)

# helper function to replace values in column
replace_values_in_column <- function(col, vals_to_replace, new_vals){
  for (i in 1:length(vals_to_replace)){
    col <- replace(col, col==vals_to_replace[i], new_vals[i])
  }
  return (col)
}

# helper function to get draft picks of a certain position with the model fitted
get_pos_draft_picks <- function(draft_picks_dup_summary, pos){
  pos_df <- draft_picks_dup_summary %>%
    filter(position==pos)
  
  m_pos <- fit_exp_model(pos_df %>% mutate(x=draft_number),
                        "draft_number")
  
  pos_df$pred_overall <- m_pos$fit
  pos_df$pred_min <- m_pos$lwr
  pos_df$pred_max <- m_pos$upr
  pos_df <- pos_df %>%
    mutate(below_curve=mean_overall < pred_min,
           mroe=mean_overall - pred_overall) # mroe = Madden Rating Over Expected
  
  return (pos_df)
}

# helper function to plot mroe vs NGS Combine Scores
plot_ngs_mroe <- function(pos_df, pos_full, pos_abbr){
  ggplot(data=pos_df %>%
           pivot_longer(cols=c("final_score", "athleticism_score", "production_score"),
                        names_to = "score_type", values_to="score"), aes(x=score, y=mroe)) +
    geom_point() + geom_smooth(method="lm") + facet_wrap(~factor(score_type, c("final_score",
                                                                               "athleticism_score",
                                                                               "production_score")),
                                                         nrow=2) +
    xlab("Score") + ylab("MRoE") + labs(title=paste("MRoE vs. NGS Combine Scores for", pos_full))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Deep Dives/", pos_abbr,
         " NGS Scores.pdf", sep=""),  width=7, height=7)
}

# helper function to get PFF play-by-play summaries for a certain position based on their
# role/position lined up at
get_pff_summaries <- function(pff_pos, pos_df, min_snaps=500, type="role"){
  pff_summary <- pff_pos %>%
    group_by(player_id, !!sym(type)) %>%
    summarize(n_plays=n()) %>%
    group_by(player_id) %>%
    filter(sum(n_plays) >= min_snaps) %>%
    mutate(n_plays=n_plays/sum(n_plays)) %>%
    left_join(select(pos_df, player_id, name, mroe), by="player_id")
  
  return (pff_summary)
}

# helper function to plot PFF play-by-play summaries for a certain position based on their 
# role/position lined up at against MRoE
plot_pff_summaries <- function(pff_summaries, pos, levels, min_snaps=500, type="role", nrow=NULL){
  pff_summaries <- pff_summaries %>%
    mutate(fct=(!!sym(type)))
  ggplot(data=pff_summaries , aes(x=n_plays, y=mroe)) +
    geom_point() + geom_smooth(method="lm") + facet_wrap(~factor(fct, levels),
                                                         scales="free", nrow=nrow) +
    xlab("% of Snaps") + ylab("MRoE") + geom_hline(yintercept=0, color="red") +
    labs(title=paste("MRoE vs. % of Snaps at each", str_to_title(type), "for", pos),
         subtitle=paste("Min.", min_snaps, "Snaps"))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Deep Dives/", 
               pos, " ", str_to_title(type), ".pdf", sep=""),
         height=7, width=10)
}

# helper function to plot certain Madden Traits vs MRoE
plot_madden_traits <- function(madden_ratings, pos_df, pos, traits, nrow=NULL){
  third_to_fifth_traits <- madden_ratings %>% 
    filter(playing_year %in% 3:5) %>%
    group_by(player_id) %>%
    summarize_at(traits, mean, na.rm=TRUE)
  
  pos_df <- pos_df %>%
    left_join(third_to_fifth_traits, by="player_id")
  
  ggplot(data=pos_df %>%
           pivot_longer(cols=all_of(traits), names_to="trait", values_to="trait_score"),
         aes(x=trait_score, y=mroe)) +
    geom_point() + geom_smooth(method="lm") + facet_wrap(~trait, scales="free") +
    xlab("Rating") + ylab("MRoE") + geom_hline(yintercept=0, color="red") +
    labs(title=paste("MRoE vs. Average Madden Traits in Years 3-5 for", pos))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Deep Dives/", 
               substr(pos, 1, nchar(pos) - 1), " Madden Traits.pdf", sep=""),
         height=7, width=12)
}

# load draft picks-Madden Ratings dataset
draft_picks_dup_summary <- read.csv("../Data/draft_picks_madden.csv") %>%
  select(-X)

# load player info
player_extract <- read.csv("../Data/ClubDB.dbo.PlayerExtract.csv")
colnames(player_extract)[15] <- "player_extract_position"

# load Madden Ratings
madden_ratings <- read.csv("../Data/dna_football_ops_db.Initial_Madden_Ratings.csv") %>%
  filter(season >= 2012) %>%
  left_join(select(draft_picks_dup_summary, player_id, draft_year),
            by="player_id") %>%
  mutate(playing_year=season - draft_year + 1)

# load NFL gamekeys
game_keys <- read.csv("../Data/ClubDB.dbo.Game.csv") %>%
  select(game_key, season)

# Load PFF play-by-play data and join with gamekeys and player info
pff_pbp <- read.csv("../Data/pff_play_by_play.csv") %>%
  left_join(game_keys, by="game_key") %>%
  left_join(select(player_extract, player_id, player_extract_position), by="player_id")

# adjust player info positions in PFF PBP data
pff_pbp$player_extract_position <- replace_values_in_column(pff_pbp$player_extract_position,
                                                            c("C", "G", "DE", "DT", "FB", "FS", "ILB",
                                                              "MLB", "NT", "OLB", "SS", "T"),
                                                            c("IOL", "IOL", "EDGE", "DL", "RB", "S",
                                                              "LB", "LB", "DL", "LB", "S", "OT"))

# Get NGS Combine data incl. final scores, athleticism scores, production scores
ngs_combine_data <- read.csv("../Data/ngs_api_combine_workout_results.csv")

# join draft picks-Madden Ratings data to player info
draft_picks_dup_summary <- draft_picks_dup_summary %>%
  left_join(select(player_extract, player_id, player_extract_position, elias_player_id),
            by="player_id")


# Get all IOLs
iols <- get_pos_draft_picks(draft_picks_dup_summary, "IOL")

# Split into centers and guards
centers <- iols %>%
  filter(player_extract_position=="C") %>%
  group_by(draft_year) %>%
  mutate(pos_rank=rank(draft_number))

guards <- iols %>%
  filter(player_extract_position=="G") %>%
  group_by(draft_year) %>%
  mutate(pos_rank=rank(draft_number))

# compute Kendalls Tau for each to see if one is drafted better than the other
centers_kendalls <- centers %>%
  group_by(draft_year) %>%
  summarize(kendalls=cor(pos_rank, -mean_overall, method="kendall"))

guards_kendalls <- guards %>%
  group_by(draft_year) %>%
  summarize(kendalls=cor(pos_rank, -mean_overall, method="kendall"))





# Get all safeties
safeties <- get_pos_draft_picks(draft_picks_dup_summary, "S")

# Compare MROE with NGS Combine scores for safeties
plot_ngs_mroe(safeties, "Safeties", "S")

# Get PFF PBP for all safeties in data and filter for three specific roles
pff_safeties <- pff_pbp %>%
  filter(player_id %in% safeties$player_id,
         role %in% c("Coverage", "Pass Rush", "Run Defense"))

# make positions less specific for grouping purposes
pff_safeties$position[grepl("LB", pff_safeties$position)] <- "LB"
pff_safeties$position[grepl("CB", pff_safeties$position)] <- "CB"
pff_safeties$position[grepl("SS", pff_safeties$position)] <- "SS"
pff_safeties$position[grepl("FS", pff_safeties$position)] <- "FS"

# get and plot summaries of snap rates at each role vs. MRoE for safeties
pff_safeties_role_summaries <- get_pff_summaries(pff_safeties, safeties)
plot_pff_summaries(pff_safeties_role_summaries, "Safeties", c("Coverage", "Run Defense", "Pass Rush"),
                   nrow=2, type="role")

# get and plot summaries of snap rates at each position lined up vs. MRoE for safeties
pff_safeties_position_summaries <- get_pff_summaries(pff_safeties %>%
                                                       filter(position %in% c("LB", "SS", "CB", "FS")),
                                                     safeties, type="position")
plot_pff_summaries(pff_safeties_position_summaries, "Safeties", c("FS", "SS", "CB", "LB"),
                   nrow=2, type="position")

# Split safeties into Free Safeties and Strong Safeties
free_safeties <- safeties %>%
  filter(player_extract_position=="FS") %>%
  group_by(draft_year) %>%
  mutate(pos_rank=rank(draft_number))

strong_safeties <- safeties %>%
  filter(player_extract_position=="SS") %>%
  group_by(draft_year) %>%
  mutate(pos_rank=rank(draft_number))

# Compute Kendalls Tau for each draft class to see if one is drafted better than the other
fs_kendalls <- free_safeties %>%
  group_by(draft_year) %>%
  summarize(kendalls=cor(pos_rank, -mean_overall, method="kendall"))

ss_kendalls <- strong_safeties %>%
  group_by(draft_year) %>%
  summarize(kendalls=cor(pos_rank, -mean_overall, method="kendall"))





# get all WRs
wrs <- get_pos_draft_picks(draft_picks_dup_summary, "WR") %>%
  filter(player_id != 46289)

# Get some relevant Madden traits for WRs
wr_madden_traits <- c("agility", "deep_route_running", "elusiveness", "medium_route_running",
                      "release_off_line", "route_running", "short_route_running", "speed",
                      "catch_in_traffic", "catching", "spectacular_catch")

# Get PFF PBP for WRs in data
pff_wrs <- pff_pbp %>%
  filter(player_id %in% wrs$player_id, role %in% c("Pass Route", "Run Block", "Run"),
         grepl("WR", position))

# Plot NGS Combine Scores vs. MRoE
plot_ngs_mroe(wrs, "WRs", "WR")

# Change positions into Slot WR/Wideout
pff_wrs$position[grepl("S", pff_wrs$position)] <- "Slot"
pff_wrs$position[!grepl("S", pff_wrs$position)] <- "Wideout"

# Get and plot summaries of snap rates at each role vs. MRoE
pff_wrs_role_summaries <- get_pff_summaries(pff_wrs, wrs)
plot_pff_summaries(pff_wrs_role_summaries, "WRs", c("Pass Route", "Run Block", "Run"),
                   type="role")

# Get and plot summaries of snap rates at each position lined up vs. MRoE
pff_wrs_position_summaries <- get_pff_summaries(pff_wrs, wrs, type="position")
plot_pff_summaries(pff_wrs_position_summaries, "WRs", c("Wideout", "Slot"),
                   type="position")

# Plot MRoE vs. Madden Traits
plot_madden_traits(madden_ratings, wrs, "WRs", wr_madden_traits)



# Same as for WRs but for RBs
rbs <- get_pos_draft_picks(draft_picks_dup_summary, "RB")
rb_madden_traits <- c("agility", "ball_carrier_vision", "elusiveness", "break_tackle",
                      "carrying", "catching", "change_of_direction", "juke_move",
                      "route_running", "speed", "trucking", "release_off_line")

pff_rbs <- pff_pbp %>%
  filter(player_id %in% rbs$player_id, role %in% c("Run", "Pass Route", "Pass Block"),
         (grepl("FB", position) | grepl("HB", position) | grepl("WR", position)))
pff_rbs$position[grepl("FB", pff_rbs$position)] <- "HB"
pff_rbs$position[grepl("HB", pff_rbs$position)] <- "HB"
pff_rbs$position[grepl("WR", pff_rbs$position)] <- "WR"

plot_ngs_mroe(rbs, "RBs", "RB")

pff_rbs_role_summaries <- get_pff_summaries(pff_rbs, rbs)
plot_pff_summaries(pff_rbs_role_summaries, "RBs", c("Run", "Pass Route", "Pass Block"),
                   type="role")

pff_rbs_position_summaries <- get_pff_summaries(pff_rbs, rbs, type="position")
plot_pff_summaries(pff_rbs_position_summaries %>%
                     filter(name != "Cordarrelle Patterson"), "RBs", c("HB", "WR"),
                   type="position")

plot_madden_traits(madden_ratings, rbs, "RBs", rb_madden_traits)





# Same as for RBs and WRs but for LBs
lbs <- get_pos_draft_picks(draft_picks_dup_summary, "LB")

pff_lbs <- pff_pbp %>%
  filter(player_id %in% lbs$player_id, role %in% c("Coverage", "Run Defense", "Pass Rush"),
         (grepl("LB", position) | grepl("CB", position) |
            grepl("RE", position) | grepl("LE", position)))
pff_lbs$position[grepl("E", pff_lbs$position)] <- "DE"
pff_lbs$position[grepl("ILB", pff_lbs$position)] <- "ILB"
pff_lbs$position[grepl("OLB", pff_lbs$position)] <- "OLB"
pff_lbs$position[grepl("CB", pff_lbs$position)] <- "CB"
pff_lbs$position[grepl("RLB", pff_lbs$position) | grepl("LLB", pff_lbs$position)] <- "RLB/LLB"

plot_ngs_mroe(lbs, "LBs", "LB")

pff_lbs_role_summaries <- get_pff_summaries(pff_lbs, lbs)
plot_pff_summaries(pff_lbs_role_summaries, "LBs", c("Coverage", "Pass Rush", "Run Defense"),
                   type="role", nrow=2)

pff_lbs_position_summaries <- get_pff_summaries(pff_lbs, lbs, type="position")
plot_pff_summaries(pff_lbs_position_summaries, "LBs", c("MLB", "ILB", "OLB", "RLB/LLB", "DE", "CB"),
                   type="position", nrow=3)




## Same as for RBs, WRs, and LBs but for CBs
cbs <- get_pos_draft_picks(draft_picks_dup_summary, "CB")
cb_madden_traits <- c("zone_coverage", "tackle", "speed", "press_coverage",
                      "man_coverage")

pff_cbs <- pff_pbp %>%
  filter(player_id %in% cbs$player_id, role %in% c("Coverage", "Pass Rush", "Run Defense"),
         (grepl("S", position) | grepl("CB", position) | grepl("LB", position)))
pff_cbs$position[grepl("FS", pff_cbs$position) | grepl("SS", pff_cbs$position)] <- "S"
pff_cbs$position[grepl("LB", pff_cbs$position)] <- "LB"
pff_cbs$position[grepl("SCB", pff_cbs$position)] <- "Slot Corner"
pff_cbs$position[grepl("CB", pff_cbs$position)] <- "Outside Corner"


plot_ngs_mroe(cbs, "CBs", "CB")

pff_cbs_role_summaries <- get_pff_summaries(pff_cbs, cbs)
plot_pff_summaries(pff_cbs_role_summaries, "CBs", c("Coverage", "Run Defense", "Pass Rush"),
                   type="role", nrow=2)

pff_cbs_position_summaries <- get_pff_summaries(pff_cbs, cbs, type="position")
plot_pff_summaries(pff_cbs_position_summaries, "CBs", c("Outside Corner", "Slot Corner", "LB", "S"),
                   type="position", nrow=2)

plot_madden_traits(madden_ratings, cbs, "CBs", cb_madden_traits)




# Same as above but for DLs
dls <- get_pos_draft_picks(draft_picks_dup_summary, "DL")
dl_madden_traits <- c("block_shedding", "finesse_move", "pursuit", "strength", "tackle",
                      "power_moves")

pff_dls <- pff_pbp %>%
  filter(player_id %in% dls$player_id, role %in% c("Pass Rush", "Run Defense"),
         (grepl("T", position) | grepl("E", position)))
pff_dls$position[grepl("N", pff_dls$position)] <- "NT"
pff_dls$position[grepl("D", pff_dls$position)] <- "DT"
pff_dls$position[grepl("E", pff_dls$position)] <- "End"

plot_ngs_mroe(dls, "DLs", "DL")

pff_dls_role_summaries <- get_pff_summaries(pff_dls, dls)
plot_pff_summaries(pff_dls_role_summaries, "DLs", c("Pass Rush", "Run Defense"),
                   type="role")

pff_dls_position_summaries <- get_pff_summaries(pff_dls, dls, type="position")
plot_pff_summaries(pff_dls_position_summaries, "DLs", c("DT", "NT", "End"),
                   type="position", nrow=2)

plot_madden_traits(madden_ratings, dls, "DLs", dl_madden_traits)