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

# helper function to replace values in column
replace_values_in_column <- function(col, vals_to_replace, new_vals){
  for (i in 1:length(vals_to_replace)){
    col <- replace(col, col==vals_to_replace[i], new_vals[i])
  }
  return (col)
}

# helper function to plot change over time in Snaps per 100 for a specific role for a
# specific position
plot_change_over_time <- function(df, pos, role_text){
  ggplot(data=df, aes(x=season, y=100*n_plays)) +
    geom_point() + geom_line() + xlab("Season") + ylab("Snaps per 100") + ylim(c(0, max(100*df$n_plays) + 1)) +
    labs(title=paste(role_text, " for ", pos, "s over time", sep="")) +
    scale_x_continuous(breaks=min(df$season):max(df$season), labels=min(df$season):max(df$season)) +
    theme(panel.grid.minor=element_blank(), axis.title=element_text(size=15),
          axis.text=element_text(size=12), plot.title=element_text(size=20))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Deep Dives/", pos, "-",
               role_text, " Over Time.pdf", sep=""), width=12, height=7)
}

# helper function to plot change over time in Snaps per 100 for multiple roles for a
# specific position
plot_stacked_bar_change_over_time <- function(df, pos, role_text, var_type="position"){
  ggplot(data=df, aes(x=season, y=100*n_plays, fill=(!!sym(var_type)))) +
    geom_bar(position="stack", stat="identity") + xlab("Season") +
    ylab("Snaps per 100") + labs(title=paste(role_text, " for ", pos, "s over time", sep=""),
                                 fill=str_to_title(var_type)) +
    scale_x_continuous(breaks=min(df$season):max(df$season), labels=min(df$season):max(df$season)) +
    theme(panel.grid.minor=element_blank(), axis.title=element_text(size=15),
          axis.text=element_text(size=12), plot.title=element_text(size=20),
          legend.text = element_text(size=12), legend.title=element_text(size=15))
  ggsave(paste("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Deep Dives/", pos, "-",
               role_text, " Over Time.pdf", sep=""), width=12, height=7)
}

# helper function to get Snaps per 100 over time at specific roles for a specific position
role_change_over_time <- function(df, var_type="position"){
  return (df %>%
            filter(season >= 2006) %>%
            group_by((!!sym(var_type)), season) %>%
            summarize(n_plays=n()) %>%
            group_by(season) %>%
            mutate(n_plays=n_plays/sum(n_plays)))
}

# load player info
player_extract <- read.csv("../Data/ClubDB.dbo.PlayerExtract.csv")
colnames(player_extract)[15] <- "player_extract_position"

# load NFL gamekeys
game_keys <- read.csv("../Data/ClubDB.dbo.Game.csv") %>%
  select(game_key, season)

# load PFF PBP and join with gamekeys and player info
pff_pbp <- read.csv("../Data/pff_play_by_play.csv") %>%
  left_join(game_keys, by="game_key") %>%
  left_join(select(player_extract, player_id, player_extract_position), by="player_id")

# change player info positions in PFF PBP data to stay consistent
pff_pbp$player_extract_position <- replace_values_in_column(pff_pbp$player_extract_position,
                                                            c("C", "G", "DE", "DT", "FB", "FS", "ILB",
                                                              "MLB", "NT", "OLB", "SS", "T"),
                                                            c("IOL", "IOL", "EDGE", "DL", "RB", "S",
                                                              "LB", "LB", "DL", "LB", "S", "OT"))

# filter out DBs and special teams players
position_roles_over_time <- pff_pbp %>%
  filter(!(player_extract_position %in% c("DB", "K", "KR", "LS", "P")))

# Get and plot info on how RB roles have changed over time
rbs_over_time <- position_roles_over_time %>%
  filter(player_extract_position=="RB", role %in% c("Run", "Pass Route", "Pass Block", "Run Block"),
         (grepl("FB", position) | grepl("HB", position) | grepl("WR", position)))
rbs_over_time$position[grepl("FB", rbs_over_time$position)] <- "HB"
rbs_over_time$position[grepl("HB", rbs_over_time$position)] <- "HB"
rbs_over_time$position[grepl("WR", rbs_over_time$position)] <- "WR"

rbs_over_time_position_summary <- role_change_over_time(rbs_over_time)
plot_change_over_time(rbs_over_time_position_summary %>% filter(position=="WR"),
                      "RB", "Snaps taken at WR per 100")
rbs_over_time_role_summary <- role_change_over_time(rbs_over_time, var_type="role")
plot_stacked_bar_change_over_time(rbs_over_time_role_summary, "RB", "Snaps taken at each Role per 100",
                                  var_type="role")


# Same as above but for DBs
dbs_over_time <- position_roles_over_time %>%
  filter(player_extract_position %in% c("S", "CB"), 
         (grepl("S", position) | grepl("CB", position) | grepl("LB", position)))
dbs_over_time$position[grepl("LB", dbs_over_time$position)] <- "LB"
dbs_over_time$position[grepl("CB", dbs_over_time$position)] <- "CB"
dbs_over_time$position[grepl("SS", dbs_over_time$position)] <- "SS"
dbs_over_time$position[grepl("FS", dbs_over_time$position)] <- "FS"
dbs_over_time <- dbs_over_time %>%
  filter(position %in% c("LB", "CB", "SS", "FS"))

dbs_over_time_position_summary <- role_change_over_time(dbs_over_time)
plot_change_over_time(dbs_over_time_position_summary %>% filter(position=="LB"),
                      "DB", "Snaps taken at LB per 100")


# Same as above but for TEs
tes_over_time <- position_roles_over_time %>%
  filter(player_extract_position=="TE", (grepl("TE", position) | grepl("WR", position) |
                                           grepl("FB", position) | grepl("HB", position)),
         role %in% c("Pass Block", "Pass Route", "Run", "Run Block")) %>%
  filter(!(position %in% c("FBL", "FBL2", "FBM", "FBR", "KFB", "MFB", "PFB")))
tes_over_time$position[grepl("B", tes_over_time$position)] <- "HB"
tes_over_time$position[grepl("TE", tes_over_time$position)] <- "TE"
tes_over_time$position[grepl("WR", tes_over_time$position)] <- "WR"

tes_over_time_position_summary <- role_change_over_time(tes_over_time)
plot_stacked_bar_change_over_time(tes_over_time_position_summary, "TE",
                                  "Snaps taken at each Position per 100")

tes_over_time_role_summary <- role_change_over_time(tes_over_time %>% filter(role != "Run"),
                                                    var_type="role")
plot_stacked_bar_change_over_time(tes_over_time_role_summary, "TE",
                                  "Snaps taken at each Role per 100", var_type="role")



# Same as above but for WRs
wrs_over_time <- position_roles_over_time %>%
  filter(player_extract_position=="WR", (grepl("TE", position) | grepl("WR", position) |
                                           grepl("FB", position) | grepl("HB", position)),
         role %in% c("Pass Route", "Run", "Run Block")) %>%
  filter(!(position %in% c("FBL", "FBL2", "FBM", "FBR", "KFB", "MFB", "PFB")))
wrs_over_time$position[grepl("B", wrs_over_time$position)] <- "HB"
wrs_over_time$position[grepl("TE", wrs_over_time$position)] <- "TE"
wrs_over_time$position[grepl("WR", wrs_over_time$position)] <- "WR"

wrs_over_time_position_summary <- role_change_over_time(wrs_over_time)
plot_stacked_bar_change_over_time(wrs_over_time_position_summary, "WR",
                                  "Snaps taken at each Position per 100")

wrs_over_time_role_summary <- role_change_over_time(wrs_over_time %>% filter(role != "Run"),
                                                    var_type="role")
plot_stacked_bar_change_over_time(wrs_over_time_role_summary, "WR",
                                  "Snaps taken at each Role per 100", var_type="role")

# Same as above but for specific types of DB
dbs_over_time <- position_roles_over_time %>%
  filter(player_extract_position %in% c("S", "CB"), 
         (grepl("S", position) | grepl("CB", position)),
         role %in% c("Coverage", "Run Defense", "Pass Rush"), position != "SLWR")
dbs_over_time$position[grepl("SCB", dbs_over_time$position)] <- "Slot Corner"
dbs_over_time$position[grepl("CB", dbs_over_time$position)] <- "Outside Corner"
dbs_over_time$position[grepl("SS", dbs_over_time$position)] <- "SS"
dbs_over_time$position[grepl("FS", dbs_over_time$position)] <- "FS"

for (pos in unique(dbs_over_time$position)){
  db_over_time_role_summary <- role_change_over_time(dbs_over_time %>% filter(position==pos),
                                                     var_type="role")
  plot_stacked_bar_change_over_time(db_over_time_role_summary, pos, 
                                    "Snaps taken at each Role per 100", var_type="role")
}


# Same as above but for LBs
lbs_over_time <- position_roles_over_time %>%
  filter(player_extract_position=="LB", (grepl("LB", position) | grepl("CB", position) |
                                           grepl("T", position) | grepl("E", position) |
                                           (grepl("S", position) & !grepl("WR", position))),
         !grepl("TE", position), !grepl("P", position),
         !(position %in% c("DL-LB", "LB", "LB1", "LB2", "LT", "RT", "FRT", "FLE", "FRE")),
         role %in% c("Coverage", "Lined up Defense", "Pass Rush", "Run Defense"))
lbs_over_time$position[grepl("SS", lbs_over_time$position) | grepl("FS", lbs_over_time$position)] <- "S"
lbs_over_time$position[grepl("E", lbs_over_time$position)] <- "DE"
lbs_over_time$position[grepl("CB", lbs_over_time$position)] <- "CB"
lbs_over_time$position[grepl("LB", lbs_over_time$position)] <- "LB"
lbs_over_time$position[grepl("T", lbs_over_time$position)] <- "DT"

lbs_over_time_position_summary <- role_change_over_time(lbs_over_time)
plot_stacked_bar_change_over_time(lbs_over_time_position_summary, "LB",
                                  "Snaps taken at each Position per 100")