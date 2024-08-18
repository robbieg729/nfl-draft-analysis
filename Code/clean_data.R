library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
source("load_data.R")

# Helper function to replace vector of values with replacements in a column
replace_values_in_column <- function(col, vals_to_replace, new_vals){
  for (i in 1:length(vals_to_replace)){
    col <- replace(col, col==vals_to_replace[i], new_vals[i])
  }
  return (col)
}

# Store offensive and defensive position labels
offensive_positions <- c("QB", "RB", "WR", "TE", "OT", "IOL", "C", "G")
defensive_positions <- c("CB", "LB", "DL", "S", "EDGE")

# Add variables to Player Info dataset and select relevant columns
players <- players %>%
  mutate(udfa=as.numeric(is.na(draft_round)),
         name=paste(football_name, last_name),
         birthdate=as.Date(birthdate),
         elias_player_id=ifelse(player_id==46653, "THO263262", elias_player_id)) %>% #impute required value
  select(player_id, gsisid, football_name, first_name, middle_name, last_name,
         college, college_conference, college_conference_abbr,
         height, weight, position_abbr, entry_year, rookie_year, 
         draft_round, draft_number, draft_pick, draft_club, current_club,
         current_club_full_name, enter_under_classman, status_description_abbr,
         status_short_description, str_status_description, accrued_seasons, nfl_experience,
         elias_player_id, entry_club, udfa, name, birthdate) %>%
  filter(player_id!=51831) # remove duplicate elias_player_id

# Change certain position labels to be consistent
players$position_abbr <- replace_values_in_column(players$position_abbr,
                                 c("T", "OLB", "DT",
                                   "SS", "ILB",
                                   "FS", "DE", "NT", "MLB",
                                   "FB", "G", "C", "OL"),
                                 c("OT", "LB", "DL", "S", "LB", "S", "EDGE", "DL", "LB", "RB",
                                   "IOL", "IOL", "IOL"))


# Cleaning Madden Ratings

# Use only player-seasons from 2012 onwards
madden_ratings <- madden_ratings %>% filter(season >= 2012)

# Update position labels to be consistent
madden_ratings$madden_position <- replace_values_in_column(madden_ratings$madden_position,
                                          c("LOLB", "ROLB", "RG", "LG","RT", "LT", "HB", "FB",
                                            "SS", "FS", "LE", "DT", "MLB",
                                            "RE", "C"),
                                          c("LB", "LB", "IOL", "IOL", "OT", "OT", "RB", "RB", "S", "S",
                                            "EDGE", "DL", "LB", "EDGE", "IOL"))

# Merge with Player Info to get additional info
madden_ratings <- inner_join(select(players, c(player_id,
                                              position_abbr,
                                              rookie_year, draft_number)), madden_ratings,
                            by="player_id")

# Long-Snappers listed as TEs in Madden, so update this
madden_ratings$madden_position <- replace(madden_ratings$madden_position,
                                          madden_ratings$position_abbr=="LS",
                                          "LS")

# Update more positions
for (i in 1:nrow(madden_ratings)){
  madden_ratings$madden_position[i] <- if (madden_ratings$position_abbr[i] %in% c("DB", "C", "G") | is.na(madden_ratings$position_abbr[i])) madden_ratings$madden_position[i] else madden_ratings$position_abbr[i]
}

# Change column name of "madden_position" to "position"
colnames(madden_ratings)[8] <- "position"

# Bind Madden Ratings to Madden 25 Ratings
madden_ratings <- bind_rows(madden_ratings, 
                            select(madden_25_ratings,
                                   player_id, madden_name, overall, season, draft_number, draft_year, team)) %>%
  group_by(player_id) %>%
  mutate(position=position[1],
         rookie_year=rookie_year[1])

# Add more variables to Madden Ratings indicating playing year of career and if player is 2024 Free Agent
madden_ratings <- madden_ratings %>%
  mutate(playing_year=season - rookie_year + 1,
         fa_2024=(season == 2024 & is.na(team)))

# Filter for 2024 Free Agents
fas_2024 <- madden_ratings %>%
  filter(fa_2024==1) %>%
  left_join(select(players, player_id, str_status_description),
            by="player_id")

# Split 2024 Free Agents into Active and Inactive
active_fas_2024 <- fas_2024 %>% filter(str_status_description=="Active")
inactive_fas_2024 <- fas_2024 %>% filter(str_status_description!="Active") %>%
  select(madden_name, str_status_description)



# Clean Game Roster Info

# Filter for Regular season games after 2012
game_rosters_inj <- game_rosters_inj %>%
  filter(season >= 2012, season_type=="Reg") %>%
  mutate(playing_year=season - rookie_year + 1,
         name=paste(football_name, last_name))

# Update previous club acronyms to be same as current ones
game_rosters_inj$draft_club <- replace_values_in_column(game_rosters_inj$draft_club,
                                                        c("OAK", "SL", "SD", "LA", "CLE"),
                                                        c("LV", "LAR", "LAC", "LAR", "CLV"))
game_rosters_inj$club_code <- replace_values_in_column(game_rosters_inj$club_code,
                                                        c("OAK", "SL", "SD", "LA", "CLE"),
                                                        c("LV", "LAR", "LAC", "LAR", "CLV"))

# Correct spelling error in status descriptions
game_rosters_inj$status_description <- replace_values_in_column(game_rosters_inj$status_description,
                                                                c("Reseve/Non-Football Injury; Designated for Return"),
                                                                c("Reserve/Non-Football Injury; Designated for Return")) 

# Add indicators for roster type for each player and each game
game_rosters_inj <- game_rosters_inj %>%
  mutate(practice_squad=as.numeric(grepl("Practice", status_description)),
         active=as.numeric(status_description=="Active" | grepl("Inactive", status_description)
                           | grepl("Exempt", status_description)
                           | (grepl("Reserve", status_description) & !(grepl("Retired", status_description)) & !(grepl("Indefinite Suspension", status_description)))),
         non_roster=as.numeric(grepl("Waivers", status_description)
                               | grepl("Retired", status_description) 
                               | grepl("Indefinite Suspension", status_description))) %>%
  filter(!is.na(game_key))

# Get roster type proportions for each player-season
players_roster_info <- game_rosters_inj %>%
  group_by(player_id, season) %>%
  summarize(games=n(), active_prop=min(sum(active)/ifelse(season < 2021, 16, 17), 1),
            practice_squad_prop=1 - active_prop,
            position=position_abbr[1],
            draft_number=draft_number[1],
            rookie_year=rookie_year[1], entry_year=entry_year[1],
            name=paste(football_name[1], last_name[1]))


# Clean data on NFL draft picks

# Filter for 2012 NFL Draft and after, add variables, and join with Player Info
draft_picks <- draft_picks %>%
  filter(season >= 2012) %>%
  select(-c(pfr_player_id, cfb_player_id)) %>%
  mutate(pro_bowl_rate=ifelse(seasons_started==0, 0, probowls/seasons_started),
         allpro_rate=ifelse(seasons_started==0, 0, allpro/seasons_started)) %>%
  left_join(select(players, entry_year, player_id, draft_number, position_abbr),
           by=c("season"="entry_year", "pick"="draft_number"), multiple="first")

# Update position labels to be consistent
draft_picks$position <- replace_values_in_column(draft_picks$position,
                                                 c("C", "DT", "FS", "G", "OG", "T", "OL"),
                                                 c("IOL", "DL", "S", "IOL", "IOL", "OT", "IOL"))
for (i in 1:nrow(draft_picks)){
  draft_picks$position[i] <- if (draft_picks$position_abbr[i] %in% c("DB", "C", "G") | is.na(draft_picks$position_abbr[i])) draft_picks$position[i] else draft_picks$position_abbr[i]
}

# Replace players still listed as DBs with their true position
dbs <- draft_picks %>% filter(position=="DB", season!=2024)
dbs_names <- c("Josh Furman", "Lavelle Westbrooks", "Kendall James", "Khalid Wooten", "Jamoris Slaughter",
  "Terry Hawthorne", "Greg McCoy", "Markelle Martin", "DeQuan Menzie", "Brandon Hardin")
dbs_pos <- c("S", "CB", "CB", "CB", "S", "CB", "CB", "S", "CB", "S")
for (i in 1:nrow(dbs)){
  draft_picks$position <- replace(draft_picks$position, draft_picks$pfr_player_name==dbs_names[i],
                                  dbs_pos[i])
}

# Add position ranking variable (i.e., out of all players at the position in that year, where was that player taken)
draft_picks <- draft_picks %>%
  group_by(season, position) %>%
  mutate(position_rank=rank(pick))

# Clean GM data

# Filter for GMs with at least 5 drafts
gm_data <- gm_data_raw %>%
  filter(To >= 2016, Teams != "", grepl("General Manager", Positions)) %>%
  mutate(years_as_gm=To - From,
         n_teams=str_count(Teams, ",") + 1) %>%
  filter(years_as_gm >= 5)
gm_data <- rbind(gm_data, gm_data %>% filter(n_teams > 1))
gm_data <- rbind(gm_data, gm_data %>% filter(n_teams==3))
write.csv(gm_data, "../Data/GM Data.csv")

# Prepare data for merging with draft picks dataset
gm_data$stint_id <- 1:nrow(gm_data)
gm_data <- gm_data %>%
  group_by(stint_id) %>%
  mutate(From=max(From, 2012),
         To=min(To, 2021),
         years_as_gm=To - From + 1) %>%
  group_by(Name) %>%
  filter(sum(years_as_gm) >= 5)

# Add team abbreviations
gm_team_names <- unique(gm_data$Teams)
gm_team_abbrs <- c("SFO", "CHI", "CIN", "DEN", "KAN", "CLE",
                   "NWE", "BAL", "TAM", "WAS", "ARI", "IND", "DAL",
                   "MIA", "MIN", "PHI", "ATL", "NYG", "JAX", "DET",
                   "GNB", "CAR", "LVR", "LAC", "LAR", "NOR", "SEA",
                   "PIT", "HOU", "TEN")
gm_data$team_abbr <- gm_data$Teams
gm_data$team_abbr <- replace_values_in_column(gm_data$team_abbr,
                                              gm_team_names, gm_team_abbrs)

# Long-format data storing GM for each team-draft
gm_by_team_season <- data.frame(team=rep(append(gm_team_abbrs, c("NYJ", "BUF")), 10)) %>%
  group_by(team) %>%
  mutate(season=2012:2021,
         gm=rep(NA, 10))
for (tm in gm_team_abbrs){
  gms <- gm_data %>% filter(team_abbr==tm)
  for (i in 1:nrow(gms)){
    yrs <- gms[i,]$From:gms[i,]$To
    gm_by_team_season[gm_by_team_season$season %in% yrs & gm_by_team_season$team==tm,]$gm <- gms[i,]$Name
  }
}