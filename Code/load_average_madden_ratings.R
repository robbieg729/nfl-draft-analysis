# Load and clean data
source("load_data.R")
source("clean_data.R")

# Load required functions
source("draft_curve_functions.R")

# Helper function to calculate overall Madden Rank of player within draft position class
ranks_overall <- function(v){
  ranks <- c()
  for (i in 1:length(v)){
    if (i == 1){
      v_i <- v[2:length(v)]
    } else if (i == length(v)){
      v_i <- v[1:length(v) - 1]
    } else{
      v_i <- append(v[1:i - 1], v[i + 1:length(v)])
    }
    v_i <- v_i - 0.99
    v_i <- append(v[i], v_i)
    ranks <- append(ranks, rank(-v_i, ties.method="min")[1])
  }
  return (ranks)
}

# Helper function to compute Madden Rank of player in draft position class out of remaining players when selected
ranks_available <- function(v){
  ranks <- c()
  for (i in 1:length(v)){
    v_i <- v[i:length(v)]
    v_i[2:length(v_i)] <- v_i[2:length(v_i)] - 0.99
    ranks <- append(ranks, rank(-v_i, ties.method="min")[1])
  }
  return (ranks)
}

# Helper function to get Madden Rating difference to best available player in draft position class when selected
diff_to_best_available <- function(v){
  diffs <- c()
  for (i in 1:length(v)){
    if (i == length(v)){
      diffs <- append(diffs, 0)
    } else{
      v_i <- v[i + 1:length(v)]
      diff <- v[i] - max(v_i, na.rm=TRUE)
      diffs <- append(diffs, ifelse(diff > -1, 0, diff))
    }
    
  }
  return (diffs)
}

# Helper function to merge Player Info with more information
merge_to_required_data <- function(df_players, type="Draft Picks"){
  df_players <- df_players %>%
    group_by(player_id) %>%
    mutate(playing_year=season - season[1] + 1,
           draft_year=season[1]) %>%
    left_join(select(players_roster_info,
                     player_id, season,
                     active_prop, practice_squad_prop), 
              by=c("player_id", "season"), multiple="first") %>%
    left_join(select(madden_ratings, player_id, season, overall, fa_2024),
              by=c("player_id", "season"), multiple="first")
  if (type=="Draft Picks"){
    df_players <- df_players %>%
      left_join(select(players, player_id, str_status_description, gsisid, college_conference_abbr,
                       college_conference, enter_under_classman),
                by="player_id", relationship="many-to-one")
  }
  df_players <- df_players %>%
    mutate(active_prop=ifelse(season == 2024, ifelse(str_status_description=="Reserve/Retired" | is.na(overall), 0, 1),
                              ifelse(is.na(active_prop), 0, active_prop)),
           practice_squad_prop=ifelse(season == 2024, ifelse(str_status_description=="Reserve/Retired" | is.na(overall), 1, 0),
                                      ifelse(is.na(practice_squad_prop), 1, practice_squad_prop)),
           status=ifelse(active_prop > practice_squad_prop, "Active", "Practice Squad/Non-Rostered")) %>%
    filter(!(position %in% c("K", "LS", "P", "SAF", "DB")),
           !is.na(position))
  
  return (df_players)
}

# Helper function to impute Madden Ratings based on roster type
impute_madden_ratings <- function(df_players_dup){
  replacement_overalls_by_pos <- df_players_dup %>%
    filter(status!="Active") %>%
    group_by(position) %>%
    summarize(ovr_5th=quantile(overall, probs=0.05, na.rm=TRUE),
              ovr_10th=quantile(overall, probs=0.1, na.rm=TRUE),
              ovr_15th=quantile(overall, probs=0.15, na.rm=TRUE),
              ovr_20th=quantile(overall, probs=0.2, na.rm=TRUE),
              ovr_25th=quantile(overall, probs=0.25, na.rm=TRUE),
              ovr_30th=quantile(overall, probs=0.3, na.rm=TRUE),
              ovr_35th=quantile(overall, probs=0.35, na.rm=TRUE),
              ovr_40th=quantile(overall, probs=0.4, na.rm=TRUE),
              ovr_45th=quantile(overall, probs=0.45, na.rm=TRUE),
              ovr_50th=quantile(overall, probs=0.5, na.rm=TRUE))
  
  df_players_dup_adj <- df_players_dup
  
  for (pos in replacement_overalls_by_pos$position){
    replacements <- replacement_overalls_by_pos %>% filter(position==pos)
    nas <- df_players_dup %>% 
      filter(position==pos, is.na(overall))
    non_nas <- df_players_dup %>%
      filter(position==pos, !is.na(overall))
    nas <- nas %>%
      mutate(overall=ifelse(status=="Active", replacements$ovr_50th,
                            ifelse(practice_squad_prop==0.5,
                                   replacements$ovr_25th,
                                   ifelse(practice_squad_prop < 0.75,
                                          replacements$ovr_15th,
                                          replacements$ovr_5th))))
    non_nas <- non_nas %>%
      mutate(overall=ifelse(status=="Active", overall,
                            ifelse(practice_squad_prop==0.5,
                                   ifelse(overall < replacements$ovr_25th,
                                          overall, replacements$ovr_25th),
                                   ifelse(practice_squad_prop < 0.75,
                                          ifelse(overall < replacements$ovr_15th,
                                                 overall, replacements$ovr_15th),
                                          replacements$ovr_5th))))
    pos_players <- rbind(nas, non_nas)
    df_players_dup_adj <- rbind(df_players_dup_adj %>% filter(position != pos),
                                 pos_players)
  }
  
  return (df_players_dup_adj)
}


# Get draft picks from 2012 onwards and duplicate until 2024 season
draft_picks_dup <- draft_picks %>%
  group_by(player_id) %>%
  uncount(2024 - season + 1) %>%
  mutate(season=season[1]:2024) %>%
  ungroup()

# Merge to other info
draft_picks_dup <- merge_to_required_data(draft_picks_dup)

# Check for players that played multiple positions and remove if they did before their 5th season

# Get info on players and how many positions/what position combinations they have played
players_position_info <- game_rosters_inj %>%
  filter(entry_year >= 2012) %>%
  group_by(player_id) %>%
  summarize(name=name[1], n_positions=length(unique(position_abbr)),
            comb=paste(unique(position_abbr), collapse="_"))

# All players that have played more than one position
mult_position_players <- players_position_info %>%
  filter(n_positions > 1)

# Position combinations to check (e.g. DT_NT is not worth checking)
mult_positions <- distinct(mult_position_players, comb)
combs_to_check <- c("FB_TE", "DE_TE", "RB_TE", "DE_DT_G",
                    "WR_TE", "WR_CB", "DT_G_C", "T_TE", "DT_T",
                    "RB_TE_FB", "DB_WR", "LB_FB", "WR_RB", "TE_T", "QB_WR", 
                    "DT_G", "RB_CB", "DE_T", "QB_TE", "RB_WR",
                    "G_DE_DT", "QB_SS", "DE_FB", "LB_LS", "FB_NT",
                    "CB_WR", "NT_T", "TE_FB_RB", "G_DT", "TE_OLB",
                    "DB_SS_RB", "DB_CB_WR", "TE_FB", "TE_DE", "TE_WR", "CB_RB")

# Draft picks who have one of the above position combinations
draft_picks_to_check <- select(draft_picks_dup, pfr_player_name, player_id) %>%
  left_join(select(players_position_info, player_id, comb), by="player_id", relationship="many-to-one") %>%
  filter(comb %in% combs_to_check)

# Get player ids of draft picks to check
draft_picks_dup[draft_picks_dup$player_id==42384,]$position <- "WR"
mult_pos_player_ids <- draft_picks_to_check$player_id

# Info on these players and whether they switched before year 5
mult_pos_players_info <- game_rosters_inj %>%
  filter(player_id %in% mult_pos_player_ids) %>%
  mutate(playing_year=season - entry_year + 1) %>%
  group_by(player_id, playing_year) %>%
  reframe(position=unique(position_abbr)) %>%
  filter(playing_year <= 4) %>%
  group_by(player_id) %>%
  summarize(switched_before_year_5=length(unique(position)) != 1)

# Draft picks who switched before year 5 - want to remove
draftees_to_remove <- draft_picks_to_check %>%
  left_join(mult_pos_players_info, by="player_id") %>%
  filter(switched_before_year_5)

# Filter out those players
draft_picks_dup <- draft_picks_dup %>%
  filter(!(player_id %in% draftees_to_remove$player_id))

# Plot year-over-year differences and roster type histograms for Madden Ratings
plot_yoy_diff(draft_picks_dup, "All Player")
plot_roster_type_lm(draft_picks_dup, "All Player")
plot_roster_type_faceted(draft_picks_dup, "All Player")
for (pos in unique(draft_picks_dup$position)){
  plot_yoy_diff(draft_picks_dup %>% filter(position==pos), pos)
  plot_roster_type_lm(draft_picks_dup %>% filter(position==pos), pos)
  plot_roster_type_faceted(draft_picks_dup %>% filter(position==pos), pos)
}

# Impute Madden Ratings based on decided formulas from above plots
draft_picks_dup_adj <- impute_madden_ratings(draft_picks_dup)

# Filter out special teams players, summarize Madden Ratings across years 3-5, join with NCAA club info, and remove deceased players
draft_picks_dup_summary <- draft_picks_dup_adj %>%
  filter(playing_year %in% 3:5, !(position %in% c("K", "P", "LS")),
         season <= 2024) %>%
  group_by(player_id) %>%
  summarize(name=pfr_player_name[1], yrs=n(), mean_overall=mean(overall), draft_round=round[1],
            draft_number=mean(pick),
            draft_year=draft_year[1], position=position[1],
            gsisid=gsisid[1],
            team=team[1], college=college[1], conference=college_conference[1],
            conference_abbr=college_conference_abbr[1],
            underclassman=enter_under_classman[1]) %>%
  filter(yrs >= 2, draft_year <= 2021) %>%
  left_join(select(ncaa_teams, club_name),
            by=c("college"="club_name"), relationship="many-to-one",
            multiple="first") %>% 
  filter(!(name %in% c("Jeff Gladney", "Dwayne Haskins",
                       "Jaylon Ferguson", "Tray Walker")))

# Updated team acronyms
draft_picks_dup_summary$team <- replace_values_in_column(draft_picks_dup_summary$team,
                                                         c("STL", "SDG", "OAK"),
                                                         c("LAR", "LAC", "LVR"))

# Merge with GM data
draft_picks_dup_summary <- draft_picks_dup_summary %>%
  left_join(gm_by_team_season, by=c("team", "draft_year"="season"),
            relationship="many-to-one")

# Add position_rank variable
draft_picks_dup_summary <- draft_picks_dup_summary %>% 
  group_by(draft_year, position) %>%
  arrange(draft_number) %>%
  mutate(position_rank=rank(draft_number)) %>%
  ungroup(position, draft_year)



# Redo entire process for UDFAs
udfas_dup <- players %>%
  filter(entry_year >= 2012,
         !(position_abbr %in% c("K", "P", "LS", "KR", "PR", "UK", "DB")),
         is.na(draft_number)) %>%
  group_by(player_id) %>%
  mutate(season=entry_year) %>%
  uncount(2024 - season + 1) %>%
  mutate(season=season[1]:2024) %>%
  ungroup()
colnames(udfas_dup)[12] <- "position"
udfas_dup <- merge_to_required_data(udfas_dup, type="UDFAs")

udfas_dup_adj <- impute_madden_ratings(udfas_dup)

udfas_dup_summary <- udfas_dup_adj %>%
  filter(playing_year %in% 3:5, season <= 2024) %>%
  group_by(player_id) %>%
  summarize(name=name[1], yrs=n(), mean_overall=mean(overall), draft_round=NA,
            draft_number=NA, udfa=1,
            draft_year=draft_year[1], position=position[1],
            college=college[1], conference=college_conference[1],
            conference_abbr=college_conference_abbr[1]) %>%
  filter(yrs >= 2, draft_year <= 2021)


# Merge draft picks and UDFAs to calculate Madden Ranking of available players in draft position class
all_players_dup_summary <- bind_rows(draft_picks_dup_summary, udfas_dup_summary) %>%
  group_by(position, draft_year) %>%
  mutate(madden_pos_rank=ranks_overall(mean_overall),
         madden_pos_rank_available=ranks_available(mean_overall),
         diff_pos_best_available=diff_to_best_available(mean_overall)) %>%
  ungroup(position, draft_year)

# Get best players in each draft position class
best_players_in_class <- data.frame()
for (pos in unique(all_players_dup_summary$position)){
  for (year in 2012:2021){
    ordered_class <- all_players_dup_summary %>%
      filter(position==pos, draft_year==year) %>%
      arrange(desc(mean_overall))
    best_players_in_class <- rbind(best_players_in_class,
                                   select(ordered_class[1,], name, position, draft_year, mean_overall,
                                          draft_number))
  }
}

# Merge draft picks to their Madden available ranks
draft_picks_dup_summary <- draft_picks_dup_summary %>%
  left_join(select(all_players_dup_summary, player_id, madden_pos_rank,
                   madden_pos_rank_available, diff_pos_best_available),
            by="player_id")

# Write data frames to CSV
write.csv(draft_picks_dup_summary, "../Data/draft_picks_madden.csv")
write.csv(udfas_dup_summary, "../Data/udfas_madden.csv")