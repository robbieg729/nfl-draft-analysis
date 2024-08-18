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
library(nflplotR)

# Import function to fit exponential model
import::from("draft_curve_functions.R", fit_exp_model)

# Helper function to get the picks used in each draft-day trade
get_picks_in_trades <- function(draft_day_trades, relevant_trade_ids){
  trades <- data.frame()
  
  for (id in unique(relevant_trade_ids)){
    trade <- draft_day_trades %>% filter(trade_id==id)
    s <- unique(trade$season)
    
    if (!any(is.na(trade$selection_over_all))){
      rec_team <- unique((trade %>% filter(highest_pick))$to_club_id)
      away_team <- unique((trade %>% filter(highest_pick))$club_id)
      
      rec_picks <- select(trade %>% filter(to_club_id==rec_team), selection_over_all, selection_season, highest_pick)
      away_picks <- select(trade %>% filter(to_club_id==away_team), selection_over_all, selection_season, highest_pick)
      
      highest_pick <- (trade %>% filter(highest_pick))$selection_over_all
      
      trades <- rbind(trades, data.frame(
        trade_id=rep(id, nrow(rec_picks) + nrow(away_picks)),
        season=rep(s, nrow(rec_picks) + nrow(away_picks)),
        pick_direction=append(rep("Up", nrow(rec_picks)), rep("Down", nrow(away_picks))),
        pick=append(rec_picks$selection_over_all, away_picks$selection_over_all),
        pick_season=append(rec_picks$selection_season,
                           away_picks$selection_season),
        highest_pick=append(rec_picks$highest_pick, away_picks$highest_pick),
        team_up=rep((nfl_clubs %>% filter(club_id==rec_team, season==2023))$club_nickname[1],
                    nrow(rec_picks) + nrow(away_picks)),
        team_down=rep((nfl_clubs %>% filter(club_id==away_team, season==2023))$club_nickname[1],
                      nrow(rec_picks) + nrow(away_picks))
      ))
    }
  }
  return (trades)
}

# Helper function to get trade values from each trade
get_trade_values <- function(trades_to_evaluate, pick_values){
  trade_values <- data.frame()
  
  for (id in unique(trades_to_evaluate$trade_id)){
    trade <- trades_to_evaluate %>% filter(trade_id==id)
    
    rec_team_picks <- sort((trade %>% filter(pick_direction=="Up"))$pick)
    away_team_picks <- sort((trade %>% filter(pick_direction=="Down"))$pick)
    
    rec_team_disc_picks <- sort((trade %>% filter(pick_direction=="Up"))$discounted_pick)
    away_team_disc_picks <- sort((trade %>% filter(pick_direction=="Down"))$discounted_pick)
    
    highest_pick_position <- (trade %>% filter(highest_pick))$position
    
    rec_team_value <- 0
    away_team_value <- 0
    rec_team_disc_value <- 0
    away_team_disc_value <- 0
    for (i in 1:length(rec_team_picks)){
      if (i == 1){
        rec_team_value <- rec_team_value + (pick_values %>% filter(position==ifelse(is.na(highest_pick_position), "All", highest_pick_position),
                                                                   pick==rec_team_picks[i]))$value
        rec_team_disc_value <- rec_team_disc_value + (pick_values %>% filter(position==ifelse(is.na(highest_pick_position), "All", highest_pick_position),
                                                                   pick==rec_team_disc_picks[i]))$value
      }
      else{
        rec_team_value <- rec_team_value + (pick_values %>% filter(position=="All",
                                                                   pick==rec_team_picks[i]))$value
        rec_team_disc_value <- rec_team_disc_value + (pick_values %>% filter(position=="All",
                                                                   pick==rec_team_disc_picks[i]))$value
      }
    }
    
    for (i in 1:length(away_team_picks)){
      away_team_value <- away_team_value + (pick_values %>% filter(position=="All",
                                                                   pick==away_team_picks[i]))$value
      away_team_disc_value <- away_team_disc_value + (pick_values %>% filter(position=="All",
                                                                   pick==away_team_disc_picks[i]))$value
    }
    rec_team_pick_seasons <- trade %>% filter(pick_direction=="Up") %>%
      mutate(pick_season_str=paste(pick_season, pick, sep=" - "))
    
    away_team_pick_seasons <- trade %>% filter(pick_direction=="Down") %>%
      mutate(pick_season_str=paste(pick_season, pick, sep=" - "))
    
    trade_values <- rbind(trade_values,
                          data.frame(
                            trade_up_team=unique(trade$team_up),
                            trade_up_picks=paste(rec_team_pick_seasons$pick_season_str, collapse=", "),
                            trade_up_value=rec_team_value,
                            trade_up_disc_value=rec_team_disc_value,
                            trade_down_team=unique(trade$team_down),
                            trade_down_picks=paste(away_team_pick_seasons$pick_season_str, collapse=", "),
                            trade_down_value=away_team_value,
                            trade_down_disc_value=away_team_disc_value,
                            position_traded_up_for=highest_pick_position,
                            trade_up_pick_round=(trade %>% filter(highest_pick))$draft_round
                          ))
  }
  return (trade_values)
  
}

# Helper  function to get team abbreviation from nickname (Chiefs, Bengals, Raiders, etc.)
get_team_abbr <- function(nickname){
  team_code <- (nfl_clubs %>% filter(club_nickname==nickname, season==2023))$club_code
  team_code <- ifelse(team_code=="ARZ", "ARI",
                      ifelse(team_code=="BLT", "BAL",
                             ifelse(team_code=="HST", "HOU",
                                    ifelse(team_code=="CLV", "CLE", team_code))))
  return (team_code)
}

# Load draft picks-Madden Ratings data
draft_picks_dup_summary <- read.csv("../Data/draft_picks_madden.csv") %>%
  select(-X)

# Load info on NFL clubs
nfl_clubs <- read.csv("../Data/ClubDB.dbo.Club.csv") %>%
  filter(!(club_id==5 & club_code=="CLE"))

# Get dates of NFL Draft 2012-21
nfl_draft_dates <- read.csv("../Data/ClubDB.dbo.Calendar.csv") %>%
  filter(season %in% 2012:2021, grepl("Draft", description))

# Get all NFL trades
nfl_trades <- read.csv("../Data/ClubDB.dbo.Trade.csv")

# Keep only trades that happened on draft day 2012-21 and figure out highest pick used in each trade
# (i.e. the one that was traded up for)
draft_day_trades <- nfl_draft_dates %>%
  left_join(select(nfl_trades, -season), by=c("calendardate"="trade_date"),
            relationship="one-to-many") %>%
  mutate(pick_num=selection_season + selection_over_all/1000) %>%
  group_by(trade_id) %>%
  mutate(highest_pick=pick_num==min(pick_num, na.rm=TRUE),
         pick=paste(selection_season, selection_over_all, sep="_")) %>%
  ungroup()

# Get the highest pick in each draft and filter out ones that were used in a trade multiple times,
# and the last time they were used they were not the highest pick
highest_picks <- draft_day_trades %>%
  filter(highest_pick) %>%
  group_by(pick) %>%
  mutate(final_trade_id=max(trade_id)) %>%
  distinct(pick, final_trade_id)

# Get all relevant trades based on above rules and discount picks if in the future
relevant_trades <- get_picks_in_trades(draft_day_trades, highest_picks$final_trade_id) %>%
  mutate(discounted_pick=ifelse(pick > 227, pick, pick + 32 * (pick_season - season)))

# Join trades with draft picks-Madden Ratings data
draft_picks_dup_summary <- draft_picks_dup_summary %>%
  left_join(select(relevant_trades %>% filter(highest_pick), pick_season, pick, highest_pick),
            by=c("draft_year"="pick_season", "draft_number"="pick")) %>%
  mutate(highest_pick=as.factor(ifelse(is.na(highest_pick), 0, highest_pick)))
colnames(draft_picks_dup_summary)[length(draft_picks_dup_summary)] <- "traded_up_for"

# Fit model for all players and compute parameters
model_all <- fit_exp_model(draft_picks_dup_summary %>% mutate(x=draft_number), "draft_number")
model_params <- coef(model_all)

# Grouped draft curves of traded up for vs. not traded up for
ggplot(data=draft_picks_dup_summary, aes(x=draft_number, y=mean_overall, group=traded_up_for,
                                         color=traded_up_for)) +
  stat_smooth(method="nls", formula=(y ~ alpha * exp(beta * x) + theta),
              method.args=list(start=list(alpha=model_params[1], beta=model_params[2], 
                                          theta=model_params[3])), se=FALSE) +
  xlab("Draft Pick") + ylab("Average Madden Rating") + 
  labs(title="Draft Curves for Average Madden Rating in Years 3-5 vs. Draft Pick",
       subtitle="All Offensive/Defensive Players Drafted 2012-21",
       color="Pick Traded Up For") +
  scale_color_discrete(labels=c("No", "Yes"), type=c("darkblue", "red")) +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.8),
        legend.text=element_text(size=12), legend.title=element_text(size=15))
ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Trades/Trade Curve.pdf", width=12, height=7)


# Read in pick values
pick_values <- read.csv("../Data/pick_values.csv") %>%
  select(-c(X))

# Get trades to evaluate
trades_to_evaluate <- relevant_trades %>%
  left_join(select(draft_picks_dup_summary, draft_year, position, draft_number, draft_round),
            by=c("pick_season"="draft_year", "pick"="draft_number"))

# Get value and discounted value gained in each trade
trade_values <- get_trade_values(trades_to_evaluate, pick_values) %>%
  mutate(id=1:n()) %>%
  group_by(id) %>%
  mutate(trade_up_team_abbr=get_team_abbr(trade_up_team),
         trade_down_team_abbr=get_team_abbr(trade_down_team),
         value_gained=trade_up_value - trade_down_value,
         disc_value_gained=trade_up_disc_value - trade_down_disc_value) %>%
  ungroup() %>%
  select(-c(id))

# Plot number of trade ups by team
ggplot(data=trade_values %>%
         group_by(trade_up_team) %>%
         summarize(n=n(), abbr=trade_up_team_abbr[1]), aes(x=reorder(trade_up_team, -n), y=n)) +
  geom_nfl_logos(aes(team_abbr=abbr), width=0.05, height=0.1) +
  ylab("Times Traded Up") + labs(title="Number of times Teams have Traded Up on Draft Day",
                                 subtitle="2012-21 NFL Drafts") +
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank(), panel.grid.major.x = element_blank()) +
  scale_y_continuous(breaks=seq(0, 15, by=2))
ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Trades/Trade Ups.pdf", width=12, height=7)

# Plot number of trade downs by team
ggplot(data=trade_values %>%
         group_by(trade_down_team) %>%
         summarize(n=n(), abbr=trade_down_team_abbr[1]) %>%
         add_row(trade_down_team=c("Chargers", "Saints", "Steelers"),
                 n=c(0, 0, 0),
                 abbr=c("LAC", "NO", "PIT")), aes(x=reorder(trade_down_team, -n), y=n)) +
  geom_nfl_logos(aes(team_abbr=abbr), width=0.05, height=0.1) +
  ylab("Times Traded Down") + labs(title="Number of times Teams have Traded Down on Draft Day",
                                   subtitle="2012-21 NFL Drafts") +
  theme(axis.title.x = element_blank(), axis.ticks.x=element_blank(),
        axis.text.x=element_blank(), panel.grid.major.x = element_blank()) +
  scale_y_continuous(breaks=seq(0, 30, by=4))
ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Trades/Trade Downs.pdf", width=12, height=7)


# Plot average value gained by draft round of highest pick
ggplot(data=trade_values %>%
         filter(!is.na(trade_up_pick_round)) %>%
         group_by(trade_up_pick_round) %>%
         summarize(avg_value=-1*mean(value_gained),
                   n=paste("N =", n())), aes(x=as.factor(trade_up_pick_round), y=avg_value)) +
  geom_bar(stat="identity", fill="darkblue") + xlab("Draft Round") +
  ylab("Average Value Lost") + labs(title="Average Draft Pick Value Lost on Trade-Ups into Each Draft Round",
                                    subtitle="2012-21 NFL Drafts") +
  geom_label(aes(label=n))
ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Trades/Value Gained by Round.pdf", width=12, height=7)

# Plot average value gained by player position of highest pick
ggplot(data=trade_values %>%
         filter(!is.na(position_traded_up_for)) %>%
         group_by(position_traded_up_for) %>%
         summarize(avg_value=-1*mean(value_gained),
                   n=paste("N =", n())), aes(x=reorder(position_traded_up_for, -avg_value), y=avg_value)) +
  geom_bar(stat="identity", fill="darkblue") + xlab("Position") +
  ylab("Average Value Lost") + labs(title="Average Draft Pick Value Lost on Trade-Ups for Each Position",
                                    subtitle="2012-21 NFL Drafts") +
  geom_label(aes(label=n))
ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Trades/Value Gained by Position.pdf", width=12, height=7)


# Same as above but for discounted value
ggplot(data=trade_values %>%
         filter(!is.na(trade_up_pick_round)) %>%
         group_by(trade_up_pick_round) %>%
         summarize(avg_value=-1*mean(disc_value_gained),
                   n=paste("N =", n())), aes(x=as.factor(trade_up_pick_round), y=avg_value)) +
  geom_bar(stat="identity", fill="darkblue") + xlab("Draft Round") +
  ylab("Average Value Lost") + labs(title="Average Draft Pick Value (Discounted) Lost on Trade-Ups into Each Draft Round",
                                    subtitle="2012-21 NFL Drafts") +
  geom_label(aes(label=n))
ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Trades/Discounted Value Gained by Round.pdf", width=12, height=7)

ggplot(data=trade_values %>%
         filter(!is.na(position_traded_up_for)) %>%
         group_by(position_traded_up_for) %>%
         summarize(avg_value=-1*mean(disc_value_gained),
                   n=paste("N =", n())), aes(x=reorder(position_traded_up_for, -avg_value), y=avg_value)) +
  geom_bar(stat="identity", fill="darkblue") + xlab("Position") +
  ylab("Average Value Lost") + labs(title="Average Draft Pick Value (Discounted) Lost on Trade-Ups for Each Position",
                                    subtitle="2012-21 NFL Drafts") +
  geom_label(aes(label=n))
ggsave("../Charts/Draft Curves/Average Madden Ratings/Draft Pick/Trades/Discounted Value Gained by Position.pdf", width=12, height=7)