# Read in data from CSV files

# Draft Order Data
players <- read.csv("../Data/ClubDB.dbo.PlayerExtract.csv")
ncaa_teams <- read.csv("../Data/NonProDB.dbo.Club.csv")
gm_data_raw <- read.csv("../Data/pro_football_reference_front_office.csv")
gm_data_clean <- read.csv("../Data/GM Data Clean.csv")

# Draft Success Data
madden_ratings <- read.csv("../Data/dna_football_ops_db.Initial_Madden_Ratings.csv")
madden_25_ratings <- read.csv("../Data/Madden 25 Ratings/All Cleaned.csv")
game_rosters_inj <- read.csv("../Data/ClubDB.dbo.Roster.csv")
draft_picks <- read.csv("../Data/nflreadR_load_draft_picks.csv")

# Base Data
games <- read.csv("../Data/ClubDB.dbo.Game.csv")
teams <- read.csv("../Data/ClubDB.dbo.Club.csv")