
# Set directory ----------------------------------------------------------------

setwd("C:/Users/David Harler Jr/OneDrive/Desktop/dataClass/06-cod-analysis/2024.03.12 CDL Stats Visualization/cdl_viz")

# Load packages ----------------------------------------------------------------

library(tidyverse)
library(ggimage)
library(scales)
library(DBI)
library(RODBC)
library(odbc)
library(dbplyr)
library(RPostgres)
library(gt)
library(gtExtras)
library(RColorBrewer)
library(viridis)

# Source config ----------------------------------------------------------------
source("config.R")

# Check if a connection exists to database -------------------------------------

conTest <- dbCanConnect(RPostgres::Postgres(),
                        dbname = "cdl_db", 
                        port = 5432, 
                        user = "postgres", 
                        password = db_password)

conTest

# Connect to cdl_db ------------------------------------------------------------

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "cdl_db", 
                 port = 5432, 
                 user = "postgres", 
                 password = db_password)

con

# Load cdl_data into R dataframe -----------------------------------------------

cdlDF <- dbGetQuery(con, "SELECT * FROM cdl_data")

# Close the connection ---------------------------------------------------------

dbDisconnect(con)

# Show datatypes ---------------------------------------------------------------

str(cdlDF)

# Correct Minnesota ROKKR team name --------------------------------------------

cdlDF$team <- chartr("Minnesota RØKKR", "Minnesota ROKKR", cdlDF$team)

# Add W/L columns ---------------------------------------------------------------

win_loss <- function(x){
  if (x == 1) {return("W")}
  else {return("L")}
}

win_loss_vectorized <- Vectorize(win_loss)

cdlDF <- cdlDF %>% mutate(map_wl = win_loss_vectorized(map_result), 
                          series_wl = win_loss_vectorized(series_result))

# Get Team Logos & Colors ------------------------------------------------------

team_logo <- c(
  "https://dfpiiufxcciujugzjvgx.supabase.co/storage/v1/object/public/teams/ATL-FaZe-color-darkmode.png?t=2022-12-30T15%3A46%3A15.277Z", 
  "https://static.wikia.nocookie.net/cod_esports_gamepedia_en/images/0/05/Boston_Breachlogo_square.png/revision/latest?cb=20220114204228",
  "https://dfpiiufxcciujugzjvgx.supabase.co/storage/v1/object/public/teams/CAR_ROYAL_RAVENS_ALLMODE.webp",
  "https://dfpiiufxcciujugzjvgx.supabase.co/storage/v1/object/public/teams/LV-Legion-color-allmode.png",
  "https://static.wikia.nocookie.net/cod_esports_gamepedia_en/images/6/60/Los_Angeles_Guerrillaslogo_square.png/revision/latest?cb=20200612154050",
  "https://dfpiiufxcciujugzjvgx.supabase.co/storage/v1/object/public/teams/LA-Thieves-color-allmode.png",
  "https://dfpiiufxcciujugzjvgx.supabase.co/storage/v1/object/public/teams/MIA_HERETICS_ALLMODE.webp",
  "https://dfpiiufxcciujugzjvgx.supabase.co/storage/v1/object/public/teams/MN-ROKKR-color-allmode.png",
  "https://dfpiiufxcciujugzjvgx.supabase.co/storage/v1/object/public/teams/NY-Subliners-color-darkmode.png",
  "https://static.wikia.nocookie.net/cod_esports_gamepedia_en/images/9/99/OpTic_Texaslogo_square.png/revision/latest?cb=20211114210508",
  "https://dfpiiufxcciujugzjvgx.supabase.co/storage/v1/object/public/teams/SEA-Surge-color-allmode.png",
  "https://static.wikia.nocookie.net/cod_esports_gamepedia_en/images/4/4f/Toronto_Ultralogo_square.png/revision/latest?cb=20191026213249"
)

team_color <- c(
  "#e43d30", "#02ff5b", "#0083c1", "#ee7623", "#60269e", "#ff0000", 
  "#216d6b", "#351f65", "#fff000", "#92c951", "#00ffcc", "#780df2"
)

team <- unique((cdlDF %>% arrange(team))$team)

team_abbr <- c("ATL", "BOS", "CAR", "LV", "LAG", "LAT",
               "MIA", "MIN", "NYSL", "TX", "SEA", "TOR")

team_icon <- team %>%
  str_split_i(" ", -1)

team_icon[10] <- "OpTic"

team_logos_colors_df <- data.frame(
  team, team_abbr, team_icon, team_logo, team_color
)

cdlDF <- cdlDF %>% left_join(team_logos_colors_df, by = c("team" = "team"))

# Get Opponents, Match Scores, and Score Differentials -------------------------

cdlDF <- cdlDF %>%
  mutate(player_lower = tolower(player))

cdlDF <- cdlDF %>%
  arrange(match_date, match_id, map_num, team, player_lower)

opps <- cdlDF %>%
  arrange(match_date, match_id, map_num, desc(team), player_lower) %>%
  select(team, team_score, team_logo) %>%
  rename("opp" = "team", "opp_score" = "team_score", "opp_logo" = "team_logo")

cdlDF <- bind_cols(cdlDF, opps) %>%
  mutate(total_score = team_score + opp_score, 
         score_diff = team_score - opp_score)

# Get rosters & dropped players ------------------------------------------------

rostersDF <- cdlDF %>% select(player, team) %>% distinct() %>% arrange(team)
dropped_players <- c("GodRx", "ReeaL", "JoeDeceives", "Cammy", 
                     "JurNii", "Standy", "iLLeY", "Capsidal")

# Reorder W/L Levels for Graphing ----------------------------------------------

cdlDF$map_wl <- factor(cdlDF$map_wl, levels = c("W", "L"))
