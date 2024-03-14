
# Set directory ----------------------------------------------------------------

# setwd("C:/Users/David/OneDrive/Desktop/dataClass/06-cod-analysis/2024.03.12 CDL Stats Visualization/cdl_viz")
setwd("C:/Users/David Harler Jr/OneDrive/Desktop/dataClass/06-cod-analysis/2024.03.12 CDL Stats Visualization/cdl_viz")

# Source setup -----------------------------------------------------------------

source("00_Setup/00_Setup.R")

# Function to Graph Kills over Time & Current Prize Picks line -----------------

kills_vs_line <-
  function(player_input, gamemode_input, cur_line, map_input = NULL, 
           opps = FALSE){
    if (is.null(map_input)){
      if (!opps){
        cdlDF %>%
          filter(player == player_input & gamemode == gamemode_input) %>%
          ggplot(mapping = aes(x = match_date, y = kills, color = map_name, 
                               size = total_score)) +
          geom_point() +
          # scale_x_date(date_breaks = "month", date_labels = "%b %d") +
          geom_hline(yintercept = cur_line, lty = 'dashed', color = 'purple') +
          scale_color_brewer(palette = "Spectral") +
          labs(title = paste0(player_input, " ", gamemode_input, 
                              " Kills Colored by Map")) +
          xlab("Date") + ylab("Kills") + 
          theme(plot.title = element_text(size = 16), 
                plot.margin = margin(8, 12, 12, 12, "pt")) 
      }
      else {
        cdlDF %>%
          filter(player == player_input & gamemode == gamemode_input) %>%
          ggplot(mapping = aes(x = match_date, y = kills)) +
          geom_image(aes(image = opp_logo), size = .125) +
          # scale_x_date(date_breaks = "month", date_labels = "%b %d") +
          geom_hline(yintercept = cur_line, lty = 'dashed', color = 'purple') +
          scale_color_brewer(palette = "Spectral") +
          labs(title = paste0(player_input, " ", gamemode_input, 
                              " Kills vs. Opponents")) +
          xlab("Date") + ylab("Kills") + 
          theme(plot.title = element_text(size = 16), 
                plot.margin = margin(8, 12, 12, 12, "pt")) 
      }
    }
    else {
      if (!opps){
        cdlDF %>%
          filter(player == player_input & gamemode == gamemode_input 
                 & map_name == map_input) %>%
          ggplot(mapping = aes(x = match_date, y = kills, color = map_wl, 
                               size = total_score)) +
          geom_point() + 
          # scale_x_date(date_breaks = "month", date_labels = "%b %d") +
          geom_hline(yintercept = cur_line, lty = 'dashed', color = 'purple') +
          labs(title = paste0(player_input, " Kills on ", 
                              map_input, " ", gamemode_input)) + 
          xlab("Date") + ylab("Kills") + 
          theme(plot.title = element_text(size = 16), 
                plot.margin = margin(8, 12, 12, 12, "pt")) +
          scale_color_manual(values = c("blue", "red"))
      }
      else {
        cdlDF %>%
          filter(player == player_input & gamemode == gamemode_input 
                 & map_name == map_input) %>%
          ggplot(mapping = aes(x = match_date, y = kills)) +
          geom_image(aes(image = opp_logo), size = .125) +
          # scale_x_date(date_breaks = "month", date_labels = "%b %d") +
          geom_hline(yintercept = cur_line, lty = 'dashed', color = 'purple') +
          labs(title = paste0(player_input, " Kills on ", 
                              map_input, " ", gamemode_input), " vs. Opponents") + 
          xlab("Date") + ylab("Kills") + 
          theme(plot.title = element_text(size = 16), 
                plot.margin = margin(8, 12, 12, 12, "pt"))
      }
    }
  }

# Function to Graph Box Plot of Kills & Current Prize Picks line ---------------

boxplot_of_kills <-
  function(player_input, gamemode_input, cur_line, map_input = NULL){
    if (is.null(map_input)){
      cdlDF %>%
        filter(player == player_input & gamemode == gamemode_input) %>%
        ggplot(mapping = aes(x = player, y = kills)) +
        geom_point(mapping = aes(color = map_name, size = total_score)) +
        geom_boxplot(alpha = 0.5) +
        geom_hline(yintercept = cur_line, lty = 'dashed', color = 'purple') +
        scale_color_brewer(palette = "Spectral") +
        labs(title = paste0(player_input, " ", gamemode_input, 
                            " Kills Colored by Map")) +
        xlab("") + ylab("Kills") + 
        theme(plot.title = element_text(size = 16), 
              plot.margin = margin(8, 12, 12, 12, "pt"), 
              axis.text.x = element_blank())
    }
    else {
      cdlDF %>%
        filter(player == player_input & gamemode == gamemode_input 
               & map_name == map_input) %>%
        ggplot(mapping = aes(x = player, y = kills)) +
        geom_point(mapping = aes(color = map_wl, size = total_score)) +
        geom_boxplot(alpha = 0.5) +
        geom_hline(yintercept = cur_line, lty = 'dashed', color = 'purple') +
        labs(title = paste0(player_input, " Kills on ", 
                            map_input, " ", gamemode_input)) +
        xlab("") + ylab("Kills") + 
        theme(plot.title = element_text(size = 16), 
              plot.margin = margin(8, 12, 12, 12, "pt"), 
              axis.text.x = element_blank()) +
        scale_color_manual(values = c("blue", "red"))
    }
  }




# HyDra Hardpoint analysis -----------------------------------------------------

hydra_hp_by_map_initial <- 
  cdlDF %>% 
    filter(player == "HyDra" &
             gamemode == "Hardpoint") %>%
    group_by(map_name) %>%
    summarise(avg = round(mean(kills), 0), 
              min = min(kills), 
              median = round(median(kills), 0), 
              max = max(kills), 
              times_played = n())


hydra_hp_overall <- 
  cdlDF %>% 
    filter(player == "HyDra" &
             gamemode == "Hardpoint") %>%
    summarise(avg = round(mean(kills), 0), 
              min = min(kills), 
              median = round(median(kills), 0), 
              max = max(kills), 
              times_played = n())

hydra_hp_overall$map_name = c("Overall")


hydra_hp_by_map <- 
  bind_rows(
    hydra_hp_by_map_initial, 
    hydra_hp_overall
  )

# Dashy Control analysis -------------------------------------------------------

dashy_ctrl_by_map_initial <- 
  cdlDF %>% 
  filter(player == "Dashy" &
           gamemode == "Control") %>%
  group_by(map_name) %>%
  summarise(avg = round(mean(kills), 0), 
            min = min(kills), 
            median = round(median(kills), 0), 
            max = max(kills), 
            times_played = n())


dashy_ctrl_overall <- 
  cdlDF %>% 
  filter(player == "Dashy" &
           gamemode == "Control") %>%
  summarise(avg = round(mean(kills), 0), 
            min = min(kills), 
            median = round(median(kills), 0), 
            max = max(kills), 
            times_played = n())

dashy_ctrl_overall$map_name = c("Overall")


dashy_ctrl_by_map <- 
  bind_rows(
    dashy_ctrl_by_map_initial, 
    dashy_ctrl_overall
  )

# Dashy Control analysis by W/L: ------------------------------------------------

dashy_ctrl_by_map_wl_initial <- 
  cdlDF %>% 
  filter(player == "Dashy" &
           gamemode == "Control") %>%
  group_by(map_name, map_wl) %>%
  summarise(avg = round(mean(kills), 0), 
            min = min(kills), 
            median = round(median(kills), 0), 
            max = max(kills), 
            times_played = n()) %>%
  arrange(map_name, desc(map_wl))


dashy_ctrl_by_wl <- 
  cdlDF %>% 
  filter(player == "Dashy" &
           gamemode == "Control") %>%
  group_by(map_wl) %>%
  summarise(avg = round(mean(kills), 0), 
            min = min(kills), 
            median = round(median(kills), 0), 
            max = max(kills), 
            times_played = n()) %>%
  arrange(desc(map_wl))


dashy_ctrl_by_wl$map_name = c("Overall", "Overall")


dashy_ctrl_by_map_wl <- 
  bind_rows(
    dashy_ctrl_by_map_wl_initial, 
    dashy_ctrl_by_wl,
    dashy_ctrl_by_map
  ) %>%
  arrange(map_name, desc(map_wl))

# OpTic Control analysis -------------------------------------------------------

optic_ctrl_summary <- 
  cdlDF %>%
    filter(gamemode == "Control" &
             team == "OpTic Texas" & 
             player == "Dashy") %>%
  select(map_name, map_result) %>%
  group_by(map_name) %>%
  summarise(Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2)))

# ROKKR Control analysis -------------------------------------------------------

rokkr_ctrl_summary <- 
  cdlDF %>%
  filter(gamemode == "Control" &
           team == "Minnesota ROKKR" & 
           player == "Owakening") %>%
  select(map_name, map_result) %>%
  group_by(map_name) %>%
  summarise(Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2)))

# Toronto Hardpoint analysis ---------------------------------------------------

ultra_hp_summary <- 
  cdlDF %>%
  filter(gamemode == "Hardpoint" &
           team == "Toronto Ultra" & 
           player == "Envoy") %>%
  select(map_name, map_result) %>%
  group_by(map_name) %>%
  summarise(Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2)))

# Faze Clan Testing for this weekend -------------------------------------------

# Hardpoint ----

kills_over_time_vs_line("Simp", "Hardpoint", 23.5)
kills_over_time_vs_line("aBeZy", "Hardpoint", 23.5)
kills_over_time_vs_line("Drazah", "Hardpoint", 21.5)
kills_over_time_vs_line("Cellium", "Hardpoint", 20.5)

kills_over_time_vs_line("Simp", "Hardpoint", 23.5, "Rio")
kills_over_time_vs_line("aBeZy", "Hardpoint", 23.5, "Rio")
kills_over_time_vs_line("Drazah", "Hardpoint", 21.5, "Rio")
kills_over_time_vs_line("Cellium", "Hardpoint", 20.5, "Rio")

faze_hp_map_dict <- c("Invasion" = "aBeZy under", 
                      "Karachi" = "Drazah over",
                      "Rio" = "Simp over",
                      "Skidrow" = "Cellium under",
                      "Sub Base" = "aBeZy under")

faze_hp_map_dict["Invasion"]
faze_hp_map_dict["Skidrow"]
faze_hp_map_dict["Sub Base"]

# Control ----

kills_over_time_vs_line("Simp", "Control", 24.5)
kills_over_time_vs_line("aBeZy", "Control", 25)
kills_over_time_vs_line("Drazah", "Control", 22)
kills_over_time_vs_line("Cellium", "Control", 22)

kills_over_time_vs_line("Simp", "Control", 24.5, "Invasion")
kills_over_time_vs_line("aBeZy", "Control", 25, "Invasion")
kills_over_time_vs_line("Drazah", "Control", 22, "Invasion")
kills_over_time_vs_line("Cellium", "Control", 22, "Invasion")

faze_ctrl_map_dict <- c("Highrise" = "Drazah over", 
                      "Invasion" = "No edge",
                      "Karachi" = "Cellium under")

faze_ctrl_map_dict["Highrise"]
faze_ctrl_map_dict["Invasion"]
faze_ctrl_map_dict["Karachi"]

# Minnesota ROKKR Testing for this weekend -------------------------------------

# Hardpoint ----

kills_over_time_vs_line("Lyynnz", "Hardpoint", 23.5, "Rio")
kills_over_time_vs_line("Vivid", "Hardpoint", 22.5, "Rio")
kills_over_time_vs_line("Owakening", "Hardpoint", 19.5, "Rio")
kills_over_time_vs_line("Accuracy", "Hardpoint", 19.5, "Rio")

rokkr_hp_map_dict <- c("Invasion" = "Lyynnz under", 
                      "Karachi" = "Owakening under",
                      "Rio" = "Vivid over",
                      "Skidrow" = "Lyynnz under",
                      "Sub Base" = "Vivid under")

rokkr_hp_map_dict["Invasion"]
rokkr_hp_map_dict["Skidrow"]
rokkr_hp_map_dict["Sub Base"]

# Control ----


kills_over_time_vs_line("Lyynnz", "Control", 23.5, "Invasion")
kills_over_time_vs_line("Vivid", "Control", 22.5, "Invasion")
kills_over_time_vs_line("Owakening", "Control", 20.5, "Invasion")
kills_over_time_vs_line("Accuracy", "Control", 20.5, "Invasion")

rokkr_ctrl_map_dict <- c("Highrise" = "Lyynnz over", 
                       "Invasion" = "Owakening over",
                       "Karachi" = "Accuracy over")

rokkr_ctrl_map_dict["Highrise"]
rokkr_ctrl_map_dict["Invasion"]
rokkr_ctrl_map_dict["Karachi"]

# Hardpoint Map Analysis -------------------------------------------------------

hpMaps <- cdlDF %>% filter(gamemode == "Hardpoint") %>%
  group_by(match_id, team, map_name) %>%
    summarise(points_scored = median(team_score)) %>%
    group_by(match_id, map_name) %>%
    summarise(total_points = sum(points_scored)) %>%
    group_by(map_name) %>%
    summarise(avg = round(mean(total_points), 0), 
              min = min(total_points), 
              median = round(median(total_points), 0), 
              max = max(total_points), 
              times_played = n()) %>%
    filter(map_name != "Terminal")

hpMapKills <- cdlDF %>% filter(gamemode == "Hardpoint") %>%
  group_by(match_id, team, map_name) %>%
  summarise(total_kills = sum(kills)) %>%
  group_by(map_name) %>%
  summarise(median_total_kills = round(median(total_kills), 0))  %>%
  filter(map_name != "Terminal") %>%
  arrange(desc(median_total_kills))


cdlDF %>% filter(gamemode == "Hardpoint") %>%
  group_by(match_id, team, map_name) %>%
  summarise(points_scored = median(team_score)) %>%
  group_by(match_id, map_name) %>%
  summarise(total_points = sum(points_scored)) %>%
  filter(map_name != "Terminal") %>%
  ggplot(mapping = aes(x = map_name, y = total_points)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.75)

cdlDF %>% filter(gamemode == "Hardpoint") %>%
  group_by(match_id, team, map_name) %>%
  summarise(total_team_kills = sum(kills)) %>%
  group_by(match_id, map_name) %>%
  summarise(total_kills = sum(total_team_kills)) %>%
  filter(map_name != "Terminal") %>%
  ggplot(mapping = aes(x = map_name, y = total_kills)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.75)


# Search & Destroy Map Analysis ------------------------------------------------

sndMaps <- cdlDF %>% filter(gamemode == "Search & Destroy") %>%
       group_by(match_id, team, map_name) %>%
       summarise(points_scored = median(team_score)) %>%
       group_by(match_id, map_name) %>%
       summarise(total_points = sum(points_scored)) %>%
       group_by(map_name) %>%
       summarise(avg = round(mean(total_points), 0), 
                 min = min(total_points), 
                 median = round(median(total_points), 0), 
                 max = max(total_points), 
                 times_played = n()) %>%
       filter(map_name != "Highrise")

sndMapKills <- cdlDF %>% filter(gamemode == "Search & Destroy") %>%
  group_by(match_id, team, map_name) %>%
  summarise(total_kills = sum(kills)) %>%
  group_by(map_name) %>%
  summarise(median_total_kills = round(median(total_kills), 0))  %>%
  filter(map_name != "Highrise") %>%
  arrange(desc(median_total_kills))

cdlDF %>% filter(gamemode == "Search & Destroy") %>%
  group_by(match_id, team, map_name) %>%
  summarise(points_scored = median(team_score)) %>%
  group_by(match_id, map_name) %>%
  summarise(total_points = sum(points_scored)) %>%
  filter(map_name != "Skidrow") %>%
  ggplot(mapping = aes(x = map_name, y = total_points)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.75)

cdlDF %>% filter(gamemode == "Search & Destroy") %>%
  group_by(match_id, team, map_name) %>%
  summarise(total_team_kills = sum(kills)) %>%
  group_by(match_id, map_name) %>%
  summarise(total_kills = sum(total_team_kills)) %>%
  filter(map_name != "Skidrow") %>%
  ggplot(mapping = aes(x = map_name, y = total_kills)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.75)

# Control ----------------------------------------------------------------------

ctrlMaps <- cdlDF %>% filter(gamemode == "Control") %>%
       group_by(match_id, team, map_name) %>%
       summarise(points_scored = median(team_score)) %>%
       group_by(match_id, map_name) %>%
       summarise(total_points = sum(points_scored)) %>%
       group_by(map_name) %>%
       summarise(avg = round(mean(total_points), 0),
                 median = round(median(total_points), 0), 
                 times_played = n())

ctrlMapKills <- cdlDF %>% filter(gamemode == "Control") %>%
  group_by(match_id, team, map_name) %>%
  summarise(total_kills = sum(kills)) %>%
  group_by(map_name) %>%
  summarise(median_total_kills = round(median(total_kills), 0))  %>%
  arrange(desc(median_total_kills))

cdlDF %>% filter(gamemode == "Control") %>%
  group_by(match_id, team, map_name) %>%
  summarise(points_scored = median(team_score)) %>%
  group_by(match_id, map_name) %>%
  summarise(total_points = sum(points_scored)) %>%
  ggplot(mapping = aes(x = map_name, y = total_points)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.75)

cdlDF %>% filter(gamemode == "Control") %>%
  group_by(match_id, team, map_name) %>%
  summarise(total_team_kills = sum(kills)) %>%
  group_by(match_id, map_name) %>%
  summarise(total_kills = sum(total_team_kills)) %>%
  ggplot(mapping = aes(x = map_name, y = total_kills)) +
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.75)

# Team by Team Hardpoint Analysis ----------------------------------------------

faze_hp_summary <- 
  cdlDF %>%
  filter(gamemode == "Hardpoint" &
           team == "Atlanta FaZe" & 
           player == "Simp" &
           map_name != "Terminal") %>%
  select(map_name, map_result, team_score, opp_score) %>%
  group_by(map_name) %>%
  summarise(AvgScore = round(mean(team_score), 0), 
            OppAvgScore = round(mean(opp_score), 0),
            Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2)))

rokkr_hp_summary <- 
  cdlDF %>%
  filter(gamemode == "Hardpoint" &
           team == "Minnesota ROKKR" & 
           player == "Accuracy" &
           map_name != "Terminal") %>%
  select(map_name, map_result, team_score, opp_score) %>%
  group_by(map_name) %>%
  summarise(AvgScore = round(mean(team_score), 0), 
            OppAvgScore = round(mean(opp_score), 0),
            Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2)))

optic_hp_summary <- 
  cdlDF %>%
  filter(gamemode == "Hardpoint" &
           team == "OpTic Texas" & 
           player == "Dashy" &
           map_name != "Terminal") %>%
  select(map_name, map_result, team_score, opp_score) %>%
  group_by(map_name) %>%
  summarise(AvgScore = round(mean(team_score), 0), 
            OppAvgScore = round(mean(opp_score), 0),
            Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2)))

ultra_hp_summary <- 
  cdlDF %>%
  filter(gamemode == "Hardpoint" &
           team == "Toronto Ultra" & 
           player == "Scrap" &
           map_name != "Terminal") %>%
  select(map_name, map_result, team_score, opp_score) %>%
  group_by(map_name) %>%
  summarise(AvgScore = round(mean(team_score), 0),
            MedianScore = round(median(team_score), 0),
            OppAvgScore = round(mean(opp_score), 0),
            Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2)))

surge_hp_summary <- 
  cdlDF %>%
  filter(gamemode == "Hardpoint" &
           team == "Seattle Surge" & 
           player == "Huke" &
           map_name != "Terminal") %>%
  select(map_name, map_result, team_score) %>%
  group_by(map_name) %>%
  summarise(AvgScore = round(mean(team_score), 0), 
            Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2)))

# Team by Team Control Analysis ------------------------------------------------

ultra_ctrl_summary <- 
  cdlDF %>%
  filter(gamemode == "Control" &
           team == "Toronto Ultra" & 
           player == "Scrap") %>%
  select(map_name, map_result, team_score, opp_score) %>%
  group_by(map_name) %>%
  summarise(AvgScore = round(mean(team_score), 0),
            OppAvgScore = round(mean(opp_score), 0),
            Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2)))


# More analysis ----------------------------------------------------------------

view(cdlDF %>% filter(gamemode == "Hardpoint") %>% arrange(kills) %>%
       select(match_id, match_date, player, team, map_name, kills, deaths, dmg, 
              team_score, map_result, opp, opp_score))

view(cdlDF %>% filter(gamemode == "Search & Destroy") %>% arrange(kills) %>%
       select(match_id, match_date, player, team, map_name, kills, deaths, dmg, 
              team_score, map_result, opp, opp_score))

view(cdlDF %>% filter(gamemode == "Control") %>% arrange(kills) %>%
       select(match_id, match_date, player, team, map_name, kills, deaths, dmg, 
              team_score, map_result, opp, opp_score))

# Total Team Kills by Map & Gamemode -------------------------------------------

cdlDF %>% filter(gamemode == "Hardpoint") %>%
  group_by(match_id, team, map_name) %>%
  summarise(total_team_kills = sum(kills)) %>%
  filter(map_name != "Terminal") %>%
  ggplot(mapping = aes(x = map_name, y = total_team_kills)) +
  geom_boxplot(alpha = 0.5) +
  geom_point()

cdlDF %>% filter(gamemode == "Control") %>%
  group_by(match_id, team, map_name) %>%
  summarise(total_team_kills = sum(kills)) %>%
  ggplot(mapping = aes(x = map_name, y = total_team_kills)) +
  geom_boxplot(alpha = 0.5) +
  geom_point()

# Kills vs Total Score ---------------------------------------------------------

cdlDF %>% filter(gamemode == "Hardpoint") %>%
  ggplot(mapping = aes(x = total_score, y = kills)) +
  geom_point()

cdlDF %>% filter(gamemode == "Search & Destroy") %>%
  ggplot(mapping = aes(x = total_score, y = kills)) +
  geom_point()

cdlDF %>% filter(gamemode == "Control") %>%
  ggplot(mapping = aes(x = total_score, y = kills)) +
  geom_point()

# Map & Mode Analysis: Pts Allowed in Win vs Team ------------------------------

cdlDF %>% filter(gamemode == "Hardpoint" & map_name == "Invasion" &
                   map_wl == "W") %>%
  group_by(match_id, team) %>%
  summarise(pts_allowed = median(opp_score)) %>%
  group_by(team) %>%
  summarise(median_pts_allowed = round(median(pts_allowed), 0)) %>%
  ggplot() +
  geom_col(mapping = aes(x = team, y = median_pts_allowed))

# Map & Mode Analysis: Pts For in Loss vs Team ---------------------------------

cdlDF %>% filter(gamemode == "Control" & map_name == "Karachi" &
                   map_wl == "L") %>%
  group_by(match_id, team) %>%
  summarise(pts_for = median(team_score)) %>%
  group_by(team) %>%
  summarise(median_pts_for = round(median(pts_for), 0)) %>%
  ggplot() +
  geom_col(mapping = aes(x = team, y = median_pts_for)) +
  labs(title = "Karachi Control - Points Allowed in Win vs. Team") +
  theme(plot.margin = margin(t = 8, r = 8, b = 2, l = 8, unit = "pt"), 
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8))

# Team Win Percentages ---------------------------------------------------------

cdlDF %>% 
  filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
           !(gamemode == "Search & Destroy" & map_name == "Highrise")) %>%
  group_by(match_id, team, map_num, gamemode) %>%
  summarise(wl = median(map_result)) %>%
  group_by(team, gamemode) %>%
  summarise(Wins = sum(wl), 
            Losses = n() - sum(wl), 
            Total = n()) %>%
    mutate("WinPercentage" = round(Wins/Total, 2)) %>%
  ggplot() +
  geom_col(mapping = aes(x = team, y = WinPercentage, fill = gamemode), 
           position = position_dodge2()) +
  labs(title = "Team Win Percentages by Gamemode") +
  theme(plot.margin = margin(t = 8, r = 2, b = 2, l = 8, unit = "pt"), 
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8))

# Team Win Percentages by Gamemode ---------------------------------------------

cdlDF %>% 
  filter(gamemode == "Hardpoint" & map_name != "Terminal") %>%
  group_by(match_id, team, map_num, gamemode) %>%
  summarise(wl = median(map_result)) %>%
  group_by(team, gamemode) %>%
  summarise(Wins = sum(wl), 
            Losses = n() - sum(wl), 
            Total = n()) %>%
  mutate("WinPercentage" = round(Wins/Total, 2)) %>%
  ggplot() +
  geom_col(mapping = aes(x = team, y = WinPercentage), 
           position = position_dodge2()) +
  labs(title = "Team Win Percentages: Hardpoint") +
  theme(plot.margin = margin(t = 8, r = 8, b = 2, l = 8, unit = "pt"), 
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8))

cdlDF %>% 
  filter(gamemode == "Search & Destroy" & map_name != "Highrise") %>%
  group_by(match_id, team, map_num, gamemode) %>%
  summarise(wl = median(map_result)) %>%
  group_by(team, gamemode) %>%
  summarise(Wins = sum(wl), 
            Losses = n() - sum(wl), 
            Total = n()) %>%
  mutate("WinPercentage" = round(Wins/Total, 2)) %>%
  ggplot() +
  geom_col(mapping = aes(x = team, y = WinPercentage), 
           position = position_dodge2()) +
  labs(title = "Team Win Percentages: Search & Destroy") +
  theme(plot.margin = margin(t = 8, r = 8, b = 2, l = 8, unit = "pt"), 
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8))

cdlDF %>% 
  filter(gamemode == "Control") %>%
  group_by(match_id, team, map_num, gamemode) %>%
  summarise(wl = median(map_result)) %>%
  group_by(team, gamemode) %>%
  summarise(Wins = sum(wl), 
            Losses = n() - sum(wl), 
            Total = n()) %>%
  mutate("WinPercentage" = round(Wins/Total, 2)) %>%
  ggplot() +
  geom_col(mapping = aes(x = team, y = WinPercentage), 
           position = position_dodge2()) +
  labs(title = "Team Win Percentages: Control") +
  theme(plot.margin = margin(t = 8, r = 8, b = 2, l = 8, unit = "pt"), 
        plot.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8))

# Breach vs Legion Analysis ----------------------------------------------------

# Kills by Team, Gamemode, Map ----

cdlDF %>%
  filter(team == "Boston Breach" & gamemode == "Hardpoint" & 
           map_name != "Terminal" & player != "Capsidal") %>%
  ggplot(mapping = aes(x = player, y = kills)) +
  geom_point(mapping = aes(x = player, y = kills, color = map_name), size = 2.25) +
  geom_boxplot(alpha = 0.25) +
  scale_color_brewer(palette = "Spectral")

cdlDF %>%
  filter(team == "Las Vegas Legion" & gamemode == "Hardpoint" & 
           map_name != "Terminal" & player != "Standy") %>%
  ggplot(mapping = aes(x = player, y = kills, color = map_name)) +
  geom_point()

view(cdlDF %>% filter(player == "Dashy"))

# Hardpoint ----

boxplot_of_kills("Scrap", "Hardpoint", 23, "Karachi") # U

kills_over_time_vs_line("Snoopy", "Hardpoint", 23.5, "Karachi") # U
kills_over_time_vs_line("Asim", "Hardpoint", 22.5, "Karachi") # O
kills_over_time_vs_line("Priestahh", "Hardpoint", 20.5, "Karachi") # U
kills_over_time_vs_line("SlasheR", "Hardpoint", 19.5, "Karachi") # U

boston_hp_summary <- 
  cdlDF %>%
  filter(gamemode == "Hardpoint" &
           team == "Boston Breach" & 
           player == "Snoopy" &
           map_name != "Terminal") %>%
  select(map_name, map_result) %>%
  group_by(map_name) %>%
  summarise(Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2))) 

kills_over_time_vs_line("Nero", "Hardpoint", 24.5, "Karachi") # U
kills_over_time_vs_line("Purj", "Hardpoint", 21.5, "Karachi") # U
kills_over_time_vs_line("Gio", "Hardpoint", 22.5, "Karachi") # O
kills_over_time_vs_line("Attach", "Hardpoint", 21.5, "Karachi") # O

vegas_hp_summary <- 
  cdlDF %>%
  filter(gamemode == "Hardpoint" &
           team == "Las Vegas Legion" & 
           player == "Attach" &
           map_name != "Terminal") %>%
  select(map_name, map_result) %>%
  group_by(map_name) %>%
  summarise(Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2))) 

# SnD ----

kills_over_time_vs_line("Snoopy", "Search & Destroy", 7, "Karachi") # O
kills_over_time_vs_line("Asim", "Search & Destroy", 7.5, "Karachi") # U
kills_over_time_vs_line("Priestahh", "Search & Destroy", 6.5, "Karachi") # U
kills_over_time_vs_line("SlasheR", "Search & Destroy", 7, "Karachi") # U

boston_snd_summary <- 
  cdlDF %>%
  filter(gamemode == "Search & Destroy" &
           team == "Boston Breach" & 
           player == "Snoopy" &
           map_name != "Highrise") %>%
  select(map_name, map_result) %>%
  group_by(map_name) %>%
  summarise(Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2))) 

kills_over_time_vs_line("Nero", "Search & Destroy", 6.5, "Karachi") # U
kills_over_time_vs_line("Purj", "Search & Destroy", 6.5, "Karachi") # U
kills_over_time_vs_line("Gio", "Search & Destroy", 6.5, "Karachi") # U
kills_over_time_vs_line("Attach", "Search & Destroy", 7, "Karachi") # O

vegas_snd_summary <- 
  cdlDF %>%
  filter(gamemode == "Search & Destroy" &
           team == "Las Vegas Legion" & 
           player == "Attach" &
           map_name != "") %>%
  select(map_name, map_result) %>%
  group_by(map_name) %>%
  summarise(Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2))) 

# Control ----

kills_over_time_vs_line("Snoopy", "Control", 24.5, "Invasion") # O
kills_over_time_vs_line("Asim", "Control", 23.5, "Invasion") # U
kills_over_time_vs_line("Priestahh", "Control", 23, "Invasion") # O
kills_over_time_vs_line("SlasheR", "Control", 22, "Invasion") # O

boston_ctrl_summary <- 
  cdlDF %>%
  filter(gamemode == "Control" &
           team == "Boston Breach" & 
           player == "Snoopy") %>%
  select(map_name, map_result) %>%
  group_by(map_name) %>%
  summarise(Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2))) 

kills_over_time_vs_line("Nero", "Control", 25.5, "Invasion") # O
kills_over_time_vs_line("Purj", "Control", 23.5, "Invasion") # O
kills_over_time_vs_line("Gio", "Control", 23, "Invasion") # O
kills_over_time_vs_line("Attach", "Control", 24, "Invasion") # U

vegas_ctrl_summary <- 
  cdlDF %>%
  filter(gamemode == "Control" &
           team == "Las Vegas Legion" & 
           player == "Attach") %>%
  select(map_name, map_result) %>%
  group_by(map_name) %>%
  summarise(Wins = sum(map_result),
            Losses = n() - sum(map_result), 
            Total = n()) %>%
  mutate("Win %" = percent(round(Wins/Total, 2))) 

# Big wake K/D vs. HP Score Differential ---------------------------------------

bigWakePlot <- cdlDF %>% 
  filter(player == "Owakening" & gamemode == "Hardpoint") %>%
  mutate(score_diff = team_score - opp_score) %>%
  ggplot(mapping = aes(x = score_diff, y = kd)) +
  geom_point(color = "white", fill = "skyblue", size = 3, shape = 21) +
  geom_smooth(color = "red", level = 0.5, linewidth = 0.65) +
  geom_hline(yintercept = 1, color = "white", 
             linewidth = 0.85) +
  geom_vline(xintercept = 0, color = "white", 
             linewidth = 0.85) +
  labs(title = "Owakening Hardpoint K/D vs. Team Win Differential") +
  xlab("Win Differential") + ylab("K/D") +
  theme(
    plot.background = element_rect(fill = "gray30"), 
    panel.background = element_rect(fill = "gray30", colour="white"), 
    plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "white", face = "bold"), 
    axis.title.y = element_text(color = "white", face = "bold", 
                                angle = 0, vjust = 0.5), 
    axis.line.x.bottom = element_blank(), 
    axis.line.y.right = element_blank(),
    axis.text.x = element_text(color = "white"), 
    axis.text.y = element_text(color = "white"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(colour = "gray", linewidth = 0.25),
    panel.grid.minor = element_line(colour = "gray", linewidth = 0.25)
  )

ggsave("Big Wake.png", plot = bigWakePlot, path = plotPath, 
       width = 1600, height = 1280, units = "px")

  
  





