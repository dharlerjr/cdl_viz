
# Get previous date to filter by for rank changes ------------------------------

strDate = readline();
filterDate = as.Date(strDate, "%B %d, %Y")

# Win %/s by Map & Mode DF -----------------------------------------------------

map_and_modes_DF <- 
  bind_rows(cdlDF %>% group_by(match_id, team, gamemode, map_name) %>% 
              summarise(maps_won = sum(map_result) / 4, 
                        n = n() / 4) %>%
              group_by(team, gamemode, map_name) %>%
              summarise(map_wins = sum(maps_won), 
                        maps_played = sum(n)) %>%
              mutate(win_percentage = round(map_wins / maps_played , 2)) %>%
              select(team, gamemode, map_name, win_percentage, maps_played), 
            cdlDF %>% group_by(match_id, team, gamemode) %>%
              summarise(maps_won = sum(map_result) / 4, 
                        n = n() / 4) %>%
              group_by(team, gamemode) %>%
              summarise(win_percentage = round(sum(maps_won) / sum(n), 2), 
                        maps_played = sum(n))
  ) %>%
  replace_na(list(map_name = "Overall"))

# Dictionary to filter out players from Dataframe ------------------------------

players_to_filter <-
  list(
    "Yes" = c(), 
    "No" = dropped_players
  )

# Create K/D Dataframe for gtTables --------------------------------------------

kdDF <- cdlDF %>% group_by(player, gamemode) %>%
  summarise(total_kills = sum(kills), total_deaths = sum(deaths), 
            gamemode_kd = round(sum(kills) / sum(deaths), 2)) %>%
  left_join(rostersDF, by = c("player" = "player")) %>%
  pivot_wider(names_from = gamemode, 
              values_from = c(total_kills, total_deaths, gamemode_kd)) %>%
  rename("hpkills" = "total_kills_Hardpoint", 
         "hpdeaths" = "total_deaths_Hardpoint", 
         "hpkd" = "gamemode_kd_Hardpoint",
         "sndkills" = "total_kills_Search & Destroy", 
         "snddeaths" = "total_deaths_Search & Destroy",
         "sndkd" = "gamemode_kd_Search & Destroy",
         "ctrlkills" = "total_kills_Control", 
         "ctrldeaths" = "total_deaths_Control", 
         "ctrlkd" = "gamemode_kd_Control") %>%
  mutate(overallkd = round((hpkills + sndkills + ctrlkills) / 
                             (hpdeaths + snddeaths + ctrldeaths), 2)) %>%
  arrange(desc(overallkd)) %>% ungroup() %>%
  select(player, team, overallkd, hpkd, sndkd, ctrlkd)

# Dataframe of Avg Kills per Maps 1 - 3 -------------------------------------

seriesDF <- cdlDF %>% filter(!(player %in% players_to_filter[["Yes"]]) &
                               map_num %in% 1:3) %>%
  group_by(match_id, player) %>%
  summarise(series_kills = sum(kills), series_deaths = sum(deaths)) %>%
  group_by(player) %>%
  summarise(kills_per_series = mean(series_kills), 
            series_played = n()) %>%
  mutate(overall_rank = round(rank(desc(kills_per_series), 
                                   ties.method = "min"), 0)) %>%
  arrange(desc(kills_per_series)) %>%
  left_join(rostersDF, by = c("player" = "player")) %>%
  select(overall_rank, player, team, kills_per_series, series_played)

# Dataframe of Avg Kills per Gamemode ------------------------------------------

avg_kills_per_gamemode_DF <- 
  cdlDF %>% group_by(player, gamemode) %>% 
  summarise(mean_kills = mean(kills), 
            maps_played = n()) %>%
  pivot_wider(names_from = gamemode, 
              values_from = c(mean_kills, maps_played)) %>%
  rename("hpAvgKills" = "mean_kills_Hardpoint", 
         "sndAvgKills" = "mean_kills_Search & Destroy", 
         "ctrlAvgKills" = "mean_kills_Control",
         "hpMapsPlayed" = "maps_played_Hardpoint",
         "sndMapsPlayed" = "maps_played_Search & Destroy", 
         "ctrlMapsPlayed" = "maps_played_Control")

# Points Allowed in Win & Points For in Loss DF --------------------------------

pts_by_team_and_mode_df <- 
  left_join(
    cdlDF %>% 
      filter(map_wl == "W") %>%
      group_by(match_id, team, gamemode, map_name) %>%
      summarise(pts_allowed = mean(opp_score)) %>%
      group_by(team, gamemode) %>%
      summarise(avg_pts_allowed = mean(pts_allowed)), 
    cdlDF %>% 
      filter(map_wl == "L") %>%
      group_by(match_id, team, gamemode, map_name) %>%
      summarise(pts_for = mean(team_score)) %>%
      group_by(team, gamemode) %>%
      summarise(avg_pts_for = mean(pts_for)), 
    by = c("team" = "team", "gamemode" = "gamemode")
  ) %>%
  left_join(
    team_logos_colors_df , by = c("team" = "team")
  )

# Dictionary to filter out maps by Gamemode ------------------------------------

maps_to_filter <- c(
  "Hardpoint" = "Terminal", 
  "Search & Destroy" = "Skidrow", 
  "Control" = "Placeholder"
)

# Function to create gtTable of Win %s by Map & Mode ---------------------------

win_percent_gt_fn <- function(gamemode_input) {
  
  tempDF <- cdlDF %>%
    filter(gamemode == gamemode_input & match_date < filterDate) %>%
    select(match_id, team, map_name, map_result) %>%
    distinct() %>%
    group_by(team) %>%
    summarise(maps_won = sum(map_result), maps_played = n()) %>%
    mutate(win_percentage_Overall = round(maps_won / maps_played, 2)) %>%
    mutate(old_rank = round(rank(desc(win_percentage_Overall), 
                                 ties.method = "min"), 0)) %>%
    select(team, old_rank)
  
  map_and_modes_DF %>% 
    filter(gamemode == gamemode_input &
             map_name != maps_to_filter[[gamemode_input]]) %>%
    pivot_wider(names_from = map_name, 
                values_from = c(win_percentage, maps_played)) %>%
    ungroup() %>% select(-gamemode) %>%
    arrange(desc(win_percentage_Overall)) %>%
    mutate(gamemode_rank = round(rank(desc(win_percentage_Overall), 
                                      ties.method = "min"), 0)) %>% 
    left_join(tempDF, by = c("team" = "team")) %>%
    mutate(rank_change = old_rank - gamemode_rank) %>% 
    select(-old_rank) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint | # of times played in parentheses") %>%
    opt_row_striping() %>%
    tab_header(title = paste0("CDL ", gamemode_input, " Win % by Map"),
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>% 
    data_color(
      columns = starts_with("win_percentage"),
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = c(0, 1))) %>%
    cols_width(gamemode_rank ~ px(40), team ~ px(160),
               starts_with("win_percentage") ~ px(80), 
               rank_change ~ px(40)) %>%
    cols_label(gamemode_rank = "", rank_change = "") %>%
    fmt_percent(columns = starts_with("win_percentage"), 
                decimals = 0) %>%
    tab_options(table.font.size = px(12)) %>%
    cols_move(gamemode_rank, after = team) %>%
    cols_move(rank_change, after = team) %>%
    cols_move(team, after = gamemode_rank) %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    )
}

# Dictionary for footnote ------------------------------------------------------

footnote_dictionary <-
  list(
    "Yes" = "All CDL players from the MW3 season included", 
    "No" = "Current CDL players only"
  )

# Helper function for K/D gtTable title ----------------------------------------

kd_title <- function(i, j){
  if (i >= 27){return(" K/D Red Carpet")}
  else if (j <= 26) {return(" K/D Leaders")}
  else {return(" K/D's")}
}

# Function to create gtTable of all K/Ds by Gamemode --------------------------------

all_kds_gt <- function(i, j, all_players = "Yes", lft = 96, rght = 96){
  
  tempDF <- 
    (cdlDF %>% 
       filter(match_date < filterDate & 
                !(player %in% players_to_filter[[all_players]])) %>% 
       group_by(player) %>%
       summarise(oldKD = round(sum(kills) / sum(deaths), 2))) %>%
    mutate(old_rank = rank(desc(oldKD), ties.method = "min")) %>%
    select(-oldKD)
  
  (kdDF %>% mutate(kd_rank = round(rank(desc(overallkd), 
                                        ties.method = "min"), 0)))[i:j, ] %>%
    left_join(tempDF, by = c("player" = "player")) %>%
    mutate(rank_change = old_rank - kd_rank) %>%
    select(rank_change, kd_rank, player, team, overallkd, hpkd, sndkd, ctrlkd) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
    opt_row_striping() %>%
    tab_header(title = "CDL K/D's by Gamemode",
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>% 
    data_color(
      columns = ends_with("kd"), 
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = c(0.55, 1.45))) %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    ) %>%
    cols_width(ends_with("rank") ~ px(60), player ~ px(100), 
               team ~ px(200), ends_with("kd") ~ px(100), 
               rank_change ~ px(40)) %>%
    tab_footnote(
      footnote = footnote_dictionary["Yes"],
      locations = cells_column_labels(columns = player)
    ) %>%
    cols_label(ends_with("rank") ~ "", hpkd = "Hardpoint", 
               sndkd = "SnD", ctrlkd = "Control", rank_change = "",
               overallkd = "Overall") %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght), 
                page.margin.top = px(10), page.margin.bottom = px(10))
}

# Function to create gtTable of K/D by Gamemode ------------------------------

kd_gt_fn <- function(i, j, gamemode_input, gamemode_kd_var, all_players = "Yes", 
                     lft = 96, rght = 96){
  old_KD_DF <- (cdlDF %>%
    filter(match_date < filterDate & 
             !(player %in% players_to_filter[[all_players]])) %>%
    group_by(player, gamemode) %>%
    summarise(total_kills = sum(kills), total_deaths = sum(deaths), 
              gamemode_kd = round(sum(kills) / sum(deaths), 2)) %>%
    left_join(rostersDF, by = c("player" = "player")) %>%
    pivot_wider(names_from = gamemode, 
                values_from = c(total_kills, total_deaths, gamemode_kd)) %>%
    rename("hpkills" = "total_kills_Hardpoint", 
           "hpdeaths" = "total_deaths_Hardpoint", 
           "hpkd" = "gamemode_kd_Hardpoint",
           "sndkills" = "total_kills_Search & Destroy", 
           "snddeaths" = "total_deaths_Search & Destroy",
           "sndkd" = "gamemode_kd_Search & Destroy",
           "ctrlkills" = "total_kills_Control", 
           "ctrldeaths" = "total_deaths_Control", 
           "ctrlkd" = "gamemode_kd_Control") %>%
    mutate(overallkd = round((hpkills + sndkills + ctrlkills) / 
                               (hpdeaths + snddeaths + ctrldeaths), 2)) %>%
    ungroup() %>%
    mutate(old_rank = round(rank(desc( {{gamemode_kd_var}} ), 
                                ties.method = "min"), 0)))[i:j, ] %>%
    select(player, old_rank)
    
  
  (kdDF %>% 
     filter(!(player %in% players_to_filter[[all_players]])) %>%
     arrange(desc({{ gamemode_kd_var }})) %>% 
     mutate(kd_rank = round(rank(desc( {{gamemode_kd_var}} ), 
                                 ties.method = "min"), 0)))[i:j, ] %>%
    left_join(old_KD_DF, by = c("player" = "player")) %>%
    mutate(rank_change = old_rank - kd_rank) %>%
    select(rank_change, kd_rank, player, team, {{ gamemode_kd_var }} ) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
    opt_row_striping() %>%
    tab_header(title = paste0("CDL ", gamemode_input, 
                              kd_title(i, j)),
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>% 
    data_color(
      columns = {{ gamemode_kd_var}},
      target_columns = {{ gamemode_kd_var }} , 
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = c(0.55, 1.45))) %>%
    cols_width(ends_with("rank") ~ px(60), player ~ px(100), 
               team ~ px(200), ends_with("kd") ~ px(100), rank_change ~ px(40)) %>%
    tab_footnote(
      footnote = footnote_dictionary[all_players],
      locations = cells_column_labels(columns = player)
    ) %>%
    cols_label(ends_with("rank") ~ "", ends_with("kd") ~ "K/D", 
               rank_change ~ "") %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    ) %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght))
}

# Dictionary of K/D columns to select by Gamemode -------------------------------

kd_columns_dict <- list(
  "Hardpoint" = list(kdDF$hpkd, "hpkd"), 
  "Search & Destroy" = list(kdDF$sndkd, "sndkd"),
  "Control" = list(kdDF$ctrlkd, "ctrlkd")
)

# Rank array -------------------------------------------------------------------

my_ranks <- 1:56

# Function to create gtTable of K/D's by Map & Mode ----------------------------

map_mode_kd_gt_fn <- function(i, j, gamemode_input, all_players = "Yes", 
                              lft = 96, rght = 96) {
  
  old_KD_DF <- 
    (cdlDF %>% filter(match_date < filterDate & gamemode == gamemode_input &
                        !(player %in% players_to_filter[[all_players]])) %>%
       group_by(player) %>%
       summarise(oldKD = round(sum(kills) / sum(deaths), 2)) %>%
       mutate(old_rank = round(rank(desc(oldKD), 
                                    ties.method = "min"), 0)) %>%
       arrange(old_rank) %>%
  select(player, old_rank))[i:j, ]
  
  left_join(
    (kdDF %>% filter(!(player %in% players_to_filter[[all_players]])) %>%
       select(player, team, kd_columns_dict[[gamemode_input]][[2]]) %>%
      mutate(kd_rank = as.numeric(
               round(rank(desc(kd_columns_dict[[gamemode_input]][[1]]), 
                                         ties.method = "min"), 0))) %>%
      arrange(kd_rank))[i:j, ],
    cdlDF %>% filter(map_name != maps_to_filter[[gamemode_input]] &
                       gamemode == gamemode_input) %>%
      group_by(player, gamemode, map_name) %>%
      summarise(gamemode_kd = round(sum(kills) / sum(deaths), 2), 
                maps_played = n()) %>%
      pivot_wider(names_from = map_name, 
                  values_from = c(gamemode_kd, maps_played)) %>% ungroup(), 
    by = c("player" = "player")
  ) %>% left_join(old_KD_DF, by = c("player" = "player")) %>%
    mutate(rank_change = old_rank - kd_rank) %>%
    select(-c(gamemode, old_rank)) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint | # of times played in parentheses") %>%
    opt_row_striping() %>%
    tab_header(title = paste0("CDL ", gamemode_input, " K/D by Map"),
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>% 
    data_color(
      columns = ends_with("kd"),
      fn = scales::col_bin(
        palette =  c("#CB181D", "#FB6A4A", "#FC9272", "#FCBBA1", "#FFF5F0",
                     "#F7FBFF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#2171B5"),
        bins = c(0, 0.5, 0.75, 0.9, 1, 1.1, 1.25, 1.5, 7), 
        na.color = "#d3d3d3")) %>%
    data_color(
      columns = starts_with("gamemode_kd"),
      fn = scales::col_bin(
        palette =  c("#CB181D", "#FB6A4A", "#FC9272", "#FCBBA1", "#FFF5F0",
                     "#F7FBFF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#2171B5"),
        bins = c(0, 0.5, 0.75, 0.9, 1, 1.1, 1.25, 1.5, 7), 
        na.color = "#d3d3d3")) %>%
    cols_width(kd_rank ~ px(60), team ~ px(200), player ~ px(100),
               starts_with("gamemode_kd") ~ px(100), ends_with("kd") ~ px(100), 
               rank_change ~ px(40)) %>%
    tab_footnote(
      footnote = footnote_dictionary[all_players],
      locations = cells_column_labels(columns = player)
    ) %>%
    cols_label(ends_with("rank") ~ "", ends_with("kd") ~ "Overall", 
               rank_change ~ "") %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght), 
                ) %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    ) %>%
    cols_move(kd_rank, after = player) %>%
    cols_move(rank_change, after = player) %>%
    cols_move(player, after = kd_rank)
}

# Helper function for Kills gtTable title --------------------------------------

kills_title <- function(i, j){
  if (i >= 30){return(" Red Carpet")}
  else if (j <= 26) {return(" Leaders")}
  else {return("")}
}

# Dictionary for Kills gtTable Data Color Domain -------------------------------

kills_per_gamemode_data_color_domains <- 
  list(
    "Hardpoint" =  c(15, 27),
    "Search & Destroy" = c(2, 9),
    "Control" = c(12, 33)
  )

# Function to create gtTable of Avg Kills by Gamemode --------------------------

avg_kills_gt_fn <- function(i, j, gamemode_input, all_players = "Yes", 
                            lft = 96, rght = 96){
  tempDF <- (cdlDF %>% 
    filter(match_date < filterDate & gamemode == gamemode_input & 
             !(player %in% players_to_filter[[all_players]])) %>%
    group_by(player) %>%
    summarise(mean_kills = mean(kills), 
              maps_played = n()) %>%
    arrange(desc(mean_kills)) %>%
    mutate(old_rank = round(rank(desc(mean_kills), 
                                    ties.method = "min"), 0)))[i:j, ] %>%
    select(player, old_rank)
  
  (cdlDF %>% filter(!(player %in% players_to_filter[[all_players]]) &
                      gamemode == gamemode_input) %>%
     group_by(player) %>% 
     summarise(mean_kills = mean(kills), 
               maps_played = n()) %>%
     arrange(desc(mean_kills)) %>%
     mutate(player_rank = round(rank(desc(mean_kills), 
                                     ties.method = "min"), 0)))[i:j, ] %>%
    left_join(rostersDF, by = c("player" = "player")) %>% 
    left_join(tempDF, by = c("player" = "player")) %>%
    mutate(rank_change = old_rank - player_rank) %>%
    select(rank_change, player_rank, player, team, mean_kills, maps_played) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
    opt_row_striping() %>%
    tab_header(title =  paste0("CDL Avg Kills per ", gamemode_input, 
                               kills_title(i, j)), 
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>%
    data_color(
      columns = mean_kills,
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = kills_per_gamemode_data_color_domains[[gamemode_input]])
    ) %>%
    cols_width(player_rank ~ px(60), player ~ px(100), team ~ px(200), 
               mean_kills ~ px(100), maps_played ~ px(80), rank_change ~ px(40)) %>%
    tab_footnote(
      footnote = footnote_dictionary[all_players],
      locations = cells_column_labels(columns = player)
    ) %>%
    cols_label(player_rank = "", mean_kills = "Avg Kills", 
               maps_played = "Maps Played", rank_change = "") %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    ) %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght)) 
  
}

# Dictionary of Avg Kills columns to select by Gamemode -------------------------

avg_kill_columns_dict <- list(
  "Hardpoint" = "hpMapsPlayed", 
  "Search & Destroy" = "sndMapsPlayed",
  "Control" = "ctrlMapsPlayed"
)

# Dictionary for Avg Kills by Map & Mode gtTable Data Color Domain -------------

kills_per_map_mode_data_color_domains <- 
  list(
    "Hardpoint" =  c(7, 35),
    "Search & Destroy" = c(0, 11),
    "Control" = c(8, 32)
  )

# Function to create gtTable of Avg Kills by Map & Mode ------------------------

map_mode_avg_kills_gt_fn <- function(i, j, gamemode_input, gamemode_var, 
                                     all_players = "Yes", lft = 96, rght = 96){
  tempDF <- (cdlDF %>% 
    filter(gamemode == gamemode_input & match_date < filterDate &
             !(player %in% players_to_filter[[all_players]])) %>%
    group_by(player) %>% 
    summarise(mean_kills = mean(kills), 
              maps_played = n()) %>%
    mutate(old_rank = round(rank(desc(mean_kills), ties.method = "min"), 0)) %>%
    arrange(old_rank) %>% select(player, old_rank))[i:j, ]

  
  left_join(
    (avg_kills_per_gamemode_DF %>%
       ungroup() %>%
       filter(!(player %in% players_to_filter[[all_players]])) %>%
       select(player, {{ gamemode_var} }, avg_kill_columns_dict[[gamemode_input]]) %>%
       arrange(desc( {{gamemode_var}} )) %>%
       mutate(player_rank = rank(desc( {{gamemode_var}} ), 
         ties.method = "min")))[i:j, ],
    cdlDF %>% filter(map_name != maps_to_filter[[gamemode_input]] &
                       gamemode == gamemode_input) %>%
      group_by(player, gamemode, map_name) %>%
      summarise(mean_kills = mean(kills), 
                maps_played = n()) %>%
      pivot_wider(names_from = map_name, 
                  values_from = c(mean_kills, maps_played)) %>% ungroup(), 
    by = c("player" = "player")
  ) %>% select(-gamemode) %>% ungroup() %>%
    left_join(rostersDF, by = c("player" = "player")) %>%
    left_join(tempDF, by = c("player" = "player")) %>%
    mutate(rank_change = old_rank - player_rank) %>%
    select(-old_rank) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint | # of times played in parentheses") %>%
    opt_row_striping() %>%
    tab_header(title = paste0("CDL Avg Kills per ", gamemode_input, " by Map"),
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>% 
    data_color(
      columns = starts_with("mean_kills"),
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = kills_per_map_mode_data_color_domains[[gamemode_input]], 
        na.color = "#d3d3d3")
    ) %>%
    data_color(
      columns = ends_with("AvgKills"),
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = kills_per_map_mode_data_color_domains[[gamemode_input]], 
        na.color = "#d3d3d3")
    ) %>%
    cols_width("player_rank" ~ px(60), player ~ px(100), rank_change ~ px(40), 
               team ~ px(200), ends_with("AvgKills") ~ px(100), 
               starts_with("mean_kills") ~ px(100)) %>%
    tab_footnote(
      footnote = footnote_dictionary[all_players],
      locations = cells_column_labels(columns = player)
    ) %>%
    cols_label(ends_with("rank") ~ "", ends_with("AvgKills") ~ "Overall", 
               rank_change ~ "") %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght)) %>%
    cols_merge(columns = c(2, 3), pattern = "{1} ({2})") %>%
    cols_move(columns = team, after = player) %>%
    cols_move(player_rank, after = player) %>%
    cols_move(rank_change, after = player) %>%
    cols_move(player, after = player_rank) %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    ) 
}

# Function to create gtTable of Avg Kills per Maps 1 - 3 -----------------------

series_avg_kills_gt_fn <- function(i, j, all_players = "Yes", 
                                   lft = 96, rght = 96){
  
  tempDF <- (cdlDF %>%
    filter(!(player %in% players_to_filter[[all_players]]) &
             map_num %in% 1:3 & match_date < filterDate) %>%
    group_by(match_id, player) %>%
    summarise(series_kills = sum(kills), series_deaths = sum(deaths)) %>%
    group_by(player) %>%
    summarise(kills_per_series = mean(series_kills), 
              series_played = n()) %>%
    mutate(old_rank = round(rank(desc(kills_per_series), 
                                    ties.method = "min"), 0)))[i:j, ] %>%
    select(player, old_rank)
  
  (cdlDF %>% filter(!(player %in% players_to_filter[[all_players]]) &
                      map_num %in% 1:3) %>%
     group_by(match_id, player) %>%
     summarise(series_kills = sum(kills), series_deaths = sum(deaths)) %>%
     group_by(player) %>%
     summarise(kills_per_series = mean(series_kills), 
               series_played = n()) %>%
     mutate(player_rank = round(rank(desc(kills_per_series), 
                                     ties.method = "min"), 0)) %>%
     arrange(desc(kills_per_series)))[i:j, ] %>%
    left_join(rostersDF, by = c("player" = "player")) %>%
    left_join(tempDF, by = c("player" = "player")) %>%
    mutate(rank_change = old_rank - player_rank) %>%
    select(rank_change, player_rank, player, team, kills_per_series, series_played) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
    opt_row_striping() %>%
    tab_header(title =  paste0("CDL Avg Kills per Maps 1 - 3", 
                               kills_title(i, j)), 
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>%
    data_color(
      columns = kills_per_series,
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = c(37, 56))
    ) %>%
    cols_width(player_rank ~ px(60), player ~ px(100), team ~ px(200), 
               kills_per_series ~ px(100), series_played ~ px(80), 
               rank_change ~ px(40)) %>%
    tab_footnote(
      footnote = footnote_dictionary[all_players],
      locations = cells_column_labels(columns = player)
    ) %>%
    cols_label(player_rank = "", kills_per_series = "Avg Kills per Maps 1 - 3", 
               series_played = "Series Played", rank_change = "") %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght)) %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    )
  
}

# Function to create gtTable of Avg Kills per Maps 1 - 3 by Series Result ------

avg_kills_by_series_res_gt_fn <- function(i, j, all_players = "Yes", 
                                          lft = 96, rght = 96){
  tempDF <- (cdlDF %>%
               filter(!(player %in% players_to_filter[[all_players]]) &
                        map_num %in% 1:3 & match_date < filterDate) %>%
               group_by(match_id, player) %>%
               summarise(series_kills = sum(kills), series_deaths = sum(deaths)) %>%
               group_by(player) %>%
               summarise(kills_per_series = mean(series_kills), 
                         series_played = n()) %>%
               mutate(old_rank = round(rank(desc(kills_per_series), 
                                            ties.method = "min"), 0)))[i:j, ] %>%
    select(player, old_rank)
  
  (cdlDF %>% filter(!(player %in% players_to_filter[[all_players]]) &
                      map_num %in% 1:3) %>%
      group_by(match_id, player, series_wl) %>%
      summarise(series_kills = sum(kills)) %>%
      group_by(player, series_wl) %>%
      summarise(kills_per_series = mean(series_kills), 
                series_played = n()) %>%
      pivot_wider(names_from = series_wl, 
                  values_from = c(kills_per_series, series_played)) %>%
      left_join(seriesDF, by = c("player" = "player")) %>%
      left_join(tempDF, by = c("player" = "player")) %>%
      mutate(rank_change = old_rank - overall_rank) %>%
      ungroup() %>%
      arrange(desc(kills_per_series)))[i:j, ] %>%
    select(rank_change, overall_rank, player, team, kills_per_series, 
           series_played, kills_per_series_W, series_played_W, 
           kills_per_series_L, series_played_L) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
    opt_row_striping() %>%
    tab_header(title =  paste0("CDL Avg Kills per Maps 1 - 3 by Series Win/Loss ", 
                               kills_title(i, j)), 
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>%
    data_color(
      columns = starts_with("kills_per_series"),
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = c(30, 63))
    ) %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght)) %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    ) %>%
    cols_width(overall_rank ~ px(60), player ~ px(100), team ~ px(200), 
               starts_with("kills_per_series") ~ px(100), 
               rank_change ~ px(40)) %>%
    tab_footnote(
      footnote = footnote_dictionary[all_players],
      locations = cells_column_labels(columns = player)
    ) %>%
    cols_label(overall_rank = "", kills_per_series = "Overall", 
               kills_per_series_W = "Win", kills_per_series_L = "Loss", 
               rank_change = "") %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght)) %>%
    cols_merge(columns = c(5, 6), pattern = "{1} ({2})") %>%
    cols_merge(columns = c(7, 8), pattern = "{1} ({2})") %>%
    cols_merge(columns = c(9, 10), pattern = "{1} ({2})")
}

# Function to create gtTable of Avg Kills per Series & per Gamemode ------------

big_avg_kills_gt_fn <- function(i, j, all_players = "Yes", lft = 96, rght = 96){
  
  tempDF <- (cdlDF %>%
               filter(!(player %in% players_to_filter[[all_players]]) &
                        map_num %in% 1:3 & match_date < filterDate) %>%
               group_by(match_id, player) %>%
               summarise(series_kills = sum(kills), series_deaths = sum(deaths)) %>%
               group_by(player) %>%
               summarise(kills_per_series = mean(series_kills), 
                         series_played = n()) %>%
               mutate(old_rank = round(rank(desc(kills_per_series), 
                                            ties.method = "min"), 0)))[i:j, ] %>%
    select(player, old_rank)
  
  (left_join(seriesDF, avg_kills_per_gamemode_DF, 
             by = c("player" = "player")))[i:j, ] %>%
    left_join(tempDF, by = c("player" = "player")) %>%
    mutate(rank_change = old_rank - overall_rank) %>%
    select(rank_change, overall_rank, player, team, 
           kills_per_series, series_played, 
           hpAvgKills, hpMapsPlayed, 
           sndAvgKills, sndMapsPlayed, 
           ctrlAvgKills, ctrlMapsPlayed) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
    opt_row_striping() %>%
    tab_header(title =  "CDL Avg Kills per Maps 1 - 3 & per Gamemode",
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>%
    data_color(
      columns = kills_per_series,
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = c(37, 56))
    ) %>%
    data_color(
      columns = hpAvgKills,
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = kills_per_gamemode_data_color_domains[["Hardpoint"]])
    ) %>%
    data_color(
      columns = sndAvgKills,
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = kills_per_gamemode_data_color_domains[["Search & Destroy"]])
    ) %>%
    data_color(
      columns = ctrlAvgKills,
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = kills_per_gamemode_data_color_domains[["Control"]])
    ) %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    ) %>%
    cols_merge(columns = c(5, 6), pattern = "{1} ({2})") %>%
    cols_merge(columns = c(7, 8), pattern = "{1} ({2})") %>%
    cols_merge(columns = c(9, 10), pattern = "{1} ({2})") %>%
    cols_merge(columns = c(11, 12), pattern = "{1} ({2})") %>%
    cols_width(overall_rank ~ px(60), player ~ px(100), team ~ px(200), 
               kills_per_series ~ px(100), hpAvgKills ~ px(100), 
               sndAvgKills ~ px(100), ctrlAvgKills ~ px(100), 
               rank_change ~ px(40)) %>%
    tab_spanner(label = "Avg Kills per Maps 1 - 3", columns = 5:6) %>%
    tab_spanner(label = "Avg Kills by Gamemode", columns = 7:12) %>%
    tab_footnote(
      footnote = footnote_dictionary[all_players],
      locations = cells_column_labels(columns = player)
    ) %>%
    tab_footnote(
      footnote = "# of series played in parentheses" ,
      locations = cells_column_spanners("Avg Kills per Maps 1 - 3")
    ) %>%
    tab_footnote(
      footnote = "# of maps played in parentheses" ,
      locations = cells_column_spanners("Avg Kills by Gamemode")
    ) %>%
    cols_label(overall_rank = "", kills_per_series = "Kills", 
               hpAvgKills = "Hardpoint", sndAvgKills = "Search & Destroy", 
               ctrlAvgKills = "Control", rank_change = "") %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght))
  
}

# Dictionary of kd columns to select by gamemode -------------------------------

kd_wl_columns <- list(
  "Overall" = list("gamemode_kd_overall_W", "gamemode_kd_overall_L"),
  "Hardpoint" = list("gamemode_kd_Hardpoint_W", "gamemode_kd_Hardpoint_L"),
  "Search & Destroy" = list("gamemode_kd_SnD_W", "gamemode_kd_SnD_L"),
  "Control" = list("gamemode_kd_Control_W", "gamemode_kd_Control_L")
)

# Dictionary for K/D by WL gtTable Data Color Domain ---------------------------

kd_by_wl_data_color_domains <- 
  list(
    "Overall" = c(0.45, 1.6),
    "Hardpoint" =  c(0.55, 1.45),
    "Search & Destroy" = c(0.2, 1.9),
    "Control" = c(0.55, 1.5)
  )

# Function to create gtTable of K/Ds by Gamemode & Map Result ------------------

kd_wl_gt_fn <- function(i, j, gamemode_input, gamemode_kd_var, all_players = "Yes", 
                        lft = 96, rght = 96){
  old_KD_DF <- cdlDF %>%
    filter(match_date < filterDate & 
             !(player %in% players_to_filter[[all_players]])) %>%
    group_by(player, gamemode) %>%
    summarise(total_kills = sum(kills), total_deaths = sum(deaths), 
              gamemode_kd = round(sum(kills) / sum(deaths), 2)) %>%
    left_join(rostersDF, by = c("player" = "player")) %>%
    pivot_wider(names_from = gamemode, 
                values_from = c(total_kills, total_deaths, gamemode_kd)) %>%
    rename("hpkills" = "total_kills_Hardpoint", 
           "hpdeaths" = "total_deaths_Hardpoint", 
           "hpkd" = "gamemode_kd_Hardpoint",
           "sndkills" = "total_kills_Search & Destroy", 
           "snddeaths" = "total_deaths_Search & Destroy",
           "sndkd" = "gamemode_kd_Search & Destroy",
           "ctrlkills" = "total_kills_Control", 
           "ctrldeaths" = "total_deaths_Control", 
           "ctrlkd" = "gamemode_kd_Control") %>%
    mutate(overallkd = round((hpkills + sndkills + ctrlkills) / 
                               (hpdeaths + snddeaths + ctrldeaths), 2)) %>%
    ungroup() %>%
    mutate(old_rank = round(rank(desc( {{gamemode_kd_var}} ), 
                                 ties.method = "min"), 0)) %>%
    arrange(old_rank) %>% select(player, old_rank)
  
  (cdlDF %>% filter(!(player %in% players_to_filter[[all_players]])) %>%
      group_by(player, gamemode, map_wl) %>%
      summarise(total_kills = sum(kills), total_deaths = sum(deaths), 
                gamemode_kd = round(sum(kills) / sum(deaths), 2)) %>%
      pivot_wider(names_from = c(gamemode, map_wl), 
                  values_from = c(total_kills, total_deaths, gamemode_kd)) %>%
      rename("total_kills_SnD_W" = "total_kills_Search & Destroy_W", 
             "total_kills_SnD_L" = "total_kills_Search & Destroy_L",
             "total_deaths_SnD_W" = "total_deaths_Search & Destroy_W",
             "total_deaths_SnD_L" = "total_deaths_Search & Destroy_L", 
            "gamemode_kd_SnD_W" = "gamemode_kd_Search & Destroy_W",
            "gamemode_kd_SnD_L" = "gamemode_kd_Search & Destroy_L") %>%
      left_join(kdDF, by = c("player" = "player")) %>%
      mutate(gamemode_kd_overall_W = round(
               (total_kills_Control_W + total_kills_Hardpoint_W +
                  total_kills_SnD_W) /
               (total_deaths_Control_W + total_deaths_Hardpoint_W +
                  total_deaths_SnD_L), 2),
            gamemode_kd_overall_L = round( 
               (total_kills_Control_L + total_kills_Hardpoint_L +
                  total_kills_SnD_L) /
               (total_deaths_Control_L + total_deaths_Hardpoint_L +
                  total_deaths_SnD_L), 2)) %>%
      left_join(old_KD_DF, by = c("player" = "player")) %>%
      ungroup() %>%
      mutate(kd_rank = round(rank(desc({{gamemode_kd_var}}), ties.method = "min"), 0)) %>%
      mutate(rank_change = old_rank - kd_rank) %>%
      arrange(kd_rank) %>%
      select(rank_change, kd_rank, player, team,
             {{gamemode_kd_var}}, 
             kd_wl_columns[[gamemode_input]][[1]], 
             kd_wl_columns[[gamemode_input]][[2]]))[i:j, ] %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
    opt_row_striping() %>%
    tab_header(title = paste0("CDL ", gamemode_input, " K/Ds by Map Win/Loss"),
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>% 
    data_color(
      columns = starts_with("gamemode_kd"), 
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = kd_by_wl_data_color_domains[[gamemode_input]])) %>%
    data_color(
      columns = ends_with("kd"), 
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = kd_by_wl_data_color_domains[[gamemode_input]])) %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    ) %>%
    tab_footnote(
      footnote = footnote_dictionary[all_players],
      locations = cells_column_labels(columns = player)
    ) %>%
    cols_width(rank_change ~ px(40), kd_rank ~ px(60), player ~ px(100), 
               team ~ px(200), ends_with("kd") ~ px(100), 
               columns = starts_with("gamemode_kd") ~ px(100)
               ) %>%
    cols_label(rank_change ~ "", kd_rank ~ "", ends_with("kd") ~ "Overall",
               ends_with("W") ~ "Win", ends_with("L") ~ "Loss"
               ) %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght))
    
}

# Function to create gtTable of Avg Kills by Gamemode & Map Result -------------

avg_kills_wl_gt_fn <- function(i, j, gamemode_input, all_players = "Yes", 
                               lft = 96, rght = 96){
  
  tempDF <- 
    cdlDF %>% 
    filter(match_date < filterDate & gamemode == gamemode_input & 
             !(player %in% players_to_filter[[all_players]])) %>%
    group_by(player) %>%
    summarise(mean_kills = mean(kills), maps_played = n()) %>%
    arrange(desc(mean_kills)) %>%
    mutate(old_rank = round(rank(desc(mean_kills), ties.method = "min"), 0)) %>%
    select(player, old_rank)
  
  (cdlDF %>%
    filter(!(player %in% players_to_filter[[all_players]]) &
               gamemode == gamemode_input) %>%
    group_by(player, map_wl) %>%
    summarise(total_kills = sum(kills), 
               maps_played = n()) %>%
    pivot_wider(names_from = map_wl, 
                values_from = c(total_kills, maps_played)) %>% ungroup() %>%
    mutate(mean_kills_W = round(total_kills_W / maps_played_W, 0), 
           mean_kills_L = round(total_kills_L / maps_played_L, 0), 
           mean_kills_overall = 
             round((total_kills_W + total_kills_L) / (maps_played_W + maps_played_L), 
                   0), 
           maps_played_overall = maps_played_W + maps_played_L, 
           player_rank = round(rank(desc(mean_kills_overall), ties.method = "min"), 2)) %>%
    left_join(rostersDF, by = c("player" = "player")) %>%
    left_join(tempDF, by = c("player" = "player")) %>%
    mutate(rank_change = old_rank - player_rank) %>%
    select(rank_change, player_rank, player, team, 
           mean_kills_overall, maps_played_overall, 
           mean_kills_W, maps_played_W, mean_kills_L, maps_played_L) %>%
    arrange(player_rank))[i:j, ] %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
    opt_row_striping() %>%
    tab_header(title = paste0("CDL Average Kills per ", gamemode_input, " by Map Win/Loss"),
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>% 
    data_color(
      columns = starts_with("mean_kills"), 
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = kills_per_gamemode_data_color_domains[[gamemode_input]])) %>%
    gt_fa_rank_change(
      column = rank_change,
      palette = c("#238B45", "#ffffff", "#cb181d"),
      fa_type = "arrow", 
      font_color = "match"
    ) %>%
    tab_footnote(
      footnote = footnote_dictionary[all_players],
      locations = cells_column_labels(columns = player)
    ) %>%
    cols_merge(columns = c(5, 6), pattern = "{1} ({2})") %>%
    cols_merge(columns = c(7, 8), pattern = "{1} ({2})") %>%
    cols_merge(columns = c(9, 10), pattern = "{1} ({2})") %>%
    cols_width(rank_change ~ px(40), player_rank ~ px(60), player ~ px(100), 
               team ~ px(200), starts_with("mean_kills") ~ px(100)
    ) %>%
    cols_label(rank_change = "", player_rank = "", 
               mean_kills_overall = "Overall", mean_kills_W = "Win", 
               mean_kills_L = "Loss") %>%
    tab_options(table.margin.left = px(lft), table.margin.right = px(rght))
}

