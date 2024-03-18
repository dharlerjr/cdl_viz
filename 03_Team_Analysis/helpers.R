

# Dictionary of color scales by gamemode ---------------------------------------

gamemode_color_scales <- list(
  "Hardpoint" = c("red", "orange", "green", "blue", "purple"),
  "Search & Destroy" = c("red", "orange", "green", "blue", "purple"),
  "Control" = c("red", "green", "blue")
)

# Dictionary of ylims by gamemode ----------------------------------------------

gamemode_kill_lims <- list(
  "Hardpoint" = c(0, 45), 
  "Search & Destroy" = c(0, 16), 
  "Control" = c(0, 45)
)

# Dictionary of binwidths by gamemode ------------------------------------------

gamemode_bins <- list(
  "Hardpoint" = 50, 
  "Search & Destroy" = 1, 
  "Control" = 1
)

# Dictionary of viridis color scales by gamemode -------------------------------

gamemode_color_scales <- list(
  "Hardpoint" = 
    c("#FDE725FF", "#56c667ff", "#21908CFF", "#3B528BFF", "#440154FF"), 
  "Search & Destroy" = 
    c("#FDE725FF", "#56c667ff", "#21908CFF", "#3B528BFF", "#440154FF"), 
  "Control" = c("#FDE725FF", "#56c667ff", "#21908CFF")
)

# Dictionary of viridis colors by map & mode -----------------------------------

map_and_mode_colors <- list(
  "Hardpoint" = list(
    "Invasion" = "#FDE725FF", 
    "Karachi" = "#56c667ff",
    "Rio" = "#21908CFF",
    "Skidrow" = "#3B528BFF",
    "Sub Base" = "#440154FF"
  ), 
  "Search & Destroy" = list(
    "Highrise" = "#FDE725FF", 
    "Invasion" = "#56c667ff",
    "Karachi" = "#21908CFF", 
    "Rio" = "#3B528BFF",
    "Terminal" = "#440154FF"
  ), 
  "Control" = list(
    "Highrise" = "#FDE725FF", 
    "Invasion" = "#56c667ff", 
    "Karachi" = "#21908CFF"
  )
)

# Team Summary DF --------------------------------------------------------------

team_summaries <- 
  bind_rows(
    cdlDF %>%
      select(match_id, team, map_name, gamemode, map_result) %>%
      distinct() %>% 
      group_by(team, gamemode, map_name) %>%
      summarise(wins = sum(map_result), 
                losses = n() - sum(map_result), 
                win_percentage = round(sum(map_result) / n(), 2)) %>%
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") &
               !(gamemode == "Search & Destroy" & map_name == "Skidrow")), 
    cdlDF %>%
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") &
               !(gamemode == "Search & Destroy" & map_name == "Skidrow")) %>%
      select(match_id, team, map_name, gamemode, map_result) %>%
      distinct() %>%
      group_by(team, gamemode) %>%
      summarise(wins = sum(map_result), 
                losses = n() - sum(map_result), 
                win_percentage = round(sum(map_result) / n(), 2))
  ) %>%
  replace_na(list(map_name = "Overall"))

team_summaries$gamemode <- factor(
  team_summaries$gamemode, levels = c("Hardpoint", "Search & Destroy", "Control")
  )

team_summaries$map_name <- factor(
  team_summaries$map_name, 
  levels = c("Highrise", "Invasion", "Karachi", "Rio", "Skidrow", 
             "Sub Base", "Terminal", "Overall")
)

# 01 Team Summary --------------------------------------------------------------

team_summary_gt_fn <- function(team_input) {
  
  team_summaries %>% filter(team == team_input) %>% 
    ungroup() %>% select(-team) %>% arrange(gamemode, map_name) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint | # of times played in parentheses") %>%
    opt_row_striping() %>%
    tab_header(title = paste0(team_input, " Team Summary"),
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>%
    tab_row_group(label = "Hardpoint", 
                  rows = gamemode == "Hardpoint") %>%
    tab_row_group(label = "Search & Destroy", 
                  rows = gamemode == "Search & Destroy") %>%
    tab_row_group(label = "Control", 
                  rows = gamemode == "Control") %>%
    row_group_order(c("Hardpoint", "Search & Destroy", "Control")) %>%
    cols_hide(columns = gamemode) %>%
    data_color(
      columns = ends_with("percentage"),
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = c(0, 1))) %>%
    cols_width(map_name ~ px(160), wins ~ px(76), losses ~ px(76), 
               win_percentage ~ px(100)) %>%
    cols_label(map_name = "Map", win_percentage = "Win %", 
               wins = "W", losses = "L") %>%
    fmt_percent(columns = starts_with("win_percentage"), 
                decimals = 0)
  
}

# 02 Team vs Team Summaries ----------------------------------------------------

team_summary_vs_fn <- function(team_x, team_y) {
  
  left_join(
    team_summaries %>% filter(team == team_x) %>% 
      ungroup(),
    team_summaries %>% filter(team == team_y) %>% 
      ungroup(),
    by = c("gamemode" = "gamemode", "map_name" = "map_name")
  ) %>%
    select(-c(team.x, team.y)) %>% arrange(gamemode, map_name) %>%
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint | # of times played in parentheses") %>%
    opt_row_striping() %>%
    tab_header(title = paste0(team_x, " & ", team_y, " Summaries"),
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>%
    tab_spanner(label = team_x, columns = ends_with("x")) %>%
    tab_spanner(label = team_y, columns = ends_with("y")) %>%
    tab_row_group(label = "Hardpoint", 
                  rows = gamemode == "Hardpoint") %>%
    tab_row_group(label = "Search & Destroy", 
                  rows = gamemode == "Search & Destroy") %>%
    tab_row_group(label = "Control", 
                  rows = gamemode == "Control") %>%
    row_group_order(c("Hardpoint", "Search & Destroy", "Control")) %>%
    cols_hide(columns = gamemode) %>%
    data_color(
      columns = starts_with("win_percentage"),
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = c(0, 1))) %>%
    cols_width(map_name ~ px(160), starts_with("win_percentage") ~ px(100),
               starts_with("wins") ~ px(76), starts_with("losses") ~ px(76)) %>%
    cols_label(map_name = "Map", starts_with("win_percentage") ~ "Win %", 
               starts_with("wins") ~ "W", starts_with("losses") ~ "L") %>%
    fmt_percent(columns = starts_with("win_percentage"), 
                decimals = 0)
    
    
}

# 03 Team H2H  -----------------------------------------------------------------

team_h2h_gt_fn <- function(team_a, team_b){
  
  tempDF <- cdlDF %>%
    select(match_id, team, map_name, gamemode, map_result, opp) %>%
    filter(!(gamemode == "Hardpoint" & map_name == "Terminal") &
             !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
             team == team_a & opp == team_b) %>%
    distinct() %>%
    group_by(gamemode) %>%
    summarise(team_a_wins = sum(map_result),
              team_a_losses = n() - sum(map_result),
              team_a_win_percentage = round(sum(map_result) / n(), 2),
              team_b_wins = n() - sum(map_result), 
              team_b_losses = sum(map_result), 
              team_b_win_percentage = round((n() - sum(map_result)) / n(), 2))
  
  tempDF$map_name = c("Overall", "Overall", "Overall")
  
  tempDF <- tempDF %>%
    select(gamemode, map_name, 
           team_a_wins, team_a_losses, team_a_win_percentage, 
           team_b_wins, team_b_losses, team_b_win_percentage)
  
  tempDF <- bind_rows(
    tempDF, 
    cdlDF %>%
      select(match_id, team, map_name, gamemode, map_result, opp) %>%
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") &
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               team == team_a & opp == team_b) %>%
      distinct() %>%
      group_by(gamemode, map_name) %>%
      summarise(team_a_wins = sum(map_result),
                team_a_losses = n() - sum(map_result),
                team_a_win_percentage = round(sum(map_result) / n(), 2),
                team_b_wins = n() - sum(map_result), 
                team_b_losses = sum(map_result), 
                team_b_win_percentage = round((n() - sum(map_result)) / n(), 2))
    )
  
  tempDF$map_name <- factor(
    tempDF$map_name, 
    levels = c("Highrise", "Invasion", "Karachi", "Rio", "Skidrow", 
               "Sub Base", "Terminal", "Overall")
  )
  
  tempDF %>% ungroup() %>% arrange(map_name) %>% 
    gt() %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint | # of times played in parentheses") %>%
    opt_row_striping() %>%
    tab_header(title = paste0(team_a, " & ", team_b, " H2H Summary"),
               subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
    gt_theme_espn() %>%
    tab_spanner(label = team_a, columns = starts_with("team_a")) %>%
    tab_spanner(label = team_b, columns = starts_with("team_b")) %>%
    tab_row_group(label = "Hardpoint", 
                  rows = gamemode == "Hardpoint") %>%
    tab_row_group(label = "Search & Destroy", 
                  rows = gamemode == "Search & Destroy") %>%
    tab_row_group(label = "Control", 
                  rows = gamemode == "Control") %>%
    row_group_order(c("Hardpoint", "Search & Destroy", "Control")) %>%
    cols_hide(columns = gamemode) %>%
    data_color(
      columns = ends_with("win_percentage"),
      fn = scales::col_numeric(
        palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                     "#bdd7e7", "#2171b5", "#2171b5"),
        domain = c(0, 1))) %>%
    cols_width(map_name ~ px(160), ends_with("win_percentage") ~ px(100),
               ends_with("wins") ~ px(76), ends_with("losses") ~ px(76)) %>%
    cols_label(map_name = "Map", ends_with("win_percentage") ~ "Win %", 
               ends_with("wins") ~ "W", ends_with("losses") ~ "L") %>%
    fmt_percent(columns = ends_with("win_percentage"), 
                decimals = 0)
  
}

# 04 Kills by Player -----------------------------------------------------------

kills_by_player <- function(team_input, gamemode_input, map_input = NULL){
  if(is.null(map_input)){
    cdlDF %>% 
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               gamemode == gamemode_input & team == team_input &
               !(player %in% dropped_players)) %>%
      ggplot(mapping = aes(x = player, y = kills)) +
      geom_jitter(mapping = aes(x = player, y = kills, color = map_name), 
                  size = 1.5, width = 0.075, height = 0.075) +
      geom_boxplot(alpha = 0.25, outlier.alpha = 0) +
      scale_color_manual(values = gamemode_color_scales[[gamemode_input]]) +
      labs(title = paste0(team_input, " Kills by Player colored by Map: ", 
                          gamemode_input), 
           subtitle = format(Sys.Date(), "%B %d, %Y"), 
           caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
      xlab("Player") + ylab("Kills") +
      ylim(gamemode_kill_lims[[gamemode_input]]) +
      theme(
        plot.title = element_text(size = 16),
        axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
        axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
      )
    
  }
  
  else{
    cdlDF %>%
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               gamemode == gamemode_input & team == team_input &
               !(player %in% dropped_players) & map_name == map_input) %>%
      ggplot(mapping = aes(x = player, y = kills)) +
      geom_jitter(mapping = aes(x = player, y = kills), 
                  size = 1.5, width = 0.075, height = 0.075) +
      geom_boxplot(alpha = 0.25, outlier.alpha = 0) +
      scale_color_manual(values = gamemode_color_scales[[gamemode_input]]) +
      labs(title = paste0(team_input, " Kills by Player: ", 
                          map_input, " ", gamemode_input), 
           subtitle = format(Sys.Date(), "%B %d, %Y"), 
           caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
      xlab("Player") + ylab("Kills") +
      ylim(gamemode_kill_lims[[gamemode_input]]) +
      theme(
        plot.title = element_text(size = 16),
        axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
        axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
      )
  }
}

# 05 K/D by Player -------------------------------------------------------------

kd_by_player <- function(team_input, gamemode_input, map_input = NULL){
  if(is.null(map_input)){
    cdlDF %>% 
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               gamemode == gamemode_input & team == team_input &
               !(player %in% dropped_players)) %>%
      ggplot(mapping = aes(x = player, y = kd)) +
      geom_jitter(mapping = aes(x = player, y = kd, color = map_name), 
                  size = 1.5, width = 0.075, height = 0.075) +
      geom_boxplot(alpha = 0.25, outlier.alpha = 0) +
      scale_color_manual(values = gamemode_color_scales[[gamemode_input]]) +
      labs(title = paste0(team_input, " K/D by Player colored by Map: ", 
                          gamemode_input), 
           subtitle = format(Sys.Date(), "%B %d, %Y"), 
           caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
      xlab("Player") + ylab("K/D") +
      theme(
        plot.title = element_text(size = 16),
        axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
        axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
      )
    
  }
  
  else{
    cdlDF %>%
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               gamemode == gamemode_input & team == team_input &
               !(player %in% dropped_players) & map_name == map_input) %>%
      ggplot(mapping = aes(x = player, y = kd)) +
      geom_jitter(mapping = aes(x = player, y = kd), 
                  size = 1.5, width = 0.075, height = 0.075) +
      geom_boxplot(alpha = 0.25, outlier.alpha = 0) +
      scale_color_manual(values = gamemode_color_scales[[gamemode_input]]) +
      labs(title = paste0(team_input, " K/D by Player: ", 
                          map_input, " ", gamemode_input), 
           subtitle = format(Sys.Date(), "%B %d, %Y"), 
           caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
      xlab("Player") + ylab("K/D") +
      theme(
        plot.title = element_text(size = 16),
        axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
        axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
      )
  }
}

# 06 Kills vs Score Differential by Player -------------------------------------

kills_vs_score_diff <- function(team_input, gamemode_input, map_input = NULL){
  
  if(is.null(map_input)){
    cdlDF %>% 
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               gamemode == gamemode_input & team == team_input &
               !(player %in% dropped_players)) %>%
      ggplot(mapping = aes(x = score_diff, y = kills, color = player)) +
      geom_jitter(width = 0.1, height = 0.1, size = 2) +
      geom_smooth(level = 0) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      scale_color_manual(values = c("red", "orange", "green", "blue")) +
      labs(title = paste0(team_input, " Kills vs. Score Differential: ", gamemode_input), 
           subtitle = format(Sys.Date(), "%B %d, %Y"), 
           caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
      xlab("Score Differential") + ylab("Kills") +
      theme(
        plot.title = element_text(size = 16),
        axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
        axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
      )
  }
  
  else{
    cdlDF %>% 
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               gamemode == gamemode_input & team == team_input &
               !(player %in% dropped_players) & map_name == map_input) %>%
      ggplot(mapping = aes(x = score_diff, y = kills, color = player)) +
      geom_jitter(width = 0.1, height = 0.1, size = 2) +
      geom_smooth(level = 0) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      scale_color_manual(values = c("red", "orange", "green", "blue")) +
      labs(title = paste0(team_input, " Kills vs. Score Differential: ", 
                          map_input, " ", gamemode_input), 
           subtitle = format(Sys.Date(), "%B %d, %Y"), 
           caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
      xlab("Score Differential") + ylab("Kills") +
      theme(
        plot.title = element_text(size = 16),
        axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
        axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
      )
  }
  
}

# 07 K/D vs Score Differential by Player ---------------------------------------

kd_vs_score_diff <- function(team_input, gamemode_input, map_input = NULL){
  
  if(is.null(map_input)){
    cdlDF %>% 
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               gamemode == gamemode_input & team == team_input &
               !(player %in% dropped_players)) %>%
      ggplot(mapping = aes(x = score_diff, y = kd, color = player)) +
      geom_jitter(width = 0.1, height = 0.1, size = 2) +
      geom_smooth(level = 0) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      scale_color_manual(values = c("red", "orange", "green", "blue")) +
      labs(title = paste0(team_input, " K/D vs. Score Differential: ", gamemode_input), 
           subtitle = format(Sys.Date(), "%B %d, %Y"), 
           caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
      xlab("Score Differential") + ylab("K/D") +
      theme(
        plot.title = element_text(size = 16),
        axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
        axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
      )
  }
  
  else{
    cdlDF %>% 
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               gamemode == gamemode_input & team == team_input &
               !(player %in% dropped_players) & map_name == map_input) %>%
      ggplot(mapping = aes(x = score_diff, y = kd, color = player)) +
      geom_jitter(width = 0.1, height = 0.1, size = 2) +
      geom_smooth(level = 0) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      scale_color_manual(values = c("red", "orange", "green", "blue")) +
      labs(title = paste0(team_input, " K/D vs. Score Differential: ", 
                          map_input, " ", gamemode_input), 
           subtitle = format(Sys.Date(), "%B %d, %Y"), 
           caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
      xlab("Score Differential") + ylab("K/D") +
      theme(
        plot.title = element_text(size = 16),
        axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
        axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
      )
  }
  
}

# 08 Score Differentials by Map & Mode -----------------------------------------

team_score_diffs <- function(team_input, gamemode_input, map_input = NULL){
  
  if(is.null(map_input)){
    cdlDF %>%
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               gamemode == gamemode_input & team == team_input) %>%
      select(match_id, map_name, score_diff) %>%
      distinct() %>%
      ggplot(aes(score_diff, fill = map_name)) +
      geom_histogram(position = position_dodge(), 
                     binwidth = gamemode_bins[[gamemode_input]], 
                     color = "white") +
      facet_wrap(~map_name) +
      scale_fill_manual(values = gamemode_color_scales[[gamemode_input]]) +
      labs(title = paste(team_input, "Distribution of Score Differentials:", 
                         gamemode_input),
           subtitle = format(Sys.Date(), "%B %d, %Y"), 
           caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
           fill = "Map") +
      xlab("Score Differential") + ylab("Count") +
      theme(
        plot.title = element_text(size = 16),
        axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
        axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
      )
  }
  else{
    cdlDF %>%
      filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
               !(gamemode == "Search & Destroy" & map_name == "Skidrow") &
               gamemode == gamemode_input & team == team_input & 
               map_name == map_input) %>%
      select(match_id, map_name, score_diff) %>%
      distinct() %>%
      ggplot(aes(score_diff, fill = map_name)) +
      geom_histogram(position = position_dodge(), 
                     binwidth = gamemode_bins[[gamemode_input]], 
                     color = "white") +
      scale_fill_manual(
        values = map_and_mode_colors[[gamemode_input]][[map_input]]
        ) +
      labs(title = paste(team_input, "Distribution of Score Differentials:", 
                         map_input, gamemode_input),
           subtitle = format(Sys.Date(), "%B %d, %Y"), 
           caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
           fill = "Map") +
      xlab("Score Differential") + ylab("Count") +
      theme(
        plot.title = element_text(size = 16),
        axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
        axis.text.y.left = element_text(size = 10, color = "#3b3b3b"), 
        legend.position = "none"
      )
  }
  
}



