
# Set directory ----------------------------------------------------------------

setwd("C:/Users/David Harler Jr/OneDrive/Desktop/dataClass/06-cod-analysis/2024.03.12 CDL Stats Visualization/cdl_viz")

# Save figure path -------------------------------------------------------------

figPath = 
  "C:/Users/David Harler Jr/OneDrive/Desktop/dataClass/06-cod-analysis/2024.03.12 CDL Stats Visualization/cdl_viz/02_League_Analysis/Figures"

# Source setup -----------------------------------------------------------------

source("00_Setup/00_Setup.R")

# Source helpers ---------------------------------------------------------------

source("02_League_Analysis/helpers.R")

# 01 Standings -----------------------------------------------------------------

standings_temp <- cdlDF %>% 
  group_by(match_id, team) %>% 
  summarise(series_res = median(series_result), 
            maps_won = sum(map_result) / 4, 
            maps_played = n() / 4) %>%
  mutate(maps_lost = maps_played - maps_won) %>%
  group_by(team) %>%
  summarise(series_wins = sum(series_res), 
            series_losses = n() - sum(series_res), 
            map_wins = sum(maps_won), 
            map_losses = sum(maps_lost), 
            total_maps = sum(maps_played)) %>%
  arrange(desc(series_wins), desc(map_wins)) 

standings_temp$rank_change <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

standings_temp$series_wins <- as.numeric(standings_temp$series_wins)
standings_temp$series_losses <- as.numeric(standings_temp$series_losses)

correct_standings_order = c(
  "Toronto Ultra", "Atlanta FaZe", "OpTic Texas", "New York Subliners", 
  "Minnesota ROKKR", "Seattle Surge", "Miami Heretics", "Las Vegas Legion", 
  "Los Angeles Thieves", "Los Angeles Guerrillas", "Carolina Royal Ravens", 
  "Boston Breach"
)

standings <- data.frame(
  team = character(), 
  series_wins = numeric(), 
  series_losses = numeric(), 
  map_wins = numeric(), 
  map_losses = numeric(), 
  total_maps = numeric(), 
  rank_change = numeric()
)

for(i in 1:12){
  standings[i, ] <- standings_temp[standings_temp$team == 
                                     correct_standings_order[i], ]
}

standings <- standings %>%
  mutate(series_percent = 
           round(series_wins / (series_wins + series_losses), 2),
         map_percent = round(map_wins / total_maps, 2))

place <- 1:12

standingsTbl <- standings %>% 
  mutate(series_percentage = percent(series_percent), 
         map_percentage = percent(map_percent)) %>% 
  bind_cols(place) %>%
  rename("place" = "...12") %>%
  select(rank_change, place, team, series_wins, series_losses, 
         series_percentage, map_wins, map_losses, map_percentage) %>%
  gt %>%
  opt_align_table_header("center") %>%
  cols_align("center") %>%
  tab_source_note("By: David Harler Jr. | Date from: @GGBreakingPoint") %>%
  opt_row_striping() %>%
  tab_header(title = "CDL Standings", 
             subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
  gt_theme_espn() %>%
  cols_width(
    place ~ px(60), team ~ px(180), 
    series_wins ~ px(80), series_losses ~ px(80), series_percentage ~ px(80), 
    map_wins ~ px(80), map_losses ~ px(80), map_percentage ~ px(80), 
    rank_change ~ px(40)
    ) %>%
  cols_label(
    place = "Place", team = "Team", series_wins = "Series Wins", 
    series_losses = "Series Losses", series_percentage = "Win %",
    map_wins = "Map Wins", map_losses = "Map Losses", map_percentage = "Map Win %",
    rank_change = ""
    ) %>%
  gt_fa_rank_change(
    column = rank_change,
    palette = c("#238B45", "#ffffff", "#cb181d"),
    fa_type = "arrow", 
    font_color = "match"
  )
  
gtsave(standingsTbl, "01_Standings.png", path = figPath)

# 02 Map Win % -----------------------------------------------------------------

leagueStats <- cdlDF %>% 
  group_by(match_id, team, gamemode) %>% 
  summarise(maps_won = sum(map_result) / 4, 
            maps_played = n() / 4) %>%
  mutate(maps_lost = maps_played - maps_won) %>%
  select(team, gamemode, maps_won, maps_lost) %>%
  group_by(team, gamemode) %>%
  summarise(map_wins = sum(maps_won), 
            map_losses = sum(maps_lost)) %>%
  arrange(desc(map_wins)) %>%
  pivot_wider(names_from = gamemode, 
              values_from = c(map_wins, map_losses)) %>%
  rename("hpwins" = "map_wins_Hardpoint", 
         "hplosses" = "map_losses_Hardpoint", 
         "sndwins" = "map_wins_Search & Destroy", 
         "sndlosses" = "map_losses_Search & Destroy",
         "ctrlwins" = "map_wins_Control", 
         "ctrllosses" = "map_losses_Control") %>%
  mutate(hp_percent = round(hpwins / (hpwins + hplosses), 2),
         snd_percent = round(sndwins / (sndwins + sndlosses), 2),
         ctrl_percent = round(ctrlwins / (ctrlwins + ctrllosses), 2)) %>%
  select(team, hpwins, hplosses, hp_percent, 
           sndwins, sndlosses, snd_percent, 
           ctrlwins, ctrllosses, ctrl_percent)

leagueStats$team <- factor(leagueStats$team, levels = correct_standings_order)
  
leagueStatsTbl <- leagueStats %>% arrange(team) %>% ungroup() %>% gt() %>%
  opt_align_table_header("center") %>%
  cols_align("center") %>%
  tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
  opt_row_striping() %>%
  tab_header(title = "CDL Team Records & Win %'s by Gamemode", 
             subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
  gt_theme_espn() %>%
  cols_width(
    team ~ px(180), hpwins ~ px(80), hplosses ~ px(80), hp_percent ~ px(80), 
    sndwins ~ px(80), sndlosses ~ px(80), snd_percent ~ px(80), 
    ctrlwins ~ px(80), ctrllosses ~ px(80), ctrl_percent ~ px(80)
  ) %>%
  tab_spanner(label = "Hardpoint", columns = 2:4) %>%
  tab_spanner(label = "Search & Destroy", columns = 5:7) %>%
  tab_spanner(label = "Control", columns = 8:10) %>%
  cols_label(team = "Team", 
             hpwins = "Wins", hplosses = "Losses", hp_percent = "Win %", 
             sndwins = "Wins", sndlosses = "Losses", snd_percent = "Win %", 
             ctrlwins = "Wins", ctrllosses = "Losses", ctrl_percent = "Win %") %>%
  fmt_percent(columns = ends_with("percent"), 
              decimals = 0)

gtsave(leagueStatsTbl, "02_Win_Percentages.png", path = figPath)

# 03 Win %s Colored ---------------------------------------------------------------

leagueStatsColoredTbl <- leagueStatsTbl %>%
  data_color(
    columns = ends_with("percent"),
    fn = scales::col_numeric(
      palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                   "#bdd7e7", "#2171b5", "#2171b5"),
      domain = c(0.15, 0.9)))

gtsave(leagueStatsColoredTbl, "03_Win_Percentages_Colored.png", path = figPath)

# 04 Hardpoint Win %s ----------------------------------------------------------

hpWinPercentageTbl <- win_percent_gt_fn("Hardpoint") %>%
  cols_merge(columns = c(2, 8), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(3, 9), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(4, 10), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(5, 11), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 12), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 13), pattern = "{1} ({2})") %>%
  cols_label(win_percentage_Invasion = "Invasion", 
             win_percentage_Karachi = "Karachi", 
             win_percentage_Rio = "Rio", 
             win_percentage_Skidrow = "Skidrow", 
             "win_percentage_Sub Base" = "Sub Base", 
             win_percentage_Overall = "Overall")

gtsave(hpWinPercentageTbl, "04_Win_Percentages_HP.png", path = figPath)

# 05 Search & Destroy Win %s ----------------------------------------------------

sndWinPercentageTbl <- win_percent_gt_fn("Search & Destroy") %>%
  cols_merge(columns = c(2, 8), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(3, 9), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(4, 10), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(5, 11), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 12), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 13), pattern = "{1} ({2})") %>%
  cols_label(win_percentage_Highrise = "Highrise", 
             win_percentage_Invasion = "Invasion", 
             win_percentage_Karachi = "Karachi", 
             win_percentage_Rio = "Rio", 
             win_percentage_Terminal = "Terminal", 
             win_percentage_Overall = "Overall")

gtsave(sndWinPercentageTbl, "05_Win_Percentages_SnD.png", path = figPath)

# 06 Control Win %s ------------------------------------------------------------

ctrlWinPercentageTbl <- win_percent_gt_fn("Control") %>%
  cols_merge(columns = c(2, 6), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(3, 7), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(4, 8), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(5, 9), pattern = "{1} ({2})") %>%
  cols_label(win_percentage_Highrise = "Highrise", 
             win_percentage_Invasion = "Invasion", 
             win_percentage_Karachi = "Karachi", 
             win_percentage_Overall = "Overall")

gtsave(ctrlWinPercentageTbl, "06_Win_Percentages_CTRL.png", path = figPath)

# 07 K/Ds by Gamemode ----------------------------------------------------------

all_kds_Tbl_left <- all_kds_gt(1, 28, "Yes", 60, 5)
all_kds_Tbl_right <- all_kds_gt(29, 56, "Yes", 5, 60)
all_kds_Tbl <- 
  gt_two_column_layout(
    tables = list(all_kds_Tbl_left, all_kds_Tbl_right)
  )

gt_two_column_layout(
  tables = list(all_kds_Tbl_left, all_kds_Tbl_right), 
  output = "save", 
  filename = "07_KDs_by_Gamemode.png", 
  path = figPath, 
  vwidth = 1800, 
  vheight = 1280, 
  zoom = 4
)

# 08 & 09 Overall K/Ds ---------------------------------------------------------

kd_overall_Tbl_left <- kd_gt_fn(1, 28, "Overall", overallkd, "Yes", 60, 5)
kd_overall_Tbl_right <- kd_gt_fn(29, 56, "Overall", overallkd, "Yes", 5, 60)
kd_overall_Tbl <- 
  gt_two_column_layout(
    tables = list(kd_overall_Tbl_left, kd_overall_Tbl_right)
  )

gt_two_column_layout(
  tables = list(kd_overall_Tbl_left, kd_overall_Tbl_right), 
  output = "save", 
  filename = "08_Overall_KDs.png", 
  path = figPath, 
  vwidth = 1200, 
  vheight = 1000, 
  zoom = 4
)

ovrl_kds_by_wl_left <- kd_wl_gt_fn(1, 24, "Overall", overallkd, "No", 60, 5)
ovrl_kds_by_wl_rght <- kd_wl_gt_fn(25, 48, "Overall", overallkd, "No", 5, 60)

gt_two_column_layout(
  tables = list(ovrl_kds_by_wl_left, ovrl_kds_by_wl_rght), 
  output = "save", 
  filename = "09_Overall_KDs_by_WL.png", 
  path = figPath, 
  vwidth = 1600, 
  vheight = 1000, 
  zoom = 4
)

# 10 & 11 Hardpoint K/Ds -------------------------------------------------------

kd_hp_Tbl_left <- kd_gt_fn(1, 28, "Hardpoint", hpkd, "Yes", 60, 5)
kd_hp_Tbl_right <- kd_gt_fn(29, 56, "Hardpoint", hpkd, "Yes", 5, 60)
big_kd_hp_Tbl <- 
  gt_two_column_layout(
    tables = list(kd_hp_Tbl_left, kd_hp_Tbl_right)
  )

gt_two_column_layout(
  tables = list(kd_hp_Tbl_left, kd_hp_Tbl_right), 
  output = "save", 
  filename = "10_HP_KDs.png", 
  path = figPath, 
  vwidth = 1200, 
  vheight = 1000, 
  zoom = 4
)

hp_kds_by_wl_left <- kd_wl_gt_fn(1, 24, "Hardpoint", hpkd, "No", 60, 5)
hp_kds_by_wl_rght <- kd_wl_gt_fn(25, 48, "Hardpoint", hpkd, "No", 5, 60)

gt_two_column_layout(
  tables = list(hp_kds_by_wl_left, hp_kds_by_wl_rght), 
  output = "save", 
  filename = "11_HP_KDs_by_WL.png", 
  path = figPath, 
  vwidth = 1600, 
  vheight = 1000, 
  zoom = 4
)

# 12 & 13 SnD K/Ds -------------------------------------------------------------

kd_snd_Tbl_left <- kd_gt_fn(1, 28, "Search & Destroy", sndkd, "Yes", 60, 5)
kd_hp_Tbl_right <- kd_gt_fn(29, 56, "Search & Destroy", sndkd, "Yes", 5, 60)
big_kd_snd_Tbl <- 
  gt_two_column_layout(
    tables = list(kd_snd_Tbl_left, kd_hp_Tbl_right)
  )

gt_two_column_layout(
  tables = list(kd_snd_Tbl_left, kd_hp_Tbl_right), 
  output = "save", 
  filename = "12_SnD_KDs.png", 
  path = figPath, 
  vwidth = 1200, 
  vheight = 1000, 
  zoom = 4
)

snd_kds_by_wl_left <- kd_wl_gt_fn(1, 24, "Search & Destroy", sndkd, "No", 60, 5)
snd_kds_by_wl_rght <- kd_wl_gt_fn(25, 48, "Search & Destroy", sndkd, "No", 5, 60)

gt_two_column_layout(
  tables = list(snd_kds_by_wl_left, snd_kds_by_wl_rght), 
  output = "save", 
  filename = "13_SnD_KDs_by_WL.png", 
  path = figPath, 
  vwidth = 1600, 
  vheight = 1000, 
  zoom = 4
)

# 14 & 15 Control K/Ds ---------------------------------------------------------

kd_ctrl_Tbl_left <- kd_gt_fn(1, 28, "Control", ctrlkd, "Yes", 60, 5)
kd_ctrl_Tbl_right <- kd_gt_fn(29, 56, "Control", ctrlkd, "Yes", 5, 60)
big_ctrl_kd_Tbl <- 
  gt_two_column_layout(
    tables = list(kd_ctrl_Tbl_left, kd_ctrl_Tbl_right)
  )

gt_two_column_layout(
  tables = list(kd_ctrl_Tbl_left, kd_ctrl_Tbl_right), 
  output = "save", 
  filename = "14_CTRL_KDs.png", 
  path = figPath, 
  vwidth = 1200, 
  vheight = 1000, 
  zoom = 4
)

ctrl_kds_by_wl_left <- kd_wl_gt_fn(1, 24, "Control", ctrlkd, "No", 60, 5)
ctrl_kds_by_wl_rght <- kd_wl_gt_fn(25, 48, "Control", ctrlkd, "No", 5, 60)

gt_two_column_layout(
  tables = list(ctrl_kds_by_wl_left, ctrl_kds_by_wl_rght), 
  output = "save", 
  filename = "15_CTRL_KDs_by_WL.png", 
  path = figPath, 
  vwidth = 1600, 
  vheight = 1000, 
  zoom = 4
)

# 16 Hardpoint K/Ds by Map -----------------------------------------------------

hp_kds_by_map_left <- map_mode_kd_gt_fn(1, 28, "Hardpoint", "Yes", 60, 5) %>%
  cols_merge(columns = c(5, 10), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 11), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 12), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(8, 13), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(9, 14), pattern = "{1} ({2})") %>%
  cols_label(gamemode_kd_Invasion = "Invasion", 
             gamemode_kd_Karachi = "Karachi", 
             gamemode_kd_Rio = "Rio", 
             gamemode_kd_Skidrow = "Skidrow", 
             "gamemode_kd_Sub Base" = "Sub Base")

hp_kds_by_map_right <- map_mode_kd_gt_fn(29, 56, "Hardpoint", "Yes", 5, 60) %>%
  cols_merge(columns = c(5, 10), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 11), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 12), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(8, 13), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(9, 14), pattern = "{1} ({2})") %>%
  cols_label(gamemode_kd_Invasion = "Invasion", 
             gamemode_kd_Karachi = "Karachi", 
             gamemode_kd_Rio = "Rio", 
             gamemode_kd_Skidrow = "Skidrow", 
             "gamemode_kd_Sub Base" = "Sub Base")

gt_two_column_layout(
  tables = list(hp_kds_by_map_left, hp_kds_by_map_right), 
  output = "save", 
  filename = "16_HP_KDs_by_Map.png", 
  path = figPath, 
  vwidth = 2200, 
  vheight = 1600, 
  zoom = 4
)

# 17 SnD K/Ds by Maps ----------------------------------------------------------

snd_kds_by_map_left <- map_mode_kd_gt_fn(1, 28, "Search & Destroy", "Yes", 60, 5) %>%
  cols_merge(columns = c(5, 10), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 11), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 12), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(8, 13), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(9, 14), pattern = "{1} ({2})") %>%
  cols_label(gamemode_kd_Highrise = "Highrise", 
             gamemode_kd_Invasion = "Invasion", 
             gamemode_kd_Karachi = "Karachi", 
             gamemode_kd_Rio = "Rio", 
             gamemode_kd_Terminal = "Terminal")

snd_kds_by_map_right <- map_mode_kd_gt_fn(29, 56, "Search & Destroy", "Yes", 5, 60) %>%
  cols_merge(columns = c(5, 10), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 11), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 12), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(8, 13), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(9, 14), pattern = "{1} ({2})") %>%
  cols_label(gamemode_kd_Highrise = "Highrise", 
             gamemode_kd_Invasion = "Invasion", 
             gamemode_kd_Karachi = "Karachi", 
             gamemode_kd_Rio = "Rio", 
             gamemode_kd_Terminal = "Terminal")

gt_two_column_layout(
  tables = list(snd_kds_by_map_left, snd_kds_by_map_right), 
  output = "save", 
  filename = "17_SnD_KDs_by_Map.png", 
  path = figPath, 
  vwidth = 2200, 
  vheight = 1600, 
  zoom = 4
)

# 18 Control K/Ds by Map -------------------------------------------------------

ctrl_kds_by_map_left <- map_mode_kd_gt_fn(1, 28, "Control", "Yes", 60, 5) %>%
  cols_merge(columns = c(5, 8), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 9), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 10), pattern = "{1} ({2})") %>%
  cols_label(gamemode_kd_Highrise = "Highrise", 
             gamemode_kd_Invasion = "Invasion", 
             gamemode_kd_Karachi = "Karachi")

ctrl_kds_by_map_right <- map_mode_kd_gt_fn(29, 56, "Control", "Yes", 5, 60) %>%
  cols_merge(columns = c(5, 8), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 9), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 10), pattern = "{1} ({2})") %>%
  cols_label(gamemode_kd_Highrise = "Highrise", 
             gamemode_kd_Invasion = "Invasion", 
             gamemode_kd_Karachi = "Karachi")

gt_two_column_layout(
  tables = list(ctrl_kds_by_map_left, ctrl_kds_by_map_right), 
  output = "save", 
  filename = "18_CTRL_KDs_by_Map.png", 
  path = figPath, 
  vwidth = 1860, 
  vheight = 1600, 
  zoom = 3
)

# 19 & 20 Overall K/D Leaders & Red Carpet -------------------------------------

kdTbl <- kd_gt_fn(1, 10, "Overall", overallkd, "Yes")
gtsave(kdTbl, "19_Overall_KD_Leaders.png", path = figPath)

poor_KD_Tbl <- kd_gt_fn(39, 48, "Overall", overallkd, "No")
gtsave(poor_KD_Tbl, "20_Overall_KD_Red_Carpet.png", path = figPath)
  
# 21 - 26 K/D Leaders & Red Carpet by Gamemode ---------------------------------

hp_kd_Tbl <- kd_gt_fn(1, 10, "Hardpoint", hpkd, "No")
gtsave(hp_kd_Tbl, "21_HP_KD_Leaders.png", path = figPath)

hp_poor_KD_Tbl <- kd_gt_fn(39, 48, "Hardpoint", hpkd, "No")
gtsave(hp_poor_KD_Tbl, "22_HP_KD_Red_Carpet.png", path = figPath)

snd_kd_Tbl <- kd_gt_fn(1, 10, "Search & Destroy", sndkd, "No")
gtsave(snd_kd_Tbl, "23_SnD_KD_Leaders.png", path = figPath)

snd_poor_KD_Tbl <- kd_gt_fn(39, 48, "Search & Destroy", sndkd, "No")
gtsave(snd_poor_KD_Tbl, "24_SnD_KD_Red_Carpet.png", path = figPath)

ctrl_kd_Tbl <- kd_gt_fn(1, 10, "Control", ctrlkd, "No")
gtsave(ctrl_kd_Tbl, "25_CTRL_KD_Leaders.png", path = figPath)

ctrl_poor_KD_Tbl <- kd_gt_fn(39, 48, "Control", ctrlkd, "No")
gtsave(ctrl_poor_KD_Tbl, "26_CTRL_KD_Red_Carpet.png", path = figPath)

# 27 Avg Kills per Maps 1 - 3 & Gamemode ---------------------------------------

all_avg_kills_Tbl_left <- big_avg_kills_gt_fn(1, 28, "Yes", 40, 5)
all_avg_kills_Tbl_right <- big_avg_kills_gt_fn(29, 56, "Yes", 5, 40)
all_avg_kills_Tbl <- 
  gt_two_column_layout(
    tables = list(all_avg_kills_Tbl_left, all_avg_kills_Tbl_right)
  )

gt_two_column_layout(
  tables = list(all_avg_kills_Tbl_left, all_avg_kills_Tbl_right), 
  output = "save", 
  filename = "27_Avg_Kills_per_Series_and_Gamemode.png", 
  path = figPath, 
  vwidth = 1800, 
  vheight = 1260, 
  zoom = 3
)

# 28 & 29 Avg Kills per Maps 1 - 3  --------------------------------------------

series_avg_kill_Tbl_left <- series_avg_kills_gt_fn(1, 28, "Yes", 40, 5)
series_avg_kill_Tbl_right <- series_avg_kills_gt_fn(29, 56, "Yes", 5, 40)
big_series_avg_kills_Tbl <- 
  gt_two_column_layout(
    tables = list(series_avg_kill_Tbl_left, series_avg_kill_Tbl_right)
  )

gt_two_column_layout(
  tables = list(series_avg_kill_Tbl_left, series_avg_kill_Tbl_right), 
  output = "save", 
  filename = "28_Avg_Kills_per_Maps_1_thru_3.png", 
  path = figPath, 
  vwidth = 1360, 
  vheight = 1200, 
  zoom = 4
)

series_avg_kill_by_wl_left <- avg_kills_by_series_res_gt_fn(1, 28, "Yes", 60, 5)
series_avg_kill_by_wl_right <- avg_kills_by_series_res_gt_fn(29, 56, "Yes", 5, 60)

gt_two_column_layout(
  tables = list(series_avg_kill_by_wl_left, series_avg_kill_by_wl_right), 
  output = "save", 
  filename = "29_Avg_Kills_per_Maps_1_thru_3_by_Series_Result.png", 
  path = figPath, 
  vwidth = 1600, 
  vheight = 1260, 
  zoom = 4
)

# 30 & 31 Hardpoint Avg Kills --------------------------------------------------

avg_kills_hp_Tbl_left <- avg_kills_gt_fn(1, 28, "Hardpoint", "Yes", 80, 5)
avg_kills_hp_Tbl_right <- avg_kills_gt_fn(29, 56, "Hardpoint", "Yes", 5, 80)
big_avg_kills_hp_Tbl <- 
  gt_two_column_layout(
    tables = list(avg_kills_hp_Tbl_left, avg_kills_hp_Tbl_right)
  )

gt_two_column_layout(
  tables = list(avg_kills_hp_Tbl_left, avg_kills_hp_Tbl_right), 
  output = "save", 
  filename = "30_Avg_Kills_per_HP.png", 
  path = figPath, 
  vwidth = 1400, 
  vheight = 1000, 
  zoom = 4
)

avg_kills_hp_wl_left <- avg_kills_wl_gt_fn(1, 28, "Hardpoint", "Yes", 60, 5)
avg_kills_hp_wl_right <- avg_kills_wl_gt_fn(29, 56, "Hardpoint", "Yes", 5, 60)

gt_two_column_layout(
  tables = list(avg_kills_hp_wl_left, avg_kills_hp_wl_right), 
  output = "save", 
  filename = "31_Avg_Kills_per_HP_by_WL.png", 
  path = figPath, 
  vwidth = 1600, 
  vheight = 1200, 
  zoom = 4
)

# 32 & 33 SnD Avg Kills --------------------------------------------------------

avg_kills_snd_Tbl_left <- avg_kills_gt_fn(1, 28, "Search & Destroy", "Yes", 80, 5)
avg_kills_snd_Tbl_right <- avg_kills_gt_fn(29, 56, "Search & Destroy", "Yes", 5, 80)
big_avg_kills_snd_Tbl <- 
  gt_two_column_layout(
    tables = list(avg_kills_snd_Tbl_left, avg_kills_snd_Tbl_right)
  )

gt_two_column_layout(
  tables = list(avg_kills_snd_Tbl_left, avg_kills_snd_Tbl_right), 
  output = "save", 
  filename = "32_Avg_Kills_per_SnD.png", 
  path = figPath, 
  vwidth = 1400, 
  vheight = 1000, 
  zoom = 4
)

avg_kills_snd_wl_left <- avg_kills_wl_gt_fn(1, 28, "Search & Destroy", "Yes", 60, 5)
avg_kills_snd_wl_right <- avg_kills_wl_gt_fn(29, 56, "Search & Destroy", "Yes", 5, 60)

gt_two_column_layout(
  tables = list(avg_kills_snd_wl_left, avg_kills_snd_wl_right), 
  output = "save", 
  filename = "33_Avg_Kills_per_SnD_by_WL.png", 
  path = figPath, 
  vwidth = 1600, 
  vheight = 1200, 
  zoom = 4
)

# 34 & 35 Control Avg Kills ----------------------------------------------------

avg_kills_ctrl_Tbl_left <- avg_kills_gt_fn(1, 28, "Control", "Yes", 80, 5)
avg_kills_ctrl_Tbl_right <- avg_kills_gt_fn(29, 56, "Control", "Yes", 5, 80)
big_avg_kills_ctrl_Tbl <- 
  gt_two_column_layout(
    tables = list(avg_kills_ctrl_Tbl_left, avg_kills_ctrl_Tbl_right)
  )

gt_two_column_layout(
  tables = list(avg_kills_ctrl_Tbl_left, avg_kills_ctrl_Tbl_right), 
  output = "save", 
  filename = "34_Avg_Kills_per_CTRL.png", 
  path = figPath, 
  vwidth = 1400, 
  vheight = 1000, 
  zoom = 4
)

avg_kills_ctrl_wl_left <- avg_kills_wl_gt_fn(1, 28, "Control", "Yes", 60, 5)
avg_kills_ctrl_wl_right <- avg_kills_wl_gt_fn(29, 56, "Control", "Yes", 5, 60)

gt_two_column_layout(
  tables = list(avg_kills_ctrl_wl_left, avg_kills_ctrl_wl_right), 
  output = "save", 
  filename = "35_Avg_Kills_per_CTRL_by_WL.png", 
  path = figPath, 
  vwidth = 1600, 
  vheight = 1200, 
  zoom = 4
)

# 36 Avg Kills per Hardoint by Map ---------------------------------------------

avg_kills_hp_by_map_left <- 
  map_mode_avg_kills_gt_fn(1, 28, "Hardpoint", hpAvgKills, "Yes", 60, 5) %>%
  cols_merge(columns = c(5, 10), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 11), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 12), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(8, 13), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(9, 14), pattern = "{1} ({2})") %>%
  cols_label(mean_kills_Invasion = "Invasion", 
             mean_kills_Karachi = "Karachi", 
             mean_kills_Rio = "Rio", 
             mean_kills_Skidrow = "Skidrow", 
             "mean_kills_Sub Base" = "Sub Base")

avg_kills_hp_by_map_right <- 
  map_mode_avg_kills_gt_fn(29, 56, "Hardpoint", hpAvgKills, "Yes", 5, 60) %>%
  cols_merge(columns = c(5, 10), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 11), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 12), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(8, 13), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(9, 14), pattern = "{1} ({2})") %>%
  cols_label(mean_kills_Invasion = "Invasion", 
             mean_kills_Karachi = "Karachi", 
             mean_kills_Rio = "Rio", 
             mean_kills_Skidrow = "Skidrow", 
             "mean_kills_Sub Base" = "Sub Base")

gt_two_column_layout(
  tables = list(avg_kills_hp_by_map_left, avg_kills_hp_by_map_right), 
  output = "save", 
  filename = "36_Avg_Kills_per_HP_by_Map.png", 
  path = figPath, 
  vwidth = 2260, 
  vheight = 1800, 
  zoom = 4
)

# 37 Avg Kills per SnD by Map --------------------------------------------------

avg_kills_snd_by_map_left <- 
  map_mode_avg_kills_gt_fn(1, 28, "Search & Destroy", sndAvgKills, "Yes", 60, 5) %>%
  cols_merge(columns = c(5, 10), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 11), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 12), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(8, 13), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(9, 14), pattern = "{1} ({2})") %>%
  cols_label(mean_kills_Highrise = "Highrise", 
             mean_kills_Invasion = "Invasion", 
             mean_kills_Karachi = "Karachi", 
             mean_kills_Rio = "Rio", 
             mean_kills_Terminal = "Terminal")

avg_kills_snd_by_map_right <- 
  map_mode_avg_kills_gt_fn(29, 56, "Search & Destroy", sndAvgKills, "Yes", 10, 10) %>%
  cols_merge(columns = c(5, 10), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 11), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 12), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(8, 13), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(9, 14), pattern = "{1} ({2})") %>%
  cols_label(mean_kills_Highrise = "Highrise", 
             mean_kills_Invasion = "Invasion", 
             mean_kills_Karachi = "Karachi", 
             mean_kills_Rio = "Rio", 
             mean_kills_Terminal = "Terminal")

gt_two_column_layout(
  tables = list(avg_kills_snd_by_map_left, avg_kills_snd_by_map_right), 
  output = "save", 
  filename = "37_Avg_Kills_per_SnD_by_Map.png", 
  path = figPath, 
  vwidth = 2260, 
  vheight = 1800, 
  zoom = 4
)

# 38 Avg Kills per Control by Map ----------------------------------------------

avg_kills_ctrl_by_map_left <- 
  map_mode_avg_kills_gt_fn(1, 28, "Control", ctrlAvgKills, "Yes", 60, 5) %>%
  cols_merge(columns = c(5, 8), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 9), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 10), pattern = "{1} ({2})") %>%
  cols_label(mean_kills_Highrise = "Highrise", 
             mean_kills_Invasion = "Invasion", 
             mean_kills_Karachi = "Karachi")

avg_kills_ctrl_by_map_right <- 
  map_mode_avg_kills_gt_fn(29, 56, "Control", ctrlAvgKills, "Yes", 60, 5) %>%
  cols_merge(columns = c(5, 8), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(6, 9), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(7, 10), pattern = "{1} ({2})") %>%
  cols_label(mean_kills_Highrise = "Highrise", 
             mean_kills_Invasion = "Invasion", 
             mean_kills_Karachi = "Karachi")

gt_two_column_layout(
  tables = list(avg_kills_ctrl_by_map_left, avg_kills_ctrl_by_map_right), 
  output = "save", 
  filename = "38_Avg_Kills_per_CTRL_by_Map.png", 
  path = figPath, 
  vwidth = 1800, 
  vheight = 1600, 
  zoom = 4
)

# 39 & 40 Avg Kills per Maps 1 - 3 Leaders & Red Carpet ------------------------

seriesKillsLeadersTbl <- series_avg_kills_gt_fn(1, 10, "No", 60, 5)
gtsave(seriesKillsLeadersTbl, "39_Avg_Kills_per_Series_Leaders.png", path = figPath)

seriesKillsPoorTbl <- series_avg_kills_gt_fn(39, 48, "No", 60, 5)
gtsave(seriesKillsPoorTbl, "40_Avg_Kills_per_Series_Red_Carpet.png", path = figPath)

# 41 - 46 Avg Kills Leaders & Red Carpet by Gamemode ---------------------------

avg_kill_leaders_hp_Tbl <- avg_kills_gt_fn(1, 10, "Hardpoint", "No")
gtsave(avg_kill_leaders_hp_Tbl, "41_Avg_Kills_per_HP_Leaders.png", path = figPath)

avg_kills_red_carpet_hp_Tbl <- avg_kills_gt_fn(39, 48, "Hardpoint", "No")
gtsave(avg_kills_red_carpet_hp_Tbl, "42_Avg_Kills_per_HP_Red_Carpet.png", path = figPath)

avg_kill_leaders_snd_Tbl <- avg_kills_gt_fn(1, 10, "Search & Destroy", "No")
gtsave(avg_kill_leaders_snd_Tbl, "43_Avg_Kills_per_SnD_Leaders.png", path = figPath)

avg_kills_per_red_carpet_snd_Tbl <- avg_kills_gt_fn(39, 48, "Search & Destroy", "No")
gtsave(avg_kills_per_red_carpet_snd_Tbl, "44_Avg_Kills_per_SnD_Red_Carpet.png", path = figPath)

avg_kill_leaders_ctrl_Tbl <- avg_kills_gt_fn(1, 10, "Control", "No")
gtsave(avg_kill_leaders_ctrl_Tbl, "45_Avg_Kills_per_CTRL_Leaders.png", path = figPath)

avg_kills_red_carpet_ctrl_Tbl <- avg_kills_gt_fn(39, 48, "Control", "No")
gtsave(avg_kills_red_carpet_ctrl_Tbl, "46_Avg_Kills_per_CTRL_Red_Carpet.png", path = figPath)

# 47 & 48 K/D vs Total Points Hardpoint ----------------------------------------

kd_vs_points_hp_plot <- 
  cdlDF %>% filter(gamemode == "Hardpoint") %>%
  ggplot(mapping = aes(x = total_score, y = kd)) +
  geom_point() + 
  geom_smooth() +
  labs(title = "CDL K/D vs. Total Points: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Total Points") + ylab("K/D") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("47_KD_vs_Points_HP.png", scale = 1.25,
       kd_vs_points_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

kd_vs_points_by_wl_hp_plot <- 
  cdlDF %>% filter(gamemode == "Hardpoint") %>%
  ggplot(mapping = aes(x = total_score, y = kd, color = map_wl)) +
  geom_point() + 
  geom_smooth() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "CDL K/D vs. Total Points, colored by Win/Loss: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"),
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint",
       color = "Map W/L") +
  xlab("Total Points") + ylab("K/D") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b"), 
    legend.title = element_text(size = 10)
  )

ggsave("48_KD_vs_Points_by_WL_HP.png", scale = 1.25,
       kd_vs_points_by_wl_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 49 & 50 K/D vs Total Rounds SnD ----------------------------------------------

kd_vs_rounds_snd_plot <- 
  cdlDF %>% filter(gamemode == "Search & Destroy") %>%
  ggplot(mapping = aes(x = total_score, y = kd)) +
  geom_point() + 
  geom_smooth() +
  labs(title = "CDL K/D vs. Total Rounds: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Total Rounds") + ylab("K/D") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("49_KD_vs_Rounds_SnD.png", scale = 1.25,
       kd_vs_rounds_snd_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

kd_vs_rounds_by_wl_snd_plot <- 
  cdlDF %>% filter(gamemode == "Search & Destroy") %>%
  ggplot(mapping = aes(x = total_score, y = kd, color = map_wl)) +
  geom_point() + 
  geom_smooth() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "CDL K/D vs. Total Rounds, colored by Win/Loss: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint",
       color = "Map W/L") +
  xlab("Total Rounds") + ylab("K/D") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b"), 
    legend.title = element_text(size = 10)
  )

ggsave("50_KD_vs_Rounds_by_WL_SnD.png", scale = 1.25,
       kd_vs_rounds_by_wl_snd_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 51 & 52 K/D vs Total Rounds Control ------------------------------------------

kd_vs_rounds_ctrl_plot <- 
  cdlDF %>% filter(gamemode == "Control") %>%
  ggplot(mapping = aes(x = total_score, y = kd)) +
  geom_point() + 
  geom_smooth() +
  labs(title = "CDL K/D vs. Total Rounds: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Total Rounds") + ylab("K/D") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("51_KD_vs_Rounds_CTRL.png", scale = 1.25,
       kd_vs_rounds_ctrl_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

kd_vs_rounds_by_wl_ctrl_plot <- 
  cdlDF %>% filter(gamemode == "Control") %>%
  ggplot(mapping = aes(x = total_score, y = kd, color = map_wl)) +
  geom_point() + 
  geom_smooth()  +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "CDL K/D vs. Total Rounds, colored by Win/Loss: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint",
       color = "Map W/L") +
  xlab("Total Rounds") + ylab("K/D") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b"), 
    legend.title = element_text(size = 10)
  )

ggsave("52_KD_vs_Rounds_by_WL_CTRL.png", scale = 1.25,
       kd_vs_rounds_by_wl_ctrl_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 53 K/D vs Score Diff Hardpoint -----------------------------------------------

kd_vs_score_diff_hp_plot <- 
  cdlDF %>% filter(gamemode == "Hardpoint") %>%
  ggplot(mapping = aes(x = score_diff, y = kd)) +
  geom_point() +
  geom_vline(xintercept = 0, color = "orange", 
             linetype = "dashed", linewidth = 0.75) +
  geom_smooth() +
  scale_x_continuous(breaks = c(-150, -75, 0, 75, 150)) +
  labs(title = "CDL K/D vs. Score Differential: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Score Differential") + ylab("K/D") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("53_KD_vs_Score_Diff_HP.png", scale = 1.25,
       kd_vs_score_diff_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 54 K/D vs Score Diff SnD -----------------------------------------------------

kd_vs_score_diff_snd_plot <- 
  cdlDF %>% filter(gamemode == "Search & Destroy") %>%
  ggplot(mapping = aes(x = score_diff, y = kd)) +
  geom_point() + 
  geom_vline(xintercept = 0, color = "orange", 
             linetype = "dashed", linewidth = 0.75) +
  geom_smooth() +
  labs(title = "CDL K/D vs. Score Differential: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Score Differential") + ylab("K/D") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("54_KD_vs_Score_Diff_SnD.png", scale = 1.25,
       kd_vs_score_diff_snd_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 55 K/D vs Score Diff Control -------------------------------------------------

kd_vs_score_diff_ctrl_plot <- 
  cdlDF %>% filter(gamemode == "Control") %>%
  ggplot(mapping = aes(x = score_diff, y = kd)) +
  geom_point() +
  geom_vline(xintercept = 0, color = "orange", 
             linetype = "dashed", linewidth = 0.75) +
  geom_smooth() +
  labs(title = "CDL K/D vs. Score Differential: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Score Differential") + ylab("K/D") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("55_KD_vs_Score_Diff_CTRL.png", scale = 1.25,
       kd_vs_score_diff_ctrl_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 56 & 57 Kills vs Total Points Hardpoint --------------------------------------

kills_vs_points_hp_plot <- 
  cdlDF %>% filter(gamemode == "Hardpoint") %>%
  ggplot(mapping = aes(x = total_score, y = kills)) +
  geom_point() + 
  geom_smooth() +
  labs(title = "CDL Kills vs. Total Points: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Total Points") + ylab("Kills") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("56_Kills_vs_Points_HP.png", scale = 1.25,
       kills_vs_points_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

kills_vs_points_by_wl_hp_plot <- 
  cdlDF %>% filter(gamemode == "Hardpoint") %>%
  ggplot(mapping = aes(x = total_score, y = kills, color = map_wl)) +
  geom_point() + 
  geom_smooth() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "CDL Kills vs. Total Points, colored by Win/Loss: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"),
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint",
       color = "Map W/L") +
  xlab("Total Points") + ylab("Kills") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b"), 
    legend.title = element_text(size = 10)
  )

ggsave("57_Kills_vs_Points_by_WL_HP.png", scale = 1.25,
       kills_vs_points_by_wl_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 58 & 59 Kills vs Total Rounds SnD --------------------------------------------

kills_vs_rounds_snd_plot <- 
  cdlDF %>% filter(gamemode == "Search & Destroy") %>%
  ggplot(mapping = aes(x = total_score, y = kills)) +
  geom_point() + 
  geom_smooth() +
  labs(title = "CDL Kills vs. Total Rounds: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Total Rounds") + ylab("Kills") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("58_Kills_vs_Rounds_SnD.png", scale = 1.25,
       kills_vs_rounds_snd_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

kills_vs_rounds_by_wl_snd_plot <- 
  cdlDF %>% filter(gamemode == "Search & Destroy") %>%
  ggplot(mapping = aes(x = total_score, y = kills, color = map_wl)) +
  geom_point() + 
  geom_smooth() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "CDL Kills vs. Total Rounds, colored by Win/Loss: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint",
       color = "Map W/L") +
  xlab("Total Rounds") + ylab("Kills") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b"), 
    legend.title = element_text(size = 10)
  )

ggsave("59_Kills_vs_Rounds_by_WL_SnD.png", scale = 1.25,
       kills_vs_rounds_by_wl_snd_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 60 & 61 Kills vs Total Rounds Control ----------------------------------------

kills_vs_rounds_ctrl_plot <- 
  cdlDF %>% filter(gamemode == "Control") %>%
  ggplot(mapping = aes(x = total_score, y = kills)) +
  geom_point() + 
  geom_smooth() +
  labs(title = "CDL Kills vs. Total Rounds: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Total Rounds") + ylab("Kills") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("60_Kills_vs_Rounds_CTRL.png", scale = 1.25,
       kills_vs_rounds_ctrl_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

kills_vs_rounds_by_wl_ctrl_plot <- 
  cdlDF %>% filter(gamemode == "Control") %>%
  ggplot(mapping = aes(x = total_score, y = kills, color = map_wl)) +
  geom_point() + 
  geom_smooth()  +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "CDL Kills vs. Total Rounds, colored by Win/Loss: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint",
       color = "Map W/L") +
  xlab("Total Rounds") + ylab("Kills") + 
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b"), 
    legend.title = element_text(size = 10)
  )

ggsave("61_Kills_vs_Rounds_by_WL_CTRL.png", scale = 1.25,
       kills_vs_rounds_by_wl_ctrl_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 62 Kills vs Score Diff Hardpoint ---------------------------------------------

kills_vs_score_diff_hp_plot <- 
  cdlDF %>% filter(gamemode == "Hardpoint") %>%
  ggplot(mapping = aes(x = score_diff, y = kills)) +
  geom_point() +
  geom_vline(xintercept = 0, color = "orange", 
             linetype = "dashed", linewidth = 0.75) +
  geom_smooth() +
  scale_x_continuous(breaks = c(-150, -75, 0, 75, 150)) +
  labs(title = "CDL Kills vs. Score Differential: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Score Differential") + ylab("Kills") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("62_Kills_vs_Score_Diff_HP.png", scale = 1.25,
       kills_vs_score_diff_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 63 Kills vs Score Diff SnD ---------------------------------------------------

kills_vs_score_diff_snd_plot <- 
  cdlDF %>% filter(gamemode == "Search & Destroy") %>%
  ggplot(mapping = aes(x = score_diff, y = kills)) +
  geom_point() + 
  geom_vline(xintercept = 0, color = "orange", 
             linetype = "dashed", linewidth = 0.75) +
  geom_smooth() +
  labs(title = "CDL Kills vs. Score Differential: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Score Differential") + ylab("Kills") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("63_Kills_vs_Score_Diff_SnD.png", scale = 1.25,
       kills_vs_score_diff_snd_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 64 Kills vs Score Diff Control -----------------------------------------------

kills_vs_score_diff_ctrl_plot <- 
  cdlDF %>% filter(gamemode == "Control") %>%
  ggplot(mapping = aes(x = score_diff, y = kills)) +
  geom_point() +
  geom_vline(xintercept = 0, color = "orange", 
             linetype = "dashed", linewidth = 0.75) +
  geom_smooth() +
  labs(title = "CDL Kills vs. Score Differential: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Score Differential") + ylab("Kills") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("64_Kills_vs_Score_Diff_CTRL.png", scale = 1.25,
       kills_vs_score_diff_ctrl_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 65 Hardpoint Score Diff by Team Boxplot --------------------------------------

score_diff_by_team_hp <- 
  cdlDF %>%
  select(match_id, team_icon, gamemode, map_name, score_diff) %>%
  distinct() %>%
  mutate(corrected_score_diff = as.numeric(score_diff)) %>%
  filter(gamemode == "Hardpoint") %>%
  ggplot(aes(x = fct_reorder(team_icon, corrected_score_diff, median), 
             y = corrected_score_diff)) +
  geom_boxplot(mapping = aes(color = team_icon), alpha = 0.5, 
               outlier.alpha = 0) + 
  # geom_jitter(width = 0.075, height = 0.1, alpha = 0.75) + 
  scale_color_manual(
    values = team_color, breaks = team_icon
  ) +
  labs(title = "CDL Hardpoint Score Differentials by Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Score Differential") + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("65_HP_Score_Diffs_by_Team.png", scale = 1.25,
       score_diff_by_team_hp, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 66 SnD Score Diff by Team Boxplot --------------------------------------------

score_diff_by_team_snd <- 
  cdlDF %>%
  select(match_id, team_icon, gamemode, map_name, score_diff) %>%
  distinct() %>%
  mutate(corrected_score_diff = as.numeric(score_diff)) %>%
  filter(gamemode == "Search & Destroy") %>%
  ggplot(aes(x = fct_reorder(team_icon, corrected_score_diff, median), 
             y = corrected_score_diff)) +
  geom_boxplot(mapping = aes(color = team_icon), alpha = 0.5, 
               outlier.alpha = 0) + 
  # geom_jitter(width = 0.075, height = 0.1, alpha = 0.75) + 
  scale_color_manual(
    values = team_color, breaks = team_icon
  ) +
  labs(title = "CDL Search & Destroy Score Differentials by Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Score Differential") + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("66_SnD_Score_Diffs_by_Team.png", scale = 1.25,
       score_diff_by_team_snd, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 67 Control Score Diff by Team Boxplot ----------------------------------------

score_diff_by_team_ctrl <- 
  cdlDF %>%
  select(match_id, team_icon, gamemode, map_name, score_diff) %>%
  distinct() %>%
  mutate(corrected_score_diff = as.numeric(score_diff)) %>%
  filter(gamemode == "Control") %>%
  ggplot(aes(x = fct_reorder(team_icon, corrected_score_diff, median), 
             y = corrected_score_diff)) +
  geom_boxplot(mapping = aes(color = team_icon), alpha = 0.5, 
               outlier.alpha = 0) + 
  # geom_jitter(width = 0.075, height = 0.1, alpha = 0.75) + 
  scale_color_manual(
    values = team_color, breaks = team_icon
  ) +
  labs(title = "CDL Control Score Differentials by Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Score Differential") + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("67_CTRL_Score_Diffs_by_Team.png", scale = 1.25,
       score_diff_by_team_ctrl, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 68 Team Avg Score Diff by Gamemode -------------------------------------------

team_avg_score_diffs_Tbl <- 
  cdlDF %>%
  filter(!(gamemode == "Hardpoint" & map_name == "Terminal") & 
           !(gamemode == "Search & Destroy" & map_name == "Skidrow"))%>%
  select(match_id, team, gamemode, map_name, score_diff) %>%
  distinct() %>%
  group_by(team, gamemode) %>%
  summarise(avg_score_diff = as.numeric(mean(score_diff))) %>%
  pivot_wider(names_from = gamemode, 
              values_from = avg_score_diff) %>% ungroup() %>%
  select(team, Hardpoint, "Search & Destroy", Control) %>%
  gt() %>%
  opt_align_table_header("center") %>%
  cols_align("center") %>%
  tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
  opt_row_striping() %>%
  tab_header(title = "CDL Average Score Differential by Gamemode",
             subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
  gt_theme_espn() %>% 
  cols_width(team ~ px(200), Control ~ px(100), Hardpoint ~ px(100), 
             "Search & Destroy" ~ px(100)) %>%
  data_color(
    columns = Control,
    fn = scales::col_numeric(
      palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                   "#bdd7e7", "#2171b5", "#2171b5"),
      domain = c(-1, 1), 
      reverse = TRUE)) %>%
  data_color(
    columns = Hardpoint, 
    fn = scales::col_numeric(
      palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                   "#bdd7e7", "#2171b5", "#2171b5"),
      domain = c(-60, 70), 
      reverse = TRUE)) %>%
  data_color(
    columns = ends_with("Destroy"), 
    fn = scales::col_numeric(
      palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                   "#bdd7e7", "#2171b5", "#2171b5"),
      domain = c(-1, 1), 
      reverse = TRUE))
  
gtsave(team_avg_score_diffs_Tbl, "68_Team_Avg_Score_Diffs.png", 
       path = figPath)

# 69 Team Avg Score Diff by Map Hardpoint --------------------------------------

team_avg_score_diffs_hp <- 
  cdlDF %>%
  filter(gamemode == "Hardpoint" & map_name != "Terminal") %>%
  select(match_id, team, map_name, score_diff) %>%
  distinct() %>%
  group_by(team, map_name) %>%
  summarise(avg_score_diff = as.numeric(mean(score_diff))) %>%
  ggplot(aes(x = map_name, y = avg_score_diff, fill = team)) +
  geom_col() +
  facet_wrap(~team) +
  scale_fill_manual(
    values = team_color, breaks = team
  ) +
  labs(title = "CDL Team Average Hardpoint Score Differentials by Map", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Map") + ylab("Avg Score Differential") + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("69_Team_Avg_Score_Diffs_HP.png", scale = 1.25,
       team_avg_score_diffs_hp, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 70 Team Avg Score Diff by Map SnD --------------------------------------------

team_avg_score_diffs_snd <- 
  cdlDF %>%
  filter(gamemode == "Search & Destroy" & map_name != "Skidrow") %>%
  select(match_id, team, map_name, score_diff) %>%
  distinct() %>%
  group_by(team, map_name) %>%
  summarise(avg_score_diff = as.numeric(mean(score_diff))) %>%
  ggplot(aes(x = map_name, y = avg_score_diff, fill = team)) +
  geom_col() +
  facet_wrap(~team) +
  scale_fill_manual(
    values = team_color, breaks = team
  ) +
  labs(title = "CDL Team Average Search & Destroy Score Differentials by Map", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Map") + ylab("Avg Score Differential") + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("70_Team_Avg_Score_Diffs_SnD.png", scale = 1.25,
       team_avg_score_diffs_snd, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 71 Team Avg Score Diff by Map Control ----------------------------------------

team_avg_score_diffs_ctrl <- 
  cdlDF %>%
  filter(gamemode == "Control") %>%
  select(match_id, team, map_name, score_diff) %>%
  distinct() %>%
  group_by(team, map_name) %>%
  summarise(avg_score_diff = as.numeric(mean(score_diff))) %>%
  ggplot(aes(x = map_name, y = avg_score_diff, fill = team)) +
  geom_col() +
  facet_wrap(~team) +
  scale_fill_manual(
    values = team_color, breaks = team
  ) +
  labs(title = "CDL Team Average Control Score Differentials by Map", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Map") + ylab("Avg Score Differential") + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("71_Team_Avg_Score_Diffs_CTRL.png", scale = 1.25,
       team_avg_score_diffs_ctrl, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 72 - 74 Points Allowed in Hardpoint Win --------------------------------------

pts_allowed_by_team_hp_boxplot <- 
  cdlDF %>%
  filter(map_wl == "W" & gamemode == "Hardpoint") %>%
  select(match_id, team_icon, gamemode, map_name, opp_score) %>%
  distinct() %>%
  ggplot(aes(x = fct_reorder(team_icon, opp_score, median), 
             y = opp_score)) +
  geom_boxplot(mapping = aes(color = team_icon), alpha = 0.5, 
               outlier.alpha = 0) + 
  # geom_jitter(width = 0.075, height = 0.1, alpha = 0.75) + 
  scale_color_manual(
    values = team_color, breaks = team_icon
  ) +
  labs(title = "CDL Hardpoint Points Allowed in Win by Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Points Allowed") + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("72_Pts_Allowed_in_HP_Win_by_Team.png", scale = 1.25,
       pts_allowed_by_team_hp_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_allowed_hp_plot <- 
  pts_by_team_and_mode_df %>%
  select(-avg_pts_for) %>% filter(gamemode == "Hardpoint") %>%
  ggplot(aes(x = reorder(team_icon, avg_pts_allowed), 
             y = avg_pts_allowed, fill = team_icon)) +
  geom_col() +
  geom_text(aes(label = avg_pts_allowed), vjust = -0.45, color = "#3b3b3b") +
  labs(title = "CDL Average Points Allowed in Hardpoint Win", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Avg Pts Allowed") + 
  ylim(0, 250) +
  scale_fill_manual(
    values = team_color, breaks = team_icon
  ) +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("73_Avg_Pts_Allowed_in_HP_Win.png", scale = 1.25,
       avg_pts_allowed_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_for_hp_plot <- 
  pts_by_team_and_mode_df %>%
  select(-avg_pts_allowed) %>% filter(gamemode == "Hardpoint") %>%
  ggplot(aes(x = reorder(team_icon, avg_pts_for), 
             y = avg_pts_for, fill = team_icon)) +
  geom_col() +
  geom_text(aes(label = avg_pts_for), vjust = -0.45, color = "#3b3b3b") +
  labs(title = "CDL Average Points For in Hardpoint Loss", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Avg Pts For") + 
  ylim(0, 250) +
  scale_fill_manual(
    values = team_color, breaks = team_icon
  ) +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("83_Avg_Pts_For_in_HP_Loss.png", scale = 1.25,
       avg_pts_for_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_allowed_by_map_hp_plot <- 
  cdlDF %>% filter(map_wl == "W" & gamemode == "Hardpoint" &
                     map_name != "Terminal") %>%
  select(match_id, team, map_name, opp_score) %>%
  distinct() %>%
  group_by(team, map_name) %>%
  summarise(avg_pts_allowed = mean(opp_score)) %>%
  ggplot(aes(x = map_name, y = avg_pts_allowed, fill = team)) +
  geom_col() +
  facet_wrap(~team) +
  # scale_y_continuous(breaks = c(0, 1, 2)) +
  scale_fill_manual(
    values = team_color, breaks = team
  ) +
  labs(title = "CDL Average Points Allowed in Hardpoint Win by Map & Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Map") + ylab("Avg Points Allowed") + 
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("74_Avg_Pts_Allowed_by_Map_in_HP_Win.png", scale = 1.25,
       avg_pts_allowed_by_map_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 75 - 77 Rounds Lost in SnD Win -----------------------------------------------

pts_allowed_by_team_snd_boxplot <-
  cdlDF %>%
  filter(map_wl == "W" & gamemode == "Search & Destroy") %>%
  select(match_id, team_icon, gamemode, map_name, opp_score) %>%
  distinct() %>%
  ggplot(aes(x = fct_reorder(team_icon, opp_score, median), 
             y = opp_score)) +
  geom_boxplot(mapping = aes(color = team_icon), alpha = 0.5, 
               outlier.alpha = 0) + 
  # geom_jitter(width = 0.075, height = 0.1, alpha = 0.75) + 
  scale_color_manual(
    values = team_color, breaks = team_icon
  ) +
  labs(title = "CDL Search & Destroy Rounds Lost in Win by Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Rounds Lost") + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("75_Rds_Allowed_in_SnD_Win_by_Team.png", scale = 1.25,
       pts_allowed_by_team_snd_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_allowed_snd_plot <- 
  pts_by_team_and_mode_df %>%
  select(-avg_pts_for) %>% filter(gamemode == "Search & Destroy") %>%
  ggplot(aes(x = reorder(team_icon, avg_pts_allowed), 
             y = avg_pts_allowed, fill = team_icon)) +
  geom_col() +
  geom_text(aes(label = avg_pts_allowed), vjust = -0.45, color = "#3b3b3b") +
  labs(title = "CDL Average Rounds Lost in Search & Destroy Win", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Avg Rounds Lost") + 
  ylim(0, 6) +
  scale_fill_manual(
    values = team_color, breaks = team_icon
  ) +
  theme(
    legend.position = "none", 
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("76_Avg_Rds_Lost_in_SnD_Win.png", scale = 1.25,
       avg_pts_allowed_snd_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_allowed_by_map_snd_plot <- 
  cdlDF %>% filter(map_wl == "W" & gamemode == "Search & Destroy" &
                     map_name != "Skidrow") %>%
  select(match_id, team, map_name, opp_score) %>%
  distinct() %>%
  group_by(team, map_name) %>%
  summarise(avg_pts_allowed = mean(opp_score)) %>%
  ggplot(aes(x = map_name, y = avg_pts_allowed, fill = team)) +
  geom_col() +
  facet_wrap(~team) +
  # scale_y_continuous(breaks = c(0, 1, 2)) +
  scale_fill_manual(
    values = team_color, breaks = team
  ) +
  labs(title = "CDL Average Rounds Lost in Search & Destroy Win by Map & Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Map") + ylab("Avg Rounds Lost") + 
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("77_Avg_Rds_Lost_by_Map_in_SnD_Win.png", scale = 1.25,
       avg_pts_allowed_by_map_snd_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 78 - 80 Rounds Lost in Control Win -------------------------------------------

pts_allowed_by_team_ctrl_boxplot <-
  cdlDF %>%
  filter(map_wl == "W" & gamemode == "Control") %>%
  select(match_id, team_icon, gamemode, map_name, opp_score) %>%
  distinct() %>%
  ggplot(aes(x = fct_reorder(team_icon, opp_score, median), 
             y = opp_score)) +
  geom_boxplot(mapping = aes(color = team_icon), alpha = 0.5, 
               outlier.alpha = 0) + 
  # geom_jitter(width = 0.075, height = 0.1, alpha = 0.75) + 
  scale_color_manual(
    values = team_color, breaks = team_icon
  ) +
  labs(title = "CDL Control Rounds Lost in Win by Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Rounds Lost") + 
  scale_y_continuous(breaks = c(0, 1, 2), limits = c(0, 2)) + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("78_Rds_Allowed_in_CTRL_Win_by_Team.png", scale = 1.25,
       pts_allowed_by_team_ctrl_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_allowed_ctrl_plot <- 
  pts_by_team_and_mode_df %>%
  select(-avg_pts_for) %>% filter(gamemode == "Control") %>%
  ggplot(aes(x = reorder(team_icon, avg_pts_allowed), 
             y = avg_pts_allowed, fill = team_icon)) +
  geom_col() +
  geom_text(aes(label = avg_pts_allowed), vjust = -0.45, color = "#3b3b3b") +
  labs(title = "CDL Average Rounds Lost in Control Win", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Avg Rounds Lost") + 
  ylim(0, 3) +
  scale_fill_manual(
    values = team_color, breaks = team_icon
  ) +
  theme(
    legend.position = "none", 
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("79_Avg_Rds_Lost_in_CTRL_Win.png", scale = 1.25,
       avg_pts_allowed_ctrl_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_allowed_by_map_ctrl_plot <- 
  cdlDF %>% filter(map_wl == "W" & gamemode == "Control") %>%
  select(match_id, team, map_name, opp_score) %>%
  distinct() %>%
  group_by(team, map_name) %>%
  summarise(avg_pts_allowed = mean(opp_score)) %>%
  ggplot(aes(x = map_name, y = avg_pts_allowed, fill = team)) +
  geom_col() +
  facet_wrap(~team) +
  # scale_y_continuous(breaks = c(0, 1, 2)) +
  scale_fill_manual(
    values = team_color, breaks = team
  ) +
  labs(title = "CDL Average Rounds Lost in Control Win by Map & Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Map") + ylab("Avg Rounds Lost") + 
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("80_Avg_Rds_Lost_by_Map_in_CTRL_Win.png", scale = 1.25,
       avg_pts_allowed_by_map_ctrl_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 81 Avg Points Allowed in Win by Gamemode -------------------------------------

avg_pts_allowed_by_team_and_mode_Tbl <- 
  pts_by_team_and_mode_df %>% select(team, gamemode, avg_pts_allowed) %>%
  pivot_wider(names_from = gamemode, 
              values_from = c(avg_pts_allowed)) %>%
  ungroup() %>%
  select(team, Hardpoint, "Search & Destroy", Control) %>%
  gt() %>%
  opt_align_table_header("center") %>%
  cols_align("center") %>%
  tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
  opt_row_striping() %>%
  tab_header(title = "CDL Avergage Points Allowed in Win by Gamemode",
             subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
  gt_theme_espn() %>% 
  cols_width(team ~ px(200), Control ~ px(100), Hardpoint ~ px(100), 
             "Search & Destroy" ~ px(100)) %>%
  data_color(
    columns = Control,
    fn = scales::col_numeric(
      palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                   "#bdd7e7", "#2171b5", "#2171b5"),
      domain = c(0, 2), 
      reverse = TRUE)) %>%
  data_color(
    columns = Hardpoint, 
    fn = scales::col_numeric(
      palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                   "#bdd7e7", "#2171b5", "#2171b5"),
      domain = c(170, 220), 
      reverse = TRUE)) %>%
  data_color(
    columns = ends_with("Destroy"), 
    fn = scales::col_numeric(
      palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                   "#bdd7e7", "#2171b5", "#2171b5"),
      domain = c(0, 5), 
      reverse = TRUE))

gtsave(avg_pts_allowed_by_team_and_mode_Tbl, "81_Avg_Pts_Allowed_in_Win_by_Gamemode.png", 
       path = figPath)

# 82 - 84 Points For in Hardpoint Loss -----------------------------------------

pts_for_by_team_hp_boxplot <- 
  cdlDF %>%
  filter(map_wl == "L" & gamemode == "Hardpoint") %>%
  select(match_id, team_icon, gamemode, map_name, team_score) %>%
  distinct() %>%
  ggplot(aes(x = fct_reorder(team_icon, team_score, median), 
             y = team_score)) +
  geom_boxplot(mapping = aes(color = team_icon), alpha = 0.5, 
               outlier.alpha = 0) + 
  # geom_jitter(width = 0.075, height = 0.1, alpha = 0.75) + 
  scale_color_manual(
    values = team_color, breaks = team_icon
  ) +
  labs(title = "CDL Hardpoint Points For in Loss by Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Points For") + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("82_Pts_For_in_HP_Loss_by_Team.png", scale = 1.25,
       pts_for_by_team_hp_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_for_hp_plot <- 
  pts_by_team_and_mode_df %>%
  select(-avg_pts_allowed) %>% filter(gamemode == "Hardpoint") %>%
  ggplot(aes(x = reorder(team_icon, avg_pts_for), 
             y = avg_pts_for, fill = team_icon)) +
  geom_col() +
  geom_text(aes(label = avg_pts_for), vjust = -0.45, color = "#3b3b3b") +
  labs(title = "CDL Average Points For in Hardpoint Loss", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Avg Pts For") + 
  ylim(0, 250) +
  scale_fill_manual(
    values = team_color, breaks = team_icon
  ) +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("83_Avg_Pts_For_in_HP_Loss.png", scale = 1.25,
       avg_pts_for_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_for_by_map_hp_plot <- 
  cdlDF %>% filter(map_wl == "L" & gamemode == "Hardpoint" &
                     map_name != "Terminal") %>%
  select(match_id, team, map_name, team_score) %>%
  distinct() %>%
  group_by(team, map_name) %>%
  summarise(avg_pts_for = mean(team_score)) %>%
  ggplot(aes(x = map_name, y = avg_pts_for, fill = team)) +
  geom_col() +
  facet_wrap(~team) +
  # scale_y_continuous(breaks = c(0, 1, 2)) +
  scale_fill_manual(
    values = team_color, breaks = team
  ) +
  labs(title = "CDL Average Points For in Hardpoint Loss by Map & Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Map") + ylab("Avg Points For") + 
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("84_Avg_Pts_For_by_Map_in_HP_Loss.png", scale = 1.25,
       avg_pts_for_by_map_hp_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 85 - 87 Rounds Won in SnD Loss -----------------------------------------------

pts_for_by_team_snd_boxplot <-
  cdlDF %>%
  filter(map_wl == "L" & gamemode == "Search & Destroy") %>%
  select(match_id, team_icon, gamemode, map_name, team_score) %>%
  distinct() %>%
  ggplot(aes(x = fct_reorder(team_icon, team_score, median), 
             y = team_score)) +
  geom_boxplot(mapping = aes(color = team_icon), alpha = 0.5, 
               outlier.alpha = 0) + 
  # geom_jitter(width = 0.075, height = 0.1, alpha = 0.75) + 
  scale_color_manual(
    values = team_color, breaks = team_icon
  ) +
  labs(title = "CDL Search & Destroy Rounds Won in Loss by Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Rounds Won") + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("85_Rds_Won_in_SnD_Loss_by_Team.png", scale = 1.25,
       pts_for_by_team_snd_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_for_snd_plot <- 
  pts_by_team_and_mode_df %>%
  select(-avg_pts_allowed) %>% filter(gamemode == "Search & Destroy") %>%
  ggplot(aes(x = reorder(team_icon, avg_pts_for), 
             y = avg_pts_for, fill = team_icon)) +
  geom_col() +
  geom_text(aes(label = avg_pts_for), vjust = -0.45, color = "#3b3b3b") +
  labs(title = "CDL Average Rounds Won in Search & Destroy Loss", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Avg Pts For") + 
  ylim(0, 6) +
  scale_fill_manual(
    values = team_color, breaks = team_icon
  ) +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("86_Avg_Rds_Won_in_SnD_Loss.png", scale = 1.25,
       avg_pts_for_snd_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_for_by_map_snd_plot <- 
  cdlDF %>% filter(map_wl == "L" & gamemode == "Search & Destroy" &
                     map_name != "Skidrow") %>%
  select(match_id, team, map_name, team_score) %>%
  distinct() %>%
  group_by(team, map_name) %>%
  summarise(avg_pts_for = mean(team_score)) %>%
  ggplot(aes(x = map_name, y = avg_pts_for, fill = team)) +
  geom_col() +
  facet_wrap(~team) +
  # scale_y_continuous(breaks = c(0, 1, 2)) +
  scale_fill_manual(
    values = team_color, breaks = team
  ) +
  labs(title = "CDL Average Rounds Won in Search & Destroy Loss by Map & Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Map") + ylab("Avg Rounds Won") + 
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("87_Avg_Rds_Won_by_Map_in_SnD_Loss.png", scale = 1.25,
       avg_pts_for_by_map_snd_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 88 - 90 Rounds Won in Control Loss -------------------------------------------

pts_for_by_team_ctrl_boxplot <-
  cdlDF %>%
  filter(map_wl == "L" & gamemode == "Control") %>%
  select(match_id, team_icon, gamemode, map_name, team_score) %>%
  distinct() %>%
  ggplot(aes(x = fct_reorder(team_icon, team_score, median), 
             y = team_score)) +
  geom_boxplot(mapping = aes(color = team_icon), alpha = 0.5, 
               outlier.alpha = 0) + 
  # geom_jitter(width = 0.075, height = 0.1, alpha = 0.75) + 
  scale_color_manual(
    values = team_color, breaks = team_icon
  ) +
  labs(title = "CDL Control Rounds Won in Loss by Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Rounds Won") + 
  scale_y_continuous(breaks = c(0, 1, 2), limits = c(0, 2)) + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("88_Rds_Won_in_CTRL_Loss_by_Team.png", scale = 1.25,
       pts_for_by_team_ctrl_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_for_ctrl_plot <- 
  pts_by_team_and_mode_df %>%
  select(-avg_pts_allowed) %>% filter(gamemode == "Control") %>%
  ggplot(aes(x = reorder(team_icon, avg_pts_for), 
             y = avg_pts_for, fill = team_icon)) +
  geom_col() +
  geom_text(aes(label = avg_pts_for), vjust = -0.45, color = "#3b3b3b") +
  labs(title = "CDL Average Rounds Won in Control Loss", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Team") + ylab("Avg Pts For") + 
  ylim(0, 3) +
  scale_fill_manual(
    values = team_color, breaks = team_icon
  ) +
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("89_Avg_Rds_Won_in_CTRL_Loss.png", scale = 1.25,
       avg_pts_for_ctrl_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

avg_pts_for_by_map_ctrl_plot <- 
  cdlDF %>% filter(map_wl == "L" & gamemode == "Control") %>%
  select(match_id, team, map_name, team_score) %>%
  distinct() %>%
  group_by(team, map_name) %>%
  summarise(avg_pts_for = mean(team_score)) %>%
  ggplot(aes(x = map_name, y = avg_pts_for, fill = team)) +
  geom_col() +
  facet_wrap(~team) +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  scale_fill_manual(
    values = team_color, breaks = team
  ) +
  labs(title = "CDL Average Rounds Won in Control Loss by Map & Team", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Map") + ylab("Avg Rounds Won") + 
  theme(
    legend.position = "none", 
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b",
                                      angle = 30, hjust = 1), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("90_Avg_Rds_Won_by_Map_in_CTRL_Loss.png", scale = 1.25,
       avg_pts_for_by_map_ctrl_plot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)
  

# 91 Avg Points For in Loss by Gamemode ----------------------------------------

avg_pts_for_by_team_and_mode_Tbl <- 
  pts_by_team_and_mode_df %>% select(team, gamemode, avg_pts_for) %>%
  pivot_wider(names_from = gamemode, 
              values_from = c(avg_pts_for)) %>%
  ungroup() %>% select(team, Hardpoint, "Search & Destroy", Control) %>%
  gt() %>%
  opt_align_table_header("center") %>%
  cols_align("center") %>%
  tab_source_note("By: David Harler Jr. | Data from: @GGBreakingPoint") %>%
  opt_row_striping() %>%
  tab_header(title = "CDL Avergage Points For in Loss by Gamemode",
             subtitle = format(Sys.Date(), "%B %d, %Y")) %>%
  gt_theme_espn() %>% 
  cols_width(team ~ px(200), Control ~ px(100), Hardpoint ~ px(100), 
             "Search & Destroy" ~ px(100)) %>%
  data_color(
    columns = Control,
    fn = scales::col_numeric(
      palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                   "#bdd7e7", "#2171b5", "#2171b5"),
      domain = c(0, 2))) %>%
  data_color(
    columns = Hardpoint, 
    fn = scales::col_numeric(
      palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                   "#bdd7e7", "#2171b5", "#2171b5"),
      domain = c(150, 210))) %>%
  data_color(
    columns = ends_with("Destroy"), 
    fn = scales::col_numeric(
      palette =  c("#cb181d", "#cb181d", "#fcae91", "#ffffff", 
                   "#bdd7e7", "#2171b5", "#2171b5"),
      domain = c(0, 4)))

gtsave(avg_pts_for_by_team_and_mode_Tbl, 
       "91_Avg_Pts_For_in_Loss_by_Gamemode.png", path = figPath)

# 92 - 98 Hardpoint Map Analysis -----------------------------------------------

total_kills_by_map_hp_boxplot <- 
  cdlDF %>% filter(gamemode == "Hardpoint" & map_name != "Terminal") %>%
  group_by(match_id, map_name) %>%
  summarise(total_match_kills = sum(kills)) %>%
  ggplot(mapping = aes(x = map_name, y = total_match_kills)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  geom_jitter(width = 0.05, height = 0.1, alpha = 0.75) +
  labs(title = "CDL Total Match Kills by Map: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Total Match Kills") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("92_Total_Match_Kills_by_Map_HP_Box.png", scale = 1.25,
       total_kills_by_map_hp_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



total_points_by_map_hp_boxplot <- 
  cdlDF %>% filter(gamemode == "Hardpoint" & map_name != "Terminal") %>%
  select(match_id, map_name, total_score) %>%
  distinct() %>%
  ggplot(mapping = aes(x = map_name, y = total_score)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  geom_jitter(width = 0.05, height = 0.1, alpha = 0.75) +
  labs(title = "CDL Total Points by Map: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Total Points") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("93_Total_Pts_by_Map_HP_Box.png", scale = 1.25,
       total_points_by_map_hp_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



score_diff_in_win_by_map_hp_boxplot <- 
  cdlDF %>% 
  filter(gamemode == "Hardpoint" & map_name != "Terminal" & map_wl == "W") %>%
  select(match_id, map_name, score_diff) %>% 
  distinct() %>%
  ggplot(mapping = aes(x = map_name, y = score_diff)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  geom_jitter(width = 0.075, height = 0.2, alpha = 0.75) +
  labs(title = "CDL Winning Score Differentials by Map: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Score Differential") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("94_Score_Diff_in_Win_by_Map_HP_Box.png", scale = 1.25,
       score_diff_in_win_by_map_hp_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



total_points_by_map_hp_histogram <- 
  cdlDF %>% 
  filter(gamemode == "Hardpoint" & map_name != "Terminal") %>%
  select(match_id, map_name, total_score) %>%
  distinct() %>%
  ggplot(mapping = aes(total_score, fill = map_name)) +
  geom_histogram(position = position_dodge(), binwidth = 50) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "CDL Count of Total Points by Map: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Total Points") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("95_Total_Pts_by_Map_HP_Histogram.png", scale = 1.25,
       total_points_by_map_hp_histogram, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



score_diffs_in_win_by_map_hp_histogram <- 
  cdlDF %>% 
  filter(gamemode == "Hardpoint" & map_name != "Terminal" & map_wl == "W") %>%
  select(match_id, map_name, score_diff) %>%
  distinct() %>%
  ggplot(mapping = aes(score_diff, fill = map_name)) +
  geom_histogram(position = position_dodge(), binwidth = 50) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "CDL Count of Winning Score Differentials by Map: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Score Differentials") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("96_Score_Diff_in_Win_by_Map_HP_Histogram.png", scale = 1.25,
       score_diffs_in_win_by_map_hp_histogram, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



total_points_by_map_hp_wrapped <-
  cdlDF %>% 
  filter(gamemode == "Hardpoint" & map_name != "Terminal") %>%
  select(match_id, map_name, total_score) %>%
  distinct() %>%
  ggplot(mapping = aes(total_score, fill = map_name)) +
  geom_histogram(position = position_dodge(), binwidth = 50) +
  facet_wrap(~map_name) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "CDL Count of Total Points by Map: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Total Points") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("97_Total_Pts_by_Map_HP_Wrapped.png", scale = 1.25,
       total_points_by_map_hp_wrapped, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



score_diffs_in_win_by_map_hp_wrapped <-
  cdlDF %>% 
  filter(gamemode == "Hardpoint" & map_name != "Terminal" & map_wl == "W") %>%
  select(match_id, map_name, score_diff) %>%
  distinct() %>%
  ggplot(mapping = aes(score_diff, fill = map_name)) +
  geom_histogram(position = position_dodge(), binwidth = 50) +
  facet_wrap(~map_name) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "CDL Count of Winning Score Differentials by Map: Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Score Differential") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("98_Score_Diff_in_Win_by_Map_HP_Wrapped.png", scale = 1.25,
       score_diffs_in_win_by_map_hp_wrapped, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 99 - 105 SnD Map Analysis ----------------------------------------------------

total_kills_by_map_snd_boxplot <-
  cdlDF %>% filter(gamemode == "Search & Destroy" & map_name != "Skidrow") %>%
  group_by(match_id, map_name) %>%
  summarise(total_match_kills = sum(kills)) %>%
  ggplot(mapping = aes(x = map_name, y = total_match_kills)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  geom_jitter(width = 0.05, height = 0.1, alpha = 0.75) +
  labs(title = "CDL Total Match Kills by Map: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Total Match Kills") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("99_Total_Match_Kills_by_Map_SnD_Box.png", scale = 1.25,
       total_kills_by_map_snd_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



total_points_by_map_snd_boxplot <-
  cdlDF %>% filter(gamemode == "Search & Destroy" & map_name != "Skidrow") %>%
  select(match_id, map_name, total_score) %>%
  distinct() %>%
  ggplot(mapping = aes(x = map_name, y = total_score)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0) +
  geom_jitter(width = 0.075, height = 0.2, alpha = 0.75) +
  labs(title = "CDL Total Rounds by Map: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Total Rounds") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("100_Total_Rds_by_Map_SnD_Box.png", scale = 1.25,
       total_points_by_map_snd_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



score_diff_in_win_by_map_snd_boxplot <- 
  cdlDF %>% filter(gamemode == "Search & Destroy" & 
                     map_name != "Terminal" & map_wl == "W") %>%
  select(match_id, map_name, score_diff) %>% 
  distinct() %>%
  ggplot(mapping = aes(x = map_name, y = score_diff)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  geom_jitter(width = 0.05, height = 0.2, alpha = 0.75) +
  labs(title = "CDL Winning Score Differentials by Map: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Score Differential") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("101_Score_Diff_in_Win_by_Map_HP_Box.png", scale = 1.25,
       score_diff_in_win_by_map_snd_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



total_points_by_map_snd_histogram <-
  cdlDF %>% 
  filter(gamemode == "Search & Destroy" & map_name != "Skidrow") %>%
  select(match_id, map_name, total_score) %>%
  distinct() %>%
  ggplot(mapping = aes(total_score, fill = map_name)) +
  geom_bar(position = position_dodge()) +
  scale_x_continuous(breaks = c(6, 7, 8, 9, 10, 11)) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "CDL Count of Total Rounds by Map: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Total Rounds") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("102_Total_Rds_by_Map_SnD_Histogram.png", scale = 1.25,
       total_points_by_map_snd_histogram, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



score_diffs_in_win_by_map_snd_histogram <-
  cdlDF %>% filter(gamemode == "Search & Destroy" & 
                     map_name != "Skidrow" & map_wl == "W") %>%
  select(match_id, map_name, score_diff) %>%
  distinct() %>%
  ggplot(mapping = aes(score_diff, fill = map_name)) +
  geom_bar(position = position_dodge()) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6))+
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "CDL Count of Winning Score Differentials by Map: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Score Differential") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("103_Score_Diff_in_Win_by_Map_SnD_Histogram.png", scale = 1.25,
       score_diffs_in_win_by_map_snd_histogram, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



total_points_by_map_snd_wrapped <-
  cdlDF %>% 
  filter(gamemode == "Search & Destroy" & map_name != "Skidrow") %>%
  select(match_id, map_name, total_score) %>%
  distinct() %>%
  ggplot(mapping = aes(total_score, fill = map_name)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~map_name) +
  scale_x_continuous(breaks = c(6, 7, 8, 9, 10, 11)) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "CDL Count of Total Rounds by Map: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Total Rounds") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("104_Total_Rds_by_Map_SnD_Wrapped.png", scale = 1.25,
       total_points_by_map_snd_wrapped, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



score_diffs_in_win_by_map_snd_wrapped <-
  cdlDF %>% filter(gamemode == "Search & Destroy" & 
                     map_name != "Skidrow" & map_wl == "W") %>%
  select(match_id, map_name, score_diff) %>%
  distinct() %>%
  ggplot(mapping = aes(score_diff, fill = map_name)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~map_name) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6))+
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16)) +
  scale_fill_viridis_d(direction = -1) +
  labs(title = "CDL Count of Winning Score Differentials by Map: Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Score Differential") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("105_Score_Diff_in_Win_by_Map_SnD_Wrapped.png", scale = 1.25,
       score_diffs_in_win_by_map_snd_wrapped, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 106 - 112 Control Map Analysis -----------------------------------------------

total_kills_by_map_ctrl_boxplot <-
  cdlDF %>% filter(gamemode == "Control") %>%
  group_by(match_id, map_name) %>%
  summarise(total_match_kills = sum(kills)) %>%
  ggplot(mapping = aes(x = map_name, y = total_match_kills)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  geom_jitter(width = 0.05, height = 0.1, alpha = 0.75) +
  labs(title = "CDL Total Match Kills by Map: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Total Match Kills") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("106_Total_Match_Kills_by_Map_CTRL_Box.png", scale = 1.25,
       total_kills_by_map_ctrl_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



total_points_by_map_ctrl_boxplot <-
  cdlDF %>% filter(gamemode == "Control") %>%
  select(match_id, map_name, total_score) %>%
  distinct() %>%
  ggplot(mapping = aes(x = map_name, y = total_score)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  geom_jitter(width = 0.075, height = 0.1, alpha = 0.75) +
  labs(title = "CDL Total Rounds by Map: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Total Rounds") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("107_Total_Rds_by_Map_CTRL_Box.png", scale = 1.25,
       total_points_by_map_ctrl_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



score_diff_in_win_by_map_ctrl_boxplot <- 
  cdlDF %>% 
  filter(gamemode == "Control" & map_wl == "W") %>%
  select(match_id, map_name, score_diff) %>% 
  distinct() %>%
  ggplot(mapping = aes(x = map_name, y = score_diff)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  geom_jitter(width = 0.05, height = 0.2, alpha = 0.75) +
  labs(title = "CDL Winning Score Differentials by Map: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Score Differential") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("108_Score_Diff_in_Win_by_Map_CTRL_Box.png", scale = 1.25,
       score_diff_in_win_by_map_ctrl_boxplot, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



total_points_by_map_ctrl_histogram <-
  cdlDF %>% 
  filter(gamemode == "Control") %>%
  select(match_id, map_name, total_score) %>%
  distinct() %>%
  ggplot(mapping = aes(total_score, fill = map_name)) +
  geom_bar(position = position_dodge()) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), limits = c(0, 24)) +
  scale_fill_manual(values = c("#FDE725FF", "#56c667ff", "#238A8DFF")) +
  labs(title = "CDL Counts of Total Rounds by Map: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Total Rounds") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("109_Total_Rds_by_Map_CTRL_Histogram.png", scale = 1.25,
       total_points_by_map_ctrl_histogram, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



score_diffs_in_win_by_map_ctrl_histogram <-
  cdlDF %>% filter(gamemode == "Control" & map_wl == "W") %>%
  select(match_id, map_name, score_diff) %>%
  distinct() %>%
  ggplot(mapping = aes(score_diff, fill = map_name)) +
  geom_bar(position = position_dodge()) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), limits = c(0, 24)) +
  scale_fill_manual(values = c("#FDE725FF", "#56c667ff", "#238A8DFF")) +
  labs(title = "CDL Count of Winning Score Differentials by Map: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Score Differential") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("110_Score_Diff_in_Win_by_Map_CTRL_Histogram.png", scale = 1.25,
       score_diffs_in_win_by_map_ctrl_histogram, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



total_points_by_map_ctrl_wrapped <-
  cdlDF %>% 
  filter(gamemode == "Control") %>%
  select(match_id, map_name, total_score) %>%
  distinct() %>%
  ggplot(mapping = aes(total_score, fill = map_name)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~map_name) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), limits = c(0, 24)) +
  scale_fill_manual(values = c("#FDE725FF", "#56c667ff", "#238A8DFF")) +
  labs(title = "CDL Counts of Total Rounds by Map: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Total Rounds") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("111_Total_Rds_by_Map_CTRL_Wrapped.png", scale = 1.25,
       total_points_by_map_ctrl_wrapped, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)



score_diffs_in_win_by_map_ctrl_wrapped <-
  cdlDF %>% 
  filter(gamemode == "Control" & map_wl == "W") %>%
  select(match_id, map_name, score_diff) %>%
  distinct() %>%
  ggplot(mapping = aes(score_diff, fill = map_name)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~map_name) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20, 24), limits = c(0, 24)) +
  scale_fill_manual(values = c("#FDE725FF", "#56c667ff", "#238A8DFF")) +
  labs(title = "CDL Count of Winning Score Differentials by Map: Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint", 
       fill = "Map") +
  xlab("Score Differential") + ylab("Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("112_Score_Diff_in_Win_by_Map_CTRL_Wrapped.png", scale = 1.25,
       score_diffs_in_win_by_map_ctrl_wrapped, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

# 113 - 115 More ----

kills_per_hp <- cdlDF %>%
  filter(gamemode == "Hardpoint" & map_name != "Terminal") %>%
  ggplot(aes(x = map_name, y = kills)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0 ) +
  geom_jitter(width = 0.1, height = 0.25, alpha = 0.5) +
  labs(title = "CDL Kills per Hardpoint", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Kills") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("113_Kills_per_HP.png", scale = 1.25,
       kills_per_hp, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

kills_per_snd <- cdlDF %>%
  filter(gamemode == "Search & Destroy" & map_name != "Skidrow") %>%
  ggplot(aes(x = map_name, y = kills)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0 ) +
  geom_jitter(width = 0.1, height = 0.25, alpha = 0.5) +
  labs(title = "CDL Kills per Search & Destroy", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Kills") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("114_Kills_per_SnD.png", scale = 1.25,
       kills_per_snd, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)

kills_per_ctrl <- cdlDF %>%
  filter(gamemode == "Control") %>%
  ggplot(aes(x = map_name, y = kills)) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0 ) +
  geom_jitter(width = 0.1, height = 0.25, alpha = 0.5) +
  labs(title = "CDL Kills per Control", 
       subtitle = format(Sys.Date(), "%B %d, %Y"), 
       caption = "By: David Harler Jr. | Data from: @GGBreakingPoint") +
  xlab("Maps") + ylab("Kills") +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x.bottom = element_text(size = 10, color = "#3b3b3b"), 
    axis.text.y.left = element_text(size = 10, color = "#3b3b3b")
  )

ggsave("115_Kills_per_CTRL.png", scale = 1.25,
       kills_per_ctrl, path = figPath, 
       width = 2000, height = 1400, units = "px", dpi = 300)


