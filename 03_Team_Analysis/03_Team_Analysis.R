
# Set directory ----------------------------------------------------------------

setwd("C:/Users/David/OneDrive/Desktop/dataClass/06-cod-analysis/2024.03.01 CDL Stats Visualization/cdl_viz")

# Save figure path -------------------------------------------------------------

figPath = 
  "C:/Users/David Harler Jr/OneDrive/Desktop/dataClass/06-cod-analysis/2024.03.12 CDL Stats Visualization/cdl_viz/03_Team_Analysis/Figures"

# Source setup -----------------------------------------------------------------

source("00_Setup/00_Setup.R")

# 01 Team Summary --------------------------------------------------------------

team_summary_gt_fn("OpTic Texas")

# 02 Team vs Team Summary ------------------------------------------------------

team_summary_vs_fn("OpTic Texas", "Las Vegas Legion")

# 03 Team H2H ------------------------------------------------------------------

team_h2h_gt_fn("OpTic Texas", "Minnesota ROKKR")

# 04 Kills by Player -----------------------------------------------------------

kills_by_player("OpTic Texas", "Hardpoint")

# 05 K/D by Player -------------------------------------------------------------

kd_by_player("OpTic Texas", "Control")

# 06 Kills vs Score Differential -----------------------------------------------

kills_vs_score_diff("Toronto Ultra", "Hardpoint", "Karachi")

# 07 K/D vs Score Differential -------------------------------------------------
  
kd_vs_score_diff("Toronto Ultra", "Hardpoint", "Karachi")

# 08 Score Differentials by Map & Mode -----------------------------------------

team_score_diffs("OpTic Texas", "Hardpoint")

