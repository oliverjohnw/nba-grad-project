## script designed to build data for season
source("second_break.R")
source("first_break.R")
source("winner.R")

season_cleaner <- function(df) {
  #all first_break match
  good_df1 <- second_break(df)
  
  #all first_break break, non overtime
  good_df2 <- first_break(df)
  
  df_comb <- rbind(good_df1, good_df2)
  df_final <- winner(df_comb)
  
}


