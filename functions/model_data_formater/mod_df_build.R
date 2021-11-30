# function to get data ready for model fitting

mod_df_build <- function(df) {
  df_trim <- df %>% 
    select(-c(X.1, X, year, month, date, home,
              h_points, away, a_points, winner, 
              winning_margin, home_id, away_id)) %>% 
    mutate(win_status = as.factor(win_status))
}
