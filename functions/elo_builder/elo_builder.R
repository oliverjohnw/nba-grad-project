# adds elo data to avg data
library(fivethirtyeightdata)

elo_builder <- function(year, df) {
  elo_df <- nba_elo %>% 
    filter(season == year) %>% 
    mutate(month = substr(date, 6,7),
           day = substr(date, 9,10)) %>% 
    mutate(gameid_cr = paste(team1, team2, month, day, sep = ""))
  
  df <- df %>% 
    mutate(month = replace(month, month == 1, "01"),
           month = replace(month, month == 2, "02"),
           month = replace(month, month == 3, "03"),
           month = replace(month, month == 4, "04"),
           date = replace(date, date == 1, "01"),
           date = replace(date, date == 2, "02"),
           date = replace(date, date == 3, "03"),
           date = replace(date, date == 4, "04"),
           date = replace(date, date == 5, "05"),
           date = replace(date, date == 6, "06"),
           date = replace(date, date == 7, "07"),
           date = replace(date, date == 8, "08"),
           date = replace(date, date == 9, "09"),
           away = replace(away, away == " CHA", "CHO"),
           away = replace(away, away == " NOH", "NOP"),
           home = replace(home, home == " CHA", "CHO"),
           home = replace(home, home == " NOH", "NOP")) %>% 
    mutate(gameid_cr = paste(home, away, month, date, sep = "")) %>% 
    mutate(gameid_cr = gsub(" ", "", gameid_cr, fixed = T))
  
  elo_df <- elo_df %>% 
    select(gameid_cr, elo1_pre, elo2_pre) %>% 
    rename(home_elo = elo1_pre,
           away_elo = elo2_pre)
  
  elo_add <- df %>% inner_join(elo_df, by = "gameid_cr")
  elo_add
}
