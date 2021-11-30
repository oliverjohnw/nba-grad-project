library(dplyr)

#function that determines winner and loser
winner <- function(df) {
  #makes variables for winner, winning margin, and home/away win
  df <- df %>% 
    mutate(winner = ifelse(a_points > h_points, away, home),
         winning_margin = abs(h_points - a_points),
         win_status = ifelse(a_points > h_points, "Away", "Home")) %>% 
    select(year,	month, date,	home,	away,	h_points, a_points, winner, 
           winning_margin, win_status, h_fg,	a_fg, h_fga,	a_fga,
           h_3p,	a_3p, h_3pa,	a_3pa, h_ft,	a_ft, h_fta,	a_fta,
           h_orb, a_orb, h_drb,	a_drb,	h_trb, a_trb, h_ast,	a_ast, h_stl,
           a_stl, h_blk,	a_blk, h_tov,	a_tov, h_pf, a_pf, h_tsp, a_tsp,
           h_efgp, a_efgp, h_ortg, a_ortg, h_drtg,	a_drtg)
}
