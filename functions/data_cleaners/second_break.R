library(dplyr)

second_break <- function(df) {
  #inputs original NBA dataframe
  original_df <- df
  
  #split into good at first_break
  first_match_df <- original_df %>% 
    filter(first_break == 240)
  
  #begin working on observations good at first break
  complete_df1 <- first_match_df %>% 
    filter(second_break == 240)
  
  second_break_df <- first_match_df %>% 
    filter(second_break != 240)
  
  #lets work on second_break_df
  #these are shifted over one
  second_shift1_df <- second_break_df %>%
    filter(X.227 == 240)
  #these are shifted over two
  second_shift2_df <- second_break_df %>% 
    filter(X.226 == 240)
  #these are shifted over three
  second_shift3_df <- second_break_df %>% 
    filter(X.225 == 240)
  #these are shifted over four
  second_shift4_df <- second_break_df %>% 
    filter(X.224 == 240)
  #these are shifted over five
  second_shift5_df <- second_break_df %>% 
    filter(X.223 == 240)
  
  #fix shift over one
  complete_df2 <- second_shift1_df %>% 
    mutate(h_drtg = h_ortg,
           h_ortg = h_usg,
           h_tovp = h_usg,
           h_tovp = h_blkp,
           h_blkp = h_stlp,
           h_stlp = h_astp,
           h_astp = h_trbp,
           h_trbp = h_drbp,
           h_drbp = h_orbp,
           h_orbp = h_ftr,
           h_ftr = h_3par,
           h_3par = h_efgp,
           h_efgp = h_tsp,
           h_tsp = second_break,
           second_break = X.227)
  
  #fix shift over two
  complete_df3 <- second_shift2_df %>% 
    mutate(h_drtg = h_usg,
           h_ortg = h_tovp,
           h_tovp = h_blkp,
           h_tovp = h_stlp,
           h_blkp = h_astp,
           h_stlp = h_trbp,
           h_astp = h_drbp,
           h_trbp = h_orbp,
           h_drbp = h_ftr,
           h_orbp = h_3par,
           h_ftr = h_efgp,
           h_3par = h_tsp,
           h_efgp = second_break,
           h_tsp = X.227,
           second_break = X.226)
  
  #fix shift over three
  complete_df4 <- second_shift3_df %>% 
    mutate(h_drtg = h_tovp,
           h_ortg = h_blkp,
           h_tovp = h_stlp,
           h_tovp = h_astp,
           h_blkp = h_trbp,
           h_stlp = h_drbp,
           h_astp = h_orbp,
           h_trbp = h_ftr,
           h_drbp = h_3par,
           h_orbp = h_efgp,
           h_ftr = h_tsp,
           h_3par = second_break,
           h_efgp = X.227,
           h_tsp = X.226,
           second_break = X.225)
  
  #fix shift over four
  complete_df5 <- second_shift4_df %>% 
    mutate(h_drtg = h_blkp,
           h_ortg = h_stlp,
           h_tovp = h_astp,
           h_tovp = h_trbp,
           h_blkp = h_drbp,
           h_stlp = h_orbp,
           h_astp = h_ftr,
           h_trbp = h_3par,
           h_drbp = h_efgp,
           h_orbp = h_tsp,
           h_ftr = second_break,
           h_3par = X.227,
           h_efgp = X.226,
           h_tsp = X.225,
           second_break = X.224)
  
  complete_df6 <- second_shift5_df %>% 
    mutate(h_drtg = h_stlp,
           h_ortg = h_astp,
           h_tovp = h_trbp,
           h_tovp = h_drbp,
           h_blkp = h_orbp,
           h_stlp = h_ftr,
           h_astp = h_3par,
           h_trbp = h_efgp,
           h_drbp = h_tsp,
           h_orbp = second_break,
           h_ftr = X.227,
           h_3par = X.226,
           h_efgp = X.225,
           h_tsp = X.224,
           second_break = X.223)
  
  #at this point, everything should be good for first match
  overall_df <- rbind(complete_df1, complete_df2, complete_df3, complete_df4,
                      complete_df5, complete_df6) %>% 
    select(year,	month, date,	home,	away,	mins,	a_fg,	a_fga,
           a_fgp,	a_3p,	a_3pa,	a_3pp,	a_ft,	a_fta,	a_ftp,
           a_orb,	a_drb,	a_trb,	a_ast,	a_stl,	a_blk,	a_tov,
           a_pf,	a_points,	a_tsp,	a_efgp, a_ortg,	a_drtg,	h_fg,	h_fga,
           h_fgp,	h_3p,	h_3pa,	h_3pp,
           h_ft,	h_fta,	h_ftp,	h_orb,	h_drb,	h_trb,	h_ast,	h_stl,
           h_blk,	h_tov,	h_pf,	h_points,	h_tsp,	h_efgp,
           h_ortg,	h_drtg)
}

