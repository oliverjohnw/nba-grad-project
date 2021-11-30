# script to build last n avg's, win percent for last n games, and variable for back to back games

last_n_avg_build <- function(df, n_games) {
  # select necessary variables
  df_choose <- df %>% 
    select(X, year, month, date, home, h_points,
           away, a_points, winner, win_status, winning_margin,
           h_fg, a_fg, h_fga, a_fga, h_3p, a_3p, h_3pa, a_3pa,
           h_ft, a_ft, h_fta, a_fta, h_orb, a_orb, h_drb, a_drb,
           h_ast, a_ast, h_stl, a_stl, h_blk, a_blk, h_tov, a_tov,
           h_pf, a_pf, h_tsp, a_tsp, h_ortg, a_ortg, h_drtg, a_drtg, date_var) %>% 
    mutate(date_var = as.Date(date_var))
  
  df_choose$month <- factor(df_choose$month, levels = c("10", "11", 
                                                  "12", "1", "2",
                                                  "3","4"))
  df_choose <- df_choose[order(df_choose$month,df_choose$date),] 
  
  ## vector of all 30 team names
  teams <- levels(as.factor(df_choose$home))
  
  ## get 30 different data sets, one for each team
  teams_df <- vector("list", length(teams))
  for (i in 1:length(teams)) {
    teams_df[[i]]<- df_choose %>% filter(home == teams[i] |
                                           away == teams[i])
  } 
  ## getting a team ID for each team df
  teams_id <- vector("list", 30)
  home_id <- vector("list", 30)
  for (i in 1:length(teams)) {
    for (j in 1:nrow(teams_df[[i]])) {
      teams_id[[i]][j] <- paste(teams[i], j, sep = "")
      home_id[[i]][j] <- ifelse(teams_df[[i]]$home[j] == teams[i], "home", "away")
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(game_id = teams_id[[i]])
    teams_df[[i]] <- teams_df[[i]] %>% mutate(home_away = home_id[[i]])
  }

  # last n win percentage
  team_perc <- vector("list", 30)
  perc <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for(k in 1:n_games) {
        perc[k] <- ifelse(teams_df[[i]]$winner[j-(n_games-k)] == teams[[i]],
                          1,0)
      }
      team_perc[[i]][j+1] <- sum(perc)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(win_perc = team_perc[[i]][1:nrow(teams_df[[i]])])
  }
  
  # back to back indicator
  dif <- vector("list", 30)
  for (i in 1:length(teams)) {
    for (j in 1:(nrow(teams_df[[i]])-1)) {
      dif[[i]][j+1] <- teams_df[[i]]$date_var[j+1] - teams_df[[i]]$date_var[j]
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(days_off = dif[[i]]) %>%
      mutate(back_to_back = ifelse(days_off == 1, 1, 0))
  }    

  # points
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_points[j - (n_games-k)], teams_df[[i]]$a_points[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_points = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  
  # field goal
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_fg[j - (n_games-k)], teams_df[[i]]$a_fg[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_fg = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  
  # fga
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_fga[j - (n_games-k)], teams_df[[i]]$a_fga[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_fga = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  # 3p
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_3p[j - (n_games-k)], teams_df[[i]]$a_3p[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_3p = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  # 3pa
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_3pa[j - (n_games-k)], teams_df[[i]]$a_3pa[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_3pa = teams_avg[[i]][1:nrow(teams_df[[i]])]) 
  }
  # ft
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_ft[j - (n_games-k)], teams_df[[i]]$a_ft[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_ft = teams_avg[[i]][1:nrow(teams_df[[i]])]) 
  }
  # fta
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_fta[j - (n_games-k)], teams_df[[i]]$a_fta[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_fta = teams_avg[[i]][1:nrow(teams_df[[i]])]) 
  }
  # oreb
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_orb[j - (n_games-k)], teams_df[[i]]$a_orb[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_orb = teams_avg[[i]][1:nrow(teams_df[[i]])]) 
  }
  # dreb
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_drb[j - (n_games-k)], teams_df[[i]]$a_drb[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_drb = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  # assists
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_ast[j - (n_games-k)], teams_df[[i]]$a_ast[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_ast = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  # stl
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_stl[j - (n_games-k)], teams_df[[i]]$a_stl[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_stl = teams_avg[[i]][1:nrow(teams_df[[i]])]) 
  }
  # block
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_blk[j - (n_games-k)], teams_df[[i]]$a_blk[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_blk = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  # tov
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_tov[j - (n_games-k)], teams_df[[i]]$a_tov[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_tov = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  # pf
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_pf[j - (n_games-k)], teams_df[[i]]$a_pf[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_pf = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  # tsp
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_tsp[j - (n_games-k)], teams_df[[i]]$a_tsp[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_tsp = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  # ortg
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_ortg[j - (n_games-k)], teams_df[[i]]$a_ortg[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_ortg = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }
  # drtg
  teams_avg <- vector("list", 30)
  avg <- vector()
  for (i in 1:length(teams)) {
    for (j in n_games:nrow(teams_df[[i]])) {
      for (k in 1:n_games) {
        avg[k] <- ifelse(teams_df[[i]]$home[j-(n_games-k)] == teams[i], teams_df[[i]]$h_drtg[j - (n_games-k)], teams_df[[i]]$a_drtg[j - (n_games-k)])
      }
      teams_avg[[i]][j+1] <- sum(avg)/n_games
    }
    teams_df[[i]] <- teams_df[[i]] %>% mutate(avg_drtg = teams_avg[[i]][1:nrow(teams_df[[i]])])
  }

  all_df <- rbind(teams_df[[1]], teams_df[[2]], teams_df[[3]], teams_df[[4]],
                  teams_df[[5]], teams_df[[6]], teams_df[[7]], teams_df[[8]],
                  teams_df[[9]], teams_df[[10]], teams_df[[11]], teams_df[[12]],
                  teams_df[[13]], teams_df[[14]], teams_df[[15]], teams_df[[16]],
                  teams_df[[17]], teams_df[[18]], teams_df[[19]], teams_df[[20]],
                  teams_df[[21]], teams_df[[22]], teams_df[[23]], teams_df[[24]],
                  teams_df[[25]], teams_df[[26]], teams_df[[27]], teams_df[[28]],
                  teams_df[[29]], teams_df[[30]])
  all_df <- all_df[order(all_df$X),]
  
  split1 <- all_df[seq(1,nrow(all_df)-1, by = 2),]
  split2 <- all_df[seq(2,nrow(all_df), by = 2),] 
  
  comb_df <- split1 %>% 
    mutate(home_id = ifelse(home_away == "home", game_id, split2$game_id),
           away_id = ifelse(home_away == "away", game_id, split2$game_id),
           h_avg_points = ifelse(home_away == "home", avg_points, split2$avg_points),
           a_avg_points = ifelse(home_away == "away", avg_points, split2$avg_points),
           h_avg_fg = ifelse(home_away == "home", avg_fg, split2$avg_fg),
           a_avg_fg = ifelse(home_away == "away", avg_fg, split2$avg_fg),
           h_avg_fga = ifelse(home_away == "home", avg_fga, split2$avg_fga),
           a_avg_fga = ifelse(home_away == "away", avg_fga, split2$avg_fga),
           h_avg_3p = ifelse(home_away == "home", avg_3p, split2$avg_3p),
           a_avg_3p = ifelse(home_away == "away", avg_3p, split2$avg_3p),
           h_avg_3pa = ifelse(home_away == "home", avg_3pa, split2$avg_3pa),
           a_avg_3pa = ifelse(home_away == "away", avg_3pa, split2$avg_3pa),
           h_avg_ft = ifelse(home_away == "home", avg_ft, split2$avg_ft),
           a_avg_ft = ifelse(home_away == "away", avg_ft, split2$avg_ft),
           h_avg_orb = ifelse(home_away == "home", avg_orb, split2$avg_orb),
           a_avg_orb = ifelse(home_away == "away", avg_orb, split2$avg_orb),
           h_avg_drb = ifelse(home_away == "home", avg_drb, split2$avg_drb),
           a_avg_drb = ifelse(home_away == "away", avg_drb, split2$avg_drb),
           h_avg_ast = ifelse(home_away == "home", avg_ast, split2$avg_ast),
           a_avg_ast = ifelse(home_away == "away", avg_ast, split2$avg_ast),
           h_avg_stl = ifelse(home_away == "home", avg_stl, split2$avg_stl),
           a_avg_stl = ifelse(home_away == "away", avg_stl, split2$avg_stl),
           h_avg_blk = ifelse(home_away == "home", avg_blk, split2$avg_blk),
           a_avg_blk = ifelse(home_away == "away", avg_blk, split2$avg_blk),
           h_avg_tov = ifelse(home_away == "home", avg_tov, split2$avg_tov),
           a_avg_tov = ifelse(home_away == "away", avg_tov, split2$avg_tov),
           h_avg_pf = ifelse(home_away == "home", avg_pf, split2$avg_pf),
           a_avg_pf = ifelse(home_away == "away", avg_pf, split2$avg_pf),
           h_avg_tsp = ifelse(home_away == "home", avg_tsp, split2$avg_tsp),
           a_avg_tsp = ifelse(home_away == "away", avg_tsp, split2$avg_tsp),
           h_avg_ortg = ifelse(home_away == "home", avg_ortg, split2$avg_ortg),
           a_avg_ortg = ifelse(home_away == "away", avg_ortg, split2$avg_ortg),
           h_avg_drtg = ifelse(home_away == "home", avg_drtg, split2$avg_drtg),
           a_avg_drtg = ifelse(home_away == "away", avg_drtg, split2$avg_drtg),
           h_win_perc = ifelse(home_away == "home", win_perc, split2$win_perc),
           a_win_perc = ifelse(home_away == "away", win_perc, split2$win_perc),
           h_back = ifelse(home_away == "home", back_to_back, split2$back_to_back),
           a_back = ifelse(home_away == "away", back_to_back, split2$back_to_back)) %>% 
    select(X, year, month, date, home, h_points, away, a_points, winner, win_status,
           winning_margin, home_id, away_id, h_avg_points,
           a_avg_points,
           h_avg_fg,
           a_avg_fg,
           h_avg_fga,
           a_avg_fga,
           h_avg_3p,
           a_avg_3p,
           h_avg_3pa,
           a_avg_3pa,
           h_avg_ft,
           a_avg_ft,
           h_avg_orb,
           a_avg_orb,
           h_avg_drb,
           a_avg_drb,
           h_avg_ast,
           a_avg_ast,
           h_avg_stl,
           a_avg_stl,
           h_avg_blk,
           a_avg_blk,
           h_avg_tov,
           a_avg_tov,
           h_avg_pf,
           a_avg_pf,
           h_avg_tsp,
           a_avg_tsp,
           h_avg_ortg,
           a_avg_ortg,
           h_avg_drtg,
           a_avg_drtg, 
           h_win_perc,
           a_win_perc,
           h_back,
           a_back) 
  
  comb_fin <- comb_df[complete.cases(comb_df),]
  comb_fin
  
}
  

