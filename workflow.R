# Script designed to automate work flow of data cleaning up to model implementation

# load packages
library(dplyr)

# read in scraped data (raw/uncleaned)
setwd("~/Downloads/grad-nba-wins/data/scraped_data")
raw_df19 <- read.csv("raw_2019.csv")

# clean and save raw data
setwd("~/Downloads/grad-nba-wins/data_cleaners")
source("season_cleaner.R")
clean19 <- season_cleaner(raw_df19)
setwd("~/Downloads/grad-nba-wins/data/cleaned_data")
write.csv(clean19, "clean_19.csv")

# get last n averages and save data 
clean_df19 <- read.csv("clean_19.csv") %>%
  mutate(date_var = as.Date(paste(year, month, date, sep = "-")))
setwd("~/Downloads/grad-nba-wins/last_n_avg_build")
source("last_n_stats.R")
avg19 <- last_n_avg_build(clean_df19, 15)
setwd("~/Downloads/grad-nba-wins/data/avg_data")
write.csv(avg19, "avg19.csv")

# add elo data and save
avg19 <- read.csv("avg19.csv")
setwd("~/Downloads/grad-nba-wins/elo_builder")
source("elo_builder.R")
elo19 <- elo_builder(2019, avg19)
setwd("~/Downloads/grad-nba-wins/data/elo_data")
write.csv(elo19, "elo19.csv")

# format data for model input and save
setwd("~/Downloads/grad-nba-wins/model_data_formater")
source("mod_df_build.R")
mod19 <- mod_df_build(elo19)
setwd("~/Downloads/grad-nba-wins/final_data")
write.csv(mod19, "mod19.csv")

# data used in python models


