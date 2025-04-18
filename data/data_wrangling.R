library(tidyverse)
library(ggplot2)
library(gridExtra)
library(readr)
library(car)
# Import Data -------------------------------------------------------------

runs <- read_csv("data/runs.csv")
races <- read_csv("data/races.csv")

# Join --------------------------------------------------------------------

whole <- inner_join(runs, races, by="race_id") %>% 
  select(!c(horse_id, race_id, time7, sec_time7))

# Glimpse -----------------------------------------------------------------

glimpse(whole)

# Changing Data Classes ---------------------------------------------------

whole$horse_no <- as.factor(whole$horse_no)
whole$won <- as.factor(whole$won)
whole$horse_ratings <- as.factor(whole$horse_ratings)
whole$draw <- as.factor(whole$draw)
whole$jockey_id <- as.factor(whole$jockey_id)
whole$trainer_id <- as.factor(whole$trainer_id)
whole$race_no <- as.factor(whole$race_no)
whole$surface <- as.factor(whole$surface)
whole$race_class <- as.factor(whole$race_class)
whole$distance <- as.factor(whole$distance)

position_cols <- paste0("position_sec", 1:6)
place_cols <- paste0("place_combination", 1:4)
wincomb_cols <- paste0("win_combination",1:2)

for (col in c(place_cols, position_cols)) {
  whole[[col]] <- as.factor(whole[[col]])
}

glimpse(whole)
