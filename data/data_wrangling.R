
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(readr)
# Import Data -------------------------------------------------------------

runs <- read_csv("runs.csv")
races <- read_csv("races.csv")


# Join --------------------------------------------------------------------

whole <- inner_join(runs, races, by="race_id")

# Glimpse -----------------------------------------------------------------

glimpse(whole)

# Changing Data Classes ---------------------------------------------------

whole$race_id <- as.character(whole$race_id)
whole$horse_no <- as.character(whole$horse_no)
whole$horse_id <- as.character(whole$horse_id)
whole$won <- as.logical(whole$won)
whole$horse_ratings <- as.character(whole$horse_ratings)
whole$draw <- as.character(whole$draw)
whole$jockey_id <- as.character(whole$jockey_id)
whole$trainer_id <- as.character(whole$trainer_id)
whole$race_no <- as.character(whole$race_no)
whole$surface <- as.character(whole$surface)
whole$race_class <- as.character(whole$race_class)
whole$time7 <- as.numeric(whole$time7)
whole$sec_time7 <- as.numeric(whole$sec_time7)
whole$distance <- as.character(whole$distance)
whole$result <- as.character(whole$result)

position_cols <- paste0("position_sec", 1:6)
place_cols <- paste0("place_combination", 1:4)
wincomb_cols <- paste0("win_combination",1:2)

for (col in c(place_cols, position_cols)) {
  whole[[col]] <- as.character(whole[[col]])
}

glimpse(whole)


# Sample Data -------------------------------------------------------------

set.seed(510)
horses <- slice_sample(whole, n=10000)

# Check Representativeness ------------------------------------------------

p1 <- ggplot(whole, aes(x = won)) +
  geom_bar(fill = "steelblue") +
  ggtitle("Distribution of Won in whole_runs") +
  xlab("Won") +
  ylab("Count") +
  theme_minimal()

p2 <- ggplot(horses, aes(x = won)) +
  geom_bar(fill = "darkred") +
  ggtitle("Distribution of Won in horses (Sampled Data)") +
  xlab("Won") +
  ylab("Count") +
  theme_minimal()

grid.arrange(p1,p2)


# Rare Events -------------------------------------------------------------

horses %>% 
  summarise(total_wins = sum(won),
            proportion_wins = sum(won)/n())

sum(horses$won) / (ncol(whole)-8) > 15

#fails the fule of thumb for every predictor. Still might be fine to start with forward selction



