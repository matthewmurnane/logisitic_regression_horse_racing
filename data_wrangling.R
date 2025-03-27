
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

whole$race_id <- factor(whole$race_id)
whole$horse_no <- factor(whole$horse_no)
whole$horse_id <- factor(whole$horse_id)
whole$won <- as.logical(whole$won)
whole$horse_ratings <- factor(whole$horse_ratings)
whole$draw <- factor(whole$draw)
whole$jockey_id <- factor(whole$jockey_id)
whole$trainer_id <- factor(whole$trainer_id)
whole$race_no <- factor(whole$race_no)
whole$surface <- factor(whole$surface)
whole$race_class <- factor(whole$race_class)
whole$time7 <- as.numeric(whole$time7)
whole$sec_time7 <- as.numeric(whole$sec_time7)
whole$distance <- factor(whole$distance)

position_cols <- paste0("position_sec", 1:6)
place_cols <- paste0("place_combination", 1:4)
wincomb_cols <- paste0("win_combination",1:2)

for (col in c(place_cols, position_cols)) {
  whole[[col]] <- as.factor(whole[[col]])
}

glimpse(whole)


# Sample Data -------------------------------------------------------------

set.seed(510)
horses <- slice_sample(whole, n=8000)

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


