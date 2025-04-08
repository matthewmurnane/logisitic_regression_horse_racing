
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

whole$horse_no <- as.factor(whole$horse_no)
whole$won <- as.logical(whole$won)
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

sum(horses$won) / (ncol(whole)-8) > 10



