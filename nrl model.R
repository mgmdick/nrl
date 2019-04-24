path = "nrl.xlsx"
library(h2o)
library(openxlsx)
library(tidyverse)
library(lubridate)
library(zoo)
library(ggplot2)
library(forcats)

nrl <- read.xlsx(path, startRow = 2)
nrl_update <- read.xlsx("nrl_updated.xlsx", startRow = 2)

nrl_mod <- nrl %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"),
         Year = year(Date)) %>%
  mutate(winner = ifelse(Home.Score > Away.Score, "Home", "Away"),
         odd_pick = ifelse(Home.Odds < Away.Odds, "Home", "Away")) %>%
  mutate(bookmaker_correct = odd_pick == winner,
         home_winner = winner == "Home") %>%
  group_by(Home.Team) %>%
  summarise(bookie_percent = sum(bookmaker_correct, na.rm =T) / length(bookmaker_correct),
            always_home = sum(home_winner, na.rm =T) / length(home_winner)) %>%
  arrange(desc(always_home))

h2o.shutdown()
h2o.init()

nrl_home <- nrl %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"),
         Year = year(Date)) %>%
  mutate(winner = ifelse(Home.Score > Away.Score, "Home", "Away"),
         odd_pick = ifelse(Home.Odds < Away.Odds, "Home", "Away")) %>%
  mutate(bookmaker_correct = odd_pick == winner,
         home_winner = winner == "Home",
         play_off_game = ifelse(is.na(`Play.Off.Game?`), F,T)) %>%
  group_by(Home.Team) %>%
  arrange(desc(Date)) %>%
  mutate(home_points_conceded_3 = rollsum(lead(Away.Score), k = 3, align = "left", na.pad = T)) %>%
  mutate(home_points_total_3 = rollsum(lead(Home.Score), k = 3, align = "left", na.pad = T)) %>%
  mutate(home_wins_in_last_1 = lead(home_winner)) %>%
  mutate(home_wins_in_last_3 = rollsum(lead(home_winner), k = 3, align = "left", na.pad = T)) %>%
  mutate(home_wins_in_last_5 = rollsum(lead(home_winner), k = 5, align = "left", na.pad = T)) %>%
  mutate(home_wins_in_last_10 = rollsum(lead(home_winner), k = 10, align = "left", na.pad = T)) %>%
  mutate(home_odds_in_last_1 = lead(Home.Odds)) %>%
  mutate(home_odds_in_last_3 = rollmean(lead(Home.Odds), k = 3, align = "left", na.pad = T)) %>%
  mutate(home_odds_in_last_5 = rollmean(lead(Home.Odds), k = 5, align = "left", na.pad = T)) %>%
  mutate(home_odds_in_last_10 = rollmean(lead(Home.Odds), k = 10, align = "left", na.pad = T)) %>%
  mutate(max_home_odds_in_last_3 = rollapply(lead(Home.Odds), width = 3, FUN = min, align = "left", na.pad = T)) %>%
  mutate(max_home_odds_in_last_5 = rollapply(lead(Home.Odds), width= 5, FUN = min, align = "left", na.pad = T)) %>%
  mutate(max_home_odds_in_last_10 = rollapply(lead(Home.Odds), width = 10, FUN = min, align = "left", na.pad = T)) %>%
  select(Date, Round, Home.Team, Away.Team, Year,play_off_game, Home.Odds, Away.Odds,
         home_points_conceded_3,
         home_points_total_3,
         home_wins_in_last_1,
         home_wins_in_last_3,
         home_wins_in_last_5,
         home_wins_in_last_10,
         home_odds_in_last_1,
         home_odds_in_last_3,
         home_odds_in_last_5,
         home_odds_in_last_10,
         home_winner,
         max_home_odds_in_last_3,
         max_home_odds_in_last_5,
         max_home_odds_in_last_10
         )


nrl_away <- nrl %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"),
         Year = year(Date)) %>%
  mutate(winner = ifelse(Home.Score > Away.Score, "Home", "Away"),
         odd_pick = ifelse(Home.Odds < Away.Odds, "Home", "Away")) %>%
  mutate(bookmaker_correct = odd_pick == winner,
         away_winner = winner == "Away") %>%
  group_by(Away.Team) %>%
  arrange(desc(Date)) %>%
  mutate(away_points_conceded_3 = rollsum(lead(Home.Score), k = 3, align = "left", na.pad = T)) %>%
  mutate(away_points_total_3 = rollsum(lead(Away.Score), k = 3, align = "left", na.pad = T)) %>%
  mutate(away_wins_in_last_1 = lead(away_winner)) %>%
  mutate(away_wins_in_last_3 = rollsum(lead(away_winner), k = 3, align = "left", na.pad = T)) %>%
  mutate(away_wins_in_last_5 = rollsum(lead(away_winner), k = 5, align = "left", na.pad = T)) %>%
  mutate(away_wins_in_last_10 = rollsum(lead(away_winner), k = 10, align = "left", na.pad = T)) %>%
  mutate(away_odds_in_last_1 = lead(Away.Odds)) %>%
  mutate(away_odds_in_last_3 = rollmean(lead(Away.Odds), k = 3, align = "left", na.pad = T)) %>%
  mutate(away_odds_in_last_5 = rollmean(lead(Away.Odds), k = 5, align = "left", na.pad = T)) %>%
  mutate(away_odds_in_last_10 = rollmean(lead(Away.Odds), k = 10, align = "left", na.pad = T)) %>%
  mutate(min_away_odds_in_last_3 = rollapply(lead(Home.Odds), width = 3, FUN = min, align = "left", na.pad = T)) %>%
  mutate(min_away_odds_in_last_5 = rollapply(lead(Home.Odds), width= 5, FUN = min, align = "left", na.pad = T)) %>%
  mutate(min_away_odds_in_last_10 = rollapply(lead(Home.Odds), width = 10, FUN = min, align = "left", na.pad = T)) %>%
  arrange(Date) %>%
  select(Date,  Away.Team, Home.Team,
         away_wins_in_last_1,
         away_wins_in_last_3,
         away_wins_in_last_5,
         away_wins_in_last_10,
         away_odds_in_last_3,
         away_odds_in_last_5,
         away_odds_in_last_10,
         min_away_odds_in_last_3,
         min_away_odds_in_last_5,
         min_away_odds_in_last_10)



nrl_df <- nrl_home %>% inner_join(nrl_away, by = c("Date", "Home.Team", "Away.Team")) %>%
  arrange(Date) %>%
  ungroup() %>%
  mutate(weights = 1)

curr_round = 6

train_df = nrl_df %>% filter(Year < 2019 | (Year == 2019 & Round < curr_round))
test_df =  nrl_df %>% filter(Year == 2019 & Round == curr_round)
train <- as.h2o(train_df)
valid <- as.h2o(test_df)

colnames(train_df)

x <- setdiff(colnames(train_df), c("Date", "Year", "home_winner"))
y <- "home_winner"

h2o.model <- h2o.automl(x, y, training_frame = train, validation_frame = valid, max_runtime_secs = 400, weights_column = "weights")

#summary(h2o.model@leader)
predict(h2o.model, valid)
test_df$prediction = as.data.frame(predict(h2o.model, valid))$predict

test_df %>% select(Home.Team, Away.Team, prediction)

h2o.confusionMatrix(h2o.model@leader, train)

h2o.varimp_plot(h2o.model@leader)

test_df$Home.Odds[2] = 3.1

test_df %>%
  ungroup() %>%
  mutate(got_it_right = prediction == home_winner,
         odd_pick = ifelse(Home.Odds < Away.Odds, "Home", "Away"),
         winner = ifelse(home_winner, "Home", "Away"),
         bookmaker_correct = odd_pick == winner) %>%
  arrange(Date) %>%
  mutate(my_picks = cumsum(got_it_right),
         bookie_picks = cumsum(bookmaker_correct)) %>%
  ggplot(aes(x = Date, y = my_picks, color = "Dick Machine")) + geom_line(size = 2) + geom_line(aes(y = bookie_picks, color = "Bookmaker Picks"), size = 2) +
  scale_color_manual(values = c("firebrick", "steelblue")) +
  ggtitle("My Footy Tipping Model versus Bookmaker Favourite Picking", 2018) +
  ylab("Wins")
  
  nrl %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"),
         Year = year(Date)) %>%
  mutate(winner = ifelse(Home.Score > Away.Score, "Home", "Away"),
         odd_pick = ifelse(Home.Odds < Away.Odds, "Home", "Away")) %>%
  mutate(bookmaker_correct = odd_pick == winner,
         away_winner = winner == "Away") %>%
    group_by(Year) %>%
    summarise(sum(bookmaker_correct) / n())

