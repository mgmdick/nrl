
get_latest_odds <- function(curr_round) {
  
  library(jsonlite)
  library(tidyverse)
  
# cache the results to csv on a daily basis - don't use up too many requests by calling this function ####
daily_cache_path <- paste("odds_", Sys.Date(), ".csv", sep = "")
if (file.exists(daily_cache_path)) {
  odds <- read.csv(daily_cache_path)
  odds$Date <- ymd_hms(odds$Date, tz = 'Australia/Brisbane')
  odds$DateTime <- ymd_hms(odds$Date, tz = 'Australia/Brisbane')
  odds$Date <- as.Date(odds$DateTime)
  odds <- odds %>% arrange(desc(Date))
  return(odds)
}
odds_api_key <- "93cd11e0d1653dbfcf2eab4a6ed72c0f"

url = URLencode(glue::glue('https://api.the-odds-api.com/v3/odds/?sport=rugbyleague_nrl&region=au&mkt=h2h&apiKey={odds_api_key}'))

data <- fromJSON(url)
nrl_odds <- data$data
nrl_odds$DateTime <- ymd_hms(as.POSIXct(nrl_odds$commence_time, origin = "1970-01-01"), tz = 'Australia/Brisbane')

#### Get Data and standardise the names to the current dataset ####
teams <- map_df(nrl_odds$teams, function(x) data.frame(team1 = x[1], team2 = x[2]))

teams$team1 <- str_replace_all(teams$team1, "St George Illawarra Dragons", "St George Dragons")
teams$team1 <- str_replace_all(teams$team1, "Cronulla Sutherland Sharks", "Cronulla Sharks")
teams$team1 <- str_replace_all(teams$team1, "North Queensland Cowboys", "North QLD Cowboys")
teams$team1 <- str_replace_all(teams$team1, "Manly Warringah Sea Eagles", "Manly Sea Eagles")


teams$team2 <- str_replace_all(teams$team2, "St George Illawarra Dragons", "St George Dragons")
teams$team2 <- str_replace_all(teams$team2, "Cronulla Sutherland Sharks", "Cronulla Sharks")
teams$team2 <- str_replace_all(teams$team2, "North Queensland Cowboys", "North QLD Cowboys")
teams$team2 <- str_replace_all(teams$team2, "Manly Warringah Sea Eagles", "Manly Sea Eagles")


### Split out the nested components of the dataframe ####

odds <- map_df(nrl_odds$sites, function(x) {
  #x <- nrl_odds$sites[[1]]
  tables <- data.frame(x[,1:3])
  odds_split <- map_df(x[,4][[1]], function(y) data.frame(team1_odds = y[1], team2_odds = y[2]))
  odds_mean <- odds_split %>% summarise_all(mean)
  odds_mean
})

odds_clean <- bind_cols(nrl_odds, teams, odds) %>%
  select(DateTime, home_team, team1, team2, team1_odds, team2_odds) %>%
  mutate(Date = as.Date(ymd_hms(DateTime, tz = 'Australia/Brisbane')),
         Home.Team = ifelse(home_team == team1, team1, team2),
         Away.Team = ifelse(home_team == team1, team2, team1),
         Home.Odds = ifelse(home_team == team1, team1_odds, team2_odds),
         Away.Odds = ifelse(home_team == team1, team2_odds, team1_odds),
         Round = curr_round) %>%
  select(DateTime, Round, Home.Team, Away.Team, Home.Odds, Away.Odds)

write.csv(odds_clean, daily_cache_path, row.names = F)
odds_clean

}
  

