library(tidyverse)
library(httr)
library(jsonlite)

# Custom function to get the mode from a list
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# List of positions to loop through
positions = c('c', '1b', '2b', '3b', 'ss', 'lf', 'cf', 'rf', 'dh')

# Function to scrape the minor league stats from FanGraphs given a year, minor league level, and position
getStats <- function(year, level, pos) {
  url <- paste0('https://www.fangraphs.com/api/leaders/minor-league/data?pos=', pos, '&level=', level, '&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32,33&stats=bat&qual=0&type=0&team=&season=', year, '&seasonEnd=', year, '&org=&ind=0&splitTeam=false')
  POST(url, content_type_json(), accept_json()) %>%
    content(as = 'text') %>%
    fromJSON() -> data
  if (length(data) > 0) {
    data %>%
      select(-c(Name, Team)) %>%
      mutate(Pos = pos) %>%
      return() 
  } else {
    return()
  }
}

# Looping through the years 2016-2023, the minor league levels, and the positions list to fetch the minor leagues statistics for each combination of them
map_df(2016:2023, function(i) {
  map_df(1:8, function(j) {
    map_df(positions, function(k){
      getStats(i, j, k) -> df
      return(df)
    }) %>%
      return()
  }) %>%
    return()
}) -> milbStats

# Marking a player's position as the position they played the most over the course of the minor leagues
milbStats %>%
  group_by(minormasterid) %>%
  mutate(Pos = getmode(Pos)) %>%
  ungroup() -> milbStats

# Filtering under the age of 26 and a minimum of 130 plate appearances
milbStats %>%
  filter(Age < 26) %>%
  group_by(Season, minormasterid) %>%
  filter(sum(PA) >= 130) %>%
  ungroup() -> milbStats

# Function to scrape the Major League stats for a given year
getMLBstats <- function(year) {
  url <- 'https://www.fangraphs.com/api/leaders/splits/splits-leaders'
  POST(url, 
       body = paste0('{
                "strPlayerId": "all",
                "strSplitArr": [],
                "strGroup": "career",
                "strPosition": "B",
                "strType": "1",
                "strStartDate": "2016-03-01",
                "strEndDate": "', year, '-11-01",
                "strSplitTeams": false,
                "dctFilters": [
                  {
                    "stat": "PA",
                    "comp": "gt",
                    "low": "130",
                    "high": -99,
                    "label": "PA ≥ 130",
                    "value": 0
                  }
                ],
                "strStatType": "player",
                "strAutoPt": "false",
                "arrPlayerId": [],
                "strSplitArrPitch": [],
                "arrWxTemperature": null,
                "arrWxPressure": null,
                "arrWxAirDensity": null,
                "arrWxElevation": null,
                "arrWxWindSpeed": null
              }'),
       content_type_json(), accept_json()) %>%
    content(as = 'text') %>%
    fromJSON() -> data
  return(data$data %>%
           mutate(Season = year))
}

# Getting the MLB stats from 2016-2023
map_df(2016:2023, getMLBstats) -> mlbStats

# Removing minor leaguers who got called up and lost their prospect status
milbStats %>%
  anti_join(mlbStats %>%
              mutate(playerId = as.character(playerId)) %>%
              select(Season, playerId), by = c('Season', 'playerids' = 'playerId')) -> milbStats

# Function to scrape FanGraphs' Top Prospects report
getTopProspects <- function(year) {
  url <- paste0('https://www.fangraphs.com/api/prospects/board/data?draft=', year, 'prospect&season=', year)
  GET(url, content_type_json(), accept_json()) %>%
    content(as = 'text') %>%
    fromJSON() -> data
  return(data)
}

# Getting the top prospect reports from 2017-2024
map_df(2017:2024, getTopProspects) -> topProspects

# Selecting only relevant rows and columns from the top prospects reports
topProspects %>%
  filter(!is.na(minorMasterId)) %>%
  distinct(ID, cSeason, .keep_all = T) %>%
  select(minorMasterId, cSeason, Ovr_Rank) -> topProspects
