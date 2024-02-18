library(tidyverse)
library(httr)
library(jsonlite)

# Custom function to get the mode from a list
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function to scrape the minor league stats from FanGraphs given a year and minor league level
getStatsP <- function(year, level) {
  url <- paste0('https://www.fangraphs.com/api/leaders/minor-league/data?pos=all&level=', level, '&lg=2,4,5,6,7,8,9,10,11,14,12,13,15,16,17,18,30,32,33&stats=pit&qual=0&type=0&team=&season=', year, '&seasonEnd=', year, '&org=&ind=0&splitTeam=false')
  POST(url, content_type_json(), accept_json()) %>%
    content(as = 'text') %>%
    fromJSON() -> data
  if (length(data) > 0) {
    data %>%
      select(-c(Name, Team)) %>%
      return() 
  } else {
    return()
  }
}

# Looping through the years 2016-2023 and the minor league levels to fetch the minor leagues statistics for each combination of them
map_df(2016:2023, function(i) {
  map_df(1:8, function(j) {
    getStatsP(i, j) %>%
      return()
  }) %>%
    return()
}) -> milbStatsP

# Filtering under the age of 26 and a minimum of 30 innings
milbStatsP %>%
  filter(Age < 26) %>%
  group_by(Season, minormasterid) %>%
  filter(sum(IP) >= 30) %>%
  ungroup() -> milbStatsP

# Function to scrape the Major League stats for a given year
getMLBstatsP <- function(year) {
  url <- 'https://www.fangraphs.com/api/leaders/splits/splits-leaders'
  POST(url, 
       body = paste0('{
                "strPlayerId": "all",
                "strSplitArr": [],
                "strGroup": "career",
                "strPosition": "P",
                "strType": "1",
                "strStartDate": "2016-03-01",
                "strEndDate": "', year, '-11-01",
                "strSplitTeams": false,
                "dctFilters": [
                  {
                    "stat": "IP",
                    "comp": "gt",
                    "low": "50",
                    "high": -99,
                    "label": "IP ≥ 50",
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
map_df(2016:2023, getMLBstatsP) -> mlbStatsP

# Removing minor leaguers who got called up and lost their prospect status
milbStatsP %>%
  anti_join(mlbStatsP %>%
              mutate(playerId = as.character(playerId)) %>%
              select(Season, playerId), by = c('Season', 'playerids' = 'playerId')) -> milbStatsP

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

# Function to get the MLB fWAR statistics for a given year
getFWARp <- function(year) {
  url <- paste0('https://www.fangraphs.com/api/leaders/war?season=', year, '&lg=&wartype=0&teamid=')
  GET(url, content_type_json(), accept_json()) %>%
    content(as = 'text') %>%
    fromJSON() -> data
  data %>%
    mutate(year = year) %>%
    return()
}

# Getting fWAR stats for players from 2017-2023
map_df(2017:2023, getFWARp) -> fWARp

# Filtering to only grab players with a qualifying sample size
fWARp %>%
  filter(!is.na(pitWAR)) %>%
  group_by(playerid) %>%
  filter(sum(IP) >= 50) -> fWARp

# Calculating each player's average fWAR per 200 innings over the course of 2017-2023
fWARp %>%
  mutate(playerid = as.character(playerid)) %>%
  group_by(playerid) %>%
  summarise(
    name = first(playerNameRoute),
    fWAR = 200 * sum(pitWAR) / sum(IP)
  ) -> fWARp
