library(tidyverse)

# Uncomment these if you want to run the model from scratch. Otherwise, use the csv imports below
# source('ml.R')
# dataFinal -> hitters
# source('mlP.R')
# dataFinal -> pitchers

# Uncomment these if you want to save your results from running the model from scratch
# write.csv(hitters, 'finalHitters2024.csv', row.names = F)
# write.csv(pitchers, 'finalPitchers2024.csv', row.names = F)

# Read in the final hitter and pitcher results from the model (these are the ones the article is based off of)
read.csv('finalHitters2024.csv') -> hitters
read.csv('finalPitchers2024.csv') -> pitchers

# Change this to look at whichever season you want
season <- 2023

# Prints the top 100 hitters for the selected season
hitters %>%
  select(cSeason, PlayerName, Age, Pos, AffAbbName, avg_rank) %>%
  filter(cSeason == season) %>%
  arrange(avg_rank) %>%
  head(100)

# Prints the top 50 pitchers for the selected season
pitchers %>%
  select(cSeason, PlayerName, Age, AffAbbName, avg_rank) %>%
  filter(cSeason == season) %>%
  arrange(avg_rank) %>%
  head(50)

# Creates the top 100 list for the selected season
hitters %>%
  select(cSeason, PlayerName, Age, Pos, AffAbbName, avg_rank) %>%
  filter(cSeason == season) %>%
  mutate(avg_rank = avg_rank * 0.35) %>%
  rbind(
    pitchers %>%
      mutate(Pos = 'P') %>%
      select(cSeason, PlayerName, Age, Pos, AffAbbName, avg_rank) %>%
      filter(cSeason == season) %>%
      mutate(avg_rank = avg_rank * 0.65 + 1.5)
  ) %>%
  arrange(avg_rank) %>%
  head(100) -> final

# Uncomment this to save the top 100 list
# write.csv(final, 'final2024.csv', row.names = F)

# Load in the top 100 list
read.csv('final2024.csv') -> final

# Filtering out players who lost prospect eligibility and changing players who switched teams
final %>%
  mutate(
    AffAbbName = case_when(
      PlayerName == 'Edgar Quero' ~ 'CHW',
      PlayerName == 'Drew Thorpe' ~ 'SDP',
      PlayerName == 'Kyle Manzardo' ~ 'CLE',
      PlayerName == 'Michael Busch' ~ 'CHC',
      PlayerName == 'Gabriel Gonzalez' ~ 'MIN',
      PlayerName == 'Jackson Ferris' ~ 'LAD',
      PlayerName == 'Domingo Robles' ~ 'OAK',
      PlayerName == 'Ryan Clifford' ~ 'NYM',
      PlayerName == 'Samad Taylor' ~ 'SEA',
      PlayerName == 'Jeremiah Jackson' ~ 'NYM',
      PlayerName == 'Jorbit Vivas' ~ 'NYY',
      PlayerName == 'Kahlil Watson' ~ 'CLE',
      PlayerName == 'Nasim Nuñez' ~ 'WSN',
      PlayerName == 'Luisangel Acuña' ~ 'NYM',
      PlayerName == 'Trey Sweeney' ~ 'LAD',
      PlayerName == 'Richard Fitts' ~ 'BOS',
      PlayerName == 'Hao-Yu  Lee' ~ 'DET',
      PlayerName == 'Oliver Dunn' ~ 'MIL',
      PlayerName == 'Joey Ortiz' ~ 'MIL',
      PlayerName == 'Cristian Mena' ~ 'ARI',
      .default = AffAbbName
    )
  ) %>%
  filter(!PlayerName %in% c('DL Hall', 
                            'Drew Rom',
                            'Gustavo Campero',
                            'Jonny DeLuca',
                            'Xavier Edwards',
                            'José Devers',
                            'Lawrence Butler',
                            'Omar Alfonzo',
                            'Heliot Ramos',
                            'Iván Herrera',
                            'Alfredo Zarraga')) -> final

# Printing out the top 100
final %>%
  head(100)

# Printing out the top farm systems
final %>%
  head(100) %>%
  group_by(AffAbbName) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 30)

# Printing out the top 10 prospects for each team
for (i in sort(unique(final$AffAbbName))) {
  final %>%
    filter(AffAbbName == i) %>%
    arrange(avg_rank) %>%
    head(10) %>%
    print()
}
