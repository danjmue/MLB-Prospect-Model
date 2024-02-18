library(tidyverse)
library(glmnet)

# Runs the necessary scrapes to get the data to run the model
source('scrapeFGpitchers.R')

# Pre-Processing Step:
# - Makes each player's stats a combination of their stats at every level of the minor leagues that they played at (weighted by batters faced)
# - Creates variables noting the player's starting level of the minors, ending level of the minors, and main level (where they faced the most batters)
# - Joins the minor leagues statistics with the top prospect report
milbStatsP %>%
  mutate(cSeason = as.character(Season + 1)) %>%
  group_by(Season, minormasterid) %>%
  mutate(prop = TBF / sum(TBF)) %>%
  ungroup() %>%
  mutate(across(names(milbStatsP)[1:49],  ~ . * prop)) %>%
  group_by(Season, minormasterid) %>%
  mutate(across(names(milbStatsP)[1:49],  ~ sum(., na.rm = T))) %>%
  mutate(startLevel = last(aLevel), endLevel = first(aLevel)) %>%
  ungroup() %>%
  arrange(desc(TBF)) %>%
  group_by(Season, minormasterid) %>%
  mutate(mainLevel = first(aLevel)) %>%
  ungroup() %>%
  distinct(Season, minormasterid, .keep_all = T) %>%
  left_join(
    topProspects,
    by = c('minormasterid' = 'minorMasterId', 'cSeason'),
    multiple = 'all'
  ) -> data

# Creating the variables for the previous season's prospect ranking
# If a player isn't ranked, mark them as the maxmimum rank + 25
data %>%
  arrange(minormasterid, cSeason) %>%
  mutate(Ovr_rank_prev = NA) %>%
  mutate(Ovr_Rank = ifelse(Ovr_Rank == 0, max(Ovr_Rank, na.rm = T) + 25, Ovr_Rank)) -> data
for (i in 2:nrow(data)) {
  if (data$minormasterid[i] == data$minormasterid[i - 1]) {
    data$Ovr_rank_prev[i] <- data$Ovr_Rank[i - 1]
  }
}

# Fixing NA values and standardizing all the statistics
data %>%
  mutate(Ovr_Rank = ifelse(is.na(Ovr_Rank), max(Ovr_Rank, na.rm = T) + 50, Ovr_Rank),
         Ovr_rank_prev = ifelse(is.na(Ovr_rank_prev), max(Ovr_rank_prev, na.rm = T) + 50, Ovr_rank_prev),
         Is_Ranked = ifelse(Ovr_Rank > 0 & Ovr_Rank < 101, 1, 0)) %>%
  mutate(across(names(milbStatsP)[1:49], ~ (. - mean(.)) / sd(.))) -> data

# Adding the major league fWAR data to the table and marking fWAR for players who didn't make the majors as 0
data %>%
  left_join(fWARp, by = c('playerids' = 'playerid')) %>%
  mutate(fWAR = ifelse(is.na(fWAR), 0, fWAR)) -> data

# Removing 2024 from the data to use previous years as the source data to build the model
data %>%
  filter(cSeason == 2024) -> thisYear
data %>%
  filter(cSeason != 2024) -> data

# LASSO model
lasso <- cv.glmnet(x = as.matrix(data[,c(3, 6:8, 10:42, 46:50, 66)]), y = data$Ovr_Rank, alpha = 1)
coef(lasso)

# Ridge regression model
rr <- cv.glmnet(x = as.matrix(data[,c(3, 6:8, 10:42, 46:50, 66)]), y = data$Ovr_Rank, alpha = 0)
coef(rr)
names(sort(abs(coef(rr)[, 1]), decreasing = TRUE))

# Creating train/test split for remaining models
set.seed(1)
inds <- sample(1:nrow(data), 0.7 * nrow(data))
training <- data[inds,]
testing <- data[-inds,]

# Linear regression model to predict a player's career fWAR
lm(fWAR ~ ERA + CG + ShO + SV + H + R + ER + HR + BB + IBB + HBP + WP + BK + SO +
     `K/BB` + `HR/9` + `K%` + `BB%` + AVG + WHIP + BABIP + `LOB%` + FIP +
     `GB%` + `LD%` + `IFFB%` + `HR/FB` + `GB/FB` + `SwStr%` + xFIP + Hld + BS +
     Age + 
     Ovr_rank_prev +
     factor(startLevel) + factor(endLevel) + factor(mainLevel), data = training) -> fwar_fit
summary(fwar_fit)

# Linear regression model to predict a player's prospect rank
lm(Ovr_Rank ~ ERA + CG + ShO + SV + H + R + ER + HR + BB + IBB + HBP + WP + BK + SO +
     `K/BB` + `HR/9` + `K%` + `BB%` + AVG + WHIP + BABIP + `LOB%` + FIP +
     `GB%` + `LD%` + `IFFB%` + `HR/FB` + `GB/FB` + `SwStr%` + xFIP + Hld + BS +
     Age + 
     Ovr_rank_prev +
     factor(startLevel) + factor(endLevel) + factor(mainLevel), data = training) -> lm_fit
summary(lm_fit)

# Logistic regression model
glm(Is_Ranked ~ ERA + CG + ShO + SV + H + R + ER + HR + BB + IBB + HBP + WP + BK + SO +
      `K/BB` + `HR/9` + `K%` + `BB%` + AVG + WHIP + BABIP + `LOB%` + FIP +
      `GB%` + `LD%` + `IFFB%` + `HR/FB` + `GB/FB` + `SwStr%` + xFIP + Hld + BS +
      Age + 
      Ovr_rank_prev +
      factor(startLevel) + factor(endLevel) + factor(mainLevel), data = training, family = binomial) -> glm_fit
summary(glm_fit)

# Adding 2024 back to the data
data %>%
  rbind(thisYear) -> dataFinal

# Adding predictions to the data
predict(fwar_fit, dataFinal, type = 'response') -> dataFinal$fWAR_Rank_Pred
predict(lm_fit, dataFinal, type = 'response') -> dataFinal$Ovr_Rank_Pred
predict(glm_fit, dataFinal, type = 'response') -> dataFinal$Is_Ranked_Pred
predict(lasso, newx = as.matrix(dataFinal[, c(3, 6:8, 10:42, 46:50, 66)])) -> dataFinal$lasso
predict(rr, newx = as.matrix(dataFinal[, c(3, 6:8, 10:42, 46:50, 66)])) -> dataFinal$rr

# Calculating each player's average rank amongst the four models, giving weight to the fwAR model and logistic regression
dataFinal %>%
  group_by(cSeason) %>%
  mutate(
    fwar_rank = rank(-fWAR_Rank_Pred),
    lm_rank = rank(Ovr_Rank_Pred),
    glm_rank = rank(-Is_Ranked_Pred),
    lasso_rank = rank(lasso),
    rr_rank = rank(rr)
  ) %>%
  ungroup() %>%
  mutate(avg_rank = (7 * fwar_rank + lm_rank + 5 * glm_rank + lasso_rank + rr_rank) / 15) -> dataFinal
