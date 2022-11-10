library(dplyr)
library(ggplot2)
library(lubridate)

n=100000

path = "C:/Users/sebas/Desktop/stat226/Project 2" 
setwd(path)
data <- read.csv(file = 'simulation/poisson.csv')

data <- data %>%
  mutate_all(~na_if(., "#N/A")) %>%
  na.omit(GoalsHome) %>%
  mutate(HomeGoals = as.numeric(HomeGoals),AwayGoals = as.numeric(AwayGoals))


simulationGoals <- function(homeTeamName,awayTeamName,year){
  homeTeam = data %>% 
    filter(Home == homeTeamName) %>%
    filter(Year == year) %>%
    summarise(
      scoredPerMatch = sum(HomeGoals)/n(),
      concededPerMatch = sum(AwayGoals)/n()
    )
  
  awayTeam = data %>% 
    filter(Away == awayTeamName) %>%
    filter(Year == year) %>%
    summarise(
      scoredPerMatch = sum(AwayGoals)/n(),
      concededPerMatch = sum(HomeGoals)/n()
    )
  
  allHomeTeams = data %>% 
    filter(Year == year) %>%
    summarise(
      scoredPerMatch = sum(HomeGoals)/n()
    )
  
  allAwayTeams = data %>% 
    filter(Year == year) %>%
    summarise(
      scoredPerMatch = sum(AwayGoals)/n()
    )
  
  HomeAttackStrength = homeTeam$scoredPerMatch / allHomeTeams$scoredPerMatch
  AwayAttackStrength = awayTeam$scoredPerMatch / allAwayTeams$scoredPerMatch
  HomeDefenseStrength = homeTeam$concededPerMatch / allAwayTeams$scoredPerMatch
  AwayDefenseStrength = awayTeam$concededPerMatch / allHomeTeams$scoredPerMatch
  
  
  HomeGoalsLambda = HomeAttackStrength * AwayDefenseStrength * allHomeTeams$scoredPerMatch
  AwayGoalsLambda = AwayAttackStrength * HomeDefenseStrength * allAwayTeams$scoredPerMatch
  

  sim <- tibble(
    home = rpois(n,HomeGoalsLambda),
    away = rpois(n,AwayGoalsLambda),
    outcome = case_when(
      home > away  ~ homeTeamName,
      away > home  ~ awayTeamName,
      TRUE ~ "Tie"
     
    ))
  
  mosaic::tally(~ outcome, format = "percent", data = sim)
  
}

# simulationXG <- function(homeTeamName,awayTeamName,year){
#   
#   homeTeam = data %>% 
#     filter(Home == homeTeamName) %>%
#     filter(Year == year) %>%
#     summarise(
#       scoredPerMatch = sum(HomeXG)/n(),
#       concededPerMatch = sum(AwayXG)/n()
#     )
#   
#   awayTeam = data %>% 
#     filter(Away == awayTeamName) %>%
#     filter(Year == year) %>%
#     summarise(
#       scoredPerMatch = sum(AwayXG)/n(),
#       concededPerMatch = sum(HomeXG)/n()
#     )
#   
#   allHomeTeams = data %>% 
#     filter(Year == year) %>%
#     summarise(
#       scoredPerMatch = sum(HomeXG)/n()
#     )
#   
#   allAwayTeams = data %>% 
#     filter(Year == year) %>%
#     summarise(
#       scoredPerMatch = sum(AwayXG)/n()
#     )
#   
#   HomeAttackStrength = homeTeam$scoredPerMatch / allHomeTeams$scoredPerMatch
#   AwayAttackStrength = awayTeam$scoredPerMatch / allAwayTeams$scoredPerMatch
#   HomeDefenseStrength = homeTeam$concededPerMatch / allAwayTeams$scoredPerMatch
#   AwayDefenseStrength = awayTeam$concededPerMatch / allHomeTeams$scoredPerMatch
#   
#   
#   HomeGoalsLambda = HomeAttackStrength * AwayDefenseStrength * allHomeTeams$scoredPerMatch
#   AwayGoalsLambda = AwayAttackStrength * HomeDefenseStrength * allAwayTeams$scoredPerMatch
#   
#   
#   sim <- tibble(
#     home = rpois(n,HomeGoalsLambda),
#     away = rpois(n,AwayGoalsLambda),
#     outcome = case_when(
#       home > away  ~ homeTeamName,
#       away > home  ~ awayTeamName,
#       TRUE ~ "Tie"
#     ))
#   
#   mosaic::tally(~ outcome, format = "percent", data = sim)
#   
# }


# simulation <- function(homeTeamName,awayTeamName,year,delta, return = F){
# 
#    homeTeam = data %>%
#      filter(Home == homeTeamName) %>%
#      filter(Year == year) %>%
#      summarise(
#        scoredPerMatch = sum(HomeGoals)/n(),
#        concededPerMatch = sum(AwayGoals)/n(),
#        XGscoredPerMatch = sum(HomeXG)/n(),
#        XGconcededPerMatch = sum(AwayXG)/n()
#      )
# 
#    awayTeam = data %>%
#      filter(Away == awayTeamName) %>%
#      filter(Year == year) %>%
#      summarise(
#        scoredPerMatch = sum(AwayGoals)/n(),
#        concededPerMatch = sum(HomeGoals)/n(),
#        XGscoredPerMatch = sum(AwayXG)/n(),
#        XGconcededPerMatch = sum(HomeXG)/n()
#      )
# 
#    allHomeTeams = data %>%
#      filter(Year == year) %>%
#      summarise(
#        scoredPerMatch = sum(HomeGoals)/n(),
#        XGscoredPerMatch = sum(HomeXG)/n()
#      )
# 
#    allAwayTeams = data %>%
#      filter(Year == year) %>%
#      summarise(
#        scoredPerMatch = sum(AwayGoals)/n(),
#        XGscoredPerMatch = sum(AwayXG)/n()
#      )
# 
#    HomeAttackStrength = (delta * homeTeam$scoredPerMatch + (1-delta)*homeTeam$XGscoredPerMatch) / (delta * allHomeTeams$scoredPerMatch + (1-delta)*allHomeTeams$XGscoredPerMatch)
#    AwayAttackStrength = (delta * awayTeam$scoredPerMatch + (1-delta)*awayTeam$XGscoredPerMatch) / (delta * allAwayTeams$scoredPerMatch + (1-delta)*allAwayTeams$XGscoredPerMatch)
#    HomeDefenseStrength = (delta * homeTeam$concededPerMatch + (1-delta)*homeTeam$XGconcededPerMatch) / (delta * allAwayTeams$scoredPerMatch + (1-delta)*allAwayTeams$XGscoredPerMatch)
#    AwayDefenseStrength = (delta * awayTeam$concededPerMatch + (1-delta)*awayTeam$XGconcededPerMatch) / (delta * allHomeTeams$scoredPerMatch + (1-delta)*allHomeTeams$XGscoredPerMatch)
# 
# 
#    HomeGoalsLambda = HomeAttackStrength * AwayDefenseStrength * (delta * allHomeTeams$scoredPerMatch + (1-delta)*allHomeTeams$XGscoredPerMatch)
#    AwayGoalsLambda = AwayAttackStrength * HomeDefenseStrength * (delta * allAwayTeams$scoredPerMatch + (1-delta)*allAwayTeams$XGscoredPerMatch)
# 
# 
#    sim <- tibble(
#      home = rpois(n,HomeGoalsLambda),
#      away = rpois(n,AwayGoalsLambda),
#      outcome = case_when(
#        home > away  ~ homeTeamName,
#        away > home  ~ awayTeamName,
#        TRUE ~ "Tie"
#      ))
# 
#    mosaic::tally(~ outcome, format = "percent", data = sim)
# 
#    if(return == TRUE){
#      return(sim)
#    }
# 
#  }
# 
# 
# 
# 
# 
# 
#  df = data.frame(matrix(ncol = 2, nrow = 0))
#  colnames(df) <- c('Delta', 'Difference')
# 
# 
#  sumDifference = 0
# 
#  for(delta in (seq(0, 1, by = 0.1))){
#    for(i in 1:nrow(data)){
#      difference = simulation(data$Home[i],data$Away[i],2023,delta,T) %>%
#        mutate(differenceHome = abs(data$HomeGoals[i]-home),
#               differenceAway = abs(data$AwayGoals[i]-home)) %>%
#        summarise(
#          sumDifference = (sum(differenceHome) + sum(differenceAway)) / n
#        )
# 
#      sumDifference = sumDifference + difference[[1]]
# 
# 
#    }
# 
#    df = df %>% add_row(Delta = delta, Difference = sumDifference)
#    sumDifference = 0
#  }

simulationGoals("Leeds","Bournemouth",2023) # Leeds win (75%)
simulationGoals("Man City","Fulham",2023) # Man City wins by at least 2 goals (75%)
simulationGoals("Nottingham","Brentford",2023) # No more than 3 goals in a game (62%)
simulationGoals("Wolves","Brighton",2023) # Brighton win (71%)
simulationGoals("Everton","Leicester",2023) # BTTS (61%)
simulationGoals("Chelsea","Arsenal",2023) # Arsenal win (62%)
simulationGoals("Aston Villa","Man United",2023) # Aston Villa wins or ties (73%)
simulationGoals("Southampton","Newcastle",2023) # Newcastle wins (66%)
simulationGoals("West Ham","Crystal Palace",2023) # no BTTS (69%) 
simulationGoals("Tottenham","Liverpool",2023) # Tottenham wins (69%)
