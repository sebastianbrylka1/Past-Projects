library(dplyr)
library(purrr)
library(mosaic)
library(ggplot2)
library(lubridate)
library(tidyverse)

path = "C:/Users/sebas/Desktop/stat226/Project 2" 
setwd(path)
data <- read.csv(file = 'simulation/all_matches.csv')


data <- data %>%
  mutate(Date = gsub("p","-",Dte)) %>%
  mutate(year = year(dmy(Date))) %>%
  mutate_all(~na_if(., "#N/A")) %>%
  na.omit(FTHG) %>%
  mutate(FTHG = as.numeric(FTHG),FTAG = as.numeric(FTAG))


n <- 100000

# -----------------------------------------------------------------------------
# Data wrangling 

standings <- read.csv(file = 'simulation/EPLstandings.csv')

standings = standings %>%
  pivot_longer(-Team,
               names_to = "Year",
               values_to = "Place"
  ) %>%
  mutate(Year = gsub("X","",Year)) %>%
  na.omit(Place) %>%
  mutate(Year = as.integer(Year))

data <- data %>%
  left_join(standings, by=c('HomeTeam'='Team', 'year'='Year')) %>%
  rename(HomePlace=Place) %>%
  mutate(HomePlace = ifelse(is.na(HomePlace),21,HomePlace)) %>%
  left_join(standings, by=c('AwayTeam'='Team', 'year'='Year')) %>%
  rename(AwayPlace=Place) %>%
  mutate(AwayPlace = ifelse(is.na(AwayPlace),21,AwayPlace))

#------------------------------------------------------------------------------
# home goals - away goals


data %>%
  mutate(difference = FTHG - FTAG) %>%
  ggplot(aes(x=difference)) +
    geom_density(adjust = 3) +
    xlab("Home Team Goals - Away Team Goals")

home_away <- data %>%
  mutate(difference = FTHG - FTAG) %>%
  summarize(
    mean = mean(difference),
    sd = sd(difference)
  )


sim <- tibble(
  diff = rnorm(n,mean=home_away$mean,sd=home_away$sd),
  win = case_when(
    diff > 0.5 ~ "Home",
    diff < -0.5 ~ "Away",
    TRUE ~ "Tie"
  )
  )

mosaic::tally(~ win, format = "percent", data = sim)

rm(home_away)

# -----------------------------------------------------------------------------
# Manchester United goals scored - conceded

data %>%
  filter(HomeTeam=="Man United"  | AwayTeam == "Man United") %>%
  mutate(difference = ifelse(HomeTeam=="Man United", FTHG - FTAG, FTAG - FTHG)) %>%
  ggplot(aes(x=difference)) +
  geom_density(adjust = 2) +
  xlab("Man Utd Goals - Opponent Goals")

man_utd <- data %>%
  filter(HomeTeam=="Man United"  | AwayTeam == "Man United") %>%
  mutate(difference = ifelse(HomeTeam=="Man United", FTHG - FTAG, FTAG - FTHG)) %>%
  summarize(
    n=n(),
    mean = mean(difference),
    sd = sd(difference)
  )


sim <- tibble(
  diff = rnorm(n,mean=man_utd$mean,sd=man_utd$sd),
  win = case_when(
    diff > 0.5 ~ "Man United",
    diff < -0.5 ~ "Opponent",
    TRUE ~ "Tie"
  )
)

mosaic::tally(~ win, format = "percent", data = sim)   

rm(man_utd)

# -----------------------------------------------------------------------------
# Manchester United at home goals scored - conceded

data %>%
  filter(HomeTeam=="Man United" ) %>%
  mutate(difference = FTHG - FTAG) %>%
  ggplot(aes(x=difference)) +
  geom_density(adjust=2) +
  xlab("Home Man Utd Goals Scored - Conceded")

man_utd_home <- data %>%
  filter(HomeTeam=="Man United" ) %>%
  mutate(difference = FTHG - FTAG) %>%
  summarize(
    n=n(),
    mean = mean(difference),
    sd = sd(difference)
  )


sim <- tibble(
  diff = rnorm(n,mean=man_utd_home$mean,sd=man_utd_home$sd),
  win = case_when(
    diff > 0.5 ~ "Man United",
    diff < -0.5 ~ "Opponent",
    TRUE ~ "Tie"
  )
)

mosaic::tally(~ win, format = "percent", data = sim)  



#------------------------------------------------------------------------------
# West Ham away goals scored - conceded

west_ham_away <- data %>%
  filter(AwayTeam=="West Ham" ) %>%
  mutate(difference = FTAG - FTHG) %>%
  summarize(
    mean = mean(difference),
    sd = sd(difference)
  )

# -----------------------------------------------------------------------------
# Manchester vs West Ham 

data %>%
  filter(HomeTeam=="Man United") %>%
  filter(AwayTeam=="West Ham") %>%
  mutate(difference = FTHG - FTAG) %>%
  ggplot(aes(x=difference)) +
  geom_density() +
  xlab("Home Man Utd Against West Ham Goals Scored - Conceded")



# ------------------------------------------------------------------------------
# Man Utd against teams from 7th place

standings %>%
  filter(Team == "Man United" | Team == "West Ham") %>%
  ggplot(aes(x = Year, y=Place,col=Team))+
  geom_point(size=3)+
  geom_line(size=1) +
  scale_y_reverse()

data %>%
  filter(HomeTeam=="Man United") %>%
  filter(AwayPlace==7) %>%
  mutate(difference = FTHG - FTAG) %>%
  ggplot(aes(x=difference)) +
  geom_density() +
  xlab("Home Man Utd Against 7th team Goals Scored - Conceded")

# ------------------------------------------------------------------------------
# Man Utd home and West Ham away against teams from 5-8 places 

d = data %>%
  filter(HomeTeam=="Man United") %>%
  filter(AwayPlace >= 5 & AwayPlace <= 8) %>%
  mutate(difference = FTHG - FTAG) 
d %>%
  ggplot(aes(x=difference)) +
  geom_density(adjust=3) +
  xlab("Home Man Utd Against 5th-8th Scored - Conceded") +
  stat_function(fun=dnorm,args =list(
    mean=mean(d$difference),sd = sd(d$difference)),color="red"
  )

d = data %>%
  filter(AwayTeam=="West Ham") %>%
  filter(HomePlace >= 5 & HomePlace <= 8) %>%
  mutate(difference = FTAG - FTHG) 
d %>%
  ggplot(aes(x=difference)) +
  geom_density(adjust = 2) +
  xlab("Away West Ham Against 5th-8th Scored - Conceded") +
  stat_function(fun=dnorm,args =list(
    mean=mean(d$difference),sd = sd(d$difference)),color="red"
  )


# ----------------------------------------------------------------------------
# team from 5-8 home - team from 5-8 

d = data %>%
  filter(HomePlace >= 5 & HomePlace <= 8) %>%
  filter(AwayPlace >= 5 & AwayPlace <= 8) %>%
  mutate(difference = FTHG - FTAG) 

d %>%
  ggplot(aes(x=difference)) +
  geom_density() +
  xlab("Home 5th-8th against 5th-8th Goal Difference") +
  stat_function(fun=dnorm,args =list(
    mean=mean(d$difference),sd = sd(d$difference)),color="red"
  )
 
remove(d)

home <- data %>%
  filter(HomePlace >= 5 & HomePlace <= 8) %>%
  filter(AwayPlace >= 5 & AwayPlace <= 8) %>%
  mutate(difference = FTHG - FTAG) %>%
  summarize(
    mean = mean(difference),
    sd = sd(difference)
  )


sim <- tibble(
  home1 = rnorm(n,mean=home$mean,sd=home$sd),
  outcome = case_when(
    home1 > 0.5 ~ "Home",
    home1 < -0.5 ~ "Away",
    TRUE ~ "Tie"
  )
)

mosaic::tally(~ outcome, format = "percent", data = sim)

rm(home)

# -----------------------------------------------------------------------------
# Weighted normal
# a = data %>%
#   mutate(difference = FTHG - FTAG) %>% 
#   mutate(difference = case_when(
#     year > 1999 & year < 2008 ~ 0.1 * difference,
#     year >= 2008 & year < 2014 ~ 0.2 * difference,
#     year >= 2014 & year < 2016 ~ 0.3 * difference,
#     year >= 2015 ~ 0.4 * difference
#   ))
# 
# a %>%
#   ggplot(aes(x=difference)) +
#   geom_density(adjust=2) +
#   xlab("Weighted Normal") +
#   stat_function(fun=dnorm,args =list(
#     mean=mean(a$difference),sd = sd(a$difference)),color="red"
#   )

# -----------------------------------------------------------------------------
# Function Definitions

ranges <- function(place){
  if(place >=1 & place <= 4){
    return(c(1,4))
  }
  if(place >=5 & place <= 8){
    return(c(5,8))
  }
  if(place >=8 & place <= 11){
    return(c(8,11))
  }
  if(place >=12 & place <= 16){
    return(c(12,16))
  }
  if(place >=17 & place <= 20){
    return(c(17,20))
  }
  if(place == 21){
    return(c(21,21))
  }
}


outcome_function <- function(homeTeam,awayTeam){
  
  homePlace = value(filter(standings,Team == homeTeam & Year == 2022)$Place)
  awayPlace = value(filter(standings,Team == awayTeam & Year == 2022)$Place)
  
  home <- data %>%
    filter(HomePlace >= ranges(homePlace)[1] & HomePlace <= ranges(homePlace)[2]) %>%
    filter(AwayPlace >= ranges(awayPlace)[1] & AwayPlace <= ranges(awayPlace)[2]) %>%
    mutate(difference = FTHG - FTAG) %>%
    summarize(
      mean = mean(difference),
      sd = sd(difference)
    )
  
  
  sim <- tibble(
    home1 = rnorm(n,mean=home$mean,sd=home$sd),
    outcome = case_when(
      home1 > 0.5 ~ homeTeam,
      home1 < -0.5 ~ awayTeam,
      TRUE ~ "Tie"
    )
  )
  
  return(mosaic::tally(~ outcome, format = "percent", data = sim))
  
  rm(home,away)
}



# -----------------------------------------------------------------------------
# WEEK 13
outcome_function(homeTeam = "Nottingham",awayTeam = "Liverpool") # Result: Nottingham 
outcome_function(homeTeam = "Everton",awayTeam = "Crystal Palace") # Result: Everton 
outcome_function(homeTeam = "Man City",awayTeam = "Brighton") # Result: Man City 
outcome_function(homeTeam = "Chelsea",awayTeam = "Man United") # Result: tie
outcome_function(homeTeam = "Aston Villa",awayTeam = "Brentford") # Result: Aston Villa
outcome_function(homeTeam = "Leeds",awayTeam = "Fulham") # Result: Fulham
outcome_function(homeTeam = "Southampton",awayTeam = "Arsenal") # Result: tie
outcome_function(homeTeam = "Wolves",awayTeam = "Leicester") # Result: Leicester
outcome_function(homeTeam = "Tottenham",awayTeam = "Newcastle") # Result: Newcastle


# WEEK 14
outcome_function(homeTeam = "Leicester",awayTeam = "Man City") # Result: Man City
outcome_function(homeTeam = "Bournemouth",awayTeam = "Tottenham") # Result: Tottenham
outcome_function(homeTeam = "Brentford",awayTeam = "Wolves") # Result: Tie
outcome_function(homeTeam = "Brighton",awayTeam = "Chelsea") # Result: Brighton
outcome_function(homeTeam = "Crystal Palace",awayTeam = "Southampton") # Result: Crystal Palace
outcome_function(homeTeam = "Newcastle",awayTeam = "Aston Villa") # Result: Newcastle
outcome_function(homeTeam = "Fulham",awayTeam = "Everton") # Result: Tie
outcome_function(homeTeam = "Liverpool",awayTeam = "Leeds") # Result: Leeds
outcome_function(homeTeam = "Arsenal",awayTeam = "Nottingham") # Result: Arsenal
outcome_function(homeTeam = "Man United",awayTeam = "West Ham") # Result: Man United

# WEEK 15
outcome_function(homeTeam = "Leeds",awayTeam = "Bournemouth") # Result: 
outcome_function(homeTeam = "Man City",awayTeam = "Fulham") # Result: 
outcome_function(homeTeam = "Nottingham",awayTeam = "Brentford") # Result: 
outcome_function(homeTeam = "Wolves",awayTeam = "Brighton") # Result: 
outcome_function(homeTeam = "Everton",awayTeam = "Leicester") # Result: 
outcome_function(homeTeam = "Chelsea",awayTeam = "Arsenal") # Result: 
outcome_function(homeTeam = "Aston Villa",awayTeam = "Man United") # Result: 
outcome_function(homeTeam = "Southampton",awayTeam = "Newcastle") # Result: 
outcome_function(homeTeam = "West Ham",awayTeam = "Crystal Palace") # Result: 
outcome_function(homeTeam = "Tottenham",awayTeam = "Liverpool") # Result: 