# LOADING LIBRARIES

library(tidyverse)
library(dplyr)
library(ggplot2)

#path to the folder with all the data
path = "C:/Users/sebas/Desktop/stat226/Project 1/data" 
setwd(path)

# ----------------------------------------------------------------
# LOADING AND MODIFYING DATABASES FOR THE RANKING ANALYSIS

y2007 <- read.csv("rankings/2007.csv")%>%
  select(name,rank) %>%   
  filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%
  mutate(rank=row_number()) %>%
  rename("2007"=rank)

y2008 <- read.csv("rankings/2008.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2008"=rank)

df <- y2007 %>%
  full_join(y2008,
            by= c("name" = "name")
  )
rm(y2007,y2008)

y2009 <- read.csv("rankings/2009.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2009"=rank)

df <- df %>%
  full_join(y2009,
            by= c("name" = "name")
  )
rm(y2009)

y2010 <- read.csv("rankings/2010.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2010"=rank)

df <- df %>%
  full_join(y2010,
            by= c("name" = "name")
  )
rm(y2010)

y2011 <- read.csv("rankings/2011.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2011"=rank)

df <- df %>%
  full_join(y2011,
            by= c("name" = "name")
  )
rm(y2011)

y2012 <- read.csv("rankings/2012.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2012"=rank)

df <- df %>%
  full_join(y2012,
            by= c("name" = "name")
  )
rm(y2012)

y2013 <- read.csv("rankings/2013.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2013"=rank)

df <- df %>%
  full_join(y2013,
            by= c("name" = "name")
  )
rm(y2013)

y2014 <- read.csv("rankings/2014.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2014"=rank)

df <- df %>%
  full_join(y2014,
            by= c("name" = "name")
  )
rm(y2014)

y2015 <- read.csv("rankings/2015.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2015"=rank)

df <- df %>%
  full_join(y2015,
            by= c("name" = "name")
  )
rm(y2015)

y2016 <- read.csv("rankings/2016.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2016"=rank)

df <- df %>%
  full_join(y2016,
            by= c("name" = "name")
  )
rm(y2016)

y2017 <- read.csv("rankings/2017.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2017"=rank)

df <- df %>%
  full_join(y2017,
            by= c("name" = "name")
  )
rm(y2017)

y2018 <- read.csv("rankings/2018.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2018"=rank)

df <- df %>%
  full_join(y2018,
            by= c("name" = "name")
  )
rm(y2018)

y2019 <- read.csv("rankings/2019.csv")%>%
  select(name,rank) %>%   filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal")) %>%   mutate(rank=row_number()) %>%
  rename("2019"=rank)

df <- df %>%
  full_join(y2019,
            by= c("name" = "name")
  )
rm(y2019)

#-------------------------------------------------------------------------------
# CREATING FACETS FOR THE 8 PLAYERS WITH THE MOST APPEARANCES IN RANKINGS OF THEIR POSITION

df %>%
  pivot_longer(-name,names_to = "year", values_to = "ranking") %>%
  na.omit() %>%
  group_by(name) %>%
  summarise(
    appearances = n()
  ) %>%
  arrange(desc(appearances)) %>%
  head(8) %>%
  ungroup() %>%
  left_join(df,
            by = c("name" = "name")
  ) %>%
  select(-appearances) %>%
  pivot_longer(-name,names_to = "year", values_to = "ranking") %>%
  na.omit()%>%
  ggplot(aes(year, ranking))+
  geom_point(aes(color=name),size=4,show.legend = F)+
  facet_wrap(~name,nrow = 4) +
  theme(strip.text.x = element_text(size = 12)) +
  labs(x="Year",y="Ranking",title = "Ranking position excluding BIG 3")



# ----------------------------------------------------------------
# LOADING AND MODIFYING DATABASES FOR THE TOP PHASES ANALYSIS


slams_titles = read.csv("phases/slams_titles.csv")
slams_finals = read.csv("phases/slams_finals.csv")
slams_semi = read.csv("phases/slams_semi.csv")

slams <- slams_semi %>%
  select(name,count) %>%
  rename(slams_semi = count) %>%
  left_join(slams_finals %>%
              select(name,count) %>%
              rename(slams_finals = count), 
            by = c("name" = "name")) %>%
  left_join(slams_titles %>%
              select(name,count) %>%
              rename(slams_titles = count), 
            by = c("name" = "name"))
rm(slams_finals)
rm(slams_semi)
rm(slams_titles)

tour_titles = read.csv("phases/tour_titles.csv")
tour_finals = read.csv("phases/tour_finals.csv")
tour_semi = read.csv("phases/tour_semi.csv")

tour <- tour_semi %>%
  select(name,count) %>%
  rename(tour_semi = count) %>%
  left_join(tour_finals %>%
              select(name,count) %>%
              rename(tour_finals = count), 
             by = c("name" = "name")) %>%
  left_join(tour_titles %>%
              select(name,count) %>%
              rename(tour_titles = count), 
            by = c("name" = "name"))
rm(tour_finals)
rm(tour_semi)
rm(tour_titles)



masters_titles = read.csv("phases/masters_titles.csv")
masters_finals = read.csv("phases/masters_finals.csv")
masters_semi = read.csv("phases/masters_semi.csv")

masters <- masters_semi %>%
  select(name,count) %>%
  rename(masters_semi = count) %>%
  left_join(masters_finals %>%
              select(name,count) %>%
              rename(masters_finals = count), 
            by = c("name" = "name")) %>%
  left_join(masters_titles %>%
              select(name,count) %>%
              rename(masters_titles = count), 
            by = c("name" = "name"))
rm(masters_finals)
rm(masters_semi)
rm(masters_titles)


olympics_titles = read.csv("phases/olympics_titles.csv")
olympics_finals = read.csv("phases/olympics_finals.csv")
olympics_semi = read.csv("phases/olympics_semi.csv")

olympics <- olympics_semi %>%
  select(name,count) %>%
  rename(olympics_semi = count) %>%
  left_join(olympics_finals %>%
              select(name,count) %>%
              rename(olympics_finals = count), 
            by = c("name" = "name")) %>%
  left_join(olympics_titles %>%
              select(name,count) %>%
              rename(olympics_titles = count), 
            by = c("name" = "name"))
rm(olympics_finals)
rm(olympics_semi)
rm(olympics_titles)


# ---------------------------------------------------------------
# MERGING ALL DATABASES TO ONE BIG DATABASE
# Left join to only use players who were in Grand Slam semi final at least once


all_players <- slams %>%
  left_join(masters,
            by = c("name" = "name")) %>%
  left_join(tour,
            by = c("name" = "name")) %>%
  left_join(olympics,
            by = c("name" = "name"))
  
big3 <- all_players %>%
  mutate(isBig3 = ifelse(name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal"),T,F))

all_players <- all_players %>%
  filter(!name %in% c("Roger Federer","Novak Djokovic","Rafael Nadal"))

#-------------------------------------------------------------------------------
# Grand Slam won

all_players %>%
  select(name,slams_titles) %>%
  na.omit() %>%
  ggplot(aes(x=reorder(name, -slams_titles),y=slams_titles)) +
  geom_bar(stat="identity", fill = "lightgreen") +
  labs(x="Player",y="Grand Slam Titles", title = "Grand Slams won by the players outside of TOP 3")

big3 %>%
  select(name,slams_titles,isBig3) %>%
  na.omit() %>%
  ggplot(aes(x=reorder(name, -slams_titles),y=slams_titles,fill = isBig3)) +
  geom_bar(stat="identity",show.legend = F) +
  labs(x="Player",y="Grand Slam Titles", title = "Grand Slams won by TOP 3 players") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_fill_manual(values=c("lightgreen","red"))

#--------------------------------------------------------------------------------
# Top phases in Grand Slams

all_players %>%
  select(name,slams_titles,slams_finals,slams_semi) %>%
  replace(is.na(.), 0) %>%
  mutate(sum = slams_titles+slams_finals+slams_semi) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  select(-sum) %>%
  rename(A_title=slams_titles) %>%
  pivot_longer(-name , names_to = "phase", values_to = "count") %>%
  ggplot(aes(fill=factor(phase),x=reorder(name,-count),y=count)) +
  geom_bar(position="stack", stat="identity") +
  labs(x="Player",y="Count", title = "Top phases in Grand Slams") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_discrete(name = "Phase", labels = c("Title", "Final", "Semifinal"))

all_players %>%
  replace(is.na(.), 0) %>%
  mutate(
    titles=masters_titles+tour_titles+olympics_titles,
    finals=masters_finals+tour_finals+olympics_finals,
    semi=masters_semi+tour_semi+olympics_semi) %>%
  select(name,titles,finals,semi) %>%
  mutate(sum = titles+finals+semi) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  select(-sum) %>%
  rename(Atitle=titles) %>%
  pivot_longer(-name , names_to = "phase", values_to = "count") %>%
  ggplot(aes(fill=factor(phase),x=reorder(name,-count),y=count)) +
  geom_bar(position="stack", stat="identity") +
  labs(x="Player",y="Count", title = "Top phases in Masters + Olympics + Tour") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_discrete(name = "Phase", labels = c("Title", "Final", "Semifinal"))


# all tournaments 

all_players %>%
  replace(is.na(.), 0) %>%
  mutate(
    titles=masters_titles+tour_titles+olympics_titles+slams_titles,
    finals=masters_finals+tour_finals+olympics_finals+slams_finals,
    semi=masters_semi+tour_semi+olympics_semi+slams_semi) %>%
  select(name,titles,finals,semi) %>%
  mutate(sum = titles+finals+semi) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  select(-sum) %>%
  rename(Atitle=titles) %>%
  pivot_longer(-name , names_to = "phase", values_to = "count") %>%
  ggplot(aes(fill=factor(phase),x=reorder(name,-count),y=count)) +
  geom_bar(position="stack", stat="identity") +
  labs(x="Player",y="Count", title = "Top phases in all competitions") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_fill_discrete(name = "Phase", labels = c("Title", "Final", "Semifinal"))

#--------------------------------------------------------------------------------
# Creating a function
  
points_function <- function(df) {
  df <- df %>%
    replace(is.na(.), 0) %>%
    mutate(total_points = 
      slams_semi*4 + slams_finals*5 +slams_titles*6 +
      masters_semi*1 + masters_finals*2 + masters_titles*3 +
      olympics_semi*3 + olympics_finals*4 + olympics_titles*5 +
      tour_semi*2 + tour_finals*3 + tour_titles*4 
    ) %>%
    select(name,total_points) %>%
    arrange(desc(total_points)) 
  
  return(df)
  
}


points = points_function(all_players)
points %>%
  head(20) %>%
  ggplot(aes(x=total_points,y=name)) +
  geom_bar(stat="identity", fill="blue") +
  labs(x = "Total Points", y = "Player", title = "Most competition points")

head(points, 10)

#-------------------------------------------------------------------------------
# LOADING THE DATA FOR STATISTICS ANALYSIS

grass <- read.csv("stats/matches_wonperc_GRASS.csv")
hard <- read.csv("stats/matches_wonperc_HARD.csv")
clay <- read.csv("stats/matches_wonperc_CLAY.csv")
aces <- read.csv("stats/aces_per_set.csv")
sets <- read.csv("stats/sets_won_perc.csv")
serve <- read.csv("stats/average_first_serve_speed.csv")
servewon <- read.csv("stats/perc_serve_won.csv")
bestplayers <- read.csv("stats/best_players.csv")

#--------------------------------------------------------------------------------

Grass <- grass%>%
  mutate(value_grass=as.numeric(sub("%","", value)))%>%
  select(name, value_grass)

Hard <- hard%>%
  mutate(value_hard=as.numeric(sub("%","", value)))%>%
  select(name, value_hard)

Clay <- clay%>%
  mutate(value_clay=as.numeric(sub("%","", value)))%>%
  select(name, value_clay)

best_court_type <- Clay%>%
  inner_join(Grass, by = c("name" = "name"))%>%
  inner_join(Hard, by = c("name"="name"))%>%
  filter(!name %in% c("Roger Federer", "Rafael Nadal", "Novak Djokovic"))%>%
  mutate(total_matchperc = (value_clay+value_grass+value_hard)/3)%>%
  arrange(desc(total_matchperc))%>%
  head(10)

by_surface<- best_court_type %>%
  select(name, value_grass, value_clay, value_hard)%>%
  pivot_longer(-name,names_to = "surface", values_to = "count")

ggplot(by_surface, aes(x=name, y=count, fill=surface))+
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 25))+
  xlab("Player name")+
  ylab("% of matches won by surface type")+
  scale_fill_discrete(name="Surface type", labels=c("Clay","Grass","Hard"))


##% of sets won and average number of aces--------
Aces <- aces%>%
  mutate(value_aces=as.numeric(sub("%","", value)))%>%
  select(name, value_aces)

Sets <- sets%>%
  mutate(value_sets=as.numeric(sub("%","", value)))%>%
  select(name, value_sets)

best_aces <- Sets%>%
  inner_join(Aces, by = c("name" = "name"))%>%
  filter(!name %in% c("Roger Federer", "Rafael Nadal", "Novak Djokovic"))%>%
  arrange(desc(value_sets))%>%
  head(10)

ggplot(data=best_aces, aes(x=value_sets, y=value_aces))+
  geom_point(size=4, color="blue")+
  geom_text(aes(label = name), size=4, nudge_y = 0.15, nudge_x = 0.5)+ 
  xlab("% of sets won")+
  ylab("average number of aces per set")+
  ggtitle("% of sets won vs. average num of aces/set", subtitle="Is there a correlation between aces and % of sets won?")

#add regression line


##Serve max speed vs. % first serve won -----

Serve <- serve%>%
  mutate(mpkm=as.numeric(sub("km/h","", value)))%>%
  select(name, mpkm)
ServeWon <- servewon%>%
  mutate(won_value=as.numeric(sub("%","", value)))%>%
  select(name, won_value)

best_serve <- bestplayers%>%
  inner_join(Serve, by = c("name" = "name"))%>%
  inner_join(ServeWon, by = c("name"="name"))%>%
  filter(!name %in% c("Roger Federer", "Rafael Nadal", "Novak Djokovic"))

ggplot(data=best_serve, aes(x=won_value, y=mpkm))+
  geom_point(size=4, color="blue")+
  geom_text(aes(label = name), size=4, nudge_y = 0.9, nudge_x = 0.6)+ 
  ylab("average serve speed (km/h)")+
  xlab("% of first serve won")+
  ggtitle("average serve speed vs. % of first serves won", subtitle="Is there a relationship between serve speed and % of first serves won?")
