library(dplyr)
library(purrr)
library(mosaic)
library(ggplot2)

path = "C:/Users/sebas/Desktop/stat226/Project 2" 
setwd(path)
data <- read.csv(file = 'bootstrap/all_seasons.csv')


num_simulations= 10000

point_difference <- function(vec,place1,place2){
  vec=sort(vec,decreasing = T)
  return(vec[place1]-vec[place2])
    
}

england = list("name" = "england","short" = "ENG", "size" = 20)
spain = list(name = "spain",short = "ESP", size = 20)
italy = list(name = "italy",short = "ITA", size = 20)
germany = list(name = "germany",short = "GER", size = 16)
france = list(name = "france",short = "FRA", size = 20)
countries = list(england = england,spain = spain,italy = italy,germany = germany,france = france)

for (country in countries){
  assign(country$name,
         1:num_simulations %>%
           map_dfr(
             ~data %>%
               filter(Country == country$short) %>%
               select(PtsPerMP) %>%
               slice_sample(n = country$size, replace = TRUE) %>%
               summarize(
                 range = diff(range(PtsPerMP)),
                 max = max(PtsPerMP),
                 min = min(PtsPerMP),
                 sd = sd(PtsPerMP),
                 q25 = quantile(PtsPerMP)[2],
                 q75 = quantile(PtsPerMP)[4],
                 diff_1_2=point_difference(PtsPerMP,1,2),
                 diff_1_4=point_difference(PtsPerMP,1,4),
                 diff_4_10=point_difference(PtsPerMP,4,10),
                 diff_10_16=point_difference(PtsPerMP,10,16)
               )
             )
         )
}



england %>% ggplot() + 
  geom_density(data=spain, aes(x=diff_4_10, color = "esp"), size=2) +
  geom_density(data=france, aes(x=diff_4_10, color = "fra"), size=2) +
  geom_density(data=germany, aes(x=diff_4_10, color = "ger"), size=2) +
  geom_density(data=italy, aes(x=diff_4_10, color = "ita"), size=2) +
  geom_density(data=england, aes(x=diff_4_10, color = "eng"), size=2) + 
  scale_color_manual(name='Coutries',
                     values=c(eng='white', esp='red', fra='blue',ger='black',ita='green'))


favstats(england$diff_4_10)
favstats(france$diff_4_10)
favstats(germany$diff_4_10)
favstats(italy$diff_4_10)
favstats(spain$diff_4_10)





  
