simulation <- function(beta_par,gamma_par,B_par,T_par,distribution_par = "exponential"){
  
repeat{
  S_par = 200
  I_par = 10
  #beta_par = 1.5
  #gamma_par = .7
  #distribution_par = "exponential"
  #B_par = 100
  
  source("new_functionfile_abc.R") # function to change the infectious period
  S = S_par # set the number of susceptible at time 0
  I = I_par # set the number of infected at time 0
  n = S + I # all subjects in the simulation
  I_total = I # variable to track the total number of infections
  beta = beta_par # set the beta
  gamma = gamma_par # set the gamma
  R = 0 # 0 recovered at time 0
  constant = (beta/S) # constant needed to calculate infection times from threshold
  time_of_previous_event = 0 # event is either recovery or infection
  CIP = 0 # initial CIP 
  distribution = distribution_par
  
  susceptible = c(1:S)
  infected = c((S+1):(S+I))
  
  df_susceptible = data.frame(subject = susceptible, 
                              threshold = rexp(S, rate = 1), 
                              infectious_period = NA, 
                              infection_time = NA, recovery_time = NA, status = "S", stringsAsFactors=FALSE) 
  df_infectious = data.frame(subject = infected, 
                             threshold = NA, 
                             infectious_period = infectious_period_function(I,gamma,distribution),
                             infection_time = 0, recovery_time = 0, status = "I", stringsAsFactors=FALSE)
  
  df_susceptible = df_susceptible[order(df_susceptible$threshold),] #order the susceptibles by threshold 
  index_minimum_threshold = 1 #index of the lowest
  df_infectious$recovery_time = df_infectious$infection_time + df_infectious$infectious_period
  
  df = rbind(df_susceptible, df_infectious) # combine 2 data frames into one
  rm(df_susceptible) # delete initial data frames and vectors used to create these data frames
  rm(df_infectious)
  rm(infected)
  rm(susceptible)
  
  
  
  while( I > 0 && S > 0 ){
    minimum_threshold = df$threshold[index_minimum_threshold] # minimum threshold
    
    if (minimum_threshold > CIP){ # if it takes some time after infecting one person to infect another one
      time_of_next_infection = (minimum_threshold-CIP)/(constant * I) + time_of_previous_event # time of next infection as sum of previous event time and time to next infection
    }
    
    time_of_next_recovery = as.numeric(min(subset(df, status == "I")$recovery_time)) # time of the next recovery among infected
    index_next_recovery = as.numeric(which(df$recovery_time == time_of_next_recovery))
    
    if  (time_of_next_infection < time_of_next_recovery) { # if some person gets infected before the other recovers 
      CIP = CIP + (constant * I * (time_of_next_infection - time_of_previous_event)) # change CIP
      S = S - 1 # one less susceptible
      I = I + 1 # one more infected
      I_total = I_total + 1 # one more infected in total
      df$infection_time[index_minimum_threshold] = time_of_next_infection # set infection time for the person with minimum threshold
      df$infectious_period[index_minimum_threshold] = infectious_period_function(1,gamma,distribution) # generate infectious period
      df$recovery_time[index_minimum_threshold] = 
        as.numeric(df$infection_time[index_minimum_threshold]) + as.numeric(df$infectious_period[index_minimum_threshold]) # recovery time is the sum of infection time and infectious period
      df$status[index_minimum_threshold] = "I"
      time_of_previous_event = time_of_next_infection # change of the time of previous event 
      index_minimum_threshold = index_minimum_threshold + 1
    }else{ # if some person recovers before the other gets infected  
      CIP = CIP + (constant * I * (as.numeric(time_of_next_recovery) - time_of_previous_event))
      repeat{
        I = I - 1 # one less infected
        R = R + 1 # one more recovered
        df$status[index_next_recovery] = "R"
        time_of_previous_event = time_of_next_recovery
        index_next_recovery = index_next_recovery[-length(index_next_recovery)]
        if (vector.is.empty(index_next_recovery)){
          break
        }
      }
      
    }
    
  }
  df = na.omit(df)
  df = subset(df, df$infection_time<T_par)
  if(nrow(df)>=B_par){
    break
  }
}  
  return (sort(sample(df$infection_time,B_par)))
}
  #plot(sort(sample(df$infection_time,n)))
