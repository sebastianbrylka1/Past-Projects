source("functionfile_no_vaccination.R") # function to change the infectious period
S = 1000 # set the number of susceptible at time 0
I = 1 # set the number of infected at time 0
n = S + I # all subjects in the simulation
I_total = I # variable to track the total number of infections
beta = .3  # set the beta
gamma = .15 # set the gamma
R = 0 # 0 recovered at time 0
constant = (beta/S) # constant needed to calculate infection times from threshold
time_of_previous_event = 0 # event is either recovery or infection
CIP = 0 # initial CIP 
distribution = "exponential"

vt <- c(0) # create the function of time starting at time 0
vS <- c(S) # create functions to track the number of S, I and R at any time
vI <- c(I)
vR <- c(R)

history <- data.frame(event_time = 0,event_type = NA, stringsAsFactors=FALSE) # create the data frame to track the history of epidemic
history[1,1] = 0
history[1,2] = "Start"

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
    history[nrow(history) + 1,] = c(time_of_next_infection,"Infection") # new event in history data frame
    time_of_previous_event = time_of_next_infection # change of the time of previous event 
    index_minimum_threshold = index_minimum_threshold + 1
  }else{ # if some person recovers before the other gets infected  
    CIP = CIP + (constant * I * (as.numeric(time_of_next_recovery) - time_of_previous_event))
    repeat{
      I = I - 1 # one less infected
      R = R + 1 # one more recovered
      df$status[index_next_recovery] = "R"
      time_of_previous_event = time_of_next_recovery
      history[nrow(history) + 1,] = c(time_of_next_recovery,"Recovery") # new event in history data frame
      index_next_recovery = index_next_recovery[-length(index_next_recovery)]
      if (vector.is.empty(index_next_recovery)){
        break
      }
    }
  }
  vt <- append(vt,time_of_previous_event)
  vI <- append(vI,I)
  vR <- append(vR,R)
  vS <- append(vS,S)
}


plot(vt,vS,type="l",col="blue",lwd=1,ylim = c(0,n), xlab="Time", ylab="Number of people",main=distribution)
lines(vt,vI,type="l",col="red",lwd=1)
lines(vt,vR,type="l",col="green",lwd=1)
legend("top", legend=c("Susceptible", "Infected", "Recovered"),
       col=c("blue", "red", "green"), lty=1:3, cex=0.6)
