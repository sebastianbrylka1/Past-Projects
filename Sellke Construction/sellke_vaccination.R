source("functionfile_vaccination.R") # function to change the infectious period

#SET THE PARAMETERS
beta = .8     # set the beta (rate of infection)
gamma = .4    # set the gamma (rate of recovery)
n = 1000      # population size
rho = .01     # percentage of the population initially infected 
scale = 1     # scale the results for bigger populations
V = 0         # number of vaccinated people at time 0
distribution = "exponential"  # distribution of randomly generated infectious periods, options: "exponential", "weibull", "gamma", constant (just a number)
vaccine_efficacy = .95  # vaccine does not prevent form infection, but increases the individual's threshold
lambda_v = 0.01          # instantaneous hazard of susceptible getting vaccinated


# DO NOT CHANGE
I = n * rho # set the number of infected at time 0
S = n - I # initial number of susceptible 
I_total = I * scale # variable to track the total number of infections
V_total = V
R = 0 # 0 recovered at time 0
constant = (beta/S) # constant needed to calculate infection times from threshold
time_of_previous_event = 0 # event is either recovery or infection
CIP = 0 # initial CIP
CVP = 0 # initial CVP
pressure = CIP + CVP
vaccinated_infected = 0 # track how many vaccinated people got infected


vt <- c(0) # create the function of time starting at time 0
vS <- c(S) # create functions to track the number of S, I and R at any time
vI <- c(I)
vR <- c(R)
vV <- c(V)

history <- data.frame(event_time = 0,event_type = NA, stringsAsFactors=FALSE) # create the data frame to track the history of epidemic
history[1,1] = 0
history[1,2] = "Start"



if (V > 0){ #needed to create an initial data frame of the whole population if there are any vaccinated subjects to begin with
  susceptible = c(1:S)
  vaccinated = c((S+1):(S+V))
  infected = c((S+V+1):(S+V+I))
  
  df_susceptible = data.frame(subject = susceptible, 
                              threshold = rexp(S, rate = 1), 
                              infectious_period = NA, 
                              infection_time = NA, recovery_time = NA, status = "S", stringsAsFactors=FALSE) 
  df_vaccinated = data.frame(subject = vaccinated, 
                              threshold = (rexp(V, rate = 1))/(1 - vaccine_efficacy), 
                              infectious_period = NA, 
                              infection_time = NA, recovery_time = NA, status = "V", stringsAsFactors=FALSE) 
  df_infectious = data.frame(subject = infected, 
                             threshold = NA, 
                             infectious_period = infectious_period_function(I,gamma,distribution),
                             infection_time = 0, recovery_time = 0, status = "I", stringsAsFactors=FALSE)
  
  df = rbind(df_susceptible, df_vaccinated)
  df = df[order(df$threshold),] #order the susceptibles and vaccinated by threshold 
  index_minimum_threshold = 1 #index of the lowest
  df_infectious$recovery_time = df_infectious$infection_time + df_infectious$infectious_period
  df = rbind(df, df_infectious) # combine 2 data frames into one
  
  rm(df_susceptible) # delete initial data frames and vectors used to create these data frames
  rm(df_infectious)
  rm(df_vaccinated)
  rm(infected)
  rm(susceptible)
  rm(vaccinated)
}
if (V == 0){ #needed to create an initial data frame of the whole population if there are not any vaccinated subjects to begin with
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
}

counter = 0 #count the length of the epidemics 

while( I > 0 && (S > 0 || V > 0)){ #epidemics last when there are still infected and susceptible people (vaccinated are still susceptible)
  minimum_threshold = df$threshold[index_minimum_threshold] # minimum threshold
  
  if (minimum_threshold > pressure){ # if it takes some time after infecting one person to infect another one
    time_of_next_event = ((minimum_threshold - pressure)/(I * constant +lambda_v)) + time_of_previous_event
  }
   
  time_of_next_recovery = as.numeric(min(subset(df, status == "I")$recovery_time)) # time of the next recovery among infected
  index_next_recovery = as.numeric(which(df$recovery_time == time_of_next_recovery))
  
  
  
  if  (time_of_next_event < time_of_next_recovery && (S > 0 || V > 0) ) { # if some person gets infected/vaccinated before the other recovers 
    probability = lambda_v
    random_number = as.numeric(rbinom(1, 1,1-probability))
    if( random_number == 1 ){ # if the person gets infected
      CIP = CIP + (constant * I * (time_of_next_event - time_of_previous_event)) # change CIP
      CVP = CVP + (lambda_v * (time_of_next_event - time_of_previous_event))
      pressure = CIP + CVP
      if(df$status[index_minimum_threshold] == "S"){ # if the infected person was susceptible
        S = S - 1
      }
      if(df$status[index_minimum_threshold] == "V"){ # if the infected person was vaccinated
        V = V - 1
        vaccinated_infected = vaccinated_infected + 1
      }
      I = I + 1 # one more infected
      I_total = I_total + 1*scale # one more infected in total
      df$infection_time[index_minimum_threshold] = time_of_next_event # set infection time for the person with minimum threshold
      df$infectious_period[index_minimum_threshold] = infectious_period_function(1,gamma,distribution) # generate infectious period
      df$recovery_time[index_minimum_threshold] = 
        as.numeric(df$infection_time[index_minimum_threshold]) + as.numeric(df$infectious_period[index_minimum_threshold]) # recovery time is the sum of infection time and infectious period
      df$status[index_minimum_threshold] = "I"
      time_of_previous_event = time_of_next_event # change of the time of previous event 
      history[nrow(history) + 1,] = c(time_of_next_event,"Infection") # new event in history data frame
    }
    if(random_number == 0 && df$status[index_minimum_threshold] == "S"){ # if the person gets vaccinated
      CIP = CIP + (constant * I * (time_of_next_event - time_of_previous_event)) # change CIP
      CVP = CVP + (lambda_v * (time_of_next_event - time_of_previous_event))
      pressure = CIP + CVP
      S = S - 1
      V = V + 1
      V_total = V_total + 1
      df$threshold[index_minimum_threshold] = (df$threshold[index_minimum_threshold]/(1 - vaccine_efficacy)) + CIP
      df$status[index_minimum_threshold] = "V"
      time_of_previous_event = time_of_next_event # change of the time of previous event
      history[nrow(history) + 1,] = c(time_of_next_event,"Vaccination") # new event in history data frame
    }
    if(S > 0 || V > 0){
      index_minimum_threshold = index_minimum_threshold_function(df)  
    }
  }
  if  (time_of_next_event > time_of_next_recovery) { # if some person recovers before the other gets infected  
    CIP = CIP + (constant * I * (as.numeric(time_of_next_recovery) - time_of_previous_event))
    CVP = CVP + (lambda_v * (time_of_next_event - time_of_previous_event))
    pressure = CIP + CVP
    I = I - 1 # one less infected
    R = R + 1 # one more recovered
    df$status[index_next_recovery] = "R"
    time_of_previous_event = time_of_next_recovery
    history[nrow(history) + 1,] = c(time_of_next_recovery,"Recovery") # new event in history data frame
  }
  vt <- append(vt,time_of_previous_event)
  vI <- append(vI,I)
  vR <- append(vR,R)
  vS <- append(vS,S)
  vV <- append(vV,V)
  if(time_of_previous_event > counter + 1){
    counter = counter + 1
  }
  
}

vS = vS * scale
vI = vI * scale
vR = vR * scale
vV = vV * scale


plot(vt,vS,type="l",col="blue",lwd=1,ylim = c(0,n*scale), xlab="Time", ylab="Number of people",main=c("Vaccination rate",lambda_v))
lines(vt,vI,type="l",col="red",lwd=1)
lines(vt,vR,type="l",col="green",lwd=1)
lines(vt,vV,type="l",col="purple",lwd=1)
legend("top", legend=c("Susceptible", "Infected", "Recovered","Vaccinated"),
       col=c("blue", "red", "green", "purple"), lty=1:3, cex=0.6)
