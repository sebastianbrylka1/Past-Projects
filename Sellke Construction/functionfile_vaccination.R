

probability_function <- function(I_par,n_par,beta_par,lambda_v_par){
  lambdaI = beta_par*(I_par/n_par)
  lambdaV = lambda_v_par # instantaneous hazard of a susceptible getting vaccinated
  return (lambdaI/(lambdaI + lambdaV))
}

index_minimum_threshold_function <- function(df_par){
  smallest_value = (as.numeric(min(subset(df, status == "S" | status == "V")$threshold)))
  index = as.numeric(which(df$threshold == smallest_value))
  return(index)
}

infectious_period_function <- function(I_par, gamma_par, distribution){
  if(distribution == "exponential"){
    return (rexp(I_par, rate = gamma_par))
  }
  else if(distribution == "weibull") {
    return (rweibull(I_par, shape = 3.5, scale = 1/gamma_par))
  }
  else if(distribution == "gamma"){
    return (rgamma(I_par, scale = 1, shape = 1/gamma_par))
  }
  else{
    return (as.numeric(distribution))
  } 
}