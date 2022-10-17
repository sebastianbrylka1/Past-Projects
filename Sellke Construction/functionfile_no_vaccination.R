library(statsr)

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

vector.is.empty <- function(x) return(length(x) == 0 )       