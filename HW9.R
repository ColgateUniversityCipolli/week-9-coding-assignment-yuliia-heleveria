################################################################################
# HW 9 R CODE
# YULIIA HELEVERIA
# MATH 240 - SPRING 2025
################################################################################

################################################################################
# Load libraries
################################################################################
#library(nleqslv)

################################################################################
# Load and clean data about precipitation in Madison County
################################################################################
dat.precip <- read_csv(file = "agacis.csv")

dat.precip.long <- dat.precip |>    
  dplyr::select(-Annual) |>                   # Remove annual column 
  pivot_longer(cols = c(Jan, Feb, Mar, Apr,   # pivot the column data into one col
                        May, Jun, Jul, Aug, 
                        Sep, Oct, Nov, Dec), 
               values_to = "Precipitation",   # store the values in Precipitation
               names_to = "Month") |>         # store the months in Month
  #switch 'M' to NA values and convert numbers to integers from Strings
  mutate(Precipitation = case_when(Precipitation == "M" ~ NA_character_,
                                   TRUE                 ~ Precipitation))|>
  mutate(Precipitation = as.numeric(Precipitation))


################################################################################
# Question 1a:  MLEs using a Gamma distribution
################################################################################
#Function to compute Maximum Likelihood
llgamma <- function(par, data, neg = F){
  alpha <- par[1] #get alpha and beta
  beta <- par[2]
  
  #compute log likelihood
  loglik <- sum(log(dgamma(x = data, shape = alpha, rate = beta)), na.rm = T)
  
  return(ifelse(neg, - loglik, loglik))
}

#Compute MLE 
MLE.gamma <- optim(par = c(1,1),
                   fn = llgamma,
                   data=dat.precip.long$Precipitation,
                   neg=T)

#extract alpha and beta
alpha.MLE <- MLE.gamma$par[1]
beta.MLE <- MLE.gamma$par[2]

################################################################################
# Question 2a:  MLEs using a Log-Normal distribution
################################################################################
#Function to compute Maximum Likelihood
lllognorm <- function(par, data, neg = F){
  mu <- par[1] #get mu and sigma
  sigma <- par[2]
  
  #compute log likelihood
  loglik <- sum(log(dlnorm(x = data, meanlog = mu, sdlog = sigma)), na.rm = T)
  
  return(ifelse(neg, - loglik, loglik))
}

#Compute MLE 
MLE.lognorm <- optim(par = c(1,1),
                   fn = lllognorm,
                   data=dat.precip.long$Precipitation,
                   neg=T)

#extract alpha and beta
mu.MLE <- MLE.lognorm$par[1]
sigma.MLE <- MLE.lognorm$par[2]

################################################################################
# Question 3a: Compare the Weibull and the Gamma distribution
################################################################################
