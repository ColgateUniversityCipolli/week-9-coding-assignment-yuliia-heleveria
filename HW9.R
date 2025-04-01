#why do i get that lognormal is the best

################################################################################
# HW 9 R CODE
# YULIIA HELEVERIA
# MATH 240 - SPRING 2025
################################################################################

################################################################################
# Load libraries
################################################################################
library(tidyverse)

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
# Question 1b:  MLEs using a Log-Normal distribution
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
# Question 1c: Compare the Weibull and the Gamma distribution
################################################################################
#compute log-likelihood for Gamma distribution
Gamma.loglik <- llgamma(par = c(alpha.MLE, beta.MLE), data = dat.precip.long$Precipitation, neg = T)

#compute the likelihood ratio
Weibull.loglik <- 2166.496
q.gamma.weibull <- exp(Weibull.loglik-Gamma.loglik)

################################################################################
# Question 1d: Compare the Weibull and the Log-Normal distribution
################################################################################
#compute log-likelihood for Log-Normal distribution
Lognorm.loglik <- lllognorm(c(mu.MLE, sigma.MLE), data = dat.precip.long$Precipitation, neg = T)

#compute the likelihood ratio
q.lognorm.webull <- exp(Weibull.loglik-Lognorm.loglik)

################################################################################
# Question 1e: Compare the Gamma and the Log-Normal distribution
################################################################################
#compute the likelihood ratio
q.gamma.lognorm <- exp(Gamma.loglik-Lognorm.loglik)

################################################################################
# Make a plot
################################################################################
#Get data for Weibull
llweibull <- function(par, data, neg=F){
  # a <- par[1]
  # sigma <- par[2]
  a <- exp(par[1]) # go from (-inf,inf) to (0,inf)
  sigma <- exp(par[2]) # go from (-inf,inf) to (0,inf)
  
  ll <- sum(log(dweibull(x=data, shape=a, scale=sigma)), na.rm=T)
  
  return(ifelse(neg, -ll, ll))
}

MLEs <- optim(fn = llweibull,
              par = c(1,1),
              data = dat.precip.long$Precipitation,
              neg=T)

(MLEs$par <- exp(MLEs$par)) # transform

#Data to plot
ggdat.weibull <- tibble(x = seq(0,15,length.out=1000)) |>
  mutate(pdf.mle = dweibull(x=x, shape=MLEs$par[1], scale=MLEs$par[2]))

ggdat.gamma <- tibble(x = seq(0,15,length.out=1000)) |>
  mutate(pdf.mle = dgamma(x=x, shape=alpha.MLE, rate=beta.MLE))

ggdat.lognorm <- tibble(x = seq(0,15,length.out=1000)) |>
  mutate(pdf.mle = dlnorm(x=x, meanlog =mu.MLE, sdlog = sigma.MLE))

#Make a histogram with MLEs of three distributions superimposed
ggplot() +
  geom_histogram(data=dat.precip.long,
                 aes(x=Precipitation, y=after_stat(density)),
                 breaks=seq(0, 15, 1),
                 color="lightgrey")+
  geom_line(data=ggdat.weibull,
            aes(x=x, y=pdf.mle, color="Weibull"))+
  geom_line(data=ggdat.gamma,
            aes(x=x, y=pdf.mle, color="Gamma"))+
  geom_line(data=ggdat.lognorm,
            aes(x=x, y=pdf.mle, color="Log-Normal"))+
  geom_hline(yintercept = 0)+
  theme_bw()+
  xlab("Precipitation (Inches)")+
  ylab("Density")+
  labs(color="")