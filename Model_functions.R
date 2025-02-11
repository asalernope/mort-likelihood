###########################################################
#  Disturbance - mortality in Balkans
#  Possible model functions
#  Dec 2024
###########################################################



# disturbance-mortality model

mort <- function(PS,sizeX0,sizeXb,gamma,alpha,beta,stand)
{ 
  size.effect <- exp(-0.5*(log((targets$dbh_cm-sizeXp)/sizeX0)/sizeXb)^2) 
  dist.effect <- exp(-gamma * targets$dist_time^alpha * targets$dist_severity^beta )
  logit <- PS[stand] * size.effect * dist.effect
  logit/(1+logit)
}


# time since disturbance = linear function
mort2 <- function(PS,sizeX0,sizeXb,a,b,alpha,beta,stand)
{ 
  size.effect <- exp(-0.5*(log((targets$dbh_cm-sizeXp)/sizeX0)/sizeXb)^2) 
  time <- a + b * targets$dist_time
  dist.effect <- exp(-alpha * time * targets$dist_severity^beta )
  logit <- PS[stand] * size.effect * dist.effect
  logit/(1+logit)
}


# separate size parameters for shaded vs high-light trees
mort3 <- function(PS,sizeX0,sizeXb,a,b,alpha,beta,stand,shade_class)
{ 
  size.effect <- exp(-0.5*(log((targets$dbh_cm-sizeXp)/sizeX0[shade_class])/sizeXb[shade_class])^2) 
  
  
  
  
  time <- a + b * targets$dist_time
  dist.effect <- exp(-alpha * time * targets$dist_severity^beta )
  logit <- PS[stand] * size.effect * dist.effect
  logit/(1+logit)
}
# define levels 
unique(targets$shading)
targets$shade_class <- ifelse(targets$shading == 99, 1, 0)
ns <- length(unique(targets$shade_class))


# This is my attempt at identifying the models for mortality 

mortality <- function(PS, )
{
  size <- exp(-0.5*ln(data$dbh_mm)/size_mod*[growth]) # my thought here is that growth would be a either a 1 for canopy or -1 for suppressed changing the sign of the function so that either the mortality increases with size for canopy trees or decreases with size for understory trees? - or is that solved because the size_mod would have two values - one for canopy and one for suppressed? 
  dist <- exp(-a*data$time_since^b*data$severity^c) # time increases more likely with h
  
  
}


The climate model uses an exponential function requiring three parameter estimations following the function outlined in Canham et al. (2018). The size function similarly follows the form provided in Canham et al. (2018) with the addition of different parameter estimations for canopy and suppressed trees. 


mort <- function(PS,sizeX0,sizeXb,a,b,c,plot)
{ 
  size <- exp(-0.5*(log(data$dbh_mm-sizeXp/sizeX0[status])/sizeXb[status])^2) # same as from canham class - you would estimate two sets of size parameters one for canopy and one for suppressed - log-normal; the sizeXp allows for movement for the intercept
  
  dist <- exp(-a*data$time_since^b*data$severity^c) 
  
  climate <- d*clim^e
  
  logit <- PS[plot] * size * dist * clim
  logit/(1+logit)
}
  
  # climatic water deficit - as CWD increases, so does likelihood of mortality 
  
  
  dist.effect <- exp(-gamma * targets$dist_time^alpha * targets$dist_severity^beta )
  logit <- PS[stand] * size.effect * dist.effect
  logit/(1+logit)
}

# Create an example dataset with similar structure to build these models off of? 

mort_random <- rbinom(320, 1, 0.7)
mort <- ifelse(mort_random == 0, "dead", "alive")

dbh_cm <- rnorm(n = 320, 40, 30)
dist_severity <- (seq(1800, 1900, length.out = 40))
  
  
  size.effect <- exp(-0.5*(log((targets$dbh_cm-sizeXp)/sizeX0)/sizeXb)^2)   
  
  
  dist_severity <- seq(1800, 1900, length.out = 30)

# Create a dataframe using expand.grid()
  
  targets<- expand.grid(dist_time = dist_time, dist_severity = dist_severity)

gamma <- 0.3
alpha <- 0.2
beta <- 0.4
dist.effect <- exp(-gamma * targets$dist_time^alpha * targets$dist_severity^beta )

plot(dist.effect)


sizeX0 <- 0.4
sizeXb <- 0.3

size.effect <- exp(-0.5*(log(dbh_cm)/sizeX0)/sizeXb)^2) 
