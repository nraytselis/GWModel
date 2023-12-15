# Loading packages
library(Matrix)
library(deSolve)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(deSolve)
library(stats)


###Pond Infection Sub-model Parameters
parms <- list()
parms["f"] = 0.001 #feeding rate of adults
parms["ɣ_N"] = 1/10 #feeding rate of nauplii relative to adults
parms["ɣ_J"] = 1/2 #feeding rate of juveniles relative to adults
parms["d_L1"] = 1 #background death of L1s
parms["b_m"] = 25 #birth rate of adults (only susceptibles), eggs per female per day
parms["c"] = 1/100 #inverse of carrying capacity
parms["α_N"] = 1/10 #competitive effects of nauplii relative to adults
parms["α_J"] = 1/2 #competitive effects of juveniles relative to adults
parms["m_N"] = 1/5 #maturation rate of nauplii (1/days to next stage)
parms["d_N"] = 1/10 #background death rate of nauplii
parms["m_J"] = 1/5 #maturation rate of juveniles
parms["d_J"] = 1/25 #background death of juveniles
parms["v_J"] = 1/25 #virulent effect on juveniles
parms["d_A"] = 1/50 #background death of adults
parms["v_A"] = 1/50 #virulent effect on adults
parms["l"] = 1/15 #rate of infection maturation
parms["a_n"] = 1/10 #attach rate on nauplii
parms["a_j"] = 1/2 #attack rate on juveniles
parms["a"] = 0.01 #attack rate on adults by fish
parms["h_n"] = 1/10 #handling time of nauplii
parms["h_j"] = 1/2 #handling time of juveniles
parms["h"] = 1/50 #handling time of adults
parms["WL"] = 1 #water level 
parms["e"] = 0 #conversion efficiency of copepod biomass into predator biomass
parms["W_0"] = 1 #number of worms 


###Aquatic Model

GW_model = function(t,y,parameters){
  L1=y[1]; N=y[2];Js=y[3];As=y[4];Je=y[5];Ae=y[6];Ji=y[7];Ai=y[8];P=y[9]
  with(as.list(parameters),{
    #tidy up the presentation
    J=Js+Je+Ji #total abundance of juveniles
    A=As+Ae+Ai #total abundance of adults
    T2_denom = 1+a*h*(a_n*h_n*N+a_j*h_j*J+A) 
    
    #dynamics
    dL1dt = -f*(ɣ_N*N + ɣ_J*J + A)*L1 - d_L1*L1
    dNdt = b_m*(As/2)*exp(-c*(α_N*N + α_J*J + A)) - (m_N+d_N)*N - a*a_n*N*P/T2_denom
    dJsdt = m_N*N - (m_J+d_J)*Js - f*ɣ_J*Js*L1 - a*a_j*Js*P/T2_denom
    dAsdt = m_J*Js - d_A*As - f*As*L1 - a*As*P/T2_denom
    dJedt = f*ɣ_J*Js*L1 - (d_J+v_J+l)*Je - a*a_j*Je*P/T2_denom
    dAedt = f*As*L1 - (d_A+v_A+l)*Ae - a*Ae*P/T2_denom 
    dJidt = l*Je - (d_J+v_J)*Ji - a*a_j*Ji*P/T2_denom
    dAidt = l*Ae - (d_A+v_A)*Ai - a*Ai*P/T2_denom 
    dPdt  = e*a*(a_n*N + a_j*J + A)*P/T2_denom #- death
    result = c(dL1dt, dNdt, dJsdt, dAsdt, dJedt, dAedt, dJidt, dAidt, dPdt) 
    return(list(result))
  }
  )
}

# Define the time points you want to iterate through
timespan <- 1:10

# Create separate initial conditions for each pond
initial_conditions <- t(data.frame(
  River = c(time=0, L1 = 0, N = 1000, Js = 1000, As = 1000, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 1),
  Pond2 = c(time=0, L1 = 0, N = 800, Js = 800, As = 800, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 2),
  Pond3 = c(time=0, L1 = 0, N = 1200, Js = 1200, As = 1200, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 3),
  Pond4 = c(time=0, L1 = 0, N = 900, Js = 900, As = 900, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 4),
  Pond5 = c(time=0, L1 = 0, N = 1100, Js = 1100, As = 1100, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 5),
  Pond6 = c(time=0, L1 = 0, N = 1000, Js = 1000, As = 1000, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 1),
  Pond7 = c(time=0, L1 = 0, N = 1300, Js = 600, As = 8000, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 2),
  Pond8 = c(time=0, L1 = 0, N = 1500, Js = 5000, As = 1000, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 4),
  Pond9 = c(time=0, L1 = 0, N = 2000, Js = 1000, As = 9000, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 1),
  Pond10 = c(time=0, L1 = 0, N = 500, Js = 8000, As = 1000, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 3)
))


# Create a separate data frame for columns not defined by ODE
nonODEcolumns <- data.frame(Pond = 1:10, Group = 1:10, Volume = c(100, 90, 80, 70, 60, 50, 40, 30, 20,10), Elevation = 0:9)
results <- cbind(initial_conditions, nonODEcolumns)

#double forloop, runs through a list of ponds with different initial conditions and saves data for all ponds for each timestep into a dataframe in a list
for (t in timespan) {
  time_step_data <- data.frame()# Create an empty data frame for this time step
  last_step <- subset(results, time == t-1)
  merged <- subset(last_step, Elevation <= parms$WL)
  
  if(t==5){parms$WL=5} #change WL and t to reflect different flooding scenarios, change occurs after end of 5th timestep
  if(t==7){parms$WL=1}
  
for(p in 1:dim(merged)[1]){
  last_step[p,2:10] <- apply(X=merged[,2:10],MARGIN = 2,FUN = weighted.mean, w = merged$Volume)
}
  
  for (i in 1:dim(initial_conditions)[1]) {
    inits <- as.numeric(last_step[i,2:10])
    names(inits) <- colnames(last_step)[2:10]
    result <- data.frame(ode(y = inits, times = (t-1):t, parms = parms, func = GW_model, method = "lsoda", rtol=1e-4, atol=1e-6))
    
    # Extract the data for this pond at the current time step
    pond_data <- result[result$time == t, ]
   
    #Add constant columns
    pond_data[,"Pond"] <- nonODEcolumns$Pond[i]
    pond_data[,"Group"] <- nonODEcolumns$Group[i]
    if(pond_data[,"Group"] %in% merged$Pond){
      pond_data[,"Group"] <- 1
    }
    pond_data[, "Volume"] <- nonODEcolumns$Volume[i]
    pond_data[, "Elevation"] <- nonODEcolumns$Elevation[i] 
    
    # Add the pond data to the time_step_data data frame
    time_step_data <- rbind(time_step_data, pond_data)

  }
  results = rbind(results,time_step_data)  
}
results



###Human_Infection_function - based off Schisto model need to modify 

Human_Infection = function(human.stats, L3s, parameters){
  # Parameters
  epsilon_H = as.numeric(parameters["epsilon_H"])
  sigma_H = as.numeric(parameters["sigma_H"])
  ENV = as.numeric(parameters["ENV"])
  m_Z = as.numeric(parameters["m_Z"])
  step = as.numeric(parameters["step"])
  
  # Later calculations depend on exposure probabilities
  exp.rates =epsilon_H*(human.stats[,"A"]>0)/ENV # This gives uniform exposure rates for all humans
  sum.exp.rates = sum(exp.rates)
  
  # Probabilities for fate of L3s
  P.left.in.water = exp(-(m_Z+sum(exp.rates))*step)                             # Still in water - how likely in GW? should just die quickly?
  P.infects.this.human = (1 - P.left.in.water)*(sigma_H*exp.rates/(m_Z+sum.exp.rates))  # Infect a human
  # Die in water or fail to infect
  P.dead = (1 - P.left.in.water)*(m_Z/(m_Z+sum.exp.rates)) + sum((1 - P.left.in.water)*((1-sigma_H)*exp.rates/(m_Z+sum.exp.rates))) # die
  
  prob.vector = c(P.infects.this.human, P.left.in.water, P.dead)
  
  # Multinomial outcome
  rmultinom(n=1, size=L3s, prob=prob.vector)
}

###Dog Infection Function - based off Schisto model need to modify 
Human_Infection = function(dog.stats, L3s, parameters){
  # Parameters
  epsilon_H = as.numeric(parameters["epsilon_H"])
  sigma_H = as.numeric(parameters["sigma_H"])
  ENV = as.numeric(parameters["ENV"])
  m_Z = as.numeric(parameters["m_Z"])
  step = as.numeric(parameters["step"])
  
  # Later calculations depend on exposure probabilities
  exp.rates =epsilon_H*(dog.stats[,"A"]>0)/ENV # This gives uniform exposure rates for all humans
  sum.exp.rates = sum(exp.rates)
  
  # Probabilities for fate of L3s
  P.left.in.water = exp(-(m_Z+sum(exp.rates))*step)                             # Still in water
  P.infects.this.dog = (1 - P.left.in.water)*(sigma_H*exp.rates/(m_Z+sum.exp.rates))  # Infect a human
  # Die in water or fail to infect
  P.dead = (1 - P.left.in.water)*(m_Z/(m_Z+sum.exp.rates)) + sum((1 - P.left.in.water)*((1-sigma_H)*exp.rates/(m_Z+sum.exp.rates))) # die
  
  prob.vector = c(P.infects.this.dog, P.left.in.water, P.dead)
  
  # Multinomial outcome
  rmultinom(n=1, size=L3s, prob=prob.vector)
}


###Worm Development in Agents Function - Linear Chain Rule 
Worm_development = function(worm_state, parameters){
  juveniles = worm_state[1:(parameters["k"] - 1)]
  Adults = worm_state[parameters["k"]]
  # Which juveniles 
  juv.develop = rbinom(n = length(juveniles), size = juveniles, prob = parameters["r"])
  new.juveniles = juveniles - juv.develop
  new.juveniles[2:(parameters["k"] - 1)] = new.juveniles[2:(parameters["k"] - 1)] + juv.develop[1:(parameters["k"] - 2)]
  new.Adults = Adults + juv.develop[parameters["k"] - 1]
  return(c(new.juveniles, new.Adults))
}

parameters = c("k" = 146, "r" = 0.4)
worms = c(100, rep(0, times=parameters["k"] - 1))

day = 1
adults = 0

for(i in 1:730){
  worms = Worm_dev(worm_state = worms, parameters)
  adults[i] = worms[parameters["k"]]
  worms[parameters["k"]] = 0
  day[i] = i
}

plot(day, adults)
abline(v = 335); abline(v=395) #worms take ~365 days to develop
weighted.mean(x = day, w = adults)
#var(day, adults) #this doesn't seem right, how do I calculate? 

###Initialize ABM
Initialize_ABM = function(N.human,N.dog){
  
  # Compile human statistics
  human.stats = cbind("ID" = 1:N.human,
                      matrix(0, nrow=N.human, ncol=145, dimnames = list(c(), paste0(rep("J", times=145), 1:145))), 
                      "Adults" = rpois(n=N.human, lambda = as.numeric(parms["W_0"])), "t" = rep(0, times=N.human))
  
  
  # Compile dog statistics
  dog.stats = cbind("ID" = 1:N.dog,
                      matrix(0, nrow=N.dog, ncol=145, dimnames = list(c(), paste0(rep("J", times=145), 1:145))), 
                      "Adults" = rpois(n=N.dog, lambda = as.numeric(parms["W_0"])), "t" = rep(0, times=N.dog))
  
  # Return all starting statistics
  list("Humans" = list(human.stats), "Dogs" = list(dog.stats))
}

#Initialize_ABM(3,3) test 

###run ABM
run_ABM = function(N.human = 5, N.dog = 10){
  
  #set up initial conditions
  state = Initialize_ABM(N.human,N.dog)
  
  #run ABM
  for(tick in timespan){
    
    #let worms develop in humans and dogs 
    state$Humans[[tick+1]] = Worm_development(state$Humans[[tick]],parms)
    state$Dogs[[tick+1]] = Worm_development(state$Dogs[[tick]],parms)
    
    #have humans and dogs make contact with water bodies and deposit worms and/or pick up new infections, this step needs to interact with ODE
    #somehow add L1s to GW_model
    
    if(state$Humans == Adults){   #structure not quite right
      #deposit L1 larvae in random pond
      #exposed to L3 larvae in copepods if pond contains any
    } else{
      #wander around landscape and make contact with ponds at some rate, exposed to L3 larvae in copepods if pond contains any
    }
    
    if(state$Dogs == Adults){   #structure not quite right
      #deposit L1 larvae in random pond
      #exposed to L3 larvae in copepods if pond contains any
    } else{
      #wander around landscape and make contact with ponds at some rate, exposed to L3 larvae in copepods if pond contains any 
    }
    
    #update pond ODE
  }
}

run_ABM(10,10)
