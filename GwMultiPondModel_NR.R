# Loading packages
library(Matrix)
library(deSolve)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(deSolve)
library(stats)


###Pond Infection Sub-model Parameters
parms <- numeric() #parms needs to be a vector for other functions
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
parms["k"] = 146
parms["r"] = 0.4


### --------------------Aquatic Model ODE--------------------
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

#Initial pond conditions (is there a more generalized way to do this?)
initial_conditions = function(n.pond){
  #Pond = paste("Pond", 1:n.pond)
  time = rep(0, times=n.pond)
  L1 = rep(0, times=n.pond)
  N = rpois(n=n.pond, lambda = 500)
  Js = rpois(n=n.pond, lambda = 300)
  As = rpois(n=n.pond, lambda = 200)
  Je = rep(0, times=n.pond)
  Ae = rep(0, times=n.pond)
  Ji = rep(0, times=n.pond)
  Ai = rep(0, times=n.pond)
  P = rep(1, times=n.pond)
  Pond = 1:n.pond
  Group = 1:n.pond
  Volume = c(100, 90, 80, 70, 60, 50, 40, 30, 20,10)
  Elevation = 0:(n.pond-1)
  data.frame(time, L1, N, Js, As, Je, Ae, Ji, Ai, P,Pond,Group,Volume,Elevation)
}

###Initialize Hosts
Initialize_Hosts = function(N.human,N.dog,parameters=parms){
  
  # Compile human statistics
  human.stats = cbind("ID" = 1:N.human,
                      matrix(0, nrow=N.human, ncol=(parameters["k"]-1), dimnames = list(c(), paste0(rep("J", times=(parameters["k"]-1)), 1:(parameters["k"]-1)))), 
                      "Adults" = rpois(n=N.human, lambda = as.numeric(parms["W_0"])), "t" = rep(0, times=N.human))
  
  
  # Compile dog statistics
  dog.stats = cbind("ID" = 1:N.dog,
                    matrix(0, nrow=N.dog, ncol=(parameters["k"]-1), dimnames = list(c(), paste0(rep("J", times=(parameters["k"]-1)), 1:(parameters["k"]-1)))), 
                    "Adults" = rpois(n=N.dog, lambda = as.numeric(parms["W_0"])), "t" = rep(0, times=N.dog))
  
  # Return all starting statistics
  list("Humans" = list(human.stats), "Dogs" = list(dog.stats))
}

#Initialize_Hosts(3,3)

#Worm Development Function 
Worm_development = function(Human.stats, parameters){
  new_Human.stats = Human.stats
  for(i in 1:dim(Human.stats)[1]){  #how many rows of humans?
    juveniles = Human.stats[i,2:parameters["k"]]
    Adults = Human.stats[i,(parameters["k"]+1)]
    juv.develop = rbinom(n = length(juveniles), size = juveniles, prob = parameters["r"])
    new.juveniles = juveniles - juv.develop
    new.juveniles[2:(parameters["k"] - 1)] = new.juveniles[2:(parameters["k"]- 1)] + juv.develop[1:(parameters["k"]- 2)]
    new.Adults = Adults + juv.develop[parameters["k"]- 1]
    new_Human.stats[i, 2:(parameters["k"]+1)] = c(new.juveniles, new.Adults)
  }
  new_Human.stats[,"t"] = new_Human.stats[,"t"] +1
  new_Human.stats
}

#Initial Landscape Conditions Function
GWABM = function(n.pond,timespan,WaterLevel,N.human,N.dog,parameters=parms){
  initials = initial_conditions(n.pond)
  results = initials
  hosts = Initialize_Hosts(N.human,N.dog,parameters)
  Humans = hosts[["Humans"]]
  Dogs = hosts[["Dogs"]]
  
  if(length(WaterLevel)!=length(timespan))stop("length of timespan and water level needs to match.")
  #double forloop, runs through a list of ponds with different initial conditions
  for (t in timespan) {
    time_step_data <- data.frame()# Create an empty data frame for this time step
    last_step <- subset(results, time == t-1)
    merged <- subset(last_step, Elevation <= parms[['WL']])
    parms[['WL']] = WaterLevel[t]
    
    for(p in 1:dim(merged)[1]){
      last_step[p,2:10] <- apply(X=merged[,2:10],MARGIN = 2,FUN = weighted.mean, w = merged$Volume)
    }
    
    for (i in 1:n.pond) {
      inits <- as.numeric(last_step[i,2:10])
      names(inits) <- colnames(last_step)[2:10]
      result <- data.frame(ode(y = inits, times = (t-1):t, parms = parms, func = GW_model, method = "lsoda", rtol=1e-4, atol=1e-6))
      
      # Extract the data for this pond at the current time step
      pond_data <- result[result$time == t, ]
      
      #Add constant columns
      pond_data[,"Pond"] <- initials$Pond[i]
      pond_data[,"Group"] <- initials$Group[i]
      if(pond_data[,"Group"] %in% merged$Pond){
        pond_data[,"Group"] <- 1
      }
      pond_data[, "Volume"] <- initials$Volume[i]
      pond_data[, "Elevation"] <- initials$Elevation[i] 
      
      # Add the pond data to the time_step_data data frame
      time_step_data <- rbind(time_step_data, pond_data)
      
    }
    #store the ODE results for the current timestep
    results = rbind(results,time_step_data)  
    
   #information about humans is a function of last time step (worm development)
  #same for dogs (worm development)
  #infection step that can add juveniles to first stage (exposure step)
  }
  results
}

#output for each timestep
result = GWABM(n.pond=10,timespan=1:10,WaterLevel=1:10)


###run ABM
runHosts = function(N.human = 5, N.dog = 10, parameters = parms){
  
  #set up initial conditions
  state = Initialize_ABM(N.human,N.dog)
  
  #run ABM
  for(tick in timespan){
    
    #let worms develop in humans and dogs 
    state$Humans[[tick+1]] = Worm_development(state$Humans[[tick]],parms)
    state$Dogs[[tick+1]] = Worm_development(state$Dogs[[tick]],parms)
    
  #   #have humans and dogs make contact with water bodies and deposit worms and/or pick up new infections, this step needs to interact with ODE
  #   #somehow add L1s to GW_model
  #   
  #   if(state$Humans == Adults){   #structure not quite right
  #     #deposit L1 larvae in random pond
  #     #exposed to L3 larvae in copepods if pond contains any
  #   } else{
  #     #wander around landscape and make contact with ponds at some rate, exposed to L3 larvae in copepods if pond contains any
  #   }
  #   
  #   if(state$Dogs == Adults){   #structure not quite right
  #     #deposit L1 larvae in random pond
  #     #exposed to L3 larvae in copepods if pond contains any
  #   } else{
  #     #wander around landscape and make contact with ponds at some rate, exposed to L3 larvae in copepods if pond contains any 
  #   }
  #   
  #   #update pond ODE
  }
  state
}

runHosts(10,10)$Dogs[[10]]

Worm_development(Initialize_ABM(3,3)$Humans[[1]],parms) #runs the model for 1 day

Initialize_Hosts(3,3)$Humans[[1]]

