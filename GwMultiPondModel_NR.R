# Loading packages
library(Matrix)
library(deSolve)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(deSolve)
library(stats)
library(mc2d)
library(gridExtra)
library(cowplot)



###Pond Infection Sub-model Parameters
parms <- numeric() #parms needs to be a vector for other functions
parms["f"] = 0.001 #feeding rate of adults
parms["ɣ_N"] = 1/10 #feeding rate of nauplii relative to adults
parms["ɣ_J"] = 1/2 #feeding rate of juveniles relative to adults
parms["d_L1"] = 1 #background death of L1s
parms["b_m"] = 25 #birth rate of adults (only susceptibles), eggs per female per day
parms["c"] = 1/100 #inverse of carrying capacity (strength of competition among copepods)
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
#parms["WL"] = 1 #water level 
parms["e"] = 0 #conversion efficiency of copepod biomass into predator biomass
parms["W_0"] = 0.05 #number of worms initially per mammal host 
parms["k"] = 146 #number of steps in worm development (based on linear chain rule)
parms["r"] = 0.4 #rate of moving from one worm step to next
parms["n.pond"] = 5
parms["L.per.day"] = 0.2 #liters drank per day 
parms["sigma"] = 0.01 #probability of infection from exposure in mammals 
parms["cop.sigma"] = 0.01 #prob copepod gets infected given it eats GW L1 


#Water Level 
monthly_seq = c(0,0,0,0,0,1,3,4,3,0,0,0)
#water level vector 
WL = rep(monthly_seq, c(31,28,31,30,31,30,31,31,30,31,30,31)) 

#Pond ODE function
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
    dJsdt = m_N*N - (m_J+d_J)*Js - cop.sigma*f*ɣ_J*Js*L1 - a*a_j*Js*P/T2_denom
    dAsdt = m_J*Js - d_A*As - cop.sigma*f*As*L1 - a*As*P/T2_denom
    dJedt = cop.sigma*f*ɣ_J*Js*L1 - (d_J+v_J+l)*Je - a*a_j*Je*P/T2_denom
    dAedt = cop.sigma*f*As*L1 - (d_A+v_A+l)*Ae - a*Ae*P/T2_denom 
    dJidt = l*Je - (d_J+v_J)*Ji - a*a_j*Ji*P/T2_denom
    dAidt = l*Ae - (d_A+v_A)*Ai - a*Ai*P/T2_denom 
    dPdt  = e*a*(a_n*N + a_j*J + A)*P/T2_denom #- death
    result = c(dL1dt, dNdt, dJsdt, dAsdt, dJedt, dAedt, dJidt, dAidt, dPdt) 
    return(list(result))
  }
  )
}

#Initial pond conditions
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
  P = rep(0, times=n.pond) #no predators 
  Pond = 1:n.pond
  Group = 1:n.pond
  Volume = c(100, 90, 80, 70, 60, 50, 40, 30, 20,10) #how to generalize this more?
  Elevation = 0:(n.pond-1)
  data.frame(time, L1, N, Js, As, Je, Ae, Ji, Ai, P,Pond,Group,Volume,Elevation)
}

###Initialize Hosts
Initialize_Hosts = function(N.human,N.dog,parameters=parms){
  
  # Compile human statistics
  human.stats = cbind("ID" = 1:N.human,
                      matrix(0, nrow=N.human, ncol=(parameters["k"]-1), dimnames = list(c(), paste0(rep("J", times=(parameters["k"]-1)), 1:(parameters["k"]-1)))), 
                      "Adults" = c(1, rep(0,times=(N.human-1))), "t" = rep(0, times=N.human))  #initial infection(s) in humans, only 1
  
  
  # Compile dog statistics
  dog.stats = cbind("ID" = 1:N.dog,
                    matrix(0, nrow=N.dog, ncol=(parameters["k"]-1), dimnames = list(c(), paste0(rep("J", times=(parameters["k"]-1)), 1:(parameters["k"]-1)))), 
                    "Adults" = c(1, rep(0,times=(N.dog-1))), "t" = rep(0, times=N.dog)) #only 1 initial infection
  
  #host preferences 
  human.prefs = matrix(data=1/(parameters["n.pond"]),nrow=N.human,ncol=parameters["n.pond"])
  dog.prefs = matrix(data=1/(parameters["n.pond"]),nrow=N.dog,ncol=parameters["n.pond"])
  
  # Return all starting statistics
  list("Humans" = human.stats, "Dogs" = dog.stats, "Human.prefs" = human.prefs, "Dog.prefs" = dog.prefs)
}


#Mammal Infection 
mammal_infection = function(n.pond, sigma, inf_copes, preferences, L.per.day){
  sum(rpois(n=n.pond, lambda = sigma*pmax(inf_copes)*L.per.day*preferences)) #for one individual, total copepods they are infected by
}

#mammal_infection(n.pond = 10,sigma=0.1,inf_copes = 1:times=10,preferences = rep(0.1,times=10), L.per.day = 5)

#Worm Development Function 
Worm_development = function(Human.stats, parameters, kill.Adults=TRUE){
  new_Human.stats = Human.stats
  for(i in 1:dim(Human.stats)[1]){  #how many rows of humans?
    juveniles = Human.stats[i,2:parameters["k"]]
    Adults = Human.stats[i,(parameters["k"]+1)]
    juv.develop = rbinom(n = length(juveniles), size = juveniles, prob = parameters["r"])
    new.juveniles = juveniles - juv.develop
    new.juveniles[2:(parameters["k"] - 1)] = new.juveniles[2:(parameters["k"]- 1)] + juv.develop[1:(parameters["k"]- 2)]
    #new.Adults = Adults + juv.develop[parameters["k"]- 1]
    new.Adults = ifelse(kill.Adults,0,Adults) + juv.develop[parameters["k"]- 1]
    new_Human.stats[i, 2:(parameters["k"]+1)] = c(new.juveniles, new.Adults)
  }
  new_Human.stats[,"t"] = new_Human.stats[,"t"] +1
  new_Human.stats
}

#Egg Deposition
Egg_Deposition = function(stats,n.pond,pond_probs){
  
  worms = stats[,"Adults"]
    
  #which ponds did worms go to from each person?
  #rmultinomial(n=length(worms),size=worms,prob=pond_probs) #n = run through each individual, size = number of things needed to be sorted into ponds
  
  #how many worms went into each pond?
  #colSums(rmultinomial(n=length(worms),size=worms,prob=pond_probs))  
  
  #how many eggs were deposited in each pond?
  rnbinom(n=n.pond, mu = 1e6*colSums(rmultinomial(n=length(worms),size=worms,prob=pond_probs)),size=80) #the higher the size value, the less spread
  
}

#Egg_Deposition(stats=result$Humans[[1]],n.pond=10, pond_probs=c(0.025,0.025,0.05,0.05,0.05,0.1,0.1,0.1,0.2,0.3))

#Host Exposure
Exposure = function(ponds,parameters,hosts){
  n.hosts = dim(hosts)[1]
  rpois(n=n.ponds,)
}


#Initial Landscape Conditions Function
GWABM = function(n.pond,timespan,WaterLevel,N.human,N.dog,parameters=parms){
  initials = initial_conditions(n.pond)
  results = initials
  hosts = Initialize_Hosts(N.human,N.dog,parameters)
  Humans = hosts["Humans"]
  Dogs = hosts["Dogs"]
  

    if(length(WaterLevel)!=length(timespan))stop("length of timespan and water level needs to match.")
  
  #double forloop, runs through a list of ponds with different initial conditions
  for (t in timespan) {
    time_step_data <- data.frame()# Create an empty data frame for this time step
    last_step <- subset(results, time == t-1) #define last step of results
    merged <- subset(last_step, Elevation <= WL[(t %% 365) + 1]) #define merged
    #parms[['WL']] = WaterLevel[t] #define water level at every given time step
    
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
    
    #Allow hosts to deposit worms in water bodies on day 1 of the model
    New_L1s.H = Egg_Deposition(stats=Humans[[t]],n.pond=parms["n.pond"], pond_probs=rep(1/parms["n.pond"],times=parms["n.pond"])) #uniform probs for deposition for all ponds
    New_L1s.D = Egg_Deposition(stats=Dogs[[t]],n.pond=parms["n.pond"], pond_probs=rep(1/parms["n.pond"],times=parms["n.pond"]))
    New_L1s = New_L1s.H + New_L1s.D
    time_step_data[,"L1"] = time_step_data[,"L1"] + New_L1s/time_step_data[,"Volume"]
    
    #store the ODE results for the current time step
    results = rbind(results,time_step_data)  
    
    #allow juvenile worms to develop from time t to time t+1 in mammals
    Humans[[t + 1]] = Worm_development(Humans[[t]],parms)
    Dogs[[t + 1]] = Worm_development(Dogs[[t]],parms)
    
    #allow mammal hosts to be exposed and add more J1s (first of 146 steps in host)
    Humans[[t + 1]][,"J1"] = Humans[[t + 1]][,"J1"] + apply(X=hosts$Human.prefs,MARGIN = 1,FUN = mammal_infection,n.pond=parms["n.pond"],sigma=parms["sigma"],inf_copes=pond_data[,"Ai"]+pond_data[,"Ji"],L.per.day=parms["L.per.day"]) 
    Dogs[[t + 1]][,"J1"] = Dogs[[t + 1]][,"J1"] + apply(X=hosts$Dog.prefs,MARGIN = 1,FUN = mammal_infection,n.pond=parms["n.pond"],sigma=parms["sigma"],inf_copes=pond_data[,"Ai"]+pond_data[,"Ji"],L.per.day=parms["L.per.day"])
    
    }
  #results
  list(Ponds=results,Humans=Humans,Dogs=Dogs)
}

results_list <- list()
plot_list <- list()
n.sim = 1

for(i in 1:n.sim){
  #run simulation
  result = GWABM(n.pond=10,timespan=1:1000,WaterLevel=rep(0,times=1000),N.human=50,N.dog=50,parameters=parms)
  
  # Store the result
  results_list[[i]] <- result
  
  #Plot prev infected adults
  plot1 <- ggplot(data=result$Ponds,aes(x=time,y=(Ai/(As+Ae+Ai)),group=as.factor(Pond),color=as.factor(Pond)))+geom_line()+theme(legend.position = "none")
  
  print(range(result[["Ponds"]][["Ai"]]))
  
  #Plot average worms per person over time 
  forplotting = matrix(unlist(lapply(result$Humans,FUN=colMeans)),ncol=148,nrow=1001,byrow = TRUE)
  wormsperperson = as.data.frame(cbind(forplotting[,148],forplotting[,147]))
  plot2 <- ggplot(data=wormsperperson,aes(x=V1,y=V2))+geom_line()
  
  # Add the plots to the list
  plot_list[[i]] <- plot_grid(plot1, plot2, ncol = 1)
  
  print(range(wormsperperson$V2)) 
}

plot_grid(plotlist = plot_list, ncol = 5) #output is all simulation runs for all ponds 


#output for all time steps
result = GWABM(n.pond=5,timespan=1:1000,WaterLevel=rep(0,times=1000),N.human=50,N.dog=50,parameters=parms)

#plotting
ggplot(data=result$Ponds,aes(x=time,y=(Ai/(As+Ae+Ai)),group=as.factor(Pond),color=as.factor(Pond)))+geom_line() #prevelance of infected adults, too high atm

forplotting = matrix(unlist(lapply(result$Humans,FUN=colMeans)),ncol=148,nrow=1001,byrow = TRUE) 

plot(forplotting[,148],forplotting[,147],type="l") ##average worms per person over time

#behavior space function - factorial simulator
#section 543-577


########
#IN PROGRESS
#L.per.day, more liters = more infection
#higher competitive effects between copepods = less infection

GWABM_factorial_simulator = function(PoI,mins,maxs,levels,iterations,pars=parms){
  PoI_dataframe = data.frame(matrix(ncol=length(PoI),nrow=levels)) #create empty dataframe
  colnames(PoI_dataframe) = PoI 
  for(i in 1:length(PoI)){
    PoI_dataframe[,i] = seq(mins[i],maxs[i],length.out=levels)
  }
  PoI_set = expand.grid(PoI_dataframe) #doing factorials 
  #return(PoI_set)
  
  #run first parameter combination batch
  pars[PoI] = as.numeric(PoI_set[1,]) #will only update the values of the POIs, not all other parameters 
  result = GWABM(n.pond=5,timespan=1:1000,WaterLevel=rep(0,times=1000),N.human=50,N.dog=50,parameters=pars) #will only take local parameter values 
  return(list(result,pars)) #unlist and relist
}

P_o_I = c("L.per.day","c") 
mins_PoI = c(0.1,0.01)
maxs_PoI = c(10, 0.1)
n_levels_PoI = 5 #will run 5x5 simulations 

facSimOutput = GWABM_factorial_simulator(P_o_I,mins_PoI,maxs_PoI,n_levels_PoI,iterations=5,pars=parms)

str(facSimOutput)

facSimOutput[[2]] #return pars for current simulation 










PoI_vectors_L.per.day = seq(from=mins_PoI[1],to=maxs_PoI[1],length.out=n_levels_PoI)
PoI_vectors_L.per.day = seq(from=mins_PoI[2],to=maxs_PoI[2],length.out=n_levels_PoI)

expand.grid() #creates data frame that is a factorial cross of vectors 

mapply() #run through data frame rows 

#collect results 






















#target should be 0.001 - 0.01, which would be 1 infection per 100-1000 people. (0.1 - 1 %)
#Currently:
#no predators
#only one initial infection in dogs and humans


# #pattern matching - what can I tune? 
# copepod densities 100-1000L
# 
# Human prevelance - 0.1%, modify sigma or liters per day?
# Dog prev - 1%
# or try to get a prevelance for everyone between 0.1-1%
# 
# copepod infection prevelance - 0.1% in adults, also in juveniles? look at literature 
# 
# I can tune anything in parms or initial conditions
# start with one infection in humans and dogs
# c(1, rep(0,times=(N.human-1)) #first human gets infection, all others do not have infection (one initial infection)
# 
# plot(adults in humans over time)
# 
# set predators to zero 
# 
# make model stochastic on single time step basis 
# 
