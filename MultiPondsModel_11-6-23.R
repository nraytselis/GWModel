# Loading packages
library(Matrix)
library(deSolve)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(deSolve)

####Parameters

#Pond Infection Sub-model Parameters
parms <- list()
parms["f"] = 0.001 #feeding rate of adults
parms["ɣ_N"] = 1/10 #feeding rate of nauplii relative to adults
parms["ɣ_J"] = 1/2 #feeding rate of juveniles relative to adults
parms["d_L1"] = 1 #background death of L1s
parms["b_m"] = 25 #birth rate of adults (only susceptibles), eggs per female per day
parms["c"] = 1/80 #inverse of carrying capacity
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
parms["WL"] = 0 #water level 


#Aquatic Model

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
    dPdt  = a*(a_n*N + a_j*J + A)*P/T2_denom #- death
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
  Pond7 = c(time=0, L1 = 0, N = 1000, Js = 1000, As = 1000, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 1),
  Pond8 = c(time=0, L1 = 0, N = 1000, Js = 1000, As = 1000, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 1),
  Pond9 = c(time=0, L1 = 0, N = 1000, Js = 1000, As = 1000, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 1),
  Pond10 = c(time=0, L1 = 0, N = 1000, Js = 1000, As = 1000, Je = 0, Ae = 0, Ji = 0, Ai = 0, P = 1)
))


# Create a separate data frame for columns not defined by ODE
nonODEcolumns <- data.frame(Pond = 1:10, Group = 1:10, Volume = c(100, 90, 80, 70, 60, 50, 40, 30, 20,10), Elevation = 0:9)
results <- cbind(initial_conditions, nonODEcolumns)

#double forloop, runs through a list of ponds with different initial conditions and saves data for all ponds for each timestep into a dataframe in a list
for (t in timespan) {
  time_step_data <- data.frame()# Create an empty data frame for this time step
  
  for (i in 1:dim(initial_conditions)[1]) {
    inits <- as.numeric(results[((t-1)*10+i),2:10])
    names(inits) <- colnames(results)[2:10]
    result <- data.frame(ode(y = inits, times = (t-1):t, parms = parms, func = GW_model, method = "lsoda", rtol=1e-4, atol=1e-6))
    
    # Extract the data for this pond at the current time step
    pond_data <- result[result$time == t, ]
   
    #Add constant columns
    pond_data[,"Pond"] <- nonODEcolumns$Pond[i]
    pond_data[,"Group"] <- nonODEcolumns$Group[i]
    pond_data[, "Volume"] <- nonODEcolumns$Volume[i]
    pond_data[, "Elevation"] <- nonODEcolumns$Elevation[i] 
    
    # Add the pond data to the time_step_data data frame
    time_step_data <- rbind(time_step_data, pond_data)
    
    # #average the data for this pond at this time step with other ponds that have WL >= Elevation 
    # if(WL >= pond_data$Elevation){ #if WL >= elevation of Pond 2
    # weighted_average <- weighted.mean(time_step_data[, 2:10],Volume)  # Calculate weighted average of columns 2:10
    # replace(time_step_data,2:10,weighted_average)
    # }

  }
  results = rbind(results,time_step_data)  
}
results
