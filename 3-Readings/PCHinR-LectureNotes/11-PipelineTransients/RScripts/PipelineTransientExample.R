##########################################################################################
# Pipeline Transients using Explicit Finite Differences (linearized formulation)         #
##########################################################################################
rm(list=ls()) # deallocate memory
######## prototype functions ###############
celerity <- function(density,elasticity_fluid,elasticity_solid,diameter,thickness){
  temp1<- 1.0/elasticity_fluid
  temp2<- diameter/(elasticity_solid*thickness)
  temp3<- temp1 + temp2
  temp4<- density*temp3
  celerity <- sqrt(1.0/temp4)
  return(celerity)
}
### Simulator Code ##############################
# Simulation Conditions (this section could be replaced with an input file)
# fluid properties
density <- 1000  #kg/m^3
elasticity_fluid <-   2.0e09 #Pa
elasticity_solid <- 21.0e09 #Pa
diameter <-  0.500 #m
thickness <- 0.004 #m
cc <- celerity(density,elasticity_fluid,elasticity_solid,diameter,thickness)
message("        Wave Celerity : ", cc)
# simulation properties
deltax <- 60 #meters
courantRatio <- 0.99 #select courant ratio to set time step.  If bigger than 1 unstable
deltat <- courantRatio*(deltax/cc) # force to be courant number for stability
# allocate head and velocity vectors, assign initial values
startHead <- 5.0 #meters
startVelo <- 9.9 #meters/second
pipeLength <- 6000 #meters
closeTime <- 1.0 #seconds
simulationDuration <- 300 #seconds
frictionFactor <- 0.016 #Moody Chart
cellCount <- as.integer((pipeLength/600)+1)
head <- numeric(0)
velocity <- numeric(0)
for(i in 1:cellCount){
      head[i]<-startHead
  velocity[i]<-startVelo
}
# useful constants
dtdx <- deltat/deltax
cc2g <- cc^2/9.81
do2 <- 1.0/(diameter*2) #used when friction included
# allocate some output vectors
etime<-numeric(0)
headvalve<-numeric(0)
velocitytank<-numeric(0)
# simulation values
etime[1] <- 0
headvalve[1] <- head[cellCount]
velocitytank[1] <-velocity[1]
######################
# Time Stepping Loop #
######################
maxiter <- 1+simulationDuration/deltat
for(itime in 2:maxiter){
  etime[itime]<-etime[itime-1]+deltat
###### valve closure model ##############
  closeRatio <- 1.0 - (etime[itime]/closeTime)
  if(closeRatio >= 0.0){
  velocity[cellCount] <- closeRatio*sqrt(2*9.8*(head[1]))
  }
  else{
  velocity[cellCount] <- 0
  }
####### update velocity #########
for(i in 1:(cellCount-1)){
  friction <- frictionFactor*velocity[i]*abs(velocity[i])*do2
  velocity[i]=velocity[i]-9.81*dtdx*(head[i+1]-head[i])-deltat*friction
}
####### update head #########
for(i in 2:(cellCount)){
  head[i]=head[i]-cc2g*dtdx*(velocity[i]-velocity[i-1])
}
headvalve[itime]<-head[cellCount]
velocitytank[itime] <-velocity[1]
}
# report results
message("Maximum head at valve : ",max(headvalve))
message("Minimum head at valve : ",min(headvalve))
# plot results
par(mfrow=c(2,1))
plot(etime,headvalve,type="l",pch=19,lwd=3,tck=1,xlab="Time (seconds)",ylab="Head at Valve (meters)",col="red")
plot(etime,velocitytank,type="l",pch=19,lwd=3,tck=1,xlab="Time (seconds)",ylab="Velocity at Tank (meters/sec)",col="blue")