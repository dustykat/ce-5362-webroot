##########################################################################################
# Pipeline Transients (lineraized) using Explicit Finite Differences (without upwinding) #
# T.G. Cleveland (ported from 1986 FORTRAN)                                              #
##########################################################################################
rm(list=ls()) # deallocate memory
#######################
# prototype functions #
#######################
celerity <- function(density,elasticity_fluid,elasticity_solid,diameter,thickness){
  temp1<- 1.0/elasticity_fluid
  temp2<- diameter/(elasticity_solid*thickness)
  temp3<- temp1 + temp2
  temp4<- density*temp3
  celerity <- sqrt(1.0/temp4)
  return(celerity)
}
# # Testing #
# density <- 1000  #kg/m^3
# elasticity_fluid <-   2.0e09 #Pa
# elasticity_solid <- 200.0e09 #Pa
# diameter <-  0.500 #m
# thickness <- 0.004 #m
# #
# print(celerity(density,elasticity_fluid,elasticity_solid,diameter,thickness))
# # Expect value of 942 m/sec #
#################################################
### Simulator Code ##############################
#################################################
# Simulation Conditions (this section could be replaced with an input file)
# fluid properties
density <- 1000  #kg/m^3
elasticity_fluid <-   2.0e09 #Pa
elasticity_solid <- 200.0e09 #Pa
diameter <-  0.500 #m
thickness <- 0.004 #m
cc <- celerity(density,elasticity_fluid,elasticity_solid,diameter,thickness)
print(cc)
# simulation properties
deltax <- 600 #meters
deltat <- 1.0*(deltax/cc) # force to be 1/2 courant number for stability
# allocate head and velocity vectors, assign initial values
startHead <- 5.0 #meters
startVelo <- 9.9 #meters/second
pipeLength <- 6000 #meters
closeTime <- 2.0 #seconds
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
# allocate some output vectors
etime<-numeric(0)
headvalve<-numeric(0)
velocitytank<-numeric(0)
# simulation values
etime[1] <- 0
headvalve[1] <- head[cellCount]
velocitytank[1] <-velocity[1]
#debug
#print(paste("time = ",etime[1]))
#print(cbind(head,velocity))
######################
# Time Stepping Loop #
######################
for(itime in 2:401){
  etime[itime]<-etime[itime-1]+deltat
# set valve
# instant close for testing
# velocity[cellCount] <- 0
# valve closure model
  velocity[cellCount] <- (1.0)*sqrt(2*9.8*head[1])
  closeRatio <- 1.0 - (etime[itime]/closeTime)
#  print(closeRatio)
  if(closeRatio >= 0.0){
  velocity[cellCount] <- closeRatio*sqrt(2*9.8*head[1])
  }
  else{
  velocity[cellCount] <- (0.0)*sqrt(2*9.8*head[1])
  }
# update velocity
for(i in 1:(cellCount-1)){
  velocity[i]=velocity[i]-9.81*dtdx*(head[i+1]-head[i])
}
# update head
for(i in 2:(cellCount)){
  head[i]=head[i]-cc2g*dtdx*(velocity[i]-velocity[i-1])
}
#debug
#print(paste("time = ",etime[itime]))
#print(cbind(head,velocity))
headvalve[itime]<-head[cellCount]
velocitytank[itime] <-velocity[1]
}
# report results
message("Maximum head at valve : ",max(headvalve))
message("Minimum head at valve : ",min(headvalve))
# will use a plot call
par(mfrow=c(2,1))
plot(etime,headvalve,type="l",pch=19,lwd=3,tck=1,xlab="Time (seconds)",ylab="Head at Valve (meters)")
plot(etime,velocitytank,type="l",pch=19,lwd=3,tck=1,xlab="Time (seconds)",ylab="Velocity at Tank (meters/sec)")