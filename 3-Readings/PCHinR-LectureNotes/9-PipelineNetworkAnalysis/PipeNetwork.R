# Pipe Network Simulator Using Newton-Raphson
########################################################
########## Read the Input Data from a file #############
########################################################
zz <- file("PipeNetwork.txt", "r") # Open a connection named zz to file named PipeNetork.txt
nodes <- as.numeric(readLines(zz, n = 1, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
pipes <-as.numeric(readLines(zz, n = 1, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
loops <-as.numeric(readLines(zz, n = 1, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
diameter <- (readLines(zz, n = 1, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
distance <- (readLines(zz, n = 1, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
roughness <- (readLines(zz, n = 1, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
viscosity <- (readLines(zz, n = 1, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
flowguess <- (readLines(zz, n = 1, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
nodearcs <- (readLines(zz, n = nodes, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
looparcs <- (readLines(zz, n = loops, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
rhs_true <- (readLines(zz, n = loops, ok = TRUE, warn = TRUE,encoding = "unknown", skipNul = FALSE))
close(zz)
#############################################################
# convert the multiple column strings into numeric
#nodes <-as.numeric(unlist(strsplit(nodes,split=" ")))
#pipes <-as.numeric(unlist(strsplit(pipes,split=" ")))
#loops <-as.numeric(unlist(strsplit(loops,split=" ")))
diameter <-as.numeric(unlist(strsplit(diameter,split=" ")))
distance <-as.numeric(unlist(strsplit(distance,split=" ")))
roughness <-as.numeric(unlist(strsplit(roughness,split=" ")))
viscosity <-as.numeric(unlist(strsplit(viscosity,split=" ")))
flowguess <-as.numeric(unlist(strsplit(flowguess,split=" ")))
nodearcs <-as.numeric(unlist(strsplit(nodearcs,split=" ")))
looparcs <-as.numeric(unlist(strsplit(looparcs,split=" ")))
rhs_true <-as.numeric(unlist(strsplit(rhs_true,split=" ")))
# convert nodearcs and looparcs into matrices
nodearcs <-matrix(nodearcs,nrow=nodes,ncol=pipes,byrow = TRUE)
looparcs <-matrix(looparcs,nrow=loops,ncol=pipes,byrow = TRUE)
# echo input
# nodes 
# pipes 
# loops 
# diameter 
# distance 
# roughness 
# viscosity 
# flowguess
# nodearcs
# looparcs
# rhs_true
# need some vectors
HowMany <- 25
tolerance1 <- 1e-24
tolerance2 <- 1e-16
velocity_pipe <-numeric(0)
reynolds <- numeric(0)
friction <- numeric(0)
geometry <- numeric(0)
lossfactor <- numeric(0)
funcMatrix <- matrix(0,nodes+loops,pipes)
jacbMatrix <- matrix(0,nodes+loops,pipes)
gq <- numeric(0)
###############################################################
##############Forward Define Support Functions#################
###############################################################
# Jain Friction Factor Function -- Tested OK 23SEP16
friction_factor <- function(roughness,diameter,reynolds){
  temp1 <- roughness/(3.7*diameter);
  temp2 <- 5.74/(reynolds^(0.9));
  temp3 <- log10(temp1+temp2);
  temp3 <- temp3^2;
  friction_factor <- 0.25/temp3;
  return(friction_factor)
}
# Velocity Function
velocity <- function(diameter,discharge){
  velocity <- discharge/(0.25*pi*diameter^2)
  return(velocity)
}
# Reynolds Number Function
reynolds_number <- function(velocity,diameter,mu){
  reynolds_number <- abs(velocity)*diameter/mu
  return(reynolds_number)
}
# Geometric factor function
k_factor <- function(howlong,diameter,gravity){
  k_factor <- (16*howlong)/(2.0*gravity*pi^2*diameter^5)
  return(k_factor)
}
###############################################################
###############################################################
###############################################################
# compute geometry factors (only need once, goes outside iteration loop)
for (i in 1:pipes)
{
  geometry[i] <- k_factor(distance[i],diameter[i],32.2)
}
geometry
###############################################################
############# iteration outer loop start here #################
###############################################################
for (iteration in 1:HowMany){
# compute current velocity
for (i in 1:pipes)
{
  velocity_pipe[i]<-velocity(diameter[i],flowguess[i])
}
velocity_pipe
# compute current reynolds
for (i in 1:pipes) 
{
  reynolds[i]<-reynolds_number(velocity_pipe[i],diameter[i],viscosity)
}
reynolds
# compute current friction factors
for (i in 1:pipes) 
{
  friction[i]<-friction_factor(roughness[i],diameter[i],reynolds[i])
}
friction
# compute current loss factor
for (i in 1:pipes)
{
  lossfactor[i] <- friction[i]*geometry[i]*abs(flowguess[i])  
}
lossfactor
# build the function matrix
for (i in 1:nodes)
{
  for (j in 1:pipes)
  {
    funcMatrix[i,j] <- nodearcs[i,j]
  }
}
for (i in nodes+1:loops)
{
  for(j in 1:pipes)
  {
    funcMatrix[i,j] <- looparcs[i-nodes,j]*lossfactor[j]
  }
}
funcMatrix
# build the current jacobian matrix
for (i in 1:nodes)
{
  for (j in 1:pipes)
  {
    jacbMatrix[i,j] <- nodearcs[i,j]
  }
}
for (i in nodes+1:loops)
{
  for(j in 1:pipes)
  {
    jacbMatrix[i,j] <- looparcs[i-nodes,j]*2*lossfactor[j]
  }
}
jacbMatrix
# now try the multiplication to build the G(Q) vector
gq <- funcMatrix %*% flowguess - rhs_true # cool -- working to here as expected
# now the tricky part! want to solve the linear system Jacob*dQ = G(Q) for dQ -- system is linear
dq <- solve(jacbMatrix,gq)
# now update the flows
flownew <- flowguess - dq
# now test for stopping
test <- abs(flownew - flowguess)
if(t(test) %*% test < tolerance1)
{
  message("Update not changing -- exit loop and report current update")
  message("Iteration count = ",iteration)
  flowguess <- flownew
  break
}
test <- abs(gq)
if(t(test) %*% test < tolerance2)
{
  message("G(Q) close to zero -- exit loop and report current update")
  message("Iteration count = ",iteration)
  flowguess <- flownew
  break
}
flowguess <- flownew
}
###############################################################
########### End of iteration loop -- solution below ###########
###############################################################
message("Current results ")
print(cbind(flowguess, gq, dq))
