# R script to illustrate numerical differencing 
rm(list=ls()) # clear all objects
############ Prototype (forward define) Functions #########
############## slope function prototype ####################
slopeOfSecant<-function(f1,f2,x1,x2){
  slopeOfSecant <- (f2-f1)/(x2-x1);
  return(slopeOfSecant)}
########      disaggregate function prototype    ###########
disaggregate<-function(f,x,dfdx){
  n<-length(x) # length of vectors
  dfdx<-rep(0,n); # zero dfdx
  for (i in 2:n){dfdx[i]<-slopeOfSecant(f[i-1],f[i],x[i-1],x[i]);
                 dfdx[i]<- (x[i]-x[i-1])*dfdx[i];};
  dfdx[1]<-0;
  return(dfdx)} 
########### backward rate, backward time prototype ###########
brbt<-function(f,x,dfdx){
  n<-length(x) # length of vectors
  dfdx<-rep(0,n); # zero dfdx
  for (i in 1:(n-1)){dfdx[i]<-slopeOfSecant(f[i],f[i+1],x[i],x[i+1]);};
  dfdx[n]<-0;
  return(dfdx)}
##############################################################

################ Build the filename #########################
filepath <- "~/Dropbox/1-CE-TTU-Classes/CE4333-PCH-R/3-Readings/PCHinR-LectureNotes/3-Differentation/RScripts"
filename <- "cumulative_rainfall.txt"
fileToRead <- paste(filepath,filename,sep="/") # build the user absolute filename
# Here we open the connection to the file (within read.table)
# Then the read.table attempts to read the entire file into an object named zz
# Upon either fail or success, read.table closes the connection
zz <- read.table(fileToRead,header=TRUE,sep=",") # comma seperated ASCII, No header
attach(zz) # attach associates the column names with the data below them.
## summary(zz) # useful to be sure data were imported correctly
incremental_depth <- disaggregate(cumulative_rain,hours,dfdx)
incremental_rate <- brbt(cumulative_rain,hours,dfdx)
dt <- 1 # how long each interval, make adaptive as exercise
incremental_depth <- incremental_depth*dt
print(cbind(zz,incremental_rate,incremental_depth))
################ Build the Plot #####################################
plot(hours,cumulative_rain,xlab="Time(hours)",ylab="Cumulative Depth 
(inches)",type="l",lwd=5,col="Blue",tck=1)
lines(hours,incremental_depth*dt,pch=16,col="green",lwd=3)
lines(hours,incremental_rate,pch=16,col="red",lwd=2,type="s")
text(3,4.1,"Cumulative Rain",col="blue")
text(3,3.1,"Incremental Rain",col="green")
text(3,2.1,"Incremental Rate",col="red")
#####################################################################
detach(zz) #deallocate the zz object
