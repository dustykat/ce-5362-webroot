# main program for st. venant
rm(list=ls())
#setwd("~/Dropbox/CE5361-2015-1/Project2/Project2-Problem2")
setwd("/Volumes/smb-share-userone/CoursesActive/ce-5362-swmodeling/1-Lessons/Lesson08/ScriptsInLecture")
# clear workspace and set directory
#source('~/Dropbox/CE5361-2015-1/Project2/Project2-Problem2/Project2.2.Lib.R')
source('/Volumes/smb-share-userone/CoursesActive/ce-5362-swmodeling/1-Lessons/Lesson08/ScriptsInLecture/Project2.2.Lib.R')

###### Problem Constants #######
# these are constants that define the problem
# change for different problems
# a good habit is to assign constants to names so the
# program is readable by people in a few years
g <-9.81 # gravitational acceleration, obviously SI units
n <- 29 # number of reaches
#q0 <- 110 # initial discharge
q0 <- 1.1 # initial discharge
#yd <- 3.069 # initial flow depth in the model
yd <- 1.000 # initial flow depth in the model
#yu <- 3.069 # upstream constant depth
yu <- 1.1 # upstream constant depth
# mn <- 0.013 # Manning's n
mn <- 0.02 # Manning's n
# b0 <- 20 # bottom width
b0 <- 5 # bottom width
## modify for problem 1 s0 <- 0.0001 # longitudinal slope (along direction of flow)
s0 <- 0.0000001 # longitudinal slope (along direction of flow)
#s  <- 2.0 # side slope (passed to calls to hydraulic variables)
s  <- 0.0 # side slope (passed to calls to hydraulic variables)
l  <- 30000.0 # total length (the lenght of computational domain)
tmax <- 1600000 # total simulation time in seconds
iprt <- 8 # print every iprt time steps
nn <- n+1 # how many nodes, will jack with boundaries later
mn2 <- mn*mn # Manning's n squared, will appear a lot.
a <- ar(yd,b0,s) # flow area at beginning of time
v0 <- q0/a # initial velocity
######## Here we build vectors ###############
y <- numeric(nn) # create nn elements of vector y
yp <- numeric(nn) # updates go in this vector, same length as y
v <- numeric(nn) # create nn elements of vecotr v
vp <-numeric(nn) # updates go in this vector, same length and v
ytmp <-numeric(nn)
vtmp <-numeric(nn)
y <- rep(yd,nn) # populate y with nn things, each thing has value yd
v <- rep(v0,nn) # populate v with nn things, each thing has value v0
b <- bt(yd,b0,s) # topwidth at beginning
c <- sqrt(g*a/b) # celerity at initial conditions
###
hydrograph <- numeric(1)
dummy<- read.csv(file="hydrograph.csv",header=F)
elapsedtime <- dummy$V1; # forst column of hydrograph is time
hydrograph <- dummy$V2; # second column of hydrograph is flow
print(cbind(c))
dx <- l/n # delta x, length of a reach
xx <- dx*seq(1:nn)-1000  # Spatial locations of nodes, used for plotting
zz <- 30 - s0*xx # bottom channel elevation
dt <- dx/(v0 + c) # the time step that satisfies the courant condtions
kmax <- round(tmax/dt)  # set maximum number of time steps
print(cbind(dx,dt))



### Run the simulation                  ###
k <- 0 # time counter
t <- 0.0 # elapsed time
pdf("junk2.2.plot.pdf") # graphics device for plots -- plotnow() sends data to plot
writenow(t,dt,y,v,b0,s)  # Write initial conditions
plotnow(t,xx,y,v)
####### BEGIN TIME STEPPING ########
message("kmax =", kmax)
for (itime in 1:kmax){
## put in the hydrograph here -- use approx function to interpolate
## we are after the second value qq$y in the approx function
qq <<- approx(elapsedtime,hydrograph,t)
print (qq$x);
print (qq$y);
#debug print(qq$y) 
#############  NEED ADAPTIVE TIME STEPPING FOR THE HYDROGRAPH ROUTING #############
bestdt(y,v);
############# THIS IS A HACK TO GET STABILITY #################
# halve the time step 
dt <- dt/16 
finitedifference(); # Finite difference a single time step
#update(ytmp,yp,vtmp,vp); # Update vectors
#update(y,yp,v,vp); # Update vectors
#bestdt(yp,vp)
message("dt now = ",dt);
#finitedifference(); # Finite difference a single time step
ytmp <- (yp+ytmp)/2;
vtmp <- (vp+vtmp)/2;
update(y,yp,v,vp); # Update vectors
message(" dt is ",dt);
t <- t+dt; # Increment simulation time
k <- k+1; # Increment loop counter
if (k%%iprt == 0){writenow(t,dt,y,v,b0,s)}; # Write current conditions every iprt time steps
if (k%%iprt == 0){plotnow(t,xx[seq(1,nn,1)],(y[seq(1,nn,1)]),(v[seq(1,nn,1)]))}; # Plot current solution
# reset the time step
############ UNHACK TO KEEP ADAPTIVE TIME STEPPING WORKING ###########
dt <- 16*dt
}
dev.off() # disconnect the pdf file.
