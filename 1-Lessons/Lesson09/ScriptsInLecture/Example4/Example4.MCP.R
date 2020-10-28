# main program for st. venant
# library(crayon)
rm(list=ls())
# set directories to local working directory.  Data files must be in this directory
setwd("./")
# clear workspace and set directory
source('hydraulic.elements.lib.R')
source('output.script.lib.R')
source('finite.difference.lib.R')
###### Problem Constants #######
# these are constants that define the problem
# change for different problems
# a good habit is to assign constants to names so the
# program is readable by people in a few years
g <-9.81 # gravitational acceleration, obviously SI units
n <- 10 # number of reaches
q0 <- 110 # initial discharge
yd <- 3.069 # initial flow depth in the model
yu <- 3.069 # upstream constant depth
mn <- 0.013 # Manning's n
b0 <- 20 # bottom width
s0 <- 0.0001 # longitudinal slope (along direction of flow)
s  <- 2.0 # side slope (passed to calls to hydraulic variables)
l  <- 1000.0 # total length (the lenght of computational domain)
tmax <-86400 # total simulation time in seconds
iprt <- 4 # print every iprt time steps
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
dx <- l/n # delta x, length of a reach
xx <- dx*seq(1:nn)-1000  # Spatial locations of nodes, used for plotting
bse <- 30 - s0*xx # bottom channel elevation
dt <- dx/(v0 + c) # the time step that satisfies the courant condtions
kmax <- round(tmax/dt)  # set maximum number of time steps
#message(green('Celerity = '),green(c))
#message(green('Delta x  = '),green(dx))
#message(green('Delta t  = '),green(dt))
#message(green("ITmax = "),green(kmax))
### Run the simulation                  ###
k <- 0 # time counter
t <- 0.0 # elapsed time
pdf("junk1.1.plot.pdf") # graphics device for plots -- plotnow() sends data to plot
writenow(t,dt,y,v,b0,s)  # Write initial conditions
plotnow(t,xx,y,v)
####### BEGIN TIME STEPPING ########

for (itime in 1:kmax){
#############  USE ADAPTIVE TIME FOR STABILITY #############
bestdt(y,v);
############# THIS IS A HACK TO GET STABILITY #################
finitedifference(); # Finite difference a single time step
#message(green("dt now = "),green(dt));
update(y,yp,v,vp); # Update vectors
#message(green(" dt is ",dt));
t <- t+dt; # Increment simulation time
k <- k+1; # Increment loop counter
if (k%%iprt == 0){writenow(t,dt,y,v,b0,s)}; # Write current conditions every iprt time steps
if (k%%iprt == 0){plotnow(t,xx[seq(1,nn,1)],(y[seq(1,nn,1)]),(v[seq(1,nn,1)]))}; # Plot current solution
# reset the time step
############ UNHACK TO KEEP ADAPTIVE TIME STEPPING WORKING ###########
dt <- 16*dt
}
dev.off() # disconnect the pdf file.
