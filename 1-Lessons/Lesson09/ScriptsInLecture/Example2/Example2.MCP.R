# main program for st. venant
 library(crayon)
rm(list=ls())
# set directories to local working directory.  Data files must be in this directory
setwd("./")
# clear workspace and set directory
###########################################
#  prototype functions for hydraulics     #
###########################################
# hydraulic functions
# need to modify for vectorized geometry
# depth == flow depth
# bottom == bottom width of trapezoidal channel
# side == side slope (same value both sides) of trapezoidal channel
# bt == computed topwidth
# ar == flow area, used in fd update
# wp == wetted perimeter, used in fd update
# depth-topwidth function
bt <- function(depth,bottom,side)
  # tested 12MAR2015 TGC
{
  topwidth <- (bottom + 2.0*side*depth);
  return(topwidth);
}
# depth area function
ar <- function(depth,bottom,side)
  # tested 12MAR2015 TGC
{
  area <- (depth*(bottom+side*depth));
  return(area)
}
# depth perimeter
wp <- function(depth,bottom,side)
  # tested 12MAR2015 TGC
{
  perimeter <- (bottom+2.0*depth*sqrt(1.0+side*side));
  return(perimeter)
}
#############################################
# prototype printing functions              #
#############################################
writenow <- function(t,dt,y,v,b0,s)
{
  #  message(green('__________'));
  message("--begin writenow--")
  message("Time Now = ",t," seconds.");
  message("Delta t Now = ",dt," seconds ");
  area <- ar(y,b0,s);
  discharge <- area*v
  topwidth <- bt(y,b0,s)
  perimeter <- wp(y,b0,s)
  # message("area ",length(area))
  # message("discharge ",length(discharge))
  # message("perimeter ",length(perimeter))
  # message("topwidth ",length(topwidth))
  # message("velocity ",length(v))
  # message("depth ",length(y))
  print(cbind(y,area,v,discharge,topwidth,perimeter,bse))
  message("--end writenow--")
  return()
}
###############################################
# Prototype plot function                     #
###############################################
plotnow<-function(t,x,y,v,bse){
  mainlabel=c("Flow Depth at time = ",t," seconds")
  egl <- y+bse+(v**2)/(2*9.8)
  plot(x,y+bse,main=mainlabel,xlab="distance (m)",ylab="depth (m)",xlim=c(-30000,0),ylim=c(11,60),type="l",lwd=3,col="blue")
  lines(x,bse,col="brown",type="l")
  lines(x,egl,lwd=3,col="red",type="l")
  text(6000,9.51,'Hydraulic Grade (HGL) Line',col='blue')
  text(6000,3,'Profile Grade (Bottom) Line',col='brown')
  text(6000,18,'Energy Grade (EGL) Line',col='red')
}
###############################################
#     Adaptive Time Step Function             #
###############################################
bestdt<-function(v,y,g,b0,s,dt){
  bestdt <- dt # start with current time step
  for (i in 1:length(v)){
    a <- ar(y[i],b0[i],s[i]);
    b <- bt(y[i],b0[i],s[i]);
    c <- sqrt(g*a/b);
    dtn <- dx/abs((v[i])+c);
    # now test
    if(dtn <= bestdt){bestdt <- dtn}
  } # end loop scope
  return(bestdt)
} #end bestdt function
###########################################
#      Solution Update Function           #
###########################################
update<-function(y,yp,v,vp){
  y <<- yp;
  v <<- vp;
  return()
}
###### Problem Constants #######
# these are constants that define the problem
# change for different problems
# a good habit is to assign constants to names so the
# program is readable by people in a few years
g <-9.81 # gravitational acceleration, obviously SI units
n <- 30 # number of reaches
q0 <- 100 # initial discharge
yd <- 9.17 # initial flow depth in the model
yu <- 5.600 # upstream constant depth
mn <- 0.020 # Manning's n
b0 <- c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,9,8,7,8,9,10,11,12,13,14,13,12,11,10,10,10)
s0 <- 0.001 # longitudinal slope (along direction of flow)
s  <- rep(0.0,31) # side slope (passed to calls to hydraulic variables)
l  <- 30000.0 # total length (the lenght of computational domain)
tmax <-86400 # total simulation time in seconds
iprt <- 1 # print every iprt time steps
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
v <- rep(v0[1],nn) # populate v with nn things, each thing has value v0
b <- bt(yd,b0,s) # topwidth at beginning
c <- sqrt(g*a[1]/b[1]) # celerity at initial conditions
dx <- l/n # delta x, length of a reach
xx <- dx*seq(1:nn)-30000  # Spatial locations of nodes, used for plotting
bse <- 12 - s0*xx # bottom channel elevation
dt <- dx/(v0[1] + c) # the time step that satisfies the courant condtions
kmax <- round(tmax/dt)  # set maximum number of time steps
message(green('Celerity = '),green(c))
message(green('Delta x  = '),green(dx))
message(green('Delta t  = '),green(dt))
message(green("ITmax = "),green(kmax))
### Run the simulation                  ###
k <- 0 # time counter
t <- 0.0 # elapsed time
pdf("junk2.1.plot.pdf") # graphics device for plots -- plotnow() sends data to plot
writenow(t,dt,y,v,b0,s)  # Write initial conditions
plotnow(t,xx,y,v,bse)
####### BEGIN TIME STEPPING ########
for (itime in 1:7){
#############  USE ADAPTIVE TIME FOR STABILITY #############
dt <- bestdt(v,y,g,b0,s,dt)
############# THIS IS A HACK TO GET STABILITY #################
##message('Main Loop time now =: ',t)
###### Finite Difference Functions ############

  r <- 0.5*dt/dx;
  ###### LEFT BOUNDARY #####################################
  # UPSTREAM FIXED STAGE AT RESERVOIR                      #
  ##########################################################
  yp[1] <- yu
  ab <- ar(y[2],b0[2],s[2]);
  bb <- bt(y[2],b0[2],s[2]);
  cb <- sqrt(g*bb/ab);
  rb <- ab/wp(y[2],b0[2],s[2]);
  sfb <- (mn2*v[2]*v[2])/(rb^(1.333));
  cn <- v[2] -cb*y[2]+ g*(s0-sfb)*dt;
  #vp[1] <- cn + cb*yp[1];
  vp[1]<-q0/ar(yu,b0[1],s[1])  
  ###### RIGHT BOUNDARY ####################################
  # FIXED STAGE AT DOWNSTREAM END                  #
  ##########################################################
  
  # reflection boundary, find depth along a characteristic
  yp[nn] <- yd ;
  aa <- ar(y[n],b0[n],s[n]);
  ba <- bt(y[n],b0[n],s[n]);
  ca <- sqrt(g*ba/aa);
  ra <- aa/wp(y[n],b0[n],s[n]);
  sfa <- (mn2*v[n]*v[n])/(ra^(4.0/3.0));
  cp <- v[n] + ca*y[n]+g*(s0-sfa)*dt;
  ##yp[nn] <<- (cp - vp[nn])/ca;
  ##vp[nn] <- cp - yp[nn]*ca 
  vp[nn]<-q0/ar(yd,b0[nn],s[nn])
  ##print(cbind(y,yp,v,vp));
  ######## INTERIOR NODES AND REACHES ###############
  # loop through the interior nodes
  for (i in 2:n){ # begin interior node loop scope
    aa <- ar(y[i-1],b0[i-1],s[i-1]);
    ba <- bt(y[i-1],b0[i-1],s[i-1]);
    pa <- wp(y[i-1],b0[i-1],s[i-1]);
    ra <- aa/pa;
    sfa <- (mn2*v[i-1]*v[i-1])/(ra^(4.0/3.0));
    ab <- ar(y[i+1],b0[i+1],s[i+1]);
    bb <- bt(y[i+1],b0[i+1],s[i+1]);
    pb <- wp(y[i+1],b0[i+1],s[i+1]);
    rb <- ab/pb;
    sfb <- (mn2*v[i+1]*v[i+1])/(rb^(4.0/3.0));
    # need averages of sf, hydraulic depth
    dm <- 0.5*(aa/ba + ab/bb);
    sfm <- 0.5*(sfa+sfb);
    vm <- 0.5*(v[i-1]+v[i+1]);
    ym <- 0.5*(y[i-1]+y[i+1]);
    # update momentum
    # note the double <<, this structure forces the
    # value to be global and accessible
    # to other functions when the script is run
    vp[i] <- vm -r*g*(y[i+1] - y[i-1]) -r*vm*(v[i+1] - v[i-1]) + g*dt*(s0-sfm);
    # update depth
    yp[i] <- ym - r*dm*(v[i+1] - v[i-1]) -r*vm*(y[i+1] - y[i-1]);
  } # end of interior node loop scope






################################################################
#message(green("dt now = "),green(dt));
update(y,yp,v,vp); # Update vectors
#message(green(" dt is ",dt));
t <- t+dt; # Increment simulation time
k <- k+1; # Increment loop counter
if (k%%iprt == 0){writenow(t,dt,y,v,b0,s)}; # Write current conditions every iprt time steps
if (k%%iprt == 0){plotnow(t,xx[seq(1,nn,1)],(y[seq(1,nn,1)]),(v[seq(1,nn,1)]),bse)}; # Plot current solution
# reset the time step
############ UNHACK TO KEEP ADAPTIVE TIME STEPPING WORKING ###########
}

dev.off() # disconnect the pdf file.
