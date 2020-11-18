# Backwater Curves, variable step method, prisimatic channels only!
# clear the workspace
rm(list=ls())
# Install Prototype Functions 
# Relative Path
source("./HydraulicElements/HydraulicElementsRectangular.R")
#
#
backwater<-function(begin_depth,end_depth,how_many,discharge,width,mannings_n,slope){
#
## Example function call
##  zz<-backwater(begin_depth=8,end_depth=5,how_many=30,discharge=55.4,width=5,mannings_n=0.02,slope=0.001)
## Numerical values are values used in essay, they correspond to a particuar example from Koutitas 1983
#
# Other functions must exist otherwise will spawn errors
#
# Prepare space for vectors
depth<-numeric(0)  # numeric vector for depths
bse<-numeric(0) # numeric vector for bottom elevations
wse<-numeric(0) # numeric vector for water surface elevations
delta_depth<-(begin_depth-end_depth)/(how_many)  # change in depth for finding spatial steps
depth[1]<-begin_depth # assign downstream value
for (i in 2:how_many){depth[i]<-depth[1]-i*delta_depth} # uniform depths
velocity<-discharge/area(depth,width) # velocity for each depth
deltax<-numeric(0) # numeric vector for spatial steps
# next for loop is very FORTRANesque!  
for (i in 1:(how_many-1)){
    depth_bar<-avg2point(depth[i],depth[i+1]); #compute average depth in reach
    area_bar<-area(depth_bar,width); #compute average area in reach
    perimeter_bar<-perimeter(depth_bar,width); #compute average wetted perimeter
    radius_bar<-radius(area_bar,perimeter_bar); #compute average hydraulic radius
    friction<-slope_f(discharge,mannings_n,area_bar,radius_bar) #compute friction slope
    deltax[i]<-( (depth[i+1]+(velocity[i+1]^2)/(2*9.8))	- (depth[i] + (velocity[i]^2)/(2*9.8)) )/(slope-friction); # compute change in distance for each change in depth
}
distance<-numeric(0); # space for computing cumulative distances
distance[1]<-0;
bse[1]<-0; # bottom elevation at origin
for (i in 2:(how_many)){
	distance[i]<-distance[i-1]+deltax[i-1]; # spatial distances
	bse[i]<-bse[i-1]-deltax[i-1]*slope; # bottom elevations
	}
wse<-bse+depth # water surface elevations
# output
plot(distance,wse,col="blue",type="l",lwd=8,xlim=c(min(distance),max(distance)),ylim=c(min(bse),max(wse)));
lines(distance,bse,type="l",col="grey",lwd=6);
z<- cbind(distance,depth,bse,wse) # bind output into 4 columns
return(z)
#
}
# Textbook Example
zz <- backwater(begin_depth=8,end_depth=5,how_many=31,discharge=55.4,width=5,mannings_n=0.02,slope=0.001)
#zz<-backwater(begin_depth=0.1,end_depth=0.47,how_many=20,discharge=1,width=51,mannings_n=0.01,slope=0.00001)
