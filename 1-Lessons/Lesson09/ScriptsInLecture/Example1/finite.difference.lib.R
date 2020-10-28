###### Finite Difference Functions ############
finitedifference<-function(){
r <- 0.5*dt/dx;
###### LEFT BOUNDARY #####################################
# UPSTREAM FIXED STAGE AT RESERVOIR                      #
##########################################################
yp[1] <<- yu
ab <- ar(y[2],b0,s);
bb <- bt(y[2],b0,s);
cb <- sqrt(g*bb/ab);
rb <- ab/wp(y[2],b0,s);
sfb <- (mn2*v[2]*v[2])/(rb^(1.333));
cn <- v[2] -cb*y[2]+ g*(s0-sfb)*dt;
vp[1] <<- cn + cb*yp[1];

###### RIGHT BOUNDARY ####################################
# FIXED STAGE AT DOWNSTREAM END                  #
##########################################################

# reflection boundary, find depth along a characteristic
yp[nn] <<- yd ;
aa <- ar(y[n],b0,s);
ba <- bt(y[n],b0,s);
ca <- sqrt(g*ba/aa);
ra <- aa/wp(y[n],b0,s);
sfa <- (mn2*v[n]*v[n])/(ra^(4.0/3.0));
cp <- v[n] + ca*y[n]+g*(s0-sfa)*dt;
##yp[nn] <<- (cp - vp[nn])/ca;
vp[nn] <<- cp - yp[nn]*ca 
##print(cbind(y,yp,v,vp));
######## INTERIOR NODES AND REACHES ###############
# loop through the interior nodes
for (i in 2:n){ # begin interior node loop scope
aa <- ar(y[i-1],b0,s);
ba <- bt(y[i-1],b0,s);
pa <- wp(y[i-1],b0,s);
ra <- aa/pa;
sfa <- (mn2*v[i-1]*v[i-1])/(ra^(4.0/3.0));
ab <- ar(y[i+1],b0,s);
bb <- bt(y[i+1],b0,s);
pb <- wp(y[i+1],b0,s);
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
vp[i] <<- vm -r*g*(y[i+1] - y[i-1]) -r*vm*(v[i+1] - v[i-1]) + g*dt*(s0-sfm);
# update depth
yp[i] <<- ym - r*dm*(v[i+1] - v[i-1]) -r*vm*(y[i+1] - y[i-1]);
} # end of interior node loop scope
} # end of function scope

###### Solution Update Function ###########
update<-function(y,yp,v,vp){
y <<- yp;
v <<- vp;
return()
}
### NEED ADAPTIVE TIME STEPPING FOR THE FLOOD WAVE 
###### Adaptive Time Step Functions ###########
## here is where we do adaptive time stepping
bestdt<-function(y,v){
  bestdt <- dt # start with current time step
  for (i in 1:nn){
    a <- ar(y[i],b0,s);
    b <- bt(y[i],b0,s);
    c <- sqrt(g*a/b);
    dtn <- dx/abs((v[i])+c)
    # now test
    if(dtn <= bestdt){bestdt <- dtn}
  } # end loop scope
  dt <<- bestdt
} #end bestdt function
###### Solution Update Function ###########
update<-function(y,yp,v,vp){
  y <<- yp;
  v <<- vp;
  return()
}
