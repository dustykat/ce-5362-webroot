# library(crayon)
# printing functions
writenow <- function(t,dt,y,v,b0,s)
{
#  message(green('__________'));
  message("Time = ",t," seconds.");
  message("Delta t = ",dt," seconds ");
  aa <- ar(y,b0,s);
  discharge <- aa*v
  topwidth <- bt(y,b0,s)
  perimeter <- wp(y,b0,s)
  print(cbind(y,discharge,v,discharge,topwidth,perimeter,bse))
  message("-----------------")
  return()
}

###### Plotting Functions #####################
plotnow<-function(t,x,y,v){
  mainlabel=c("Flow Depth at time = ",t," seconds")
  plot(x,y,main=mainlabel,xlab="distance (m)",ylab="depth (m)",xlim=c(-900,100),ylim=c(-3,4),type="l",lwd=3,col="blue")
  lines(x,v,lwd=3,col="red",type="l")
}
