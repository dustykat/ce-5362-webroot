# library(crayon)
# printing functions
writenow <- function(t,dt,y,v,b0,s)
{
#  message(green('__________'));
  message("Time = ",t," seconds.");
  message("Delta t = ",dt," seconds ");
  area <- ar(y,b0,s);
  discharge <- area*v
  topwidth <- bt(y,b0,s)
  perimeter <- wp(y,b0,s)
  print(cbind(y,area,v,discharge,topwidth,perimeter,bse))
  message("-----------------")
  return()
}

###### Plotting Functions #####################
plotnow<-function(t,x,y,v,bse){
  mainlabel=c("Flow Depth at time = ",t," seconds")
  egl <- y+bse+(v**2)/(2*9.8)
  plot(x,y+bse,main=mainlabel,xlab="distance (m)",ylab="depth (m)",xlim=c(0,12000),ylim=c(0,20),type="l",lwd=3,col="blue")
  lines(x,bse,col="brown",type="l")
  lines(x,egl,lwd=3,col="red",type="l")
  text(6000,9.51,'Hydraulic Grade (HGL) Line',col='blue')
  text(6000,3,'Profile Grade (Bottom) Line',col='brown')
  text(6000,18,'Energy Grade (EGL) Line',col='red')
  text
}
