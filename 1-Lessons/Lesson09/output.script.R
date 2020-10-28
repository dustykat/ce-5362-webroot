library(crayon)
# printing functions
writenow <- function(t,dt,y,v,b0,s)
{
  message(green('__________'))
  message("Time = ",t," seconds.")
  message("Delta t = ",dt," seconds ")
  aa <- ar(y,b0,s);
  qq <- aa*v
  brb <- bt(y,b0,s)
  ww <- wp(y,b0,s)
  message("--in write now----")
  print(cbind(y,qq,v,qq,brb,ww,zz))
  message("-----------------")
  return()
}

###### Plotting Functions #####################
plotnow<-function(t,x,y,v){
  mainlabel=c("Flow Depth at time = ",t," seconds")
  plot(x,y,main=mainlabel,xlab="distance (m)",ylab="depth (m)",xlim=c(0,30000),ylim=c(0,15),type="l",lwd=3,col="blue")
  lines(x,v,lwd=3,col="red",type="l")
}
