# Hydraulic Elements for Rectangular Channels
# inputs:  depth ==flowdepth
#          width == channel width
#           area == flow area
#      perimeter == wetted perimeter
#      discharge == volumetric rate
#     mannings_n == mannings' roughneess coefficient
#       radius == hydraulic radius (area/perimeter)

rm(list=ls())
# Depth-Area Function
area<-function(depth,width){
  area<-depth*width;
  return(area)
}
# Wetted perimeter function for rectangular channel
perimeter<-function(depth,width){
  perimeter<-2*depth+width;
  return(perimeter)
}
# Hydraulic radius function
radius<-function(area,perimeter){
  radius<-area/perimeter;
  return(radius)
}
# Friction slope function
slope_f<-function(discharge,mannings_n,area,radius){
  slope_f<-(discharge^2)*(mannings_n^2)/( (radius^(4/3))*(area^2) ); #compute friction slope
  return(slope_f)
}
avg2point<-function(x1,x2){
  avg2point<-0.5*(x1+x2);
  return(avg2point)
}
