# hydraulic functions
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