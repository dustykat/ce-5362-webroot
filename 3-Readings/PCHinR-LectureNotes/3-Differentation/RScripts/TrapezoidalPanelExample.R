# R script to implement trapezoidal panel numerical integration
################################# NOTE ####################################
## The interactive input requires the script to be sourced                #
# In R console the command line would be                                  #
# source('PATH-TO-THE-FILE/RectangularPanelExample.R')                    #
# where PATH-TO-THE-FILE is replaced with the actual path on your machine #
###########################################################################
#             Function to be integrated (modify as needed)                #
###########################################################################
y <- function(x){
  y <- x * sqrt(1+x^2)
  return(y)
}
###########################################################################
#             Get lower,upper and how many panels from user               #
###########################################################################
xlow <- readline("What is the lower limit of integration? ")  
xhigh <- readline("What is the upper limit of integration? ")
howMany <- readline("How many panels? ")
# Convert the strings into numeric values
xlow <- as.numeric(unlist(strsplit(xlow, ",")))
xhigh <- as.numeric(unlist(strsplit(xhigh, ",")))
howMany <- as.numeric(unlist(strsplit(howMany, ",")))
# Compute some constants
deltax <- (xhigh - xlow)/howMany  
accumulated_area <- 0.0 # initialize the accumulator
xleft <- xlow
xright <- xleft + deltax
##########################################################################
#                      The actual numerical method                       #
##########################################################################
for (i in 1:howMany){
  yleft <- y(xleft)
  yright <- y(xright)
  accumulated_area <- accumulated_area + (yleft+yright)*deltax/2
  xleft <- xleft + deltax
  xright <- xright + deltax
}
##########################################################################
#                             Report Result                              #
##########################################################################
message("Approximate value of integral from ",xlow," to ",xhigh," is: ",accumulated_area)