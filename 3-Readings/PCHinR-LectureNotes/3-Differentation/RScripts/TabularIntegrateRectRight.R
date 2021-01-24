# R script to illustrate reading from a file using read.table
# The script is intentionally complicated to illustrate the 
# three steps: open connection, read into object, close connection
filepath <- "~/Dropbox/1-CE-TTU-Classes/CE4333-PCH-R/3-Readings/PCHinR-LectureNotes/3-Differentation/RScripts"
filename <- "MyTableOfData.txt"
fileToRead <- paste(filepath,filename,sep="/") # build the user absolute filename
# Here we open the connection to the file (within read.table)
# Then the read.table attempts to read the entire file into an object named zz
# Upon either fail or success, read.table closes the connection
zz <- read.table(fileToRead,header=FALSE,sep=" ") # comma seperated ASCII, No header
# Echo zz 
print(zz)
# Trapezoidal Integration
# Compute some constants
howMany <- length(zz$V1) # get the row count from one of the columns  
accumulated_area <- 0.0 # initialize the accumulator
##########################################################################
#                      The actual numerical method                       #
##########################################################################
for (i in 1:(howMany-1)){ #notice the we go to howMany minus 1
  deltax <- zz$V1[i+1] - zz$V1[i]  # compute the delta x this panel
  yright <- zz$V2[i+1]
  accumulated_area <- accumulated_area + yright*deltax
}
##########################################################################
#                             Report Result                              #
##########################################################################
message("Approximate value of integral from tabular data is: ",accumulated_area)
