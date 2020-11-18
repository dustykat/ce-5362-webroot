# R script for some matrix operations
############## READ IN DATA FROM A FILE ####################
filepath <- "~/Dropbox/1-CE-TTU-Classes/CE4333-PCH-R/3-Readings/PCHinR-LectureNotes/5-LinearSystems/RScripts"
filename <- "MatrixA.txt"
fileToRead <- paste(filepath,filename,sep="/") # build the user absolute filename
# Read the first file
yy <- read.table(fileToRead,header=FALSE,sep=",") # comma seperated ASCII, No header
filename <- "MatrixB.txt"   # change the filename
fileToRead <- paste(filepath,filename,sep="/") # build the user absolute filename
# Read the second file
zz <- read.table(fileToRead,header=FALSE,sep=",") # comma seperated ASCII, No header
############## Get Row and Column Counts ###################
HowManyColumnsA <- length(yy)
HowManyRowsA    <- length(yy$V1)
HowManyColumnsB <- length(zz)
HowManyRowsB    <- length(zz$V1)
###############  Build A and B Matrices  ####################
Amat <- matrix(0,nrow = HowManyRowsA, ncol = HowManyColumnsA)
Bmat <- matrix(0,nrow = HowManyRowsB, ncol = HowManyColumnsB)
for (i in 1:HowManyRowsA){
  for(j in 1:(HowManyColumnsA)){
    Amat[i,j] <- yy[i,j]
  }
}
rm(yy) # deallocate zz and just work with matrix and vectors 
for (i in 1:HowManyRowsB){
  for(j in 1:(HowManyColumnsB)){
    Bmat[i,j] <- zz[i,j]
  }
}
rm(zz) # deallocate zz and just work with matrix and vectors 
############# Echo Input ###################################
#print(Amat)
#print(Bmat)
#twoA <- 2 * Amat
#print(twoA)
#threeA <- Amat+twoA
#print(threeA)
############## New Cases ###################################
A <- matrix(c(2,4,3,-3),nrow=2,ncol=2)
B <- matrix(c(1/6,2/9,1/6,-1/9),nrow=2,ncol=2)
print(A)
print(B)
C <- A %*% B
#print(C)
A_inv <- solve(A)
print(A_inv)
