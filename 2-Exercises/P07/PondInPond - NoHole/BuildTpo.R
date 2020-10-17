# script to generate an xyz .tpo file on a regular geometry
# set formula for z=f(x,y)
z <- function(x,y){
	if (y < 1 ) {
		z <- 4
		return(z)
	}
	if (y <= 29 ) {
		z <-  1
		return(z)
}
  if (y > 29) {
      	z <- 4
		return(z)
	}
}
numx <- 11 # number x lines
numy <- 11 # number y lines
elev <- numeric(0)
xcrd <- numeric(0)
ycrd <- numeric(0)
xloc <- numeric(0)
yloc <- numeric(0)
outputObj <-character(0)
dx <- 3
dy <- 3
xloc <- seq(0,dx*numx,dx)
yloc <- seq(0,dy*numy,dy)
pcount <-0
for (irow in 1:numy){
	for (jcol in 1:numx) {
      pcount <- pcount +1
      elev[pcount] <- z(xloc[jcol],yloc[irow])
      xcrd[pcount] <- xloc[jcol]
      ycrd[pcount] <- yloc[irow]
      outputObj[pcount] <- paste(xcrd[pcount]," ",ycrd[pcount]," ",elev[pcount])
	}
}

write(length(elev),file="pond-in-pond.tpo")

write(outputObj, file = "pond-in-pond.tpo", append = TRUE)

