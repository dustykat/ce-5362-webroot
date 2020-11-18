# script to generate an xyz .tpo file on a regular geometry
# set formula for z=f(x,y)
z <- function(x,y){
	if (y < 0) {
		z <- -18
		return(z)
	}
	if (y <= 24000) {
		z <-  y/1846 - 18
		return(z)
	}
      if (y > 24000) {
      	z <- 5 
		return(z)
	}
}
numx <- 100 # number x lines
numy <- 100 # number y lines
elev <- numeric(0)
xcrd <- numeric(0)
ycrd <- numeric(0)
xloc <- numeric(0)
yloc <- numeric(0)
outputObj <-character(0)
dx <- 250
dy <- 250
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

write(length(elev),file="simple-estuary.tpo")

write(outputObj, file = "simple-estuary.tpo", append = TRUE)

