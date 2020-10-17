# R script for system of non-linear equations using Newton-Raphson with analytical derivatives
# forward define the functions
####### f(x) #########################
func <- function(x_vector){
  func <- numeric(0)
  func[1] <- x_vector[1]^2 + x_vector[2]^2 - 4
  func[2] <- exp(x_vector[1]) + x_vector[2] - 1
  return(func)
}
######## J(x) #########################
jacob <- function(x_vector,func){  #supply a vector and the function name
# the columns of the jacobian are directional derivatives 
  dv <- 1e-06 #perturbation value for finite difference
  df1 <- numeric(0);
  df2 <- numeric(0);
  dxv <- x_vector;
  dyv <- x_vector;
# perturb the vactors
  dxv[1] <- dxv[1]+dv;
  dyv[2] <- dyv[2]+dv;
  df1 <- (func(dxv) - func(x_vector))/dv;
  df2 <- (func(dyv) - func(x_vector))/dv;
  jacob <- matrix(0,nrow=2,ncol=2)
# for a more general case should put this into a loop
  jacob[1,1] <- df1[1]   ;  jacob[1,2] <- df2[1]  ;
  jacob[2,1] <- df1[2]   ;  jacob[2,2] <- df2[2]  ;
  return(jacob)
}
####### Solver Parameters #############
x_guess <- c(2.,-0.8)
tolerancef <- 1e-9  # stop if function gets to zero
tolerancex <- 1e-9  # stop if solution not changing
maxiter <- 20 # stop if too many iterations
x_now <- x_guess
###### Newton-Raphson Algorithm ########
for (iter in 1:maxiter){
  funcNow <- func(x_now)
  testf <- t(funcNow) %*% funcNow
  if(testf < tolerancef){
    message("f(x) is close to zero : ", testf);
    break
  }
  dx <- solve(jacob(x_now,func),funcNow)
  testx <- t(dx) %*% dx
  if(testx < tolerancex){
    message("solution change small : ", testx);
    break
  }
  x_now <- x_now - dx
}
#########################################
if( iter == maxiter) {message("Maximum iterations -- check if solution is converging : ")}
message("Initial Guess"); print(x_guess);
message("Initial Function Value: "); print(func(x_guess));
message("Exit Function Value : ");print(func(x_now));
message("Exit Vector : "); print(x_now)