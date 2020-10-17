# R script for system of non-linear equations using minimization
# WARNING -- This is not recomended for large systems
# forward define the functions
####### f(x) #################
func <- function(x_vector){
  func <- numeric(0)
  func[1] <- x_vector[1]^2 + x_vector[2]^2 - 4
  func[2] <- exp(x_vector[1]) + x_vector[2] - 1
  return(func)
}
######### F(x) ###############
bigF <- function(x_vector){
  vector <- numeric(0)
  vector <- func(x_vector)
  bigF <- t(vector) %*% vector
  return(bigF)
}
#############################
# forward define some variables
# starting guess
x_guess <- c(-2,-2)
message(" Initial Start Vector : ")
print(x_guess)
result <- nlm(bigF,x_guess)
message(" Estimated bigF Value : ",result$minimum)
message(" Estimated x_vector Value : ")
print(result$estimate)
message(" Estimated func Value : ")
print(func(result$estimate))