# R script to solve non-linear example by quasi-linearization
Amat <- matrix(0,nrow=2,ncol=2)
Brhs <- numeric(0)
x_guess <- c(-1.9, 0.8)
maxiter <- 20
message("Initial Guess"); print(x_guess); message("Original Equations - x_guess "); 
message( x_guess[1]^2 + x_guess[2]^2, " : should be 4 ")
message( exp(x_guess[1]) + x_guess[2], " : should be 1 ")
# Construct the current quasi-linear model
for (iter in 1:maxiter){
Amat[1,1] <- x_guess[1]; Amat[1,2] <- x_guess[2];
Amat[2,1] <- 0         ; Amat[2,2] <- 1;
Brhs[1] <- 4
Brhs[2] <- 1-exp(x_guess[1])
# Solve for the new guess
x_new <- solve(Amat,Brhs)
# Update
x_guess <- x_new
}
print(Amat); print(Brhs);
message("Current Guess"); print(x_new)
message("Original Equations - x_new")
message( x_new[1]^2 + x_new[2]^2, " : should be 4 ")
message( exp(x_new[1]) + x_new[2], " : should be 1 ")