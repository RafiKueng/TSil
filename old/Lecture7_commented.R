source("DurbinLevinson.R")


# Brockwell-David example : MA(1) process with theta = -0.9 
# and sigma^2 = 1

gamma0 <- 1 + 0.9^2
gamma1 <- -0.9

# Durbin Levinson algorithm:
DB_result <- DurbinLevinson(ts = c(1.42,1.83,0.68,-0.91) , 
               gamma0 = gamma0, 
               gamma = c(gamma1, 0 , 0 , 0) )

DB_result
# since here we run the algorithm for a low value of n 
# (i.e. n = 4) we can verify that the standard formulas
# to get the best linear predictor yield indeed the same weights


Gamma_n <- matrix(c(gamma0,gamma1,0,0,
                    gamma1,gamma0,gamma1,0,
                    0,gamma1,gamma0,gamma1,
                    0,0,gamma1,gamma0),
                    nrow=4)

gamma_n <- matrix(c(gamma1 , 0 , 0 , 0) , ncol = 1 )

# these are the weight of X_n,...,X_1 for the one step prediction 
# of X_{n+1}
weights <- solve(Gamma_n)%*%gamma_n

weights
DB_result$weights
# we see that the two quantities match

#################################################
library(itsmr)
# Let's run the same algorithm for large n (say n = 10000) in 
# a MA(1) model

# We start by simulating a random MA(1) process
set.seed(8)
n <- 10000
Zt <- rnorm(n)
ts <- rep(0,times=n)
ts[1] <- Zt[1]
for(i in 2:n) ts[i] <- Zt[i] - 0.9*Zt[i-1]
plotc(ts)

plot(x=(n-40):n , y= ts[(n-40):n] , 
     xlim=c(n-40,n+2) , type="o" , 
     col="blue" , xlab="t",ylab="")

gamma0 <- 1 + 0.9^2
gamma1 <- -0.9

DB_result2 <- DurbinLevinson(ts = ts , 
                            gamma0 = gamma0, 
                            gamma = c(gamma1, rep(0,times=n-1)) )

###the first weight is multiplicated with the last observations and so on -> weights are in decreasing order, depending on the covariances

# prediction
DB_result2$prediction

# mean squared prediction error
DB_result2$vn

# first 50 weights, of X_n, ...,X_n-50
DB_result2$weights[1:50]
# only the first weights are large. The weights become 
# small when we go more in the "past".

# let us plot all the one step predictions, and the actual 
# prediction of X_n+1
lines( x=1:n , y = DB_result2$onestep_prediction , type="l", col="red"  )
points(x=n+1,y=DB_result2$prediction,type="o",col="red")
# here the one step predictions are pretty good to actually
# predict the value of the process.
# This is the case because the process we have is indeed
# a realization of a MA(1) process !


# note that, here, for n = 10,000 it is already too hard 
# with this computer to store and invert the n times n 
# matrix Gamma_n. The Durbin Levinson algorithm is a good 
# solution to handle large n.





