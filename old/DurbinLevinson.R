# one-step prediction using the Durbin Levinson algorithm
DurbinLevinson <- function( ts , gamma0, gamma ){
  # goal : compute phi_n1, ..., phi_nn
  # which are the weights of X_n,...,X_1
  # for the prediction of X_n+1
  
  # arguments: 
  # - ts : a time series of size n. This is just an array with n data
  # - gamma0: a scalar equal to gamma(0)
  # - gamma : a vector of size n containing gamma(1),...,gamma(n) 
  
  n <- length(ts)
  
  # creation of a vector of one step prediction at all time k
  # this vector is not used in the algorithm, it is here 
  # for information only
  onestep_prediction <- rep(0,times=n)
  
  v0 <- gamma0
  phi11 <- gamma[1]/gamma0
  
  v1 <- v0 * (1 - phi11^2)
  
  onestep_prediction[2] <- phi11*ts[1]
  # initialisation of the vector Phi_k
  # which contains phi_k1,...,phi_kk
  k <- 2
  phi_k <- rep(0,times = k)
  
  # now we compute phi_22
  phi_k[k] <- ( gamma[k] - phi11 * gamma[1] ) / v1
  # and phi_21
  phi_k[1] <- phi11 - phi_k[k]*phi11
  
  #and v2
  vk <- v1*(1- phi_k[k]*phi_k[k] )
  
  k <- 3
  while(k <= n){
    
    onestep_prediction[k] <- sum(phi_k * ts[(k-1):1] )
    
    # create the vector of size k, phi_k_new
    phi_k_new <- rep(0,times = k)
    
    # compute phikk
    phi_k_new[k] <- ( gamma[k] - sum(phi_k * gamma[ (k-1):1 ]) ) / vk
    
    # compute phi_k1, ...,phi_k,k-1
    phi_k_new[1:(k-1)] <- phi_k - phi_k_new[k] * phi_k[(k-1):1]
    
    # compute vk
    vk <- vk*(1 - phi_k_new[k]*phi_k_new[k])
    
    # update phi_k and k
    phi_k <- phi_k_new
    k <- k+1
  }
  
  finalprediction <- sum( phi_k[n:1] * ts   )
  
  return(list( prediction = finalprediction, 
               weights = phi_k , 
               vn = vk,
               onestep_prediction=onestep_prediction))
  
}

