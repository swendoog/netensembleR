# Likelihood.m translation 
# Procedure for computing the likelihood function for each model when the
# input are in matrix-form
Likelihood <- 
      function(h, M, Method){
            n <- length(h)
            f <- 0
            
            if (Method=='UBCM') { # Undirected Binary Configuration Model
                  for (i in 1:(n-1)){
                        for (j in (i+1):n){
                              f <- f + (M[i,j]*log(h[i]*h[j]) - log(1 + h[i]*h[j]))

                        }
                  }
            }
            
            if (Method=='UWCM') { # Undirected Weighted Configuration Model
                  x <- h
                  for (i in 1:(n-1)){
                        for (j in (i+1):n) {
                              f <- f + (M[i,j]*log(h[i]*h[j]) + log(1-h[i]*h[j]))
                        }
                  }
            }
            
            if (Method=='DBCM') { # Directed Binary Configuration Model
                  
                  m <- n/2
                  x <- h[1:m]
                  y <- h[(m+1):length(h)]
                  
                  for (i in 1:m){
                        for (j in 1:m){
                              if (i != j) {
                                    f <- f + (M[i,j]*log(x[i]*y[j]) - log(1 + x[i]*y[j]))
                              }
                        }
                  }
            }
            
            if (Method == 'DWCM') { # Directed Weighted Configuration Model
                  
                  m <- n/2
                  x <- h[1:m]
                  y <- h[(m+1):length(h)]
                  
                  for (i in 1:m) {
                        for (j in 1:m) {
                              if (i!=j){
                                    f <- f + (M[i,j]*log(x[i]*y[j]) + log(1 - x[i]*y[j]))                                    
                              }
                        }
                  }   
            }
            
            if (Method == 'UECM') { # Undirected Enhanced Configuration Model
                  
                  m <- n/2
                  x <- h[1:m]
                  y <- h[(m+1):length(h)]
                  A <- trunc(M>0)
                  
                  for (i in 1:(m-1)) {
                        for (j in (i+1):m) {
                              f <- f + A[i,j]*log(x[i]*x[j]) + M[i,j]*log(y[i]*y[i]) + log(1-y[i]*y[j]) - log(1-(y[i]*y[j])+x[i]*x[j]*y[i]*y[j])
                        }
                  }     
            }
            
            if (Method=='RBCM') { # Reciprocal Bianry Configuration Model
                  
                  q <- n/3
                  x <- h[1:q]
                  y <- h[(q+1):(2*q)]
                  z <- h[(2*q+1):length(h)]
                  A.list <- rec(M)
                  Aout <- A.list[[1]]
                  Ain <- A.list[[2]]
                  Arec <- A.list[[3]]
                  
                  for (i in 1:(q-1)) {
                        for (j in (i+1):q) {
                              f <- f + Aout[i,j]*log(x[i]*y[j]) + Ain[i,j]*log(x[j]*y[i]) + Arec[i,j]*log(z[i]*z[j]) - log(1+x[i]*y[j]+x[j]*y[i]+z[i]*z[j])
                        }
                  }  
            }
            
            if (Method== 'RWCM'){ # Reciprocal Weighted Configuration Model
                  q <- n/3
                  x <- h[1:q]
                  y <- h[(q+1):(2*q)]
                  z <- h[(2*q+1):length(h)]
                  W.list <- recW(M)
                  Wout <- W.list[[1]]
                  Win <- W.list[[2]]
                  Wrec <- W.list[[3]]
                  Z <- matrix(ncol=q, nrow=q)
                  
                  for (i in 1:(q-1)){
                        for (j in (i+1):q){
                              Z[i,j] <- (1-(x[i]*x[j]*y[i]*y[j]))/((1-(x[i]*y[j]))*(1-(x[j]*y[i]))*(1-(z[i]*z[j])))
                              f <- f + ((Wout[i,j]*log(x[i]*y[j])) + (Win[i,j]*log(x[j]*y[i])) + (Wrec[i,j]*log(z[i]*z[j])) - (log(Z[i,j])))  
                        }
                  }
            }
            
            return(-f) # optimization algorithms minimize by default
      }
                  
# NOTE: difference in loop behavior!
# Matlab code:
# for i=1:q
# for j=(i+1):q
# --> j will never be > q in Matlab! In R j will reach q+1!!!
# in R use:
# for (i in 1:(q-1)){
#    for (j in (1+i):q){}
#}
#