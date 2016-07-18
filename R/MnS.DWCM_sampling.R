# translation of DWCM_sampling.m
#Procedure for drawing a matrix according to the Directed Weighted Configuration Model

DWCM_sampling <-
      function(sol) {
            
            n <- length(sol)
            m <- n/2
            x <- sol[1:m]
            y <- sol[(m+1):length(sol)]
            P <- matrix(0, m, m)
            W <- matrix(0, m, m)
            
            for (i in 1:m){
                  for (j in 1:m){
                        if (i != j){
                              
                              # Generating a weighted matrix extracted from a geometric
                              # distribution with given probability (1-P)
                              
                              #The probability is given by the solution of the DWCM model
                              P[i,j] <- x[i] * y[j]
                              
                              .t <- rgeom(1, 1-P[i,j])
                              W[i,j] <- .t
                              
                              
                        }
                  }
            }
            return(W)
      }



