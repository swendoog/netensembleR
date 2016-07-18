# translation of DCM_sampling.m
# Procedure for drawing a matrix according to the Directed Binary Configuration Model

DCM_sampling <-
      function(sol){
            
            n <- length(sol)
            m <- n/2
            
            x <- sol[1:m]
            y <- sol[(m+1):length(sol)]
            
            P <- matrix(0, m, m)
            A <- matrix(0, m, m)
            
            for (i in 1:m) {
                  for (j in 1:m) {
                        if (i != j) {
                              
                              # Generating a binary matrix extracted from a Bernoulli
                              # distribution
                              
                              # The probability is given by the solution of the DBCM model
                              P[i,j] <- (x[i] * y[j])/(1 + (x[i]*y[j]))
                              
                              # Random real number in the range (0,1) extracted 
                              # from the standard uniform distribution
                              z <- runif(1)
                              
                              if (z < P[i,j]) {
                                    
                                    A[i,j] <- 1
                              }
                              
                              
                        }
                  }
            }
            
            return(A)
      }


            