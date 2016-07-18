# CM_sampling(sol)
# Procedure for drawing a matrix according to the Undirected Binary Configuration Model

CM_sampling <- 
      function(sol){
            
            n <- length(sol)
            P <- matrix(0,n,n)
            A <- matrix(0,n,n)
            
            for (i in 1:(n-1)) {
                  for (j in (1+i):n) {
                        # Generating a binary matrix extracted from a Bernoulli
                        # distribution
                        
                        # The probability is given by the solution of the UBCM model
                        P[i,j] <- (sol[i]*sol[j])/(1 + (sol[i]*sol[j]))
                        
                        # Random real number in the range (0,1) extracted 
                        # from the standard uniform distribution
                        z <- runif(1)
                        
                        if (z<P[i,j]) {
                              A[i,j] <- 1
                              A[j,i] <- 1
                        }
                  }
            }
            
            return(A)
      }



