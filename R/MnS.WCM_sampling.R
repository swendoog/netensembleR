# translation of WCM_sampling.m
# Procedure for drawing a matrix according to the Undirected Weighted Configuration Model

WCM_sampling <-
      function(sol) {
            
            n <- length(sol)
            P <- matrix(0,n,n)
            W <- matrix(0,n,n)
            
            for (i in 1:(n-1)) {
                  for (j in (i+1):n) {
                        
                        
                        # Generating a weighted matrix extracted from a geometric
                        # distribution with given probability (1-P)
                        
                        # The probability is given by the solution of the UWCM model
                        P[i, j] <- sol[i] * sol[j]
                        
                        .t <- rgeom(1, 1-P[i,j])
                        
                        W[i,j] <- .t
                        W[j,i] <- W[i, j]
                        
                  }
            }
            return(W)
      }
