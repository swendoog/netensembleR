# translation of ECM_sampling.m
# Procedure for drawing a matrix according to the Undirected Enhanced Configuration Model
ECM_sampling <- 
      function(sol) {
            
            n <- length(sol)
            m <- n/2
            x <- sol[1:m]
            y <- sol[(m+1):length(sol)]
            P <- matrix(0, m, m)
            PY <- P
            W <- matrix(0, m, m)
            
            for (i in 1:(m-1)){
                for (j in (1+i):m){
                      
                      # Step 1
                      # Generating a binary matrix extracted from a Bernoulli distribution
                      
                      # Step 2
                      # Generating a weighted matrix extracted from a geometric
                      # distribution with given probability (1-P) and using the binary
                      # structure in step 1)
                      
                      # The probabilities are given by the solution of the UMCM model
                      P[i, j] <- (x[i]*x[j]*y[i]*y[j])/(1 - y[i]*y[j] + x[i]*x[j]*y[i]*y[j])
                      PY[i, j] <- y[i] * y[j]
                                            
                      # Random real number in the range (0,1) extracted 
                      # from the standard uniform distribution
                      z <- runif(1)
                      
                      if (z < P[i,j]) {
                            
                            # Integer number extracted from a geometric
                            # distribution with given probability (1-PY)
                            .t <- rgeom(1, 1-PY[i,j])
                            
                            W[i, j] <- 1 + .t
                            W[j, i] <- W[i, j]

                      }
                }
            }
            
            return(W)
      }
