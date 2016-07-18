# rec.m translation
# internal function to Likelihood.m
rec <- 
      function(A){
            n <- nrow(A) # per definition, an adjacency matrix is n x n 
            Aout <- matrix(nrow=n, ncol=n)
            Ain <- Aout
            Arec <- Aout
            
            for (i in 1:n){
                  for (j in 1:n){
                        Aout[i,j] <- A[i,j]*(1-A[j,i])
                        Arec[i,j] <- A[i,j]*A[j,i]
                  }
            }
            Ain <- t(Aout)
            
            return(list(Aout, Ain, Arec))   
      }