# recW.m translation
# internal function to Likelihood.m

recW <- 
      function(W) {
            n <- nrow(W) # w is the weight matrix (adjacency matrix but with weight values), thus n x n!
            Wrec <- matrix(nrow=n, ncol=n)
            Wout <- Wrec
            
            for (i in 1:n){
                  for (j in 1:n) {
                        if (i!=j) {
                              Wrec[i,j] <- min(W[i,j], W[j,i])
                              Wout[i,j] <- W[i,j] - Wrec[i,j]
                        }
                  }
            }
            Win <- t(Wout)
            
            return(list(Wout, Win, Wrec))
      }

