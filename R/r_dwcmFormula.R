
# computes r_DWCM given the hidden parameters
# Squartini et al. (2013), SI eq. (40 and 42):
# p_ij = x_i*y_j


r_dwcmFormula <- 
      function(sol) {
            
            n <- length(sol)
            m <- n/2
            x <- sol[1:m]
            y <- sol[(m+1):length(sol)]
            upper <- 0
            lower <- 0
            
            for (i in 1:m){
                  for (j in 1:m){
                        if (i != j){
                              
                              pstar_ij <- x[i] * y[j]
                              pstar_ji <- x[j] * y[i]
                              
                              upper <- upper + ((pstar_ij * pstar_ji) / (1- (pstar_ij * pstar_ji)) )
                              lower <- lower + pstar_ij / (1-pstar_ij)
                              
                        }
                  }
            }
            
            r_DWCM <- upper/lower
            
            return(r_DWCM)
            
      }