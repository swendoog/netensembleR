# translation of RWCM_sampling.m
# Procedure for drawing a matrix according to the Reciprocal Weighted Configuration Model

RWCM_sampling <- 
      function(sol){
            
            n <- length(sol)
            q <- n/3
            x <- sol[1:q]
            y <- sol[(q+1):(2*q)]
            z <- sol[(2*q+1):n]
            
            W_temp_rec <- matrix(0, q, q)
            A_out_temp <- matrix(nrow=q, ncol=q)
            A_in_temp <- matrix(nrow=q, ncol=q)
            
            for (i in 1:(q-1)) {
                  for (j in (i+1):q){
                        
                        w <- rgeom(1, (1-z[i]*z[j]))
                        W_temp_rec[i,j] <- w
                        W_temp_rec[j,i] <- w
                  }
            }
            
            # 2) Three-events distribution according to the prossible events:
            # (0,0) -> absence of non-reciprocal links;
            # (1,0) -> presence of non-reciprocal out-links;
            # (0,1) -> presence of non-reciprocal in-links.
            
            # The related probabilities are computed using the solutions of the
            # Reciprocal Weighted Configuration Model
            P1 <- matrix(0, q, q)
            P2 <- matrix(0, q, q)
            
            for (i in 1:q) {
                  for (j in 1:q){
                        if (i != j){
                              
                              d <- (1 -(x[i]*x[j]*y[i]*y[j]))
                              P1[i,j] <-  ((1-x[i]*y[j])*(1-x[j]*y[i]))/d
                              P2[i,j] <-  ((x[i]*y[j])*(1-x[j]*y[i]))/d
                              
                        }
                  }
            }
            
            A_out_temp <- matrix(0, q, q)
            A_in_temp <- matrix(0, q, q)
            
            for (i in 1:(q-1)) {
                  for (j in (i+1):q){
                    
                        int <- c(0, P1[i,j], P1[i,j] + P2[i,j], 1)
                        k <- runif(1)
                        f <- is_in_class(k, int)
                        
                        if (f==1) {
                              A_out_temp[i, j] <- 0
                              A_out_temp[j, i] <- 0
                              A_in_temp[i, j] <- 0
                              A_in_temp[j, i] <- 0
                        } else {
                              if (f==2){
                                    A_out_temp[i, j] <- 1
                                    A_out_temp[j, i] <- 0
                                    A_in_temp[i, j] <- 0
                                    A_in_temp[j, i] <- 1
                              } else {
                                    if (f==3) {
                                          A_out_temp[i, j] <- 0
                                          A_out_temp[j, i] <- 1
                                          A_in_temp[i, j] <- 1
                                          A_in_temp[j, i] <- 0
                                    }
                              }
                        }
                  }
            }
            
            # 3) After having defined the structure of non-reciprocal links, 
            # the weights, extracted from a geometric distribution with given
            # probabilities, are assigned to them.
            
            W_out_temp <- matrix(0, q, q)
            W_in_temp <- matrix(0, q, q)
            
            W_out_ext <- matrix(0, q, q)
            W_in_ext <- matrix(0, q, q)
            
            W_ext <- matrix(q, q)
            
            for (i in 1:(q-1)) {
                  for (j in (i+1):q){
                        t1 <- rgeom(1, (1-x[i]*x[j]))
                        t2 <- rgeom(1, (1-x[j]*x[i]))
                        
                        W_out_temp[i,j] <- 1 + t1
                        W_in_temp[j,i] <- 1 + t1
                        W_in_temp[i,j] <- 1 + t2
                        W_out_temp[j,i] <- 1 + t2
                  }
            }
            
            W_out_ext <- A_out_temp * W_out_temp
            W_in_ext <- A_in_temp * W_in_temp
            W_ext <- W_out_ext + W_temp_rec
            
            return(W_ext)
      }
            

              