# initial_check.m translation
# checks if matrix is a proper input for the chosen Method in a call to MaxAndSam

initial_check <- 
      function(Method, Matrix) {
            
            # is the matrix integer? if not round
            isint <- sum(Matrix) %% 1 == 0
            if (!isint) {
                  M <- round(Matrix, digits=0)
                  warning('The matrix was not integer as required from this methodology. The function rounded it.')
            } else {
                  M <- Matrix
            }
            
            # is the matrix symmetric (for the undirected cases)?
            if (Method=='UBCM'| Method=='UWCM' | Method=='UECM') {
                  if (!isSymmetric(Matrix)){
                        stop('Error! The matrix should be symmetric because you are using an undirected model.')
                  }
            }
            
            # Is the matrix symmetric but the Method an undirected model?
            if (Method=='DBCM' | Method=='DWCM' | Method=='RBCM' | Method=='RWCM') {
                  if (isSymmetric(Matrix)) {
                        stop('Error! The matrix should not be symmetric because you are using a directed model.')  
                  }
            }
            return(M)
      }

