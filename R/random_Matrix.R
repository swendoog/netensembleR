##' Draw a random adjacency matrix
##' 
##' The function generates a random adjacency matrix in line with a specific network model.
##' @usage random_Matrix(Method, n_vertices)
##' @param Method a character string indicating the acronym for a specific network model (see details section for a list of models)
##' @param n_vertices the number of vertices in the network (dimension of adjacency matrix).      
##' @return a matrix (numeric) with dimensions n_vertices x n_vertices.
##' @references For details on the different models see:
##'  Tiziano Squartini, Rossana Mastrandrea, Diego Garlaschelli,
##' "Unbiased sampling of network ensembles", http://arxiv.org/abs/1406.1197 
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @details The argument Method needs to refer to one of the following:
##' 'binary' - Random binary matrix
##' 'UBCM' - Undirected Binary Configuration Model
##' 'UWCM' - Undirected Weighted Configuration Model
##' 'DBCM' - Directed Binary Configuration Model
##' 'DWCM'-  Directed Weighted Configuration Model
##' 'UECM'-  Undirected Enhanced Configuration Model
##' 'RBCM'-  Reciprocal Binary Configuration Model
##' 'RWCM'-  Reciprocal Weighted Configuration Model\cr
##' @examples
##' W <- random_Matrix("DWCM", 6)
##' W
##' @export

random_Matrix <- 
      function(Method, n_vertices) {
            
            if (Method=="binary"){
                  n <- n_vertices
                  m <- n_vertices
                  
                  xs.list <- lapply(1:m, FUN=function(i){
                  xs.i <- sample.int(2, n, TRUE)-1L
                  })
                  
                  randomMatrix <- do.call("cbind", xs.list)
            }

                        
            if (Method=='UBCM') {
                                    
                  randomTriangle <- 
                        diag(1/2, n_vertices)
                  
                  for (i in 1:(n_vertices-1)){
                        for (j in (i+1):n_vertices){
                              
                              randomTriangle[i,j] <- 
                                    rbinom(1, 1, prob=.5)
                        }
                  }
                  
                  randomMatrix <- 
                        randomTriangle + t(randomTriangle)
            }
            
            if (Method=='UWCM') {
                  
                  randomTriangle <- 
                        diag(1/2, n_vertices)
                  
                  for (i in 1:(n_vertices-1)){
                        for (j in (i+1):n_vertices){
                              
                              randomTriangle[i,j] <- 
                                    rbinom(1, 1, prob=.5)
                              
                        }
                  }
                  
                  randomMatrix <- 
                        randomTriangle * matrix(round(runif(n_vertices^2, min=1, max=10)), nrow=n_vertices)
                  
                  randomMatrix <- 
                        randomMatrix + t(randomMatrix)
            }
            
            if (Method=='DBCM') { 
                  
                  randomTriangleTop <- 
                        diag(0, n_vertices)
                  
                  randomTriangleBot <- 
                        diag(0, n_vertices)
                  
                  for (i in 1:(n_vertices-1)){
                        for (j in (i+1):n_vertices){
                              
                              randomTriangleTop[i,j] <- 
                                    rbinom(1, 1, prob=.5)
                              
                              randomTriangleBot[i,j] <- 
                                    rbinom(1, 1, prob=.5)
                        }
                  }
                  
                  randomMatrix <- 
                        randomTriangleTop + t(randomTriangleBot)
            }
            
            if (Method=='DWCM') { 
                  
                  randomTriangleTop <- 
                        diag(0, n_vertices)
                  
                  randomTriangleBot <- 
                        diag(0, n_vertices)
                  
                  for (i in 1:(n_vertices-1)){
                        for (j in (i+1):n_vertices){
                              
                              randomTriangleTop[i,j] <- 
                                    rbinom(1, 1, prob=.5)
                              
                              randomTriangleBot[i,j] <- 
                                    rbinom(1, 1, prob=.5)
                              
                        }
                  }
                  
                  randomMatrix <- 
                        randomTriangleTop + t(randomTriangleBot)
                  
                  randomMatrix <- 
                        matrix(round(runif(n_vertices^2, min=1, max=10)), nrow=n_vertices) * randomMatrix
            }
            
            return(randomMatrix)
      }
            
            
