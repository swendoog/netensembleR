##' Roh reciprocity quantity 
##' 
##' Computes the rho reciprocity quantity (comparison of r with r_NM from a null model). 
##' @usage rho(g, nm="WRG", ...)
##' @param g a directed weighted graph (object of class igraph) or the respective adjacency matrix (object of class matrix)
##' @param nm character, indicating what null model should be used to compute the expected weighted reciprocity (<r>_NM), currently either "WRG" or "DWCM".
##' @param ... passed down to MAXandSAM()
##' @return numeric scalar (between -1 and +1)
##' @details The function is implemented for the follwing null models:
##' 'WRG' -  Weighted Random Graph Model
##' 'DWCM'-  Directed Weighted Configuration Model
##' @references Tiziano Squartini, Francesco Picciolo, Franco Ruzzenenti, Diego Garlaschelli, 
##' "Reciprocity of weighted networks", Scientific Reports 3: 2729 (2013).
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @seealso \code{\link{MAXandSAM}} which this function calls in case of some null models.
##' @examples
##' 
##' # weighted random graph example
##' library(igraph)
##' g1 <- graph(c(1,2,2,1, 1,3,3,2), directed=TRUE)
##' E(g1)$weight <- c(2, 7, 3, 4)
##' V(g1)$name <- c("a", "b", "c")
##' 
##' rho(g1, nm="WRG")
##' 
##' # directed weighted configuration model example
##' # (alternatively with the adjacency matrix as input...)
##' a1 <- random_Matrix(Method="DWCM", n_vertices=20)
##' 
##' rho(a1, nm="DWCM")
##' 
##' @export


rho <- 
      function(g, nm="WRG", ...) {       
            stopifnot((is.igraph(g)|is.matrix(g)), (is.character(nm)|is.igraph(g)))
            
            # convert input to igraph object if necessary
            if (is.matrix(g)){
                  
                  if (nm=="WRG") {
                        g <- graph.adjacency(g, "directed", "weight")
                  }
                  
                  if (nm=="DWCM") {
                        g <- graph.adjacency(g, "directed", "weight")
                  }
            }
            
            # compute r based on real graph
            r.g <- r(g)
            
            # compute expected r under respective null model
            if (is.character(nm)) {
                  if (nm=="WRG") {
                        r.nm <- r_wrg(g)
                  }
                  
                  if (nm=="DWCM") {
                        r.nm <- r_dwcm(g, ...)
                  }
            } else { # is a graph
                  r.nm <- r(nm)
            }
           
            
            roh_NM <- (r.g - r.nm)/(1-r.nm)
            
            return(roh_NM) 
      }

