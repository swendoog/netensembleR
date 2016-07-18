##' Weighted reciprocity measure 
##' 
##' Computes the weighted reciprocity measure 'r' of a weighted directed network. 
##' @usage r(g)
##' @param g a directed weighted graph (object of class igraph) or an adjacency matrix (object of class matrix)
##' @return numeric scalar
##' @references Tiziano Squartini, Francesco Picciolo, Franco Ruzzenenti, Diego Garlaschelli, 
##' "Reciprocity of weighted networks", Scientific Reports 3: 2729 (2013).
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' 
##' # graph example
##' library(igraph)
##' g1 <- graph(c(1,2,2,1, 1,3,3,2), directed=TRUE)
##' E(g1)$weight <- c(2, 7, 3, 4)
##' V(g1)$name <- c("a", "b", "c")
##' 
##' # compute r
##' r(g1)
##' 
##' @export


r <- 
      function(g){
            stopifnot((is.igraph(g) | is.matrix(g)))
            
            # convert input to igraph object if necessary
            if (is.matrix(g)){
                  
                  g <- graph.adjacency(g, "directed", "weight")

            }
            
            rWtotal.g <- rWtotal(g)
            W.g <- Wtotal(g)
            
            return(rWtotal.g/W.g)
      }

