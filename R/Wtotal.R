##' Total weight of weighted network 
##' 
##' Returns the total weight f a weighted network. 
##' @usage Wtotal(g)
##' @param g a directed weighted graph (object of class igraph)
##' @return numeric scalar 
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' 
##' # graph example
##' library(igraph)
##' g2 <- graph(c(1,2,2,1), directed=TRUE)
##' E(g2)$weight <- c(2,7)
##' Wtotal(g2)
##' 
##' @export


Wtotal <- 
      function(g){
            stopifnot(is.igraph(g))
            
            s_outs <- graph.strength(g, mode="out", loops=FALSE)
            totalw <- sum(s_outs)
            
            return(totalw)
      }

