##' Weight of directed edge  
##' 
##' Returns the weight w.ij of a directed edge from vertex i to vertex j. 
##' @usage w(g, i, j, eid=NULL)
##' @param g a directed weighted graph (object of class igraph)
##' @param i the id or symbolic vertex name of vertex i 
##' @param j the id or symbolic vertex name of vertex j
##' @param eid the id of the edge (can be provided instead of i or j)
##' @return numeric scalar 
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' 
##' # graph example
##' library(igraph)
##' g2 <- graph(c(1,2,2,1), directed=TRUE)
##' E(g2)$weight <- c(2,7)
##' w(g2, 2, 1)
##' w(g2, NULL, NULL, 2) 
##' 
##' @export

w <- 
      function(g, i, j, eid=NULL) {
            stopifnot(is.igraph(g))
            
            if (is.null(eid)){
                  eid <- get.edge.ids(g, vp=c(i,j), directed=TRUE)
            }
            
            wij <- get_weight(g, eid)
            if (length(wij)==0) wij <- 0
            
            return(wij)      
      }

