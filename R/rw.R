##' Reciprocated weight   
##' 
##' Returns the reciprocated weight of the link between vertices i and j. 
##' @usage rw(g, i, j)
##' @param g a directed weighted graph (object of class igraph)
##' @param i the id or symbolic vertex name of vertex i 
##' @param j the id or symbolic vertex name of vertex j
##' @return numeric scalar 
##' @references Tiziano Squartini, Francesco Picciolo, Franco Ruzzenenti, Diego Garlaschelli, 
##' "Reciprocity of weighted networks", Scientific Reports 3: 2729 (2013).
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' 
##' # graph example
##' library(igraph)
##' g2 <- graph(c(1,2,2,1), directed=TRUE)
##' E(g2)$weight <- c(2,7)
##' rw(g2, 1, 2 ) # must be 2
##' w(g2, 2, 1) - rw(g2, 1, 2 ) # must be 5 (the non-reciprocated weight from 2 to 1 )
##' 
##' @export


rw <-
function(g, i, j) {
      stopifnot(is.igraph(g))
            
            wij <- w(g, i, j)
            wji <- w(g, j, i)
            rwij <- min(wij, wji)
            
            return(rwij)  
      }
