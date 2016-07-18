##' Non-reciprocicated in-strenght  
##' 
##' Returns non-reciprocicated in-strength of vertex i. 
##' @usage snr_in(g, i)
##' @param g a directed weighted graph (object of class igraph)
##' @param i the id or symbolic vertex name of the vertex to compute the non-reciprocicated in-strength
##' @return a named numeric vector 
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
##' # compute the non-reciprocicated in-strenght of a vertex
##' snr_in(g1, 1)
##' snr_in(g1, "a") 
##' snr_in(g1, "b")
##' 
##' 
##' @export


snr_in <-
function(g, i) {
      stopifnot(is.igraph(g))
      
            sr_i <- sr(g, i)
            sin_i <- s.in(g, i)
            
            return(sin_i - sr_i)      
      }
