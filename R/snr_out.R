##' Non-reciprocicated out-strenght  
##' 
##' Returns non-reciprocicated out-strength of vertex i. 
##' @usage snr_out(g, i)
##' @param g a directed weighted graph (object of class igraph)
##' @param i the id or symbolic vertex name of the vertex to compute the non-reciprocicated out-strength
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
##' snr_out(g1, 1) 
##' snr_out(g1, "b")
##' 
##' @export


snr_out <-
      function(g, i) {
            sr_i <- sr(g, i)
            sout_i <- s.out(g, i)
            
            return(sout_i - sr_i)      
      }
