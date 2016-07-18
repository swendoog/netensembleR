##' Get weight of a given edge 
##' 
##' Returns the 'weight' attribute of given edges in a directed graph
##' @usage get_weight(g, i)
##' @param g a directed weighted graph (object of class igraph)
##' @param i a numeric vector with the ids of edges
##' @return a numeric vector conteining the respective edge weights
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' 
##' # graph example
##' library(igraph)
##' g1 <- graph(c(1,2,2,1, 1,3,3,2), directed=TRUE)
##' E(g1)$weight <- c(2, 7, 3, 4)
##' V(g1)$name <- c("a", "b", "c")
##' 
##' get_weight(g1,1:2)
##' 
##' @export

get_weight <-
      function(g, i) {
            stopifnot(is.igraph(g))
            
            attrs <-  list.edge.attributes(g)
            contains <- 'weight' %in% attrs
            if (!contains) {
                  stop('This graph does not have a weight attribute.')
            }
            
            w <- get.edge.attribute(g, 'weight', index=i)
            
            return(w)           
      }
