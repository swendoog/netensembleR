##' Find a vertex in a graph 
##' 
##' Returns TRUE if graph g has a vertex with a specific name
##' @usage has_vertex(g, v)
##' @param g a directed weighted graph (object of class igraph)
##' @param v character string with the name of the vertex
##' @return logical, TRUE if graph g has a vertex with a specific name, FALSE otherwise
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' 
##' # graph example
##' library(igraph)
##' g1 <- graph(c(1,2,2,1, 1,3,3,2), directed=TRUE)
##' E(g1)$weight <- c(2, 7, 3, 4)
##' V(g1)$name <- c("a", "b", "c")
##' 
##' has_vertex(g1, "a")
##' 
##' @export

# returns TRUE if graph x has a vertex named after the string in node
has_vertex <-
      function(g, v) {
            stopifnot(is.igraph(g), is.character(v))
            
            vnames <- vertex.attributes(g)$name
            has <- any(vnames %in% v)
            
            return(has)
      }
