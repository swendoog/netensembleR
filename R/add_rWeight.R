##' Add reciprocated weights as attribute
##' 
##' Computes and adds the reciprocated weights as edge attribute to a graph.
##' @usage add_rWeight(g)
##' @param g a directed weighted graph (object of class igraph)
##' @return a directed weighted graph (object of class igraph) with attribute r_weight
##' @references Tiziano Squartini, Francesco Picciolo, Franco Ruzzenenti, Diego Garlaschelli, 
##'   "Reciprocity of weighted networks", Scientific Reports 3: 2729 (2013).
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # create a directed weighted graph
##' library(igraph)
##' g1 <- graph(c(1,2,2,1, 1,3,3,2), directed=TRUE)
##' E(g1)$weight <- c(2, 7, 3, 4)
##' V(g1)$name <- c("a", "b", "c")
##' 
##' # compute/add reciprocated weights
##' g1 <- add_rWeight(g1)
##' 
##' @export

 
add_rWeight <-
function(g){
            rws <- rw_all(g) # get reciprocicated weight of all edges
            rws <- rws[order(rws$edge.id),]
            E(g)$r_weight <- rws$recipr_weight
            
            return(g)   
      }
