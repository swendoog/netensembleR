##' Get ids of edges that have a reciprocated component 
##' 
##' Returns the ids of edges that have a (non-)reciprocated component 
##' @usage get_rwEdges(g, recipro=TRUE)
##' @param g a directed weighted graph (object of class igraph)
##' @param recipro logical, if TRUE (default) only reciprocated compontent is returned, if FALSE only non-reciprocated component is returned
##' @return a numeric vector conteining the respective edge ids
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
##' # compute/add reciprocated weights
##' ids <- get_rwEdges(g1)
##' 
##' @export



get_rwEdges <-
function(g, recipro=TRUE){
            rws <- rw_all(g) # get reciprocicated weight of all edges
            if (recipro==TRUE) {
                  eids <- rws[rws[,2]!=0,1] # remove edges without reciprocicated component
            } else {
                  eids <- rws[rws[,2]==0,1] # remove edges without reciprocicated component
            }
            
            return(eids)
      }
