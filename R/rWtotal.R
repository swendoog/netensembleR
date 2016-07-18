##' Total reciprocated weight  
##' 
##' Returns the total reciprocated weight of a graph. 
##' @usage rWtotal(g)
##' @param g a directed weighted graph (object of class igraph)
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
##' # compute total reciprocated weight
##' rWtotal(g1) 
##' 
##' 
##' @export


rWtotal <- 
      function(g){
            verteces <- V(g)
            rweights <- lapply(1:length(verteces), FUN=function(i){ 
                  sr(g, verteces[i]) 
            })
            rweights <- unlist(rweights)
            
            return(sum(rweights))
            
      }

