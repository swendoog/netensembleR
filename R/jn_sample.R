##' Jackknife sampling of a directed weighted graph 
##' 
##' A function that builds a set of jackknife samples of a graph g (removing one edge-weight at a time). 
##' @usage jn_sample(g)
##' @param g a directed weighted graph (object of class igraph)
##' @return a list of length L (the number of edges in g)
##' @references Tiziano Squartini, Franceshttp://127.0.0.1:11315/rstudio/clear.cache.gifco Picciolo, Franco Ruzzenenti, Diego Garlaschelli, 
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
##' samples <- jn_sample(g1)
##' length(samples)
##' E(g1)
##' samples[[1]]
##' 
##' @export


jn_sample <-
      function(g) {
            stopifnot(is.igraph(g))
            
            L <- length(E(g))
            sj.list <- lapply(1:L, rm.weight, g=g )
            
            return(sj.list)
      }
