##' Plot reciprocated strength 
##' 
##' Plots only the vertices of a given graph that have a reciprocated strength > 0. 
##' @usage plot_sr(g)
##' @param g a directed weighted graph (object of class igraph)
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' 
##' # create a directed weighted graph
##' library(igraph)
##' g1 <- graph(c(1,2,2,1, 1,3,3,2), directed=TRUE)
##' E(g1)$weight <- c(2, 7, 3, 4)
##' V(g1)$name <- c("a", "b", "c")
##' 
##' plot(g1)
##' plot_sr(g1)
##' 
##' @export


plot_sr <- 
      function(g){
            stopifnot(is.igraph(g))
            
            vs_r <- get_srVerteces(g)
            vs <-  V(g)$name
            vs_nonr <- vs[!(vs %in% vs_r)]
            gsr <- g - vs_nonr
            
            plot(gsr)
            
      }
      

