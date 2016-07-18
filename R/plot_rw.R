##' Plot reciprocated graph component 
##' 
##' Plots either only the reciprocated connections of a graph or highlights them in the graph plot. 
##' @usage plot_rw(g, extract=TRUE , rcolor="red", othercolor="grey", ...)
##' @param g a directed weighted graph (object of class igraph)
##' @param extract logical if TRUE (default), only the reciprocated connections are plotted, if FALSE the whole graph is plotted with reciprocated connections highlighted
##' @param rcolor character string, color argument for edges (only used for case extract==FALSE; defaults to "red")
##' @param othercolor character string, color argument for non-reciprocated edges (only used for case extract==FALSE; defaults to "grey")
##' @param ... passed down to igraph
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
##' plot_rw(g1)
##' plot_rw(g1, extract=FALSE, edge.arrow.size=0.1, vertex.size=4,
##'  vertex.label=NA, vertex.color="lightgrey")
##' 
##' @export

plot_rw <- 
      function(g, extract=TRUE , rcolor="red", othercolor="grey", ...){
            stopifnot(is.igraph(g), is.logical(extract))
            
            if (extract==TRUE) {
                  grw <- rm_nonRecipro(g)
            } else {
                  grw <- add_rWeight(g)
                  E(grw)[E(grw)$r_weight > 0]$color <- rcolor
                  E(grw)[E(grw)$r_weight == 0]$color <- othercolor
                  
                  # reorder for plot
                  gdf <- get.data.frame(grw)
                  gdf <- gdf[order(gdf$r_weight, decreasing=FALSE),]
                  grw <- graph.data.frame(gdf)
            }
            plot(grw, ...)
      }

