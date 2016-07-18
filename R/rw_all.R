##' All reciprocated weights  
##' 
##' Returns the reciprocated weights between all verteces in a graph. 
##' @usage rw_all(g)
##' @param g a directed weighted graph (object of class igraph)
##' @return a dataframe with the edge.ids and the respective reciprocicated weight of each edge 
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
##' # compute all reciprocicated weights 
##' rw_all(g1) 
##' 
##' 
##' @export

rw_all <-
function(g) {
            
            vids <- V(g)$name
            rws <- c()
            eids <- c()
            for (i in vids){
                  for (j in vids){
                        if (i!=j){
                              wij <- w(g, i, j)
                              wji <- w(g, j, i)
                              rwij <- min(wij, wji)
                              eid <- get.edge.ids(g, c(i, j))
                              
                              rws <- c(rws, rwij)
                              eids <- c(eids, eid)
                        }
                  }
            }
            
            edges_rw <- unique(data.frame(edge.id=eids, recipr_weight=rws))
            edges_rw <- edges_rw[edges_rw$edge.id!=0,]
            
            return(edges_rw)  
      }
