##' Build bipartite network 
##' 
##' Returns a bipartite network (e.g, of bill sponsorship) 
##' @usage bipartNetwork(m)
##' @param m a matrix with one type of observation (e.g., legislators) in rows and the other type of observation (e.g., bills) in columns (see Details)
##' @return a bipartite network (object of class igraph)
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @details Example of bill (co-)sponsorthip: m a matrix with legislators in rows and bills in columns. 
##' Cell values in m are equal to 1 if legislator i sponsored bill j.
##' @examples
##' 
##' # graph example
##' library(igraph)
##' m <- random_Matrix("binary", 10) 
##' g <- bipartNetwork(m)
##' plot(g)
##' 
##' @import igraph
##' @export

# b) Build bipartite network (of sponsorship)
#    
#    
#    or equal to 2 if legislator i cosponsored bill j
#    returns an object of class igraph

bipartNetwork <-
      function(m) {
            stopifnot(is.matrix(m))
            
            # add vertices of 
            G <- graph.empty()
            vertb <- paste0('b', 1:ncol(m))
            vertl <- paste0('l', 1:nrow(m))
            G <- G + vertices(vertb, color="orange")
            G <- G + vertices(vertl, color="steelblue")
            
            for (i in 1:nrow(m)) {
                  for (j in 1:ncol(m)) {
                        
                        if (m[i,j]==1) {
                              vert <- c(paste0('l', i), 
                                        paste0('b', j)
                              )
                              
                              G <- G + edges(vert,type='s')
                        } 
                        
                        if (m[i,j]==2){
                              vert <- c(paste0('l', i), 
                                        paste0('b', j)
                              )
                              
                              G <- G + edges(vert,type='c')
                        }
                  }
            }
            
            return(G)  
      }
