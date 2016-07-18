##' Reciprocicated strength  
##' 
##' Returns the reciprocicated strength of vertex i. 
##' @usage sr(g, i)
##' @param g a directed weighted graph (object of class igraph)
##' @param i the id or symbolic vertex name of the vertex to compute the reciprocicated strength
##' @return a named numeric vector 
##' @references Tiziano Squartini, Francesco Picciolo, Franco Ruzzenenti, Diego Garlaschelli, 
##' "Reciprocity of weighted networks", Scientific Reports 3: 2729 (2013).
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' 
##' # graph example
##' library(igraph)
##' g2 <- graph(c(1,2,2,1,2,3,3,2), directed=TRUE)
##' E(g2)$weight <- c(2,7,3,3)
##' 
##' sr(g2, 2) # must be 5
##' 
##' @export


sr <-
      function(g, i) {
            stopifnot(is.igraph(g))
            
            if (is.character(i)){
                  allj <- neighbors(g, i)
                  allj <- get.vertex.attribute(g,name="name", index=allj)
            } else {
                  allj <- neighbors(g, i)
            }
            
            nneighbors <- length(allj)
            allrws <- list()
            length(allrws) <- nneighbors
            if (class(i)=="numeric" | class(i)=="integer") {
                  allrws <- lapply(allj, rw, g=g, i=i)
  
            } else {
                  for (j in 1:nneighbors) {
                        allrws[[j]] <- rw(g = g, i = i, allj[[j]])
                        
                  } 
            }

            rweights <- unlist(allrws)
            si <- sum(rweights)
            
            return(si)
      }
