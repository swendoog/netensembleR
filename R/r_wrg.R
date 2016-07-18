##' Expected weighted reciprocity under WRG model 
##' 
##' Computes the expected weighted reciprocity under the weighted random graph (WRG) null model, given a specific graph g. 
##' @usage r_wrg(g)
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
##' # compute r
##' r_wrg(g1)
##' 
##' @export

r_wrg <-
      function(g){
            
            # Squartini et al. (2013), appendix eq. (32):
            WG <- Wtotal(g)
            N <- vcount(g)
            pstar <- WG / (WG + N*(N-1))
            # Squartini et al. (2013), appendix eq. (36):
            r_WRG <- pstar / (1 + pstar)
            
            return(r_WRG)
      }
