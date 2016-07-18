##' Expected weighted reciprocity under DWCM model 
##' 
##' Computes the expected weighted reciprocity under the directed weighted configuration model (DWCM), given a specific graph g. 
##' @usage r_dwcm(g, ...)
##' @param g a directed weighted graph (object of class igraph) or an adjacency matrix (object of class matrix)
##' @param ... passed down to MAXandSAM()
##' @return numeric scalar
##' @references  [1] Tiziano Squartini, Rossana Mastrandrea, Diego Garlaschelli,
##' "Unbiased sampling of network ensembles", http://arxiv.org/abs/1406.1197 
##' (Please replace this preprint version with the final published version, as soon as the latter is available.)
##' [2] Tiziano Squartini, Francesco Picciolo, Franco Ruzzenenti, Diego Garlaschelli, 
##' "Reciprocity of weighted networks", Scientific Reports 3: 2729 (2013).
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' 
##' # graph example
##' library(igraph)
##' g1 <- graph(c(1,2,2,1, 1,3,3,2), directed=TRUE)
##' E(g1)$weight <- c(2, 1, 3, 0)
##' a1 <- random_Matrix(Method="DWCM", n_vertices=20)
##' 
##' # compute r
##' r_dwcm(g1)
##' r_dwcm(a1)
##' 
##' @export


r_dwcm <-
      function(g, ...){
            stopifnot((is.matrix(g) | is.igraph(g)))
            
            if (is.igraph(g)) {
                  
                  A <- get.adjacency(g, attr="weight")
                  Matrix <- as.matrix(A)
            } else {
                  
                  Matrix <- g
            }
            
            # solve for hidden parameters (solution to maximum entropy problem)
            sol <- suppressMessages (MAXandSAM("DWCM", Matrix, ...))
            
            # Squartini et al. (2013), SI eq. (40 and 42):
            # p_ij = x_i*y_j
            
           r_DWCM <- r_dwcmFormula(sol)
            
            return(r_DWCM)
      }

