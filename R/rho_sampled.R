##' Roh reciprocity quantity with jackknifed SD  
##' 
##' A function that computes the rho reciprocity quantity and the jackknife standard deviation of the rho. 
##' @usage rho_jnsd(g, nm=c("WRG", "DWCM"), ...)
##' @param g a directed weighted graph (object of class igraph)
##' @param nm character, indicating what null model should be used to compute the expected weighted reciprocity (<r>_NM), currently only "WRG" and "DWCM"  implemented.
##' @param ... passed down to MAXandSAM()
##' @return numeric vector of lenght 2 with rho in the first element and the jackknifed sd in the second element.
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
##' rho_jnsd(g1, "WRG")
##' 
##' @export