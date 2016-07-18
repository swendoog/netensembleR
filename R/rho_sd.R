##' Roh reciprocity quantity and SD  
##' 
##' A function that computes the rho reciprocity quantity and the standard deviation of rho based on a sampled network ensemble. 
##' @usage rho_sd(g, nm=c("DWCM"), n_samples, ...)
##' @param g a directed weighted graph (object of class igraph) or the respective adjacency matrix (object of class matrix)
##' @param nm character, indicating what null model should be used to compute the expected weighted reciprocity (<r>_NM), currently only "DWCM"  implemented.
##' @param n_samples numeric, the number of null graphs to be drawn to compute the standard deviation (default is 1500).
##' @param ... passed down to MAXandSAM()
##' @return numeric vector of lenght 2 with rho in the first element and the sd in the second element.
##' @references Tiziano Squartini, Francesco Picciolo, Franco Ruzzenenti, Diego Garlaschelli, 
##' "Reciprocity of weighted networks", Scientific Reports 3: 2729 (2013).
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' 
##' # directed weighted configuration model example
##' # (alternatively with the adjacency matrix as input...)
##' a1 <- random_Matrix(Method="DWCM", n_vertices=10)
##' 
##' rho_sd(a1, nm="DWCM", 20)
##' 
##' @export
##' 

rho_sd <-
      function(g, nm=c("DWCM"), n_samples, ...) {
            stopifnot((is.igraph(g) | is.matrix(g)), is.character(nm), is_wholenumber(n_samples))
            
            # convert input to igraph object if necessary
            if (is.matrix(g)){
                  
                  if (nm=="DWCM") {
                        Matrix <- g
                        g <- graph.adjacency(Matrix, "directed", "weight")    
                  }
                  
            } else {
                  A <- get.adjacency(g, attr="weight")
                  Matrix <- as.matrix(A)
            }
            
            
            # compute r based on real graph
            r.g <- r(g)
            
            # find the hidden variables
            sol <- suppressMessages (MAXandSAM("DWCM", Matrix, n_samples=n_samples, ...))
            
            # compute expectation of r under null model
            r_DWCM <- r_dwcmFormula(sol$Solution)
            g_rho <- (r.g - r_DWCM)/(1-r_DWCM)
            
            # sd of r under null model (based on sample from null model)
            all_s <- sol$Sample
            all_rs <- lapply(all_s, r) # very slow! try to improve
            all_rhos <- lapply(all_rs, rho_formula, r_DWCM ) # r_DWCM (expected value under DWCM) is fixed 
            sd_rho <- sd(unlist(all_rhos))
            
            rsd <- c(g_rho, sd_rho)
            names(rsd) <- c(paste0("Rho_",nm), "SD")
            
            return(rsd)
      }
            
            
            
            
            
            
            
            