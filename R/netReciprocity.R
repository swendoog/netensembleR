##' Analysis of reciprocity 
##' 
##' A function to analyze several aspects of reciprocity in a directed weighted network (see details).
##' @usage netReciprocity(g, nm=c("DWCM"), n_samples, ...)
##' @param g a directed weighted graph (object of class igraph) or the respective adjacency matrix (object of class matrix)
##' @param nm character, indicating what null model should be used to compute the expected weighted reciprocity (<r>_NM), currently only "DWCM"  implemented.
##' @param n_samples numeric, the number of null graphs to be drawn to compute the standard deviation (default is 1500).
##' @param ... passed down to MAXandSAM()
##' @return a object of class recipro
##' @references Tiziano Squartini, Francesco Picciolo, Franco Ruzzenenti, Diego Garlaschelli, 
##' "Reciprocity of weighted networks", Scientific Reports 3: 2729 (2013).
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @details The function computes inter alia the rho reciprocity quantity and the standard deviation of rho based on a sampled network ensemble. 


##' @examples
##' 
##' # directed weighted configuration model example
##' # (alternatively with the adjacency matrix as input...)
##' a1 <- random_Matrix(Method="DWCM", n_vertices=10)
##' 
##' netReciprocity(a1, nm="DWCM", 20)
##' 
##' @export
##' 

netReciprocity <-
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
            
            # distribution of r and rho under null model (based on sample from null model)
            all_s <- sol$Sample
            all_rs <- lapply(all_s, r) # very slow! try to improve!
            all_rhos <- unlist(lapply(all_rs, rho_formula, r_DWCM )) # r_DWCM (expected value under DWCM) is fixed 
            
            # compute div. measures of the distribution
            sd_rho <- sd(all_rhos)
            mean_rho <- mean(all_rhos)
            median_rho <- median(all_rhos)
            dens_rho <- density(all_rhos)
            
            recipro <- list(g_rho, mean_rho, sd_rho, median_rho, dens_rho)
                  
                  
            rsd <- c(g_rho, sd_rho)
            names(rsd) <- c(paste0("Rho_",nm), "SD")
            
            return(rsd)
      }

