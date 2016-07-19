##' Function for solving max-entropy models 
##' 
##' An R-implementation of the MAX&SAM method (see references below) for unbiased sampling of network ensembles.
##' @usage MAXandSAM(Method, Matrix, eps=10^-6, altOpt=FALSE, n_samples=0)
##' @param Method a character string indicating the acronym for a specific network model (see details section for a list of models)
##' @param Matrix a binary or Weighted Matrix according to the chosen model (user's choice) 
##' @param eps a control parameter for the maximum relative error between the observed and the expected value of the constraint(s) (defaults to 10^-6). Note: it might make sense to set another value for this parameter depending on the model/data!
##' @param n_samples the number of matrices (samples) to draw from the specific distribution.      
##' @param altOpt logical, optional specification of an alternative optimization algorithm. The default algorithm is 'L-BFGS-B', the alternative (if altOpt=TRUE) is 'NLOPT_LN_SBPLX'.           
##' @return If n_samples==0 (default), a numeric vector containing the 'hidden variables' (solution to the max-entropy problem at hand). 
##' @references This code is a (partial) translation of MAXandSAM.m of the 'MAX&SAM'-matlab package. 
##' The original matlab code copyright is: Mastrandrea Rossana, 2014 (rossmastrandrea-at-gmail.com). 
##' In accordance with the use of the original matlab package, note the following: 
##' "The MAX&SAM method was introduced in [1] and the ensembles it 
##' generates were defined in [2] (BCM, WCM, RCM), [3] (WRCM), [4] (ECM).
##' You can use this routine freely, provided that in all your
##' publications and presentations showing any result that builds upon the
##' use of this routine you ALWAYS cite ref. [1] or its published version
##' (as soon as it is available) and you ALWAYS cite the relevant
##' references (either [2], [3], [4], or combinations of them) defining
##' the specific ensemble(s) you select in the routine."
##' 
##' [1] Tiziano Squartini, Rossana Mastrandrea, Diego Garlaschelli,
##' "Unbiased sampling of network ensembles", 
##' New Journal of Physics 17: 023052
##' 
##' [2] Tiziano Squartini, Diego Garlaschelli, "Analytical maximum-likelihood method to detect patterns in real networks", 
##' New Journal of Physics 13: 083001, (2011).
##' 
##' [3] Tiziano Squartini, Francesco Picciolo, Franco Ruzzenenti, Diego Garlaschelli, 
##' "Reciprocity of weighted networks", Scientific Reports 3: 2729 (2013).
##' 
##' [4] Rossana Mastrandrea, Tiziano Squartini, Giorgio Fagiolo, Diego Garlaschelli, 
##' "Enhanced reconstruction of weighted networks from strengths and degrees" 
##' New Journal of Physics 16: 043022, (2014).
##' 
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @details The method is implemented for the following models:
##' 'UBCM' - Undirected Binary Configuration Model
##' 'UWCM' - Undirected Weighted Configuration Model
##' 'DBCM' - Directed Binary Configuration Model
##' 'DWCM'-  Directed Weighted Configuration Model
##' 'UECM'-  Undirected Enhanced Configuration Model
##' 'RBCM'-  Reciprocal Binary Configuration Model
##' 'RWCM'-  Reciprocal Weighted Configuration Model\cr
##' Some differences to the original matlab version:\cr
##' Unlike the original matlab version, this function only takes matrices as input.\cr
##' The optimization algorithm that maximizes the likelihood diverges from the original\cr 
##' matlab version. Therefore, results should not be expected to be numerically identical.\cr
##' However, a simulation study showed that the R version is rather more precise in finding optima than vice versa.
##' 
##' @examples
##' W <- random_Matrix("DWCM", 6)
##' MAXandSAM(Method="DWCM", Matrix=W)
##' @import nloptr
##' @import nleqslv
##' @export


         


MAXandSAM <-
      function(Method, Matrix, eps=10^-6, altOpt=FALSE, n_samples=0) {  # NOTE: only matrix case is implemented!
            stopifnot(is_wholenumber(n_samples), is.character(Method), is.matrix(Matrix))
            
            #packageStartupMessage('\n', 'You can use this routine freely, provided that in all your publications and presentations showing any result that builds upon the use of this routine you ALWAYS cite references ([1] and either [2], [3], [4], or combinations of them) defining the specific ensemble(s) you select in the routine. You find them in the commented part or writing "help MAXandSAM" in the command window.\n')
            
            # find the 'hidden variables' with the maximum-likelihood procedure 
            M <- initial_check(Method, Matrix);
            solution <- matrix_case(Method, M, altOpt); 
            sol <- solution$sol
            status <- solution$status
            
            # check if maximum was found
            if (status == 0) {
                  message('MAXIMUM FOUND.')
            } else {
                  message('MAXIMUM NOT FOUND!')
            }
            
            # check solution (control relative error between observed and expected values of the contstraints)
            arg <- 1
            err <- system_check(Method, sol, M, arg) 
            
            # depending on comparison with predefined epsilon, solve proplem by solving the associated system 
            if (err < eps) { 
                  msg <- paste0('Constraints preserved with maximum relative error: ', err, '\n')
                  message(msg)
                  output <- sol
            } else {
                  msg <- paste0('Constraints NOT preserved with maximum relative error: ', err, 
                                '.\n', 'Maximum entropy problem will be solved by solving the associated system of equations...', '\n')
                  message(msg)
                  output <- solve_system(Method, M, sol, eps, arg) 
            }
            
            if (n_samples==0) { # no samples to be drawn? only return the solution ('hidden parameters')
                  return(output)
            } else { # draw the samples, return together with solution
                  
                  samples <- list()
                  for (i in 1:n_samples) {
                        W_ext <- samplingAll(output, Method)
                        samples[[i]] <- W_ext
                  }
                  
                  msg <- paste0('\n', n_samples, ' matrices drawn from the ensemble related to the chosen ', Method, ' model.', '\n')
                  message(msg)

                  output <- list(Solution=output, Sample=samples)
                  
                  return(output)
            }
      }
