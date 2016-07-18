###########################
# 'matrix_case' R version #
###########################


matrix_case <- 
      function(Method, M, altOpt){
            #require(nloptr)
            
            obs <- dim(M)[1]
            
            if (Method == 'UBCM') {
                  
                  # Number of parameters
                  n <- obs
                  
                  # Start values for the parameters
                  x0 <- 0.9 * rep(1, n)
                  
                  # Lower bound of parameters
                  lb <- rep(0, n)
                  
                  # Upper bound of parameters
                  ub <- rep(Inf, n)
                  
            }
            
            if (Method == 'UWCM') {
                  
                  # Number of parameters
                  n <- obs
                  
                  # Start values for the parameters
                  x0 <- 0.9 * rep(1, n)
                  
                  # Lower bound of parameters
                  lb <- rep(0, n)
                  
                  # Upper bound of parameters
                  ub <- rep(1, n)
                  
            }
            
            if (Method == 'DBCM') {
                  
                  # Number of parameters
                  n <- 2 * obs
                  
                  # Start values for the parameters
                  x0 <- 0.9 * rep(1, n)
                  
                  # Lower bound of parameters
                  lb <- rep(0, n)
                  
                  # Upper bound of parameters
                  ub <- rep(Inf, n)
                  
            }
            
            if (Method == 'DWCM') {
                  
                  # Number of parameters
                  n <- 2 * obs
                  
                  # Start values for the parameters
                  x0 <- 0.9 * rep(1, n)
                  
                  # Lower bound of parameters
                  lb <- rep(0, n)
                  
                  # Upper bound of parameters
                  ub <- rep(1, n)
                  
            }
            
            if (Method == 'UECM') {
                  
                  # Number of parameters
                  n <- 2 * obs
                  
                  # Start values for the parameters
                  x0 <- 0.9 * rep(1, n)
                  
                  # Lower bound of parameters
                  lb <- rep(0, n)
                  
                  # Upper bound of parameters
                  ub <- c(rep(Inf, n/2), rep(1, n/2))
                  
            }
            
            if (Method == 'RBCM') {
                  
                  # Number of parameters
                  n <- 3 * obs
                  
                  # Start values for the parameters
                  x0 <- 0.9 * rep(1, n)
                  
                  # Lower bound of parameters
                  lb <- rep(0, n)
                  
                  # Upper bound of parameters
                  ub <- rep(Inf, n)
                  
            }
            
            if (Method == 'RWCM') {
                  
                  # Number of parameters
                  n <- 3 * obs
                  
                  # Start values for the parameters
                  x0 <- 0.9 * rep(1, n)
                  
                  # Lower bound of parameters
                  lb <- rep(0, n)
                  
                  # Upper bound of parameters
                  ub <- rep(1, n)
                  
            }
            
            
            suppressWarnings( 
                  solution1 <-    
                        nlminb(
                              start     = x0,
                              objective = Likelihood,
                              lower     = lb,
                              upper     = ub,
                              M         = M,
                              Method    = Method,
                              control   = list(
                                    eval.max = 10^4,
                                    iter.max = 5*10^4,
                                    x.tol    = 10^(-32),
                                    abs.tol  = 10^(-32))
                        )
            )
            
            solutions <- list()
            solutions$sol <- solution1$par
            solutions$status <- solution1$convergence 
            
            if (altOpt == TRUE){
                  
                  suppressWarnings(
                        solution2 <- 
                              nloptr(
                                    x0     = x0,
                                    eval_f = Likelihood,
                                    lb     = lb,
                                    ub     = ub,
                                    M      = M, 
                                    Method = Method,
                                    opts   = list(
                                          'algorithm' = 'NLOPT_LN_SBPLX',
                                          'ftol_abs'  = 10^(-32),
                                          'xtol_abs'  = 10^(-32),
                                          'maxeval'   = 10^4
                                    )
                              )
                  )
                  
                  temp <- 
                        round(solution1$obj - solution2$obj, 2) 
                  
                  if(temp==0){
                        
                        message('The L-BFGS-B and Subplex algorithms deliver the same results.') 
                        
                  } 
                  
                  else if(temp > 0){
                        
                        message('The Subplex solution differs from the L-BFGS-B result and has a higher likelihood.\n',
                                'The Suplex solution is returned.')
                        solutions$sol <- solution2$solution
                        solutions$status <- solution2$status
                              
                  } 
                  
                  else if(temp < 0){
                        
                        message('The Subplex solution differs from the L-BFGS-B result, but has a lower likelihood.\n',
                                'The L-BFGS-B solution is returned.')
                        
                  } 
                  
                  
            }
            
            return(solutions)
            
            
      
  
  
}