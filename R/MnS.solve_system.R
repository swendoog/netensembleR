# R-implementation of the second part in MAXandSAM.m 
# (for the case that the constraints are NOT preserved with the maximum likelihood solution, i.e. err > eps)
# This procedure solves the maximum-entropy problem not just by maximizing the likelihood of the entropy function,
# but takes the result from maximizing the likelihood as starting values to solve the associated system (Squartini and Garlaschelli, 2011).

solve_system <- 
      function(Method, M, sol, eps, arg) {
            
            #require(nleqslv)
                        
            # first, an atempt is made without too many iterations to get quicker results
            # set parameters to control the solver (according to original matlab implementation)
            control <- list(maxit=10^4,
                            xtol=10^(-32),
                            ftol=10^(-32),
                            cndtol=10^(-32)
                            )
            
            x0 <- sol # starting values based on solution of max. likelihood
            
            # solve system
            solved <- nleqslv(x=x0, 
                              system_check,
                              jac=NULL,
                              Method=Method,
                              M=M,
                              arg=0,
                              method="Broyden",
                              global="dbldog", # trust region method using the double dogleg method as described in Dennis and Schnabel (1996)
                              control= control
            )
            solnew <- solved$x
            
            # check again if solution is satifactory
            err2 <- system_check(Method, solnew, M, arg)
            if (err2 < eps) { 
                  msg <- paste0('SYSTEM SOLVED. Constraints preserved with maximum relative error: ', err2)
                  message(msg)
                  output <- solnew
            } else { # still no satisfactory solution, try again with longer iteration...
                  
                  x0 <- solnew # use last value as starting point
                  control <- list(maxit=10^5, # --> more iterations than before
                                  xtol=10^(-32),
                                  ftol=10^(-32),
                                  cndtol=10^(-32)
                  )
                  
                  # solve system again
                  solved <- nleqslv(x=x0, 
                                    system_check,
                                    jac=NULL,
                                    Method=Method,
                                    M=M,
                                    arg=0,
                                    method="Broyden",
                                    global="dbldog", # trust region method using the double dogleg method as described in Dennis and Schnabel (1996)
                                    control= control
                  )
                  solnew2 <- solved$x
                  
                  # check again, report accordingly
                  err3 <- system_check(Method, solnew2, M, arg)
                  if (err3 < eps) {
                        msg <- paste0('SYSTEM SOLVED. Constraints preserved with maximum relative error: ', err3)
                        message(msg)
                        output <- solnew2
                  } else {
                        msg <- paste0('SYSTEM SOLVED. Constraints preserved with maximum relative error: ',
                                      err3,
                                      '\n',
                                      'NOTE: the maximum relative error is still bigger than the defined tolerance of ',
                                      eps, '!')
                        message(msg)
                        output <- solnew2
                        
                  }
            }
            
            return(output)
      }

