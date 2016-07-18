
# Procedure for selecting the proper sampling procedure according to the
# chosen model (method)

samplingAll <- 
      function(sol, method) {
            
            if (method=='UBCM'){
                  W_ext <- CM_sampling(sol)
            }
            
            if (method=='UWCM'){
                  W_ext <- WCM_sampling(sol)
            }
            
            if (method=='DBCM'){
                  W_ext <- DCM_sampling(sol)
            }
            
            if (method=='DWCM'){
                  W_ext <- DWCM_sampling(sol)
            }
            
            if (method=='UECM'){
                  W_ext <- ECM_sampling(sol)
            }
            
            if (method=='RBCM'){
                  W_ext <- RCM_sampling(sol)
            }
            
            if (method=='RWCM'){
                  W_ext <- RWCM_sampling(sol)
            }
            
            return(W_ext)
      }



