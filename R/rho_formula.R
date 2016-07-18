# rho reciprocity quanity. Squartini et al. (2013)

rho_formula <- 
      function(r_g, r_nm) {
            
        .rho <-  (r_g - r_nm)/(1-r_nm)
        
        return(.rho)
      }
