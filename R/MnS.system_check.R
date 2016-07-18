############################
# 'system_check' R version #
############################

# Define function
system_check <- 
  function(Method, sol, M, arg){
    
    n <- length(sol)
    
    if (Method=='UBCM'){
      
      # Compute the expected values for the constraints
      
      Exp <- rep(0, n)
      
      for (i in 1:(n-1)){
        for(j in (i+1):n){
          
          P[i, j ] <- (sol[i] * sol[j])/(1 + sol[i] * sol[j])
          P[j, i] <- P[i, j]
          
        }
      }
    
      Exp <- colSums(P)
      
      if (dim(M)[2] > 1){
        
        Obs <- colSums(M)
        
      }
      else if (dim(M)[2]==1){
        
        Obs <- M
        
      }
      
    }
    if (Method=='UWCM'){
      
      S <- outer(rep(0,n), rep(0,n))
      
      for (i in 1:(n-1)){
        for(j in (i+1):n){
          
          S[i,j] <- (sol[i] * sol[j]) / (1 - (sol[i] * sol[j]))
          S[j,i] <- S[i,j]
          
        }
      }
      
      Exp <- colSums(S)
      
      if (dim(M)[2] > 1){
        
        Obs <- colSums(M)
        
      } 
      else{
        
        Obs <- M
        
      } 
      
    }
    if (Method=='DBCM'){
      
      m <- n/2
      x <- sol[1:m]
      y <- sol[(m+1):length(sol)]
      
      P <- outer(rep(0,m), rep(0,m))
      
      for (i in 1:m){
        for(j in 1:m){
          
          if (i != j) P[i,j] <- (x[i] * y[j]) / (1 + x[i] * y[j])
          
        }
      }
      
      Exp <- c(colSums(t(P)), colSums(P))
      
      if (dim(M)[2] > 1){
        
        Obs <- c(colSums(t(M)), colSums(M))
        
      }
      else{
        
        Obs <- M
        
      }
      
    }
    if (Method=='DWCM'){
      
      m <- n / 2
      x <- sol[1:m]
      y <- sol[(m+1):length(sol)]
      
      P <- outer(rep(0, m), rep(0, m))
      
      for (i in 1:m){
        for (j in 1:m){
          
          if (i != j){
            
            P[i, j] <- (x[i] * y[j]) / (1 - (x[i] * y[j]))
            
          }
        }
      }
      
      Exp <- c(colSums(t(P)), colSums(P))
      
      if (dim(M)[2] > 1){
        
        Obs <- c(colSums(t(M)), colSums(M))
        
      }
      else{
        
        Obs <- M
        
      }
            
    }
    if (Method=='UECM'){
      
      m <- n/2;
      x <- sol[1:m]
      y <- sol[(m+1):length(sol)]
      
      P <- outer(rep(0, m), rep(0, m))
      S <- outer(rep(0, m), rep(0, m))
      
      for (i in 1:(m-1)){
        for (j in (i+1):m){
          
          P[i, j] <- (x[i] * x[j] * y[i] * y[j])/(1 - y[i] * y[j] + x[i] * x[j] * y[i] * y[j])
          P[j, i] <- P[i, j]
      
          S[i, j] <- P[i, j]/(1 - y[i] * y[j])
          S[j, i] <- S[i, j]
          
        }
      }
      
      Exp <- c(colSums(P), colSums(S))
      
      if (dim(M)[2] > 1){
        
        A <- floor(M > 0)
        Obs <- c(colSums(A), colSums(M))
        
      }
      else{
        
        Obs <- M
        
      }
      
    }
    if (Method=='RBCM'){
      
      for (l in 1:n){
        
        if (sol[l] < 10^(-3)){
          
          sol[l] <- 0
          
        }
      }
      
      q <- n/3
      x <- sol[1:q]
      y <- sol[(q+1):(2*q)]
      z <- sol[(2*q+1):length(sol)]
      
      put  <- outer(rep(0, q), rep(0, q))
      pin  <- pout
      prec <- pout
      
      for (i in 1:q){
        for (j in 1:q){
          if (i != j){
            
            d <- 1 + x[i] * y[j] + x[j] * y[i] +z[i] * z[j]
            pout[i, j] <- (x[i] * y[j]) / d
            pin[i, j]  <- (x[j] * y[i]) / d
            prec[i, j] <- (z[i] * z[j]) / d
            
          }
        }
      }
      
      Exp <- c(colSums(pin), colSums(pout), colSums(prec))
      
      if(dim(M)[2] > 1){
        
        recList <- rec(M)
        Obs <- unlist(lapply(recList, colSums))
        
      }
      
      else{
        
        Obs <- M
        
      }
      
    }
    if (Method=='RWCM'){
      
      for (l in 1:n){
        
        if (sol[l] < 10^(-3)){
          
          sol[l] <- 0
          
        }
      }
      
      q <- n/3
      x <- sol[1:q]
      y <- sol[(q + 1):(2 * q)]
      z <- sol[(2 * q + 1):length(sol)]
      
      W_out_exp <- outer(rep(0, q), rep(0, q))
      W_in_exp  <- W_out_exp
      W_rec_exp <- W_out_exp
      
      for (i in 1:q){
        for (j in 1:q){
          if (i != j){
            
            W_out_exp[i, j] <- ((x[i] * y[j]) * (1 - (x[j] * y[i]))) / ((1 - (x[i] * y[j])) * (1 - (x[i] * x[j] * y[i] * y[j])))
            W_in_exp[i, j]  <- ((x[j] * y[i]) * (1 - (x[i] * y[j]))) / ((1 - (x[j] * y[i])) * (1 - (x[i] * x[j] * y[i] * y[j])))
            W_rec_exp[i, j] <- (z[i] * z[j]) / (1 - (z[i] * z[j]))
            
          }
        }
      }
      
      Exp <- c(colSums(W_in_exp), colSums(W_out_exp), colSums(W_rec_exp))
      
      if(dim(M)[2] > 1){
        
        recwList <- recW(M)
        Obs <- unlist(lapply(recwList, colSums))
        
      }
      
      else{
        
        Obs <- M
        
      }
      
      
    }
    
    if (arg == 0){
      
      return(Exp - Obs)
      
    }
      
    else{
      
      diff <- abs(Obs - Exp)
      
      temp <- list()
      for (i in 1:length(diff)){
        
        if (Obs[i] == 0){
          
          temp[[i]] <- diff[i]
          
        }
        else{
          
          temp[[i]] <- diff[i]/Obs[i]
          
        }
        
      }
        
    }
    return(max(unlist(temp)))
      
  }
    
    
    
  