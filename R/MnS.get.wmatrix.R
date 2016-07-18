# return the 'weighted' adjacency matrix of graph g
# the returned matrix serves as input for parameter 'Matrix' in
# the MaxAndSam function
# g a weighted directed graph based on real data
# 
get.wmatrix <-
      function(g){
            m <- get.adjacency(g, 
                               attr="weight", 
                               sparse=FALSE)
            return(m)
      }