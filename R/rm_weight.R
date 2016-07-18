# a) a function that removes one weight from the vote trade graph g
#    g a directed weighted graph based on real data. 
#    i the index of the edge from which a weight should be removed 
rm.weight <-
function(i, g) {
            weights <- edge.attributes(g)$weight
            weights[i] <- weights[i] -1
            edge.attributes(g)$weight <- weights
            
            return(g)
      }
