# d) remove non-reciprocated components from graph (remove all edges without reciprocated weight, then remove all verteces without edges)
#    g, a directed weighted graph (object of class igraph)
rm_nonRecipro <-
function(g){
            eids <- get_rwEdges(g, recipro=FALSE)
            g <- g - edge(eids)
            g <- delete.vertices(g, degree(g)==0)
            
            return(g)
      }
