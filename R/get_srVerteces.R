# a) internal function: get ids of verteces that have a reciprocicated strength >0
#    g, a directed weighted graph (object of class igraph)
get_srVerteces <-
function(g){
           vs <-  V(g)$name
           srs <- unlist(lapply(vs, sr, g=g))
           vs_r <- vs[srs>0] 
           
           return(vs_r)
      }
