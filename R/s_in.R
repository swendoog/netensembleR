# d) return the in strength of vertex i
# g a directed weighted graph
# i the ids or symbolic vertex names of verteces to compute the out/in strength
s.in <-
function(g, i) {
            graph.strength(g, i, mode="in", loops=FALSE)   
      }
