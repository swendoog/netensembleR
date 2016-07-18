# d) return the out strength of vertex i
# g a directed weighted graph
# i the ids or symbolic vertex names of verteces to compute the out/in strength
s.out <-
function(g, i) {
            graph.strength(g, i, mode="out", loops=FALSE)   
      }
