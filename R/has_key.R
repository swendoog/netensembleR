# internal function
# returns TRUE if list l contains an element named after the string in the key parameter
# example
# dict <- list(Name="Zara", Age=7 )
# has_key(dict, "Sex")
# has_key(dict, "Age")

has_key <-
function(l, key){
            lnames <- names(l)
            has <- any(lnames %in% key)
            
            return(has)
      }
