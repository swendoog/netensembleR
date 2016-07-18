# internal function to check whether a value is a whole number (is.integer does not do that!!!)
# code is taken from example in help(is.integer), integer {base}

is_wholenumber <-     
      function(x, tol = .Machine$double.eps^0.5) { 
            abs(x - round(x)) < tol
      }