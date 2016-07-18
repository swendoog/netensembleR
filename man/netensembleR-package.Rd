\name{netensembleR-package}
\alias{netensembleR-package}
\alias{netensembleR}
\docType{package}
\title{
Description, Analysis, and Sampling of Directed Networks
}
\description{
This package provides a variety of functions to statistically describe and analyze 
directed graphs as well as an R-implementation of the 'maximize-and-sample' method for
unbiased sampling of network ensembles.
}
\details{
\tabular{ll}{
Package: \tab netensembleR\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2015-01-19\cr
License: \tab GPL-2\cr
}
An overview of how to use the package, including the most important functions
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
[1] Tiziano Squartini, Rossana Mastrandrea, Diego Garlaschelli,
"Unbiased sampling of network ensembles", http://arxiv.org/abs/1406.1197
(Please replace this preprint version with the final published version, as soon as the latter is available.)

[2] Tiziano Squartini, Diego Garlaschelli, "Analytical maximum-likelihood method to detect patterns in real networks",
New Journal of Physics 13: 083001, (2011).

[3] Tiziano Squartini, Francesco Picciolo, Franco Ruzzenenti, Diego Garlaschelli,
"Reciprocity of weighted networks", Scientific Reports 3: 2729 (2013).

[4] Rossana Mastrandrea, Tiziano Squartini, Giorgio Fagiolo, Diego Garlaschelli,
"Enhanced reconstruction of weighted networks from strengths and degrees"
New Journal of Physics 16: 043022, (2014).
}
\keyword{ package }
\seealso{
\code{\link[igraph:igraph-package]{igraph}}
}
\examples{



# example graph
library(igraph)
g1 <- graph(c(1,2,2,1, 1,3,3,2), directed=TRUE)
E(g1)$weight <- c(2, 7, 3, 4)
V(g1)$name <- c("a", "b", "c")

# plot reciprocated compontent
plot_rw(g1)

# compute rho under the assumption of a weighted random graph model 
rho(g1, g1) # must be 0
rho(g1, nm="WRG")

# find 'hidden variables' of DWCM model
W <- random_Matrix("DWCM", 6)
MAXandSAM(Method="DWCM", Matrix=W)
}
