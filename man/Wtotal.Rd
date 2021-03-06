% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Wtotal.R
\name{Wtotal}
\alias{Wtotal}
\title{Total weight of weighted network}
\usage{
Wtotal(g)
}
\arguments{
\item{g}{a directed weighted graph (object of class igraph)}
}
\value{
numeric scalar
}
\description{
Returns the total weight f a weighted network.
}
\examples{

# graph example
library(igraph)
g2 <- graph(c(1,2,2,1), directed=TRUE)
E(g2)$weight <- c(2,7)
Wtotal(g2)

}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}

