# README #


### netensembleR: Analysis and Sampling of Directed Networks in R ###

An R package that provides a variety of functions to statistically analyze directed graphs as well as an R-implementation of the 'maximize-and-sample' method for unbiased sampling of network ensembles. The package contains an R-implementation of the MAX&SAM method (see references below) for unbiased sampling of network ensembles.

* Current version: 0.1

### How to get started? ###

* Install via the devtools package: install_bitbucket("ulrich-matter/netensembleR")
* Dependencies: see DESCRIPTION-File in Source-directory

### Contribution guidelines ###

* Code review
* Other guidelines: suggestions for extensions etc.

### References ###
The part of the package containing the R-implementation of the MAX&SAM method is a (partial) translation of MAXandSAM.m of the 'MAX&SAM'-matlab package. The original matlab code copyright is: Mastrandrea Rossana, 2014. See Tiziano Squartini, Rossana Mastrandrea, and Diego Garlaschelli (2015) "Unbiased sampling of network ensembles", New Journal of Physics 17: 023052, and http://www.mathworks.com/matlabcentral/fileexchange/46912-max-sam-package-zip.