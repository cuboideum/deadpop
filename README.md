# Analysis of Skeletal Populations

## Description

This R package is an appendix to Felix Engel's dissertation thesis "Traces of Armed Conflict: Methodological Implications from Anthropological Graveyard Analyses - Applied to the Site lauchheim 'Wasserfurche' With Special Reference to Cranial Trauma". It provides functions for a number of analyses on skeletal material developed and discussed there:

* Moving focus approach for assessing spatial patterns in cemetery layouts (function 'SpatialNeighbours')
* Calibration of Prevalences (function 'Pcal')
* Production of age profiles from age-at-death estimates (function 'ProportionalAgeDistribution')
* Simulation of material preservation bias on anthropological analyses (functions 'PresMod' and 'PresSer')
* Assessment of material preservation bias in anthropological analyses (function 'Pderiv')

## Installation using *devtools*

You can use the *devtools* package to install this package directly from this repository by following these steps:

1. Install *devtools* from within R

```R
install.packages("devtools")
```
Depending on your system, various prerequisites might have to be met for *devtools* to install correctly. Please refer to the respective documentation.

2. Install *deadpop* from within R

```R
require(devtools)
install_github("cuboideum/deadpop", local = FALSE)
```
