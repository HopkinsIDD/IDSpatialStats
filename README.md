# IDSpatialStats

This github repository provides source code for the `IDSpatialStats` R package, which is designed to help epidemiologists assess the scale of spatial and temporal dependence in epidemic case occurrence data. 

The current implementation of the package includes a function which simulates infectious disease spread as a spatial branching process, along with two novel spatial statistics that estimate: 1) the mean of the spatial transmission kernelm which is a measure of fine-scale spatial dependence between two cases, that is interpreted as the mean distance between sequential cases in a transmission chain, and 2) the tau-statistic, a measure of global clustering based on pathogen subtype.

This package is maintained by John Giles (@gilesjohnr) and Justin Lessler (@jlessler) as part of the Johns Hopkins Bloomberg School of Public Health Infectious Disease Dynamics team (@HopkinsIDD).

### Installation

To install the offical release of the `IDSpatialStats` package, open `R` and type:

```
install.packages('IDSpatialStats')
```
