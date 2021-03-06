## IDSpatialStats

This GitHub repository provides source code for the `IDSpatialStats` R package, which is designed to help epidemiologists assess the scale of spatial and temporal dependence in epidemic case occurrence data. 

The current implementation of the package includes a function which simulates infectious disease spread as a spatial branching process, along with two novel spatial statistics that estimate: 1) the mean of the spatial transmission kernel, which is a measure of fine-scale spatial dependence between two cases, and 2) the tau-statistic, a measure of global clustering based on pathogen subtype.

Detailed description of the methods can be found here:

[The IDSpatialStats R package: Quantifying spatial dependence of infectious disease spread (Giles et al. 2019)](https://journal.r-project.org/archive/2019/RJ-2019-043/index.html)

[Measuring spatial dependence for infectious disease epidemiology (Lessler et al. 2016)](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0155249)

[Estimating infectious disease transmission distances using the overall distribution of cases (Salje et al. 2016)](http://www.sciencedirect.com/science/article/pii/S1755436516300317)

This package is maintained by John Giles ([@gilesjohnr](https://github.com/gilesjohnr)) and Justin Lessler ([@jlessler](https://github.com/jlessler)) as part of the Johns Hopkins Bloomberg School of Public Health Infectious Disease Dynamics team ([@HopkinsIDD](https://github.com/HopkinsIDD)).

### Installation

To install the offical release of the `IDSpatialStats` package, open `R` and type:
```r
install.packages('IDSpatialStats')
```

To install the install the development version, first install the `devtools` package and then install `IDSpatialStats` from source via GitHub:
```r
install.packages('devtools')
devtools::install_github('HopkinsIDD/IDSpatialStats')
```

### Troubleshooting

For general questions, contact package maintainers John Giles (giles@jhu.edu) or Justin Lessler (justin@jhu.edu).

To report bugs or problems with documentation, please go to the [Issues](https://github.com/HopkinsIDD/IDSpatialStats/issues) page associated with this GitHub page and click *new issue*.

If you wish to contribute to `IDSpatialStats`, please get in touch via email and then fork the latest version of the package. After committing your code to your own forked version, submit a pull request when you are ready to share.
