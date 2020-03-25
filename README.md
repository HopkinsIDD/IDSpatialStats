# IDSpatialStats

**Previous users: please read [News on the latest release](../master/NEWS.md "News on the latest release") to update you on changes to the tau statistic functions.**

This GitHub repository provides source code for the `IDSpatialStats` R package, which is designed to help epidemiologists assess the scale of spatial and temporal dependence in epidemic case occurrence data. This package can simulate infectious disease spread as a spatial branching process, along with two novel spatial statistics that estimate: 

1. the mean of the spatial transmission kernel, which is a measure of fine-scale spatial dependence between two cases, and 
2. the tau statistic &tau;, a measure of global clustering based on pathogen subtype and/or, serotype and/or case onset time.

This package is maintained by John Giles (GitHub: @[gilesjohnr](https://github.com/gilesjohnR)) and Justin Lessler (GitHub: @[jlessler](https://github.com/jlessler)) as part of the Johns Hopkins Bloomberg School of Public Health Infectious Disease Dynamics team (GitHub: @[HopkinsIDD](https://github.com/HopkinsIDD)).

## Detailed description of the methods and relevant literature

[The IDSpatialStats R package: Quantifying spatial dependence of infectious disease spread (Giles et al. accepted)](https://journal.r-project.org/archive/2019/RJ-2019-043/index.html)

[Measuring spatial dependence for infectious disease epidemiology (Lessler et al. 2016)](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0155249)

[Estimating infectious disease transmission distances using the overall distribution of cases (Salje et al. 2016)](http://www.sciencedirect.com/science/article/pii/S1755436516300317)

[Measuring spatiotemporal disease clustering with the tau statistic (Pollington et al. in review)](https://arxiv.org/abs/1911.08022)

[The spatiotemporal tau statistic: a review (Pollington et al. preprint)](https://arxiv.org/abs/1911.11476)

## Installation

To install the official release of the `IDSpatialStats` package, open `R` and type:
```
install.packages('IDSpatialStats')
```

To install the development version, first install the `devtools` package and then install `IDSpatialStats` from source via GitHub:
```
install.packages('devtools')
devtools::install_github('HopkinsIDD/IDSpatialStats')
```

## Troubleshooting and contributions

For general questions, contact package maintainers John Giles (giles@jhu.edu) or Justin Lessler (justin@jhu.edu).

To report bugs or problems with documentation, please go to the [Issues](https://github.com/HopkinsIDD/IDSpatialStats/issues) page associated with this GitHub page and click *New issue*.

If you wish to contribute to `IDSpatialStats`, please first get in touch via email. Then please:

1. Fork a copy of the current development version on GitHub
2. Add your functions and edits to your forked copy
  * pay attention to existing naming conventions and outputs
  * add examples
  * line comments are welcome for non-intuitive commands.
  * commit to your own forked version:
    * often
    * describe what was done and why, but not how
    * use the imperative
    * $\leq$ 72 characters  
      e.g. "*Replace percentile CI with BCa CI, as tau bootstrap distrib. non-symm*"

3. Any modified functions must return identical output as the current functions. Check that modified functions return the same output using package `testthat` and consider writing new test cases if appropriate. For new functions, write test cases should test that functions return expected values given expected inputs, and that they behave as expected in boundary conditions.
4. Add conditional stops to functions so that they fail gracefully with unexpected inputs.
5. Submit a pull request when you are ready to share.