# IDSpatialStats 1.0.0

## Changes (top of list are most important)
These changes mostly concern the tau statistic functions. These are big changes and we may have 
unwittingly introduced bugs. Please send us a reproducible example if you find one.

Specific changes
* `get.pi.ci()`, `get.theta.ci()`, `get.tau.ci()`: `quantile` method replaced with `coxed::bca` to 
  compute BCa (bias-corrected and accelerated) confidence intervals (CIs) rather than percentile. 
  Note this will result in a change in your results versus previous versions if if computing CIs.
  `quantile` method also updated to `coxed::bca` where possible in `inst/tests/`.
  At times the `coxed::bca()` method gives slightly different test results if it is applied to
  asymmetric distributions.
* `get.tau()` returns a new S3 `tau` class with special methods for:
  * `plot()` provides a tau-distance graph with an option for pointwise confidence intervals for 
  a visual indication of spatiotemporal clustering. In this version we use error bars as default, to
  remind the reader that this graph should not be used as a graphical hypothesis test for the whole 
  distance range observed (CITE!). It is only suitable for the purpose of a graphical hypothesis
  test if a specific distance band is decided before looking at the graph to see if it encloses
  tau=1. 
  
that follow current best-practice mentioned in a recent review of the tau statistic.
* CITATION file added
* README.md formatting updated
* `get.tau$tau` renamed to `get.tau$tau.pt.est`
* Deprecated tests that were previously commented out in `inst/tests/` as a warning of removal
  have not been removed.
* NEWS.md file added, but as you're reading this you probably knew that already ;)

Generic changes: 
* ability to have distance units defined and automatically plotted on graph
* documentation updated, with rationale for use with linked references to recent literature which has
informed this change.

## Bug fixes (top of list are most important)

# Changes on the horizon
Please note that the Modified Marked Point Spatial Bootstrap as described in !CITE has not yet been
applied. In this reference !CITE it was applied to the tau odds estimator however for consistency 
we have decided to delay its implementation so that we can apply it also to the tau prevalence
estimator also, and thus sync the implementations across all tau estimators. Therefore please be 
aware that values from `get.tau.bootstrap()` and `get.tau.D.param.est()` are still due to change.