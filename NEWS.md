# IDSpatialStats 1.0.0

## Changes (top of list are most important)
* `get.pi.ci()`, `get.theta.ci()`, `get.tau.ci()`: `quantile` method replaced with `coxed::bca` to 
  compute BCa (bias-corrected and accelerated) confidence intervals (CIs) rather than percentile. 
  Note this will result in a change in your results versus previous versions if if computing CIs.
  `quantile` method also updated to `coxed::bca` where possible in `inst/tests/`.
  At times the `coxed::bca()` method gives slightly different test results if it is applied to
  asymmetric distributions.
* NEWS.md file added
* CITATION file added
* README.md formatting updated
* Deprecated tests that were previously commented out in `inst/tests/` as a warning of removal
  have not been removed.

## Bug fixes (top of list are most important)
