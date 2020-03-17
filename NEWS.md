# IDSpatialStats 1.0.0

## Changes (top of list are most important)
Most of these changes concern the tau statistic functions:
* `get.pi.ci()`, `get.theta.ci()`, `get.tau.ci()`: `quantile` method replaced with `coxed::bca` to 
  compute BCa (bias-corrected and accelerated) confidence intervals (CIs) rather than percentile. 
  Note this will result in a change in your results versus previous versions if if computing CIs.
  `quantile` method also updated to `coxed::bca` where possible in `inst/tests/`.
  At times the `coxed::bca()` method gives slightly different test results if it is applied to
  asymmetric distributions.
* `get.tau()` returns a new S3 `tau` class
* CITATION file added
* README.md formatting updated
* `get.tau$tau` renamed to `get.tau$tau.pt.est`
* Deprecated tests that were previously commented out in `inst/tests/` as a warning of removal
  have not been removed.
* NEWS.md file added, but as you're reading this you probably knew that already ;)

## Bug fixes (top of list are most important)
