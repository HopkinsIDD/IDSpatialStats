# IDSpatialStats 1.0.0

## Changes (top of list are most important)
* `get.pi.ci()`, `get.theta.ci()`, `get.tau.ci()`: `quantile` method replaced with `coxed::bca` to 
  compute BCa (bias-corrected and accelerated) confidence intervals (CIs) rather than percentile. 
  Note this will result in a change in your results versus previous versions if if computing CIs.
* NEWS.md file added
* CITATION file added
* README.md formatting updated

## Bug fixes (top of list are most important)
