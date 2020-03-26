# News on IDSpatialStats 1.0.0 release

## Changes (top of list are most important)
These changes mostly concern the tau statistic functions and have resulted in a major change revision from 0.3.9 to 1.0.0: these are big changes and we may have introduced bugs so please send us a `reprex()` example. We also note where function outputs are likely to have changed. 

# Specific changes
Percentile confidence intervals (CIs) replaced with BCa (bias-corrected and accelerated) CIs
* `get.pi.ci()`, `get.theta.ci()`, `get.tau.ci()`: `quantile` method replaced with `coxed::bca`. This will change CI results.
  
* `quantile` method also updated to `coxed::bca` where possible in `inst/tests/`. At times the `coxed::bca()` method gives slightly different test results if it is applied to asymmetric distributions.
  
* New function `get.tau.GET()` performs graphical hypothesis tests with the tau statistic using the `GET` package, while `get.tau.D.param.est()` estimates the range of spatiotemporal clustering.

* `plot.tau()` is a new method that can produce three types of tau(y-axis)-distance(x-axis) plots [[1: see Graphical abstract](https://doi.org/10.1016/j.spasta.2020.100438 "Developments in statistical inference when assessing spatiotemporal disease clustering with the tau statistic")]:
  * Diagnostic to indicate the structure or magnitude of spatiotemporal clustering. Requires `tau` object; `tauCI` object optional to draw pointwise CIs. In this version we use piecewise error bars rather than continuous envelope lines. This reminds the user that this graph should not be used as a graphical hypothesis test for the whole distance range observed. It is only suitable for the purpose of a graphical hypothesis test for a specific distance band if that band is decided prior to graph creation.
  * Graphical hypothesis tests to assess the evidence against the Null hypothesis (no spatiotemporal clustering nor inhibition). Requires `tau` and `tauGET` objects.
  * Estimation of the clustering range (the distribution of the places on the horizontal tau=1 line, where decreasing bootstrap simulations first intercept). Requires `tau` and `tauparamest` objects; prior to this `get.tau.D.param.est()` requires a `taubstrap` object.

* New S3 classes have been added to the return objects from the following functions. The purpose of the new classes is to encourage the use of functions in an ordered and principled way, in keeping with good practices of statistical inference [[1](https://doi.org/10.1016/j.spasta.2020.100438 "Developments in statistical inference when assessing spatiotemporal disease clustering with the tau statistic")]. It also means that the objects inputted as function arguments are of a known format:
  * `get.tau()` returns a `tau` class
  * `get.tau.ci()` returns a `tauCI` class
  * `get.tau.GET()` returns a `tauGET` class
  * `get.tau.bootstrap()` returns a `taubstrap` class
  * `get.tau.D.param.est()` returns a `tauparamest` class. Requires a `taubstrap` object. Also requires a `tauGET` class to ensure the user has performed a graphical hypothesis test first, before considering parameter estimation.

* CITATION file added
* README.md formatting updated
* `get.tau$tau` renamed to `get.tau$tau.pt.est`
* Previously deprecated tests (already commented out in `inst/tests/`) have now been removed.
* NEWS.md file added, but as you're reading this you probably knew that already :wink:

# Generic changes
* distance units can be defined on `r` and `r.low` and will be automatically feature in the x-axis label of `plot.tau()`
* help files added/updated for `get.tau()`, `get.tau.ci()`, `get.tau.bootstrap()`, `get.tau.GET()`, `get.tau.d.param.est()` & `plot.tau()`
* example files added/updated for `get_tau.R`, `get_tau_bootstrap.R`, `get_tau_ci.R`, `get_tau_GET.R`, `get_D_param_est.R` and `plot_tau.R`.

## Bug fixes (top of list are most important)
`get_tau.R`: using `geno.tau.R02$tau.pt.est` now allows the object to be accessed and the example run.

# Release contributors
Timothy M Pollington would like to thank the co-authors of the paper that informed this update[[1](https://doi.org/10.1016/j.spasta.2020.100438 "Developments in statistical inference when assessing spatiotemporal disease clustering with the tau statistic")] and the *essential* contribution of Peter J. Diggle (Lancaster) who advised on this principled inferential approach.

# Next changes
* The *Modified Marked Point Spatial Bootstrap* [[1](https://doi.org/10.1016/j.spasta.2020.100438 "Developments in statistical inference when assessing spatiotemporal disease clustering with the tau statistic")] has not yet been
applied. In [[2](https://doi.org/10.5281/zenodo.2552850 "t-pollington/tau-statistic-speedup: First release of tau statistic speedup")] it was applied to the tau odds estimator however for consistency we have decided to delay its implementation so that we can apply it also to the tau prevalence estimator. So please note that `get.tau.bootstrap()` and `get.tau.D.param.est()` values are still likely to change.
* Changes to the un-typed tau functions also applied to the typed tau functions.