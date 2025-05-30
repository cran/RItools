# RItools 0.3-5

* Now "Depends" on the `survival` package to bring in the `strata` and `cluster`
functions (#141).

# RItools 0.3-4

* Using `.lm.fit()` within `slm.fit.csr.fixed` given breaking changes in `SparseM::chol()` (#134)

# RItools 0.3-3

* Adjusted arguments to `xtable.xbal()` to match the generic. (#131)

# RItools 0.3-2

* Adjusted terminology on `balanceplot()` plots when using `absolute=TRUE`.
  (#129)
* Updated some **tidyverse**/**ggplot2** code to deal with deprecations.
* Using `bibentry()` in citation to address deprecation.


# RItools 0.3-1

* Skipped tests that cause problems with systems using the ATLAS linear algebra
  library.

# RItools 0.3-0

* Exporting `broom::glance()` and `broom::tidy()` methods for balance test
  objects, a work in progress (#90).
* Addressed error relating to setting of pseudo-inversion tolerances (#103).
  xBalance stops short of correcting the error, to maintain
  back-compatibility; but it gains a new argument, pseudoinversion_tol, with
  which it can readily be fixed.
* New balanceTest function extends functionality of xBalance:
  * Support for clustered designs
  * More use of formula interface
  * Multiple comparison adjustments for individual covariate tests
  * Support for unit level weighting
* Introduced ggplot2 based plotting. Optional for xBalance objects; default for
  new balanceTest objects.

# RItools 0.2

This was an internal release not placed on CRAN.

# RItools 0.1-18

This is a maintenance release.

* Ensured that the package was compatible with R 4.1.2

# RItools 0.1-17

This is a maintenance release.

* Ensured that the package was compatible with R 3.6.0

# RItools 0.1-16

This is a maintenance release.

* Fixed a problem where the test suite was calling an external package that was
  not a formal dependency of the core functionality of the package.

# RItools 0.1-15

This is a maintenance release.

* Fixed an obscure bug in which we indirectly fiddled with `data.table` related
  global options (#69)
* Disabled tests of RSVGTipsDevice-dependent functionality on Windows platforms,
  where (as of this writing) RSVGTipsDevice does not build and check reliably
  and is not distributed in binary through CRAN (#71)

# RItools 0.1-14

This version was submitted to CRAN prematurely and ultimately was not released.

# RItools 0.1-13

* With `xBalance()`, you can now specify strata "foo" and "bar" by including "+
  strata(foo) + strata(bar)" in the `fmla` argument, without need to give a
  separate `strata` argument (eg `strata=list(foo=~foo, bar=~bar)`).

# RItools 0.1-12

* Switched to base graphics instead of the lattice package for balance
  plots.
* Option to make plots on absolute, instead of signed, values.
* Added the ability to include tooltips to balance plots when using
  the RSVGTipsDevice package.
* Added a `balanceplot()` function to handle plotting matrices, not just `xbal`
  objects.
* Balance plots can be grouped into related variables. By default
  factors are automatically grouped for `xbal` objects.
* Some performance improvements when certain results are not requested.
* `post.alignment.transform` argument to `xBalance()` allows modifying the data
  after per-stratum centering.
* `subset` method for `xbal` objects.

# CHANGES in RItools v. 0.1-11

## NEW FEATURES
* the null SD of the difference in adjusted means is now reportable as
  "adj.means.diffs.sd"
* `report="all"` provides all relevant statistics
* Entering `args(xBalance)` is now useful as a reminder of report= options.

## DEPRECATED & DEFUNCT
* The default imputation method for missing data is the median. Before 0.1-9 it
  was the mean. To use the mean, use `impfn=mean.default`.
* Passing `strata=NULL` to `xBalance()` is depracated, as it can be easy to do
  inadvertently, with a misspelling of a non-null intended argument. For no
  stratification, use `list(unstrat=NULL)`, as is now the default.

##  BUG FIXES
* `display`, `align`, etc arguments to `xtable.xbal()` have been enabled.
* In `xBalance()`, when specifying a stratification using a formula with several
  variables, as in `~x+y`, the stratification you get is now interaction(x,y)
  rather than just y.
