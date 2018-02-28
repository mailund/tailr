
## Test environments
* local OS X install, R 3.4.3
* ubuntu 14.04 (on travis-ci), R 3.4.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is an initial release.
* This is also a resubmission where "This package" was removed from DESCRIPTION
* Also a resubmission where I have removed Suggest dependencies on a package
  with inappropriate version number (I have a circular dependency between the two
  packages, but only through the tests; the packages use a hook mechanism to 
  communicate, so it was not a real dependency).

