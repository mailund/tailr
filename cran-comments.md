
* This is release 0.1.3 that only contains changes for rlang 0.3.0 compatibility.
  It does not add any new functionality.

## Test environments

* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.2 - 3.5
* win-builder (devel and release)
* Rhub:
    - Fedora Linux, R-devel, clang, gfortran
    - Ubuntu Linux 16.04 LTS, R-release, GCC
    

## R CMD check results

0 errors | 0 warnings | 1 note

The node is caused by expressions on the form `!!exp1 <- !!exp2`
which are handled correctly by rlang 0.3.0 but raises issues
with the CHECK:

❯ checking R code for possible problems ... NOTE
  build_transformed_function: no visible global function definition for
    ‘!<-’
  translate_recursive_call: no visible global function definition for
    ‘!<-’
  Undefined global functions or variables:
    !<-


## Downstream dependencies

 * pmatch -- checks without issues using this release.
