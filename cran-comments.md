
# This is a resubmission of version 0.1.3 that silences a NOTE
  caused by quasi-quotation rlang calls.

# Main changes for version 0.1.3

* Rewrote transformations to use the foolbox framework. (new dependency)
* Made changes for rlang 0.3.0 compatibility. (updateded dependency)
* Removed dependency on package pmatch. (removed dependency)
  I still use pmatch 0.1.5 in the README examples, but I have no
  other dependencies on the package.


## Test environments

* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.2 - 3.5
* win-builder (devel and release)
* Rhub:
    - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    - Fedora Linux, R-devel, clang, gfortran
    - Ubuntu Linux 16.04 LTS, R-release, GCC
    

## R CMD check results

0 errors | 0 warnings | 0 notes
