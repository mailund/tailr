
* This is release 0.1.2 that resolves an issue when transformations were
  used as part of other packages. CMD CHECK would fail if tailr wasn't
  imported but just called.

## Test environments

* local OS X install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.2 - 3.4
* win-builder (devel and release)
* Rhub:
    - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    - Fedora Linux, R-devel, clang, gfortran
    - Ubuntu Linux 16.04 LTS, R-release, GCC
    

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

 * pmatch -- checks without issues using this release.
