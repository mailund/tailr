# tailr 0.1.3

* Rewrote transformations to use the foolbox framework.
* Made changes for rlang 0.3.0 compatibility.
* Removed dependency on package pmatch
  I still use pmatch 0.1.5 in the README examples, but I have no
  other dependencies on the package.

# tailr 0.1.2

* The transformation function do no longer use !!! and this seems
  to solve problems with CMD CHECK in downstream packages.
* the srcref attribute gets set (to the source of the input function) in
  loop_transform.

# tailr 0.1.1

* Handles functions when they are local variables.

# tailr 0.1.0

* Initial release.
