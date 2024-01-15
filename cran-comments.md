This is a patch release implementing the following bug fixes:

* passing nodal_types to `make_model()` now implements correct error handling 
* `query_distribution()` no longer overwrites type distributions internally 
* node naming checks are operational in `make_model()`

This patch release further improves type safety and implements range based
looping throughout the package. 

## Test environments

* local Ubuntu 22.04.3 LTS install, R 4.3.1
* win-builder, R version (r-devel)
* win-builder, R version 4.2.3
* win-builder, R version 4.3.2
* macOS, R version 4.3.0
* R-hub Windows Server 2022 (r-devel)
* R-hub Fedora Linux, clang, gfortran (r-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

```
❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
```
Explanation: GNU make is required for packages built using rstantools. The requirement is specified in the DESCRIPTION file.



