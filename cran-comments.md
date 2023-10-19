This is a patch release fixing a minor querying bug related to helper functions 
drawing new prior distributions when a prior distribution is already set for a given 
model. 

## Test environments

* local Ubuntu 22.04.3 LTS install, R 4.3.1
* win-builder, R version (r-devel)
* win-builder, R version 4.2.3
* win-builder, R version 4.3.1
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



