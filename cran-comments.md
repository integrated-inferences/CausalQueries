This is a patch release fixing a bug in the `print.model_query()` S3 method that 
occurred when querying models using `paramters`.

## Test environments

* local Ubuntu 22.04.3 LTS install, R 4.3.2
* win-builder, R version 4.4.0 beta
* win-builder, R version 4.2.3
* win-builder, R version 4.3.3
* macOS, R version 4.3.3
* R-hub Windows Server (r-devel)
* R-hub Fedora Linux, clang, gfortran (r-devel)

## R CMD check results

0 errors | 0 warnings | 1 note

```
❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
```
Explanation: GNU make is required for packages built using rstantools. The requirement is specified in the DESCRIPTION file.



