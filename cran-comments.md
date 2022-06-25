This is a minor release introducing new and simplified user facing functionality.

## Test environments

* local Windows install, R 4.1.1
* win-builder, R version (r-devel)
* win-builder, R version 4.2.1
* win-builder, R version 4.1.3
* R-hub windows-x86_64-devel (r-devel)
* R-hub fedora-clang-gfortran-devel (r-devel)
* macOS, R version 4.1.1
* ubuntu, R version 4.1.1

## R CMD check results

0 errors | 0 warnings | 2 note

```
checking for GNU extensions in Makefiles ... 
NOTE GNU make is a SystemRequirements.
```
Explanation: GNU make is required for packages build using rstantools. The requirement is specified in the DESCRIPTION file.


```
Compilation used the following non-portable flag(s):
    '-mmmx' '-msse' '-msse3' '-msse4.1' '-msse4.2' '-mssse3'
```




