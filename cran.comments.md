# First Submission

## Test environments

* local OS X install (oldrel and release)
* Ubuntu 14.04 on travis-ci (release and devel)
* win-builder (oldrel, release and devel) 


## R CMD check results

There were no ERRORs or WARNINGs. 2 NOTEs:
```
checking for GNU extensions in Makefiles ... 
NOTE GNU make is a SystemRequirements.
```
Explanation: GNU make is required for packages build using rstantools. The requirement is specified in the DESCRIPTION file.

```
 checking CRAN incoming feasibility ... NOTE
Maintainer: 'Lily Medina <lilymiru@gmail.com>'
```



