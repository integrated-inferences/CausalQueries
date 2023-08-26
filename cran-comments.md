This is a resubmit of a patch release fixing documentation issues and a bug in `realise_outcomes()`. 

**Invalid URI issue is resolved:**

   Found the following (possibly) invalid file URI:  
     URI: Redirect-URL  
       From: README.md  

This "Redirect-URL" is not an URL.

## Test environments

* local Ubuntu 22.04.2 LTS install, R 4.3.0
* win-builder, R version (r-devel)
* win-builder, R version 4.2.3
* win-builder, R version 4.3.1
* macOS, R version 4.3.0

## R CMD check results

0 errors | 0 warnings | 2 note

```
❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.
```
Explanation: GNU make is required for packages build using rstantools. The requirement is specified in the DESCRIPTION file.


```
❯ checking C++ specification ... NOTE
    Specified C++14: please drop specification unless essential
```

Explanation: C++14 is included in Makevars upon compilation and required for certain RcppArmadillo functions. 

