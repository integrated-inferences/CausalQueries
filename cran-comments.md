**Resubmission**: This is a major release introducing non backwards compatible changes to querying functionality. 
This release additionally fixes bugs related to parameter, parameter matrix, prior and restriction setting, 
updating with censored data and query specification. It finally eliminates computational bottlenecks and improves 
speed and usability. 

## Resolved Issues 

Issue: 

```
Flavor: r-devel-linux-x86_64-debian-gcc
Check: Rd files, Result: NOTE
  checkRd: (-1) get_event_prob.Rd:21: Lost braces
      21 | An {array} of event probabilities
         |    ^
```

has been fixed.

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



