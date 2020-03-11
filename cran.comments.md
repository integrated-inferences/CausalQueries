## Test environments

* local OS X install, R 3.6.2
* Ubuntu 14.04 (on travis-ci), R-oldrel R-release, R-devel
* OS X on travis-ci, R-oldrel R-release, R-devel
* win-builder, R-oldrel,  R-release, R-devel

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTES.

There was 1 NOTE:

checking for GNU extensions in Makefiles ... 
NOTE GNU make is a SystemRequirements.

Explanation: GNU make is required for packages build using rstantools. The requirement is specified in the DESCRIPTION file.
