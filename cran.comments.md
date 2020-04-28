# First Submission

## Test environments

* local OS X install (R-oldrel and R-release)
* Ubuntu 14.04 on travis-ci (R-release and R-devel)
* Windows 10 (R-release)


## R CMD check results

* For OS X and Ubuntu 

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

* For local Windows
There were no ERRORs or WARNINGs. 4 NOTEs:

Same notes as above plus:

```
checking line endings in C/C++/Fortran sources/headers ... NOTE

Found the following sources/headers with CR or CRLF line endings:
src/stanExports_simplexes.cc
src/RcppExports.cpp
src/stanExports_simplexes.h
```
Explanation: source files are created and compiled on each platform using rstantools during installation. Thus, CR and CRLF line endings would only appear on Windows.

```
checking for non-standard things in the check directory ... NOTE
Found the following files/directories:

'CausalQueries-Ex_i386.Rout' 'CausalQueries-Ex_x64.Rout'
'examples_i386' 'examples_x64' 'tests_i386' 'tests_x64'
```
 Output of the examples seems to be non-standard things.
