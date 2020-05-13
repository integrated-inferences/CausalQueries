# Resubmission
## Addressing comments on first submission 

- Please always add all authors, contributors and copyright holders in the
Authors@R field with the appropriate roles.

Macartan Humphreys, Alan Jacobs and Lily Medina are the authors of 'CausalQueries' and are now listed are as such. The rest of the team are listed as contributors. 


- Please add \value to .Rd

Thank you for pointing at this. We added value to Rd's


- You have examples for unexported functions which cannot run in this way.
Please either add packagename::: to the function calls in the examples,
omit these examples or export these functions.

Thanks. We fixed it.

- Please replace \dontrun with \donttest.
We \dontrun with \donttest when appropiate.

- Possible typos:
     biqq (12:283)
     dagitty (12:165)
     estimands (12:123)
     stan (12:215)
     
Thanks'biqq', 'dagitty' and 'stan' are now either within '' or linked to corresponding packages. Estimands is a statistical term we employ throughout the package documentation

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





