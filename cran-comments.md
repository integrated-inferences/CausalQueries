# First submission
## Addressing comments on previous submission 

Possibly mis-spelled words in DESCRIPTION:
     estimands (13:123)

Do you mean estimates?
- Thanks for checkin with us.  Estimands is correct. We mean by estimand the target variable to be estimated. We also employ the term estimate in other parts of the documentation by which we mean the calculated point value of an estimand. I hope that clarifies.

Please correct in your Description test:

"2009,"  -->  "2009, "
 
- Done

Missing Rd-tags:
      add_dots.Rd: \value
      add_wildcard.Rd: \value
      causal_type_names.Rd: \arguments,  \value
      CausalQueries_internal_inherit_params.Rd: \value
      check_string_input.Rd: \value
      clean_condition.Rd: \value
      clean_param_vector.Rd: \value
      ....
Please add the tag and explain in detail the returned objects.

- Done

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





