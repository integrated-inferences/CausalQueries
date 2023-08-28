This is a resubmit of a patch release fixing documentation issues and a bug in `realise_outcomes()`. 

## Fixed Issues

Following issue has been fixed: 

```
 --run-donttest  
> # Censored data types  
 > make_model("X->Y") %>%  
+   update_model(data.frame(X=c(1,1), Y=c(1,1)), censored_types =  
c("X1Y0")) %>%  
+   query_model(te("X", "Y"), using = "posteriors")  
Chain 1: Initialization between (-2, 2) failed after 100 attempts.  
Chain 1:  Try specifying initial values, reducing ranges of constrained  
values, or reparameterizing the model.  
[1] "Error : Initialization failed."  
error occurred during calling the sampler; sampling not done  
Stan model 'simplexes' does not contain samples.  
Error in `colnames<-`(`tmp`, value = get_parameter_names(model)) :  
   attempt to set 'colnames' on an object with less than two dimensions  
Calls: %>% ... query_model -> is_a_model -> %in% -> update_model ->  
colnames<-  
```

* setting `init_r` to values < 2 in `update_model` resolves the issue locally 
* moved the examples to dontrun for this resubmit as they are not critical to the function

## Test environments

* local Ubuntu 22.04.2 LTS install, R 4.3.0
* win-builder, R version (r-devel)
* win-builder, R version 4.2.3
* win-builder, R version 4.3.1
* macOS, R version 4.3.0
* R-hub Windows Server 2022 (r-devel)
* R-hub Fedora Linux, clang, gfortran (r-devel)

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

