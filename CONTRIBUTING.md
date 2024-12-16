## Contributing to CausalQueries

:tada: Welcome to our contribution guidelines and thank you for your interest in contributing! 

  
### Reporting a bug
  
  Before you report a bug make sure the same bug hasn't been reported before. We track bugs as [issues on GitHub](https://github.com/macartan/CausalQueries/issues). 
  If no related issue has been opened, create an issue keeping in mind the following guidelines:
  
  - Use an informative title
  - Write a minimal working example that allows us to reproduce the bug you found
  - If the bug you're reporting crashes your R session, please mention that in the title
  
### Contributing code
  
  You have had a look at our [issues on GitHub](https://github.com/macartan/CausalQueries/issues) and would like to solve one of them? or you would like to develop a feature? 
  That's great and we gladly welcome that. We just would like to suggest you follow these simple guidelines:
  
  - Fork the [CausalQueries repository](https://github.com/macartan/CausalQueries) 
  - Clone your fork locally 
  - Always be up to date with the `master` branch
  - Add your edits
  - Run and pass `devtools::check()`
  - Reach a 100% coverage `covr::package_coverage()`
  - Add yourself as a contributor in the `DESCRIPTION` file
  - Open a pull request
  
  
  Note: members of the `CausalQueries` dev team can skip the first two bullet points above and branch out instead.
  
### Updating or writing vignettes 
  
  As vignettes making use of `update_model()` may have long run-/compile times and we wish to minimize time and resource demands both when 
  testing an deploying `CausalQueries`; vignette writing and updating makes use of a slightly modified workflow. 
  Vignettes are complied locally from a `vignette-name.Rmd.orig` file; its outputs are dumped to the `vignette` folder and a `vignette-name.Rmd` 
  file with non-executable code chunks is constructed from these outputs. This workflow is handled via the `build_vignettes()` function in 
  the package.
  
  If you wish to add a new vignette please proceed as follows: 
  
  1. write your vignette as a R markdown file with executable code chunks + formatting in the normal fashion
  2. save this file as `vignette-name.Rmd.orig` in the vignettes folder (replace `vignette-name` with your desired vignette name here)
  3. run `build_vignettes()` specifying the name of your vignette without any file extension as an optional argument i.e. `build_vignettes("vignette-name")` 
  4. add your vignette's name as a default in the `vignettes` argument of `build_vignettes()`
  5. push changes to github 
  
  If you wish to update an existing vignette proceed as follows: 
  
  1. make desired changes in the `vignette-name.Rmd.orig` file 
  2. run `build_vignettes()` 
  3. push changes to github
  
