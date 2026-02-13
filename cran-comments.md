This patch release reverts changes made to the main Stan model in 1.4.4 which 
introduced a bug when updating models with multiple data strategies.
This patch release additionally reintroduces linking to BH as dropping this package
introduced compilation issues on some systems.

## Test environments

* local Ubuntu 24.04.3 LTS install, R 4.5.1
* win-builder, R version 4.4.3 
* win-builder, R version 4.5.2
* win-builder, R r-devel
* macOS, R version 4.5.1

## R CMD check results

0 errors | 0 warnings | 0 notes




