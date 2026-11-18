This patch release fixes a vignette rebuilding error flagged by CRAN on Linux
for CausalQueries 1.4.6. Recent Pandoc versions rejected the vignette YAML
metadata with the error `']' is indented too little to end the flow sequence`.
The vignette headers have been corrected.

## Test environments

* local Ubuntu 24.04.3 LTS install, R 4.5.1
* win-builder, R version 4.5.3
* win-builder, R version 4.6.1
* win-builder, R r-devel
* macOS, R version 4.5.1

## R CMD check results

0 errors | 0 warnings | 0 notes

