# job 0.3.1

## Notes for the reviewer
 
 * This package is RStudio-specific so automated R CMD Checks cannot test the functionality. It does have an extensive `testthat` test suite which has been run manually in the following RStudio environments with no ERRORs or WARNINGs:
   
## Manual test environments
* Windows 10, R 4.3.3 RStudio 2024.04.0
* Ubuntu 24.04, R 4.3.3 RStudio 2024.04.0
* Mac OS, R 4.0.5 RStudio 1.4.1106

## Downstream dependencies
We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages



# Resubmission 2
This is a resubmission. For this version, I have:

 * Removed the quotes in the DESCRIPTION text.
 * Removed call to `options(warn = -1)`.
 * Added \value to `print.jobcode()`.

I appreciate why one would generally ban calls to `rm(list = ls()`. 
But these lines are only found in `export()` which can only run within RStudio 
jobs (if `rstudioapi::isAvailable() == TRUE & getOption("is.job", FALSE) == TRUE`).
`job` can never remove anything from the user's global workspace or filespace.
`rm()` is the only way to control what is returned from the job since the underlying 
`rstudioapi::runJobScript()` returns the full job environment upon completion.



# Resubmission
This is a resubmission. For this version, I have:

 * Changed the README.md link that triggered a NOTE. The original link was https and valid, though, but no problem.



# job 0.3.0

## Notes for the reviewer
* This package is RStudio-specific so automated R CMD Checks cannot test the functionality. It does have an extensive `testthat` test suite, which has been run manually in the following RStudio environments:


## Manual test environments
* Windows 10, R 3.6.1
* Windows 10, R 4.0.5
* Mac OS, R 3.3
* Mac OS, R 4.0.5
* Ubuntu 20.04, R 4.0.5

This package is currently used by at least a few hundred users and the initial bugs have been fixed.


## Automated test environments
* windows-latest (on github actions): release
* macOS-latest (on github actions): release
* ubuntu-20.04 (on github actions): oldrel, release, devel
* r-hub


## R CMD check results
There were no ERRORs, WARNINGs

There was 1 NOTE:

 * Possibly mis-spelled words in DESCRIPTION:
     Addins (10:269)
     RStudio (2:23, 10:65, 10:261)
   
   These are the very terms used by RStudio: https://rstudio.github.io/rstudioaddins/


## Downstream dependencies
This is the initial submission so there are no downstream dependencies.
