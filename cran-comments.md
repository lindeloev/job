# job 0.3

## Notes for the reviewer
* This package consists of one user-facing function (`job::job()`) and two RStudio addins.
* This package is RStudio-specific so automated R CMD Checks cannot test the functionality. It does have an extensive `testthat` test suite, which has been run manually in the following RStudio environments:


## Manual test environments
* Windows 10, R 3.6.1
* Windows 10, R 4.0.5
* Mac OS, R 3.3
* Mac OS, R 4.0.5
* Ubuntu 20.04, R 4.0.5

This package is currently used by a few thousand users and the initial bugs have been fixed.


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
   
   This is the term used by RStudio: https://rstudio.github.io/rstudioaddins/


## Downstream dependencies
This is the initial submission so there are no downstream dependencies.
