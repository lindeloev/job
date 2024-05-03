# job 0.3.1
Bug fix release

### Bug fixes

* Now loads packages in the correct order. Closes https://github.com/lindeloev/job/issues/53.
* Now accurately represents `NA_character_` etc. in the job. Closes https://github.com/lindeloev/job/issues/51.
* Now gracefully handles loaded but uninstalled packages. Closes https://github.com/lindeloev/job/issues/48. 
* Avoid crash on R 4.4 by removing the check for RStudio version >= 1.2 (from April 2019). Thanks Balázs Knakker for detecting and fixing this bug!


# job 0.3.0
First release submitted to CRAN.

### New features:

* Control what objects are returned using the `job::export()` function in the code chunk. The default is `job::export("changed")` which returns all variables that were defined or changed during the job. v0.1 only returned new variable names (now `job::export("new")`). Read more in the README. (#15)
 * Use `job::export("changed", file = "my_jobresult.RData")` to save to a file rather than returning to the main RStudio session.
 * `job::job()` shows more informative messages all around to reduce state ambiguity when launching jobs.
 * Much faster import for large environments.



# job 0.2.0

### New features:

 * RStudio addins (#6)! Read more in the README.
 * Use `import = "auto"` to import all objects that are "mentioned" in the code. The default `import = "all"` replaces `import = ls()`.
 * The job title now includes a code snippet for unnamed chunks instead of defaulting to `"(untitled)"`.
 * `job::job()` now takes unnamed arguments too, e.g., `job::job({<code>}, NULL, c("rstudioapi"))`. (#25)
 * Added `job::empty()` which is short for `job::job({<code>}, import = NULL, packages = NULL, opts = NULL)` (#23)


### Bug fixes

 * RStudio-specific `options` caused errors - mostly on macs (#4, #7, #10, #27)
 * The addins can now handle apostrophes in the job title.


## Other changes

 * Unnamed chunks returns contents to global environment instead of returning nothing. `.call` is not saved in this case. (#9)
 * Peak memory usage has been considerably reduced.
 * The `digest` package was added as a dependency to support `export("changed")`.
 * Transitioned to `testthat` for unit tests and expanded the test suite considerably. (#26)



# job 0.1.0
Beta release.
