# job 0.2
First release submitted to CRAN.

### New features:

 * RStudio addins (#6)! Read more in the README.
 * Control what objects are returned using the `job::export()` function in the code chunk. The default is `job::export("changed")` which returns all variables that were defined or changed during the job. v0.1 only returned new variable names (now `job::export("new")`). Read more in the README. (#15)
 * Import only objects that are referenced in the code using `import = "auto"`. The default `import = "all"` replaces `import = ls()`.
 * The job title now includes a code snippet for unnamed chunks instead of defaulting to `"(untitled)"`.
 * `job::job()` now takes unnamed arguments too, e.g., `job::job({<code>}, NULL, c("rstudioapi"))`. (#25)


### Bug fixes

 * RStudio-specific compiler options caused error on Macs (#7 and #10)
 * Error if executed from RMarkdown Notebooks. (#4)
 * The warning about large imports now counts environments and R6 too (#20)
 * The addins can now handle apostrophes in the job title.


## Other changes

 * Unnamed chunks returns contents to global environment instead of returning nothing. `.call` is not saved in this case. (#9)
 * Peak memory usage has been considerably reduced.
 * The `digest` package was added as a dependency to support `export("changed")`.
 * Transitioned to `testthat` for unit tests and expanded the test suite considerably. (#26)



# job 0.1
Beta release.
