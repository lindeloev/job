# job 0.2
First release submitted to CRAN.

### New features:

 * RStudio addins (#6)
 * Control what objects are returned using the `job::export()` function in the code chunk. The default is `job::export("changed")` which returns all variables that were defined or changed during the job. v0.1 only returned new variable names (now `job::export("new")`). `NULL` and `c(var1, var2, ...)` are supported too. The `digest` package is now a dependency to detect changes. (#15)
 * Import only objects that are referenced in the code using `import = "auto"`. The default `import = "all"` replaces `import = ls()`. (#9)
 * The job title includes a code snippet now in most cases. See the documentation for the `title` argument for further details.


### Bug fixes

 * RStudio-specific compiler options caused error on Macs (#7 and #10)
 * Error if executed from RMarkdown Notebooks. (#4)
 * The warning about large imports now counts environments and R6 too.


## Other changes

 * Unnamed chunks returns contents to global environment instead of returning nothing. `.call` is not saved in this case.
 * Peak memory usage has been considerably reduced.



# job 0.1
Beta release.
