# job 0.2
First release submitted to CRAN.

### New features:

 * RStudio addins (#6)
 * Import only objects that are referenced in the code using `import = "auto"` (new default). The previous default is now called `import = "all"` and `import = ls()` is now deprecated. (#9)
 * The job title includes a code snippet now in most cases. See the documentation for the `title` argument for further details.


### Bug fixes

 * RStudio-specific compiler options caused error in job on Macs (#7 and #10)
 * Did not work in RMarkdown Notebooks. (#4)

## Other changes

 * Unnamed chunks returns contents to global environment instead of returning nothing.



# job 0.1
Beta release.
