#' Nice print .call
#'
#' @aliases print.jobcode
#' @export
#' @param x Text to print
#' @param ... Currently unused
print.jobcode = function(x, ...) {
  cat(x)
}

# Addin functions
jobaddin_run = function(pattern) {
  j__ <<- list()  # Don't know why it has to be in global...
  j__$selection = rstudioapi::selectionGet()$value
  j__$title = gsub("\n", "; ", substr(j__$selection, 1, 80))
  j__$call = gsub("JOBCODE", j__$selection, gsub("JOBTITLE", j__$title, pattern))
  eval(parse(text = j__$call), envir = globalenv())
  rm(j__, envir = globalenv())
}

jobaddin_selection = function() {
  jobaddin_run("job::job({JOBCODE}, title = 'JOBTITLE')")
}

jobaddin_selection_empty = function() {
  jobaddin_run("job::job({JOBCODE}, title = 'JOBTITLE', import = NULL, packages = NULL, opts = NULL)")
}
