jobaddin_run = function(jobargs = list()) {
  selection = rstudioapi::selectionGet()$value
  if (length(selection) == 0 || selection == "") {
    rstudioapi::showDialog("Error", "Error: No code was selected.")
  } else {
    jobtitle = gsub("\n", "; ", substr(selection, 1, 80))
    jobcode = parse(text = paste0("{ ", selection, "}"))[[1]]
    do.call(job::job, c(list(jobcode, title = jobtitle), jobargs), envir = globalenv())
  }
}

jobaddin_selection = function() {
  jobaddin_run()
}

jobaddin_selection_empty = function() {
  jobaddin_run(jobargs = list(import = NULL, packages = NULL, opts = NULL))
}
