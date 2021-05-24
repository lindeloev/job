#' Nice print .call
#'
#' @aliases print.jobcode
#' @export
#' @param x Text to print
#' @param ... Currently unused
print.jobcode = function(x, ...) {
  cat(x)
}



# Saves vars in env to a tempfile
# Returns the tempfile path
# - vars: character vector
# - env: calling environment
# - code_str: the code chunk in job::job()
save_env = function(vars, env, code_str) {
  custom_vars = vars

  # Identify which objects in env to save
  if (length(vars) == 0) {
    # stay NULL
  } else if (length(vars) == 1 && vars == "auto") {
    ls_varnames = ls(envir = env, all.names = TRUE)
    vars = ls_varnames[ls_varnames %in% all.names(parse(text = code_str))]
  } else if (length(vars) == 1 && vars == "all") {
    vars = ls(envir = env, all.names = TRUE)
  } else if (vars[1] == "c") {
    vars = vars[-1]
  } else {
    stop("`import` must be one of 'all', 'auto', NULL, or a c(vector, of, variables).")
  }

  # Warn about large file sizes, i.e., slow import
  tryCatch({
    #obj_bytes = sapply(vars, function(x) utils::object.size(deep_list(get(x, envir = env))))  # Fails on Macs with cstack overflow
    obj_bytes = sapply(vars, function(x) utils::object.size(get(x, envir = env)))
    import_bytes = sum(as.numeric(obj_bytes))
    message("Copying ", round(import_bytes / 10^6, 1), "MB to the RStudio job (excluding environments/R6)...", appendLF = FALSE)
    # if (import_bytes > 200 * 10^6 & custom_vars == "all") {  # Message if large
    #   #message("Copying ", round(import_bytes / 10^6, 1), "MB to the RStudio job. Consider using `import = 'auto' or `import = c(fewer, smaller, vars)`` to import relevant variables only.")
    #   message("Consider using `import = 'auto' or `import = c(fewer, smaller, vars)`` to import relevant variables only.", appendLF = FALSE)
    # }
  }, error = function(e) message("Could not evaluate the size of import. Continuing..."))

  # Save and return
  import_file = gsub("\\\\", "/", tempfile())  # Windows only: Easier to paste() later
  suppressWarnings(save(list = vars, file = import_file, envir = env))
  message("\rJob launched.", rep(" ", 70))

  import_file
}


# Saves options() and working directory to a temporary file.
# Returns the filename.
# - opts: named list of options or NULL
save_settings = function(opts) {
  # Preprocess opts
  if (is.null(opts)) {
    opts = list()
  } else if (is.list(opts) == FALSE) {
    stop("`opts` must be a list (e.g., `options()`) or NULL.")
  } else {
    # Remove RStudio-specific functions from options
    opts = rapply(opts, opts_without_rstudio, classes = "function", how = "replace")
  }

  opts$is.job = TRUE

  # Save jobsettings (js) and return
  .__js__ = list(
    opts = opts,
    wd = getwd()
  )
  settings_file = gsub("\\\\", "/", tempfile())  # Windows only: Easier to paste() later
  suppressWarnings(saveRDS(.__js__, settings_file))  # Ignore warning that some package may not be available when loading

  settings_file
}
