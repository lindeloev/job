#' Nice print .call
#'
#' @aliases print.jobcode
#' @export
#' @param x Text to print
#' @param ... Currently unused
print.jobcode = function(x, ...) {
  cat(x)
}


# Returns an environment/R6 and sub-environments/R6 as list
# Renders it possible to use object.size() and identical()
# I posted it on StackOverflow: https://stackoverflow.com/questions/67300724/identical-but-for-environments-r6-in-base-r/67316125#67316125
deep_list = function(object) {
  break_recursion__ = function(x) {
    if ("R6" %in% class(x)) {
      # R6 to list without recursion
      x = as.list(x, all.names = TRUE)
      x$.__enclos_env__$self = NULL
      x$.__enclos_env__$super = NULL
    } else if("R6ClassGenerator" %in% class(x)) {
      x$parent_env = NULL
      x$self = NULL
      x = as.list.environment(x, all.names = TRUE)
    }

    # Return
    x
  }

  rapply(
    object = as.list(break_recursion__(object), all.names = TRUE),
    f = deep_list,
    classes = c("environment", "R6", "R6ClassGenerator"),
    how = "replace"
  )
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

  # # Warn about large file sizes, i.e., slow import
  print("save_env: Computing environment size")
  tryCatch({
    #obj_bytes = sapply(vars, function(x) utils::object.size(deep_list(get(x, envir = env))))  # Fails on Macs with cstack overflow
    obj_bytes = sapply(vars, function(x) utils::object.size(get(x, envir = env)))
    import_bytes = sum(as.numeric(obj_bytes))
    if (import_bytes > 200 * 10^6) {  # Message if large
      #message("Copying ", round(import_bytes / 10^6, 1), "MB to the RStudio job. Consider using `import = 'auto' or `import = c(fewer, smaller, vars)`` to import relevant variables only.")
      message("Copying ", round(import_bytes / 10^6, 1), "MB to the RStudio job (excluding environments/R6).")
      if (custom_vars == "all")
        message("Consider using `import = 'auto' or `import = c(fewer, smaller, vars)`` to import relevant variables only.")
    }

  }, error = function(e) message("Could not evaluate size of import due to infinite recursion Continuing..."))

  # Save and return
  print("save_env: Saving environment")
  import_file = gsub("\\\\", "/", tempfile())
  suppressWarnings(save(list = vars, file = import_file, envir = env))

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
    # Options set by RStudio
    opts$buildtools.check = NULL
    opts$buildtools.with = NULL

    # Options set by RMarkdown notebooks
    opts$error = NULL
  }

  opts$is.job = TRUE

  # Save jobsettings (js) and return
  print("save_opts: Saving")
  .__js__ = list(
    opts = opts,
    wd = getwd()
  )
  settings_file = gsub("\\\\", "/", tempfile())
  suppressWarnings(saveRDS(.__js__, settings_file))  # Ignore warning that some package may not be available when loading

  settings_file
}
