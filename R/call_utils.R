# THESE FUNCTIONS SHOULD BE CALLED FROM THE MAIN SESSION

#' Nice print .jobcode
#'
#' @aliases print.jobcode
#' @export
#' @param x Text to print
#' @param ... Currently unused
#' @return No return value, called for side effects.
print.jobcode = function(x, ...) {
  cat(x)
}


# Saves vars in env to a tempfile
# Returns the tempfile path
# - vars: character vector
# - env: calling environment
# - code_str: the code chunk in job::job()
save_env = function(vars, env, code_str) {
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

  # Show import size
  import_mb = env_size_mb(vars, env)
  message("Copying ", import_mb, "MB to the RStudio job (excluding environments/R6)...", appendLF = FALSE)

  # Save and return
  import_file = gsub("\\\\", "/", tempfile())  # Windows only: Easier to paste() later
  suppressWarnings(save(list = vars, file = import_file, envir = env, compress = FALSE))

  # Return summary
  list(
    file = import_file,
    mb = import_mb,
    vars = vars
  )
}


# - packages: character vector
# - returns: character vector except default packages
get_packages = function(packages) {
  if (length(packages) > 0 & is.character(packages) == FALSE)
    stop("`packages` must be a character vector or length 0.")

  new_packages = packages[packages %in% c("base", getOption("defaultPackages")) == FALSE]
  new_packages
}
