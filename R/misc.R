# TRUE if this is the RStudio main session
check_available = function() {
  if (rstudioapi::isAvailable() == FALSE)
    stop("You must run this from the RStudio main session.")
  if (rstudioapi::getVersion() < 1.2)
    stop("Install RStudio v1.2 or greater for the 'jobs' functionality.")

  invisible(TRUE)
}



# If `opt` is a function containing .Call("rs_ or ".rs.", replace it with a warning.
opt_without_rstudio = function(opt) {
  func_code = paste0(utils::capture.output(opt), collapse = "\n")  # to one character string

  contains_rstudio_c = grep('.Call("rs_', func_code, fixed = TRUE)
  contains_rstudio_r = grep('.rs.', func_code, fixed = TRUE)

  if (length(contains_rstudio_c) != 0 | length(contains_rstudio_r) != 0) {
    opt = function(...) warning("job::job() removed this option-function because it contained RStudio-specific code.")
    environment(opt) = globalenv()
  }

  opt
}

# Recursively removes functions that contain rstudio-specific functions.
# They won't work in child sessions and caused a multitude of bugs.
opts_without_rstudio = function(opts) {
  rapply(opts, opt_without_rstudio, classes = "function", how = "replace")
}


# Returns the size of some vars an environment
env_size_mb = function(vars, env, digits = 1) {
  vars_bytes = sapply(vars, function(x) utils::object.size(get(x, envir = env)))
  total_bytes = sum(as.numeric(vars_bytes))
  round(total_bytes / 10^6, digits)
}
