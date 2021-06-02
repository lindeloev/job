# TRUE if this is the RStudio main session
check_available = function() {
  if (rstudioapi::isAvailable() == FALSE & getOption("is.job", FALSE) == FALSE)
    stop("You must run this from the RStudio main session.")
  if (rstudioapi::getVersion() < 1.2)
    stop("Install RStudio v1.2 or greater for the 'jobs' functionality.")

  invisible(TRUE)
}


# Returns the size of some vars an environment
env_size_mb = function(vars, env, digits = 1) {
  vars_bytes = sapply(vars, function(x) utils::object.size(get(x, envir = env)))
  total_bytes = sum(as.numeric(vars_bytes))
  round(total_bytes / 10^6, digits)
}


release_questions = function() {
  c(
    "Did you run all the README demos?",
    "Did you run all pkgdown code?",
    "Did you run README in MacOS VM?",
    "Did you run README in Ubuntu VM?"
  )
}
