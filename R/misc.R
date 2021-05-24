# TRUE if this is the RStudio main session
check_available = function() {
  if (rstudioapi::isAvailable() == FALSE)
    stop("You must run this from the RStudio main session.")
  if (rstudioapi::getVersion() < 1.2)
    stop("Install RStudio v1.2 or greater for the 'jobs' functionality.")

  invisible(TRUE)
}


# Use as rapply(options(), opts_without_rstudio) to clean options
# from rstudio-specific functions which won't work in child sessions.
opts_without_rstudio = function(x) {
  func_code = paste0(capture.output(x), collapse = "\n")  # to one character string

  is_rstudio_c = grep('.Call("rs_', func_code, fixed = TRUE)
  is_rstudio_r = grep('.rs.', func_code, fixed = TRUE)

  if (length(is_rstudio_c) != 0 | length(is_rstudio_r) != 0) {
    return(NULL)
  } else {
    return(x)
  }
}
