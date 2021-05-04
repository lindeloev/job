check_available = function() {
  if (rstudioapi::isAvailable() == FALSE)
    stop("You must run this from the RStudio main session.")
  if (rstudioapi::getVersion() < 1.2)
    stop("Install RStudio v1.2 or greater for the 'jobs' functionality.")
}
