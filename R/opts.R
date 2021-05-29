# Returns function or call as char. NULL for other objects.
code2char = function(x) {
  if (class(x) == "function") {
    return(paste0(utils::capture.output(x), collapse = "\n"))  # to one character string
  } else if (class(x) == "call") {
    return(as.character(x))
  }

  NULL
}

# Returns TRUE if x is an object containing rstudio-specific code
contains_rstudio_code = function(x) {
  code = code2char(x)
  if (is.character(code)) {
    contains_rstudio_c = grep('.Call("rs_', code, fixed = TRUE)
    contains_rstudio_r = grep('.rs.', code, fixed = TRUE)

    is_rstudio_code = length(contains_rstudio_c) != 0 | length(contains_rstudio_r) != 0
    return(is_rstudio_code)
  }

  FALSE
}


# Recursively removes functions that contain rstudio-specific functions.
# They won't work in child sessions and caused a multitude of bugs.
opts_without_rstudio = function(opts) {
  # Modify opts
  for (name in names(opts)) {
    # Recursive on list elements
    if (is.list(opts[[name]]))
      opts[[name]] = opts_without_rstudio(opts[[name]])

    # Remove if rstudio-specific
    if (contains_rstudio_code(opts[[name]]) | length(opts[[name]]) == 0)
      opts[[name]] = NULL

  }
  opts$cpp11_preserve_env = NULL  # cpp11 error. See https://github.com/r-lib/cpp11/issues/116

  opts
}
