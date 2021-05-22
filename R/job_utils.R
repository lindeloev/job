# Returns a named vector of hashed variables in the calling environment
hash_env = function(env) {
  varnames = ls(envir = env, all.names = TRUE)
  orig_warn = options("warn")
  options(warn = -1)
  hashes = sapply(varnames, function(x) digest::digest(get(x, envir = env)))
  options(orig_warn)
  names(hashes) = varnames

  hashes
}


#' What to return from a job
#'
#' Call this function as the last line in `job::job()` to select what is exported
#' back into `globalenv()`. `export()` does nothing if called in any other context.
#'
#' Under the hood, this function merely `rm()` variables that does not match `value`.
#' Because `job::job()` returns everything at the end of the script, this defines
#' what is returned.
#'
#' @aliases export
#' @export
#' @param value What to return. One of:
#'  * `"all"`: Return everything, including imports
#'  * `"changed"` (default): Return all variables that are not identical to import.
#'  * `"new"`: Return only new variable names.
#'  * `c(var1, var2, ...)`: Return these variable names.
#'  * `NULL` or `"none"`: Return nothing. This is particularly useful for unnamed code chunks.
#' @return `NULL` invisibly.
#' @examples
#' if (rstudioapi::isAvailable()) {
#'   a = 55
#'   b = 77
#'   d = 88
#'   job::job({n = 11; a = 55; export("all")})  # a, b, d, n
#'   job::job({n = 11; a = 11; export("changed")})  # a
#'   job::job({n = 11; a = 11; export("new")})  # n
#'   job::job({n = 11; a = 55; export(c(a, d, b))})  # a, d, b
#'   job::job({n = 11; a = 55; export("none")})  # nothing
#' }
export = function(value = "changed") {
  # Do nothing if this is not a job
  if (is.null(options("is.job")[[1]]))
    return(invisible(NULL))

  if (FALSE) .__js__ = NULL  # make R CMD Check happy

  call_env = parent.frame()
  env_varnames = ls(envir = call_env, all.names = TRUE)
  init_hashes = get(".__js__", envir = call_env)$init_hashes
  value = substitute(value)

  # Remove c(selected, via, vector)
  if (is.symbol(value) | is.language(value)) {
    # To character vector
    value = as.character(value)
    if (value[1] == "c")
      value = value[-1]

    # Check existence
    does_exist = sapply(value, exists, envir = call_env)
    if (any(does_exist == FALSE))
      stop("'", paste0(value[does_exist == FALSE], collapse = "' and '"), "' do not exist.")

    # Delete all others
    remove_vars = env_varnames[env_varnames %in% value == FALSE]
    rm(list = remove_vars, envir = call_env)

  # Remove everything
  } else if (is.null(value) || value == "none") {
    rm(list = env_varnames, envir = call_env)

  # Remove unchanged
  } else if (value == "changed"){
    post_hashes = hash_env(call_env)
    unchanged_vars = sapply(names(post_hashes), function(x) ifelse(x %in% names(init_hashes) == TRUE, identical(init_hashes[[x]], post_hashes[[x]]), FALSE))
    rm(list = names(post_hashes)[unchanged_vars], envir = call_env)

  # Remove those with imported varnames
  } else if (value == "new") {
    new_vars = env_varnames[env_varnames %in% names(.__js__$init_hashes)]
    rm(list = new_vars, envir = call_env)

  # Don't remove anything
  } else if (value == "all") {
  } else {
    stop("Invalid `value` argument.")
  }

  options("job.exported" = TRUE)
  invisible(NULL)
}
