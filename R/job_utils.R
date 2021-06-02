# THESE FUNCTIONS SHOULD BE CALLED FROM WITHIN THE JOB

# Returns a named vector of hashed variables in the calling environment
hash_env = function(env) {
  varnames = ls(envir = env, all.names = TRUE)
  hashes = lapply(varnames, function(x) suppressWarnings(digest::digest(get(x, envir = env))))
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
#' @param file Name of `.RData` file to export to. If not `NULL`, nothing will be returned
#'   to the main session (corresponding to `export("none")`).
#' @return `NULL` invisibly.
#' @encoding UTF-8
#' @author Jonas Kristoffer Lindeløv, \email{jonas@@lindeloev.dk}
#' @examples
#' if (rstudioapi::isAvailable()) {
#'   a = 55
#'   b = 77
#'   d = 88
#'   job::job({n = 11; a = 55; job::export("all")})  # export a, b, d, n
#'   job::job({n = 11; a = 11; job::export("changed")})  # export a, n
#'   job::job({n = 11; a = 11; job::export("new")})  # export n
#'   job::job({n = 11; a = 55; job::export(c(a, d, b))})  # export a, d, b
#'   job::job({n = 11; a = 55; job::export("none")})  # export nothing
#'
#'   # To file
#'   job::job({n = 11; a = 11; job::export("changed", file = "jobresult.RData")})  # save a, n
#'   jobresult = new.env()  # import to this env instead of global
#'   load("jobresult.RData", envir = jobresult)
#'   print(jobresult$n)
#' }
export = function(value = "changed", file = NULL) {
  # Do nothing if this is not a job
  if (is.null(options("is.job")[[1]]))
    return(invisible(NULL))

  if (FALSE) .__jobsettings__ = NULL  # make R CMD Check happy

  call_env = parent.frame()
  env_varnames = ls(envir = call_env, all.names = TRUE)
  init_hashes = get(".__jobsettings__", envir = call_env)$init_hashes
  init_hashes$.__jobsettings__ = NULL  # Ignore this
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
    imported_vars = env_varnames[env_varnames %in% names(init_hashes)]
    rm(list = imported_vars, envir = call_env)

  # Don't remove anything
  } else if (value == "all") {
  } else {
    stop("Invalid `value` argument.")
  }

  # Optionally export to file (and delete all)
  if (is.character(file)) {
    objects_to_save = ls(envir = call_env, all.names = TRUE)
    objects_to_save = objects_to_save[objects_to_save != ".__jobsettings__"]
    save(list = objects_to_save, envir = call_env, file = file)
    rm(list = objects_to_save, envir = call_env)
  } else if (is.null(file) == FALSE) {
    warning("`file` must be a character or NULL. Ignoring...")
  }

  options("job.exported" = TRUE)
  invisible(NULL)
}
