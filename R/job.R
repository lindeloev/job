#' Run Code as an RStudio Job
#'
#' See examples for an introduction. See details for some warnings.
#'
#' @aliases job
#' @export
#' @details
#' This is a wrapper around `rstudioapi::jobRunScript`. To control what gets
#' returned, see \code{\link[job]{export}}. By default, all objects that *changed* during
#' the job are returned, i.e., `job::export("changed")`.
#'
#'  - **Returning large objects:**`jobRunScript` is very
#' slow at importing and exporting large objects. For exporting back into
#' `globalenv()`, it may be faster to `saveRDS()` results within the job and
#' `readRDS()` them in your environment.
#'
#' @param ... A named or unnamed code block enclosed in curly brackets, `{}`.
#'   Unnamed code blocks will assign job variables directly to `globalenv()`
#'   upon completion. This means that if you add `rm(list = ls())` to as the
#'   last line of code, nothing is returned.
#'
#'   Named code blocks will assign the that name in `globalenv()`.
#' @param import Which objects to import into the job.
#'  * `"all"`: Import all objects.
#'  * `"auto"` (default): Detect which objects are used in the code and import
#'    those.
#'  * `c(foo, bar, ...)`: A vector of un-quoted variables to import into the job.
#'  * `NULL`: import nothing.
#' @param packages Character vector of packages to load in the job. Defaults to
#'   all loaded packages in the calling environment. You can achieve the same
#'   effect by writing `library(my_package)` in the code block.
#' @param opts List of options to overwrite in the job. Defaults to `options()`,
#'   i.e., copy all options to the job. `NULL` uses defaults.
#' @param title The job title. You can write e.g., `"Cross-Validation: {code}"` to
#'   show some code in the title. If `title = NULL` (default), the name of the
#'   code chunk is used. If `...` unnamed, code is shown.
#' @return Invisibly returns the job id on which you can call other `rstudioapi::job*`
#'   functions, e.g., `rstudioapi::rstudioapi::jobRemove(job_id)`.
#' @seealso  \code{\link[job]{export}}, \code{\link[rstudioapi]{jobRunScript}}
#' @author Jonas Kristoffer Lindeløv, \email{jonas@@lindeloev.dk}
#' @encoding UTF-8
#' @examples
#' if (rstudioapi::isAvailable()) {
#'   # Unnamed code chunks returns to globalenv()
#'   global_var = 5
#'   job::job({
#'     x = rnorm(global_var)
#'     print("This text goes to the job console")
#'     m = mean(x)
#'   })
#'
#'   # later:
#'   print(x)
#'   print(m)
#'
#'
#'   # Named code chunks assign job environment to that name
#'   job::job(my_result = {
#'     y = rnorm(global_var)
#'     sigma = sd(y)
#'   }, title = "Title with code: {code}")
#'
#'   # later:
#'   print(as.list(my_result))
#'
#'
#'   # Delete everything in the job environment to return nothing.
#'   # Useful if text output + file output is primary
#'   job::job({
#'     some_cars = mtcars[mtcars$cyl > 4, ]
#'     print(mean(some_cars$mpg))
#'     print(summary(some_cars))
#'     # saveRDS(some_cars, "job_result.rds")
#'
#'     job::export("none")  # return nothing
#'   })
#'
#'
#'   # Control imports from calling environment (variables, packages, options)
#'   my_df = data.frame(names = c("alice", "bob"))
#'   ignore_var = 15
#'   job::job(result2 = {
#'     if (exists("ignore_var") == FALSE)
#'       print("ignore_var is not set here")
#'
#'     names = rep(my_df$names, global_var)
#'   }, import = c(global_var, my_df), packages = NULL, opts = list(mc.cores = 3))
#'
#'   # later
#'   print(result2$names)
#' }
job = function(..., import = "all", packages = .packages(), opts = options(), title = NULL) {
  check_available()

  ####################
  # UNFOLD ARGUMENTS #
  ####################
  # The ellipsis combined with the option for unnamed arguments renders
  # this a bit convoluted
  func_arg_names = c("import", "packages", "opts", "title")  # named arguments to this function
  args = match.call()[-1]  # args excluding function name
  if (length(args) == 0)
    stop("Must have exactly one code block.")

  # Unpack return varname and code
  result_varname = names(args)[names(args) %in% func_arg_names == FALSE]
  result_varname = result_varname[1]  # Use the first from here if there were multiple

  if (is.null(result_varname) || result_varname == "") {
    result_varname = "R_GlobalEnv"  # signals to rstudiapi::jobRunScript() that it should return everything
    names(args)[1] = "R_GlobalEnv"
    code = args[[1]]
  } else {
    code = args[[which(names(args) == result_varname)]]
  }

  # Assign values to unnamed arguments (name = "") by matching their positional order to args order
  n_unnamed = sum(names(args) == "")
  if (n_unnamed > 0) {
    func_arg_names_pruned = func_arg_names[func_arg_names %in% names(args) == FALSE]
    args_unnamed = args[names(args) == ""]
    for (i in seq_len(n_unnamed)) {
      # Evaluate some; then assign (overwriting previous values)
      if (func_arg_names_pruned[i] != "import")
        args_unnamed[i][[1]] = eval(args_unnamed[i][[1]])
      assign(func_arg_names_pruned[i], args_unnamed[i][[1]])
    }
  }


  ########################
  # CODE AND RETURN-NAME #
  ########################
  # To R code
  if (code[[1]] != quote(`{`))
    stop("invalid code input. Did you remember to put the code in {curly brackets}?")
  code_str = paste0(code[-1], collapse = "\n")


  #########
  # TITLE #
  #########
  # Set job title
  title_code = paste0("{", substr(code_str, 1, 80), "}")
  if (is.null(title)) {
    job_title = ifelse(result_varname == "R_GlobalEnv", title_code, result_varname)
  } else if (is.atomic(title) & is.character(title)) {
    job_title = gsub("{code}", title_code, title, fixed = TRUE)
  } else {
    stop("`title` must be atomic character or NULL")
  }


  ############
  # PACKAGES #
  ############
  # List currently attached Then write "library(x)" code
  if (is.character(packages) || length(packages) == 0) {
    packages_str = paste0("library(", packages, ")", collapse = "\n")
  } else {
    stop("`packages` must be a character vector or length 0.")
  }


  ##########
  # IMPORT #
  ##########
  call_frame = parent.frame()
  import_file = save_env(
    vars = as.character(substitute(import)),
    env = call_frame,
    code_str = code_str
  )


  ############
  # SETTINGS #
  ############
  settings_file = save_settings(opts)




  ########################
  # BUILD R CODE FOR JOB #
  ########################
  output = paste0("
##############
# INITIALIZE #
##############
message(Sys.time(), ': Job started.')
", packages_str, "

# Load settings
.__js__ = readRDS('", settings_file, "')  # js = jobsettings
setwd(.__js__$wd)
options(.__js__$opts)
file.remove('", settings_file, "')

# Load objects and compute hash
load('", import_file, "', envir = sys.frame(sys.nframe()))
file.remove('", import_file, "')
.__js__$init_hashes = job:::hash_env(sys.frame(sys.nframe()))



# Run code
", code_str, "



##########
# FINISH #
##########
# Fall back on default export
if (is.null(options('job.exported')[[1]]))
  job::export('changed')
if (exists('.__js__'))
  rm(.__js__)

message(Sys.time(), ': Job finished.')")


  if (result_varname != "R_GlobalEnv") {
    output = paste0(
    output, "

# Save code to environment for future reference
.call = paste0(\"
# Job started: ", Sys.time(), "

", gsub("\"", "\\\\\"", code_str), "

# Job completed: \", Sys.time())
class(.call) = c('jobcode', 'character')
options(warn = -1)")
  }


  ###########
  # EXECUTE #
  ###########
  # Write R code to file and execute it as a job
  script_file = tempfile()
  write(output, file = script_file)
  job_id = rstudioapi::jobRunScript(script_file, job_title, importEnv = FALSE, exportEnv = result_varname)

  # Return nothing
  invisible(job_id)
}



#' @aliases job_empty
#' @export
#' @describeIn job Defaults to an empty session
job_empty = function(..., import = NULL, packages = NULL, opts = NULL, title = NULL) {
  job(..., import = import, packages = packages, opts = opts, title = title)
}
