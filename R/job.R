#' Run Code as an RStudio Job
#'
#' See examples for an introduction. See details for some warnings.
#'
#' @aliases job
#' @export
#' @details
#' This is a wrapper around `rstudioapi::jobRunScript`. Some tips:
#'
#'  - **Large objects:**`jobRunScript` is very
#' slow at importing and exporting large objects. For exporting back into
#' `globalenv()`, it may be faster to `saveRDS()` results within the job and
#' `readRDS()` them in your environment.
#'
#'  - **Deletes import-names:** Upon completion, all variables with names in `import`
#' are deleted to speed up return. Avoid assigning variable names that are imported.
#'
#' @param ... A named or unnamed code block enclosed in curly brackets, `{}`.
#'   Unnamed code blocks will assign job variables directly to `globalenv()`
#'   upon completion. This means that if you add `rm(list = ls())` to as the
#'   last line of code, nothing is returned.
#'
#'   Named code blocks will assign the that name in `globalenv()`.
#' @param import A vector of un-quoted variables to import into the job. E.g.,
#'   `c(var1, var2)`. `ls()` (default) means "all" and `c()` is "nothing".
#' @param packages Character vector of packages to load in the job. Defaults to
#'   all loaded packages in the calling environment. You can achieve the same
#'   effect by writing `library(my_package)` in the code block.
#' @param title The job title. You can write e.g., `"Cross-Validation: {code}"` to
#'   show some code in the title. If `title = NULL` (default), the name of the
#'   code chunk is used. If `...` unnamed, code is shown.
#' @param opts List of options to overwrite in the job. Defaults to `options()`,
#'   i.e., copy all options to the job. `NULL` uses defaults.
#' @return Invisibly returns the job id on which you can call other `rstudioapi::job*`
#'   functions, e.g., `rstudioapi::rstudioapi::jobRemove(job_id)`.
#' @seealso \code{\link[rstudioapi]{jobRunScript}}
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
#'   }, title = "This is my title: {code}")
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
#'     rm(list = ls())  # remove everything
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
#'   }, import = c(global_var, my_df), packages = NULL, options = list(mc.cores = 3))
#'
#'   # later
#'   print(result2$names)
#' }
job = function(..., import = ls(), packages = .packages(), opts = options(), title = NULL) {
  if (rstudioapi::isAvailable() == FALSE)
    stop("job::job() must be called from within RStudio.")

  ########################
  # CODE AND RETURN-NAME #
  ########################
  args = match.call()[-1]  # args, not function name
  result_varname = names(args)[names(args) %in% c("import", "packages", "export", "title", "opts") == FALSE]
  if (length(args) == 0)
    stop("Must have exactly one code block.")
  if (length(result_varname) > 1)
    stop("Only one code block allowed. Did you misspell an argument?")

  if (is.null(result_varname) || result_varname == "") {
    result_varname = "R_GlobalEnv"  # signals to rstudiapi::jobRunScript() that it should return everything
    code = args[[1]]
  } else {
    code = args[[which(names(args) == result_varname)]]
  }

  # To R code
  code_str = paste0(as.character(substitute(code)), collapse = "\n")
  if (substr(code_str, 1, 1) != "{")
    stop("invalid code input. Did you remember to put the code in {curly brackets}?")
  code_str = substr(code_str, 3, nchar(code_str))



  #########
  # TITLE #
  #########
  # Set job title
  title_code = substr(code_str, 1, 80)
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
  # Save to CSV. Then write code that imports it to the job.
  import_varnames = as.character(substitute(import))
  if (length(import_varnames) == 0) {

  } else if (length(import_varnames) == 1 && import_varnames == "ls") {
    import_varnames = ls(envir = parent.frame())
  } else if (import_varnames[1] == "c") {
    import_varnames = import_varnames[-1]
  } else {
    stop("`import` must be a character vector or have length 0.")
  }

  import__ = lapply(import_varnames, get)
  names(import__) = import_varnames

  import_bytes = utils::object.size(import__)
  if (import_bytes > 400 * 10^6)  # Message if large
    message(Sys.time(), ": Copying ", round(import_bytes / 10^6, 1), "MB to the RStudio job (excluding environments/R6). Consider using `job::job(..., import = c(fewer, or, smaller, vars)` to speed up.")

  import_file = gsub("\\\\", "/", tempfile())
  import_startcode = paste0("
# Set list elements as variables in this environment
import__ = readRDS('", import_file, "')
importnames__ = names(import__)
for (importname__ in importnames__)
assign(importname__, import__[[importname__]])
rm(importname__)
rm(import__)
file.remove('", import_file, "')

setwd(wd__)  # From import__. Set to calling wd.
rm(wd__)")


  ###########
  # OPTIONS #
  ###########
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

  options_code = "
# Set options
options(opts__)
rm(opts__)
"



  ####################
  # SAVE IMPORT DATA #
  ####################
  # Add further info
  import__$opts__ = opts
  import__$wd__ = getwd()  # Set to current working directory

  # Save
  suppressWarnings(saveRDS(import__, import_file))  # Suppress warnings that [package] may not be available when readRDS.


  ########################
  # BUILD R CODE FOR JOB #
  ########################
  output = paste0(
    "message(Sys.time(), ': Job started.')\n\n",
    packages_str, "\n\n",
    import_startcode, "\n\n",
    options_code, "\n\n",

    "# Run code\n",
    code_str, "\n",

    "# Clean up
    if (exists('importnames__')) {
      suppressWarnings(rm(list = importnames__))
      rm(importnames__)
    }
    message(Sys.time(), ': Job finished.')\n"
  )

  # Add .call
  output = paste0(
    output, "\n# Save code to environment for future reference\n",
    ".call = paste0(\"# Job started: ", Sys.time(), "\n\n", gsub("\"", "\\\\\"", code_str), "\n\n# Job completed: \", Sys.time())\n",
    "class(.call) = c('jobcode', 'character')\n"
  )


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
