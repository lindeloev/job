#' Run Code as an RStudio Job
#'
#' See examples for an introduction. See details for some warnings.
#'
#' @aliases job
#' @export
#' @details
#' This is a wrapper around `rstudioapi::jobRunScript`.
#'
#' **Large objects:**`jobRunScript` is very
#' slow at importing and exporting large objects. Re importing, `as_job` does
#' sets `importEnv = FALSE` and passes data through a temporary .rds file.
#' Re exporting, I recommend saving large objects to an .rds file and `rm()` it
#' from the job so it isn't returned.
#'
#' **Deletes import-names:** Upon completion, all variables with names in `import`
#' are deleted to speed up return. Avoid assigning variable names that are imported.
#'
#' @param ... A named or unnamed code block. Named code blocks will have the result
#'   assigned to that name in `globalenv()` upon completion. Unnamed code blocks
#'   will not return anything. See examples.
#' @param import A vector of un-quoted variables to import into the job. E.g.,
#'   `c(var1, var2)`. `ls()` (default) means "all" and `c()` is "nothing".
#' @param packages Character vector of packages to load in the job. Defaults to
#'   all loaded packages in the calling environment. You can achieve the same
#'   effect by writing `library(my_package)` in the code block.
#' @param title The job title. If `NULL` (default), use the name of `...` if set
#'   or `"(untitled)"` if `...` is unnamed.
#' @param opts List of options to overwrite in the job. Defaults to `options()`,
#'   i.e., copy all options to the job. `NULL` uses defaults.
#' @return `NULL`. But an environment is assigned if the code block argument is named.
#'   The returned environment will include everything defined in the code block
#'   but excluding - not "untouched" imports. See `...`.
#' @seealso \code{\link[rstudioapi]{jobRunScript}}
#' @author Jonas Kristoffer Lindeløv \email{jonas@@lindeloev.dk}
#' @examples
#' if (rstudioapi::isAvailable()) {
#'   # From globalenv
#'   global_var = 5
#'   job(result = {
#'     x = rnorm(global_var)
#'     print("This text goes to the job console")
#'     m = mean(x)
#'   })
#'
#'   # later:
#'   print(as.list(result))
#'   result$m
#'
#'
#'   # Run without assigning anything to the calling environment
#'   job({
#'     result = rbind(mtcars, mtcars)
#'     print(mean(result$mpg))
#'     # saveRDS(result, "job_result.rds")
#'   })
#'
#'
#'   # Specify imports and (no) packages
#'   my_df = data.frame(names = c("alice", "bob"))
#'   ignore_var = 15
#'   job(result2 = {
#'     if (file.exists("ignore_var") == FALSE)
#'       print("ignore_var is not set here")
#'
#'     names = rep(my_df$names, global_var)
#'   }, import = c(global_var, my_df), packages = NULL)
#'
#'   # later
#'   result2$names
#' }
job = function(..., import = ls(), packages = .packages(), opts = options(), title = NULL) {
  if (rstudioapi::isAvailable() == FALSE)
    stop("Jobs can only be created if job::job() is called in RStudio.")

  ########################
  # CODE AND RETURN-NAME #
  ########################
  args = match.call()[-1]  # args, not function name
  result_varname = names(args)[names(args) %in% c("import", "packages", "export", "title", "opts") == FALSE]
  if (length(args) == 0 | length(result_varname) > 1)
    stop("Must have exactly one code block.")
  if (length(result_varname) == 0) {
    result_varname = ""
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
  if (is.null(title) == FALSE && is.atomic(title) == FALSE)
    stop("title must be NULL or length 1")
  if (is.null(title)) {
    job_title = ifelse(result_varname == "", "(untitled)", result_varname)
  } else {
    job_title = as.character(title)
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
    message(Sys.time(), ": Copying ", round(import_bytes / 10^6, 1), "MB to the RStudio job (excluding environments/R6). Consider using `as_job(..., import = c(fewer, or, smaller, vars)` to speed up.")

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
  # TO DO: what if NULL or c()?
  if (is.null(opts))
    opts = list()
  if (is.list(opts) == FALSE)
    stop("`opts` must be a list (e.g., `options()`) or NULL.")

  options_code = "
# Set options
invisible(do.call(options, opts__))  # invisible if is.null(opts__)
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
    suppressWarnings(rm(list = importnames__))
    rm(importnames__)
    message(Sys.time(), ': Job finished.')
    options(warn = -1)\n"  # Suppress warnings that [package] may not be available when readRDS.
  )

  # Add .call
  output = paste0(
    output, "\n# Save code to environment for future reference\n",
    ".call = \"# Called on ", Sys.time(), "\n", gsub("\"", "\\\\\"", code_str), "\"\n",
    "class(.call) = 'jobcode'\n"
  )


  ###########
  # EXECUTE #
  ###########
  # Write R code to file and execute it as a job
  script_file = tempfile()
  write(output, file = script_file)
  rstudioapi::jobRunScript(script_file, job_title, importEnv = FALSE, exportEnv = result_varname)  # "" means "no export"

  # Return nothing
  invisible(NULL)
}
