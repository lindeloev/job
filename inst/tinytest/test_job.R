if (rstudioapi::isAvailable()) {
  # Explicit libraries for manual testing
  library(rstudioapi)
  library(tinytest)
  library(job)

  # Helpers. Save in one obj to better control what's in the environment
  job_helpers = list(
    cleanup = function() {
      suppressWarnings(rm(list = c("a", "b", "default", "with_args", "returned",  "blank", "attached_start"), envir = parent.frame()))
      options(warning.length = job_helpers$opt_warning.length)
    },
    equal_sets = function(x, y) {
      all(x %in% y) & all(y %in% x)
    },
    vars = c("job_helpers", "importnames__"),
    packages = c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base"),
    opt_warning.length = options("warning.length")[[1]],
    opt_timeout = options("timeout")[[1]]
  )

  # For good measure
  job_helpers$cleanup()

  a = 123
  b = list(a = a, goat = "baah")


  #########################
  # TEST DEFAULT BEHAVIOR #
  #########################
  options(warning.length = 999)
  job::job(default = {
    vars = ls()
    pkgs = .packages()
    a_copy = a
    b_copy = b
    attached_rstudioapi = exists("isAvailable")
    opts = options()
  })

  # Check result
  Sys.sleep(10)  # First is slower
  expect_true(job_helpers$equal_sets(default$vars, c("a", "b", job_helpers$vars)))
  expect_true(job_helpers$equal_sets(default$pkgs, c("job", "rstudioapi", "tinytest", job_helpers$packages)))
  expect_identical(default$a_copy, a)
  expect_identical(default$b_copy, b)
  expect_true(default$attached_rstudioapi)
  expect_true(job_helpers$equal_sets(names(default), c("vars", "pkgs", "a_copy", "b_copy", "attached_rstudioapi", "opts", ".call")))
  expect_true(default$opts$warning.length == 999 & default$opts$device == options("device"))

  # Cleanup
  rm(default)
  options(warning.length = job_helpers$opt_warning.length)


  #############
  # TEST ARGS #
  #############
  options(warning.length = 999)
  returned = job::job(with_args = {
    vars = ls()
    pkgs = .packages()
    b_copy = b
    attached_rstudioapi = exists("isAvailable")
    opts = options()
  }, import = c(b, job_helpers), packages = c("job"), title = "something weird: #/(¤", opts = list(timeout = 59))

  # Check result
  Sys.sleep(3)
  expect_null(returned)
  expect_true(job_helpers$equal_sets(with_args$vars, c("b", job_helpers$vars)))
  expect_true(job_helpers$equal_sets(with_args$pkgs, c("job", job_helpers$packages)))
  expect_identical(with_args$b_copy, b)
  expect_true(with_args$attached_rstudioapi == FALSE)
  expect_true(job_helpers$equal_sets(names(with_args), c("vars", "pkgs", "b_copy", "attached_rstudioapi", "opts", ".call")))
  expect_true(with_args$opts$warning.length == job_helpers$opt_warning.length & with_args$opts$timeout == 59)

  # Cleanup
  rm(returned)
  rm(with_args)
  options(warning.length = job_helpers$opt_warning.length)


  ##############
  # TEST BLANK #
  ##############
  job::job(blank = {
    vars = ls()
    pkgs = .packages()
    opts = options()
  }, import = NULL, packages = NULL)

  Sys.sleep(3)
  expect_true(job_helpers$equal_sets(blank$vars, c("importnames__")))
  expect_true(job_helpers$equal_sets(blank$pkgs, job_helpers$packages))
  expect_true(job_helpers$equal_sets(names(blank), c("vars", "pkgs", "opts", ".call")))
  expect_true(blank$opts$warning.length == job_helpers$opt_warning.length & blank$opts$timeout == job_helpers$opt_timeout)
  rm(blank)


  ##################
  # TEST NO RETURN #
  ##################

  attached_start = ls()

  job::job({
    a = 1
  }, import = NULL, packages = NULL)  # for speed

  expect_true(job_helpers$equal_sets(c(attached_start, "attached_start"), ls()))
  rm(attached_start)


  ############
  # CLEAN UP #
  ############
  job_helpers$cleanup()
  rm(job_helpers)
} else {
  expect_error(job::job({a = 1}), pattern = "Jobs can only be created if job")
}
