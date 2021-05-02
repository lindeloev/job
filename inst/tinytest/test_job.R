if (rstudioapi::isAvailable()) {
  # Remove everything
  rm(list = ls(all.names = TRUE))
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))

  # Explicit libraries for manual testing
  library(rstudioapi)
  library(tinytest)
  #library(job)

  # Helpers. Save in one obj to better control what's in the environment
  helpers = list(
    cleanup = function() {
      suppressWarnings(rm(list = c("a", "b", "default", "with_args", "returned",  "blank", "attached_start"), envir = parent.frame()))
      options(warning.length = helpers$opt_warning.length)
    },
    equal_sets = function(x, y) {
      all(x %in% y) & all(y %in% x)
    },
    import = c(".__js__", ".Random.seed"),
    packages = c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base"),
    opt_warning.length = options("warning.length")[[1]],
    opt_timeout = options("timeout")[[1]]
  )

  # For good measure
  helpers$cleanup()

  a = 123
  b = list(a = a, goat = "baah")


  #########################
  # TEST DEFAULT BEHAVIOR #
  #########################
  options(warning.length = 999)
  job::job(default = {
    vars = ls(all.names = TRUE)
    pkgs = .packages()
    a_copy = a
    b_copy = b
    a = 123  # same value as imported; do not return
    b = list(a = a, goat = "peep")  # imported, but new value; return
    attached_rstudioapi = exists("isAvailable")
    opts = options()
  })

  # Check result
  Sys.sleep(10)  # First is slower
  expect_true(helpers$equal_sets(default$vars, c("a", "b", "helpers", helpers$import)))
  expect_true(helpers$equal_sets(default$pkgs, c("rstudioapi", "tinytest", helpers$packages)))
  expect_identical(default$a_copy, a)
  expect_identical(default$b_copy, b)
  expect_true(default$attached_rstudioapi)
  expect_true(helpers$equal_sets(names(default), c("vars", "pkgs", "a_copy", "b_copy", "b", "attached_rstudioapi", "opts", ".call")))
  expect_true(default$opts$warning.length == 999 & default$opts$device == options("device"))

  # Cleanup
  rm(default)
  options(warning.length = helpers$opt_warning.length)


  #############
  # TEST ARGS #
  #############
  options(warning.length = 999)
  returned = job::job(with_args = {
    vars = ls()
    pkgs = .packages()
    b_copy = b
    helpers = list(i_am = "different")  # change value of imported
    attached_rstudioapi = exists("isAvailable")
    opts = options()
  }, import = c(b, helpers), packages = c("job"), title = "something weird: #/(¤", opts = list(timeout = 59))

  # Check result
  Sys.sleep(3)
  expect_true(is.character(returned))
  expect_true(helpers$equal_sets(with_args$vars, c("b", "helpers", "importnames__")))
  expect_true(helpers$equal_sets(with_args$pkgs, c("job", helpers$packages)))
  expect_identical(with_args$b_copy, b)
  expect_true(with_args$attached_rstudioapi == FALSE)
  expect_true(helpers$equal_sets(names(with_args), c("vars", "pkgs", "b_copy", "helpers", "attached_rstudioapi", "opts", ".call")))
  expect_true(with_args$opts$warning.length == helpers$opt_warning.length & with_args$opts$timeout == 59)

  # Cleanup
  rm(returned)
  rm(with_args)
  options(warning.length = helpers$opt_warning.length)


  ##############
  # TEST BLANK #
  ##############
  job::job(blank = {
    vars = ls()
    pkgs = .packages()
    opts = options()
  }, import = NULL, packages = NULL)

  Sys.sleep(3)
  expect_true(helpers$equal_sets(blank$vars, character(0)))
  expect_true(helpers$equal_sets(blank$pkgs, helpers$packages))
  expect_true(helpers$equal_sets(names(blank), c("vars", "pkgs", "opts", ".call")))
  expect_true(blank$opts$warning.length == helpers$opt_warning.length & blank$opts$timeout == helpers$opt_timeout)
  rm(blank)


  ####################
  # TEST JOB::EXPORT #
  ####################

  job::job(ex_all = {
    q = 555
    job::export("all")
  })

  Sys.sleep(3)
  expect_true(ex_all$q == 555)
  expect_true(helpers$equal_sets(names(ex_all), c(".call", ".Random.seed", "a", "b", "helpers", "q")))
  rm(ex_all)

  job::job(ex_none = {
    q = 555
    job::export(NULL)
  })

  Sys.sleep(3)
  expect_true(names(ex_none) == ".call")
  rm(ex_none)


  job::job(ex_some = {
    q = 555
    stuff = "don't return me"
    job::export(c(a, q, helpers))
  })
  expect_true(helpers$equal_sets(names(ex_some), c(".call", "a", "helpers", "q")))
  rm(ex_some)


  #########################
  # TEST RETURN TO GLOBAL #
  #########################

  attached_start = ls()

  job::job({
    a = 10  # Try assigning new value
    newvar1 = 1
    newvar5 = 5 * 3
  }, packages = NULL)  # for speed

  Sys.sleep(3)
  expect_true(a == 10)  # Changed, so overwritten
  expect_true(newvar1 == 1 & newvar5 == 15)
  expect_true(helpers$equal_sets(c(attached_start, "attached_start", "newvar1", "newvar5"), ls()))
  rm(attached_start)
  rm(newvar1)
  rm(newvar5)


  ############
  # CLEAN UP #
  ############
  helpers$cleanup()
  rm(helpers)
} else {
  expect_error(job::job({a = 1}), pattern = "must be called from within RStudio.")
}
