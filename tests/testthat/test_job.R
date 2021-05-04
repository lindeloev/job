if (rstudioapi::isAvailable()) {
  # Remove everything
  rm(list = ls(all.names = TRUE))
  rm(list = ls(all.names = TRUE, envir = globalenv()), envir = globalenv())
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))

  # Explicit libraries for manual testing
  library(rstudioapi)
  library(testthat)

  # Helpers. Save in one obj to better control what's in the environment
  helpers = list(
    # Wait for some variable to be assigned.
    # Expect that it isn't assigned to begin with
    wait_for_job = function(varname) {
      expect_true(exists(varname, envir = parent.frame()) == FALSE)
      while (exists(varname, envir = parent.frame()) == FALSE) {
        Sys.sleep(0.5)
      }
    },
    cleanup = function() {
      suppressWarnings(rm(list = c("a", "b", "default", "with_args", "returned",  "empty", "attached_start"), envir = parent.frame()))
    },
    equal_sets = function(x, y) {
      all(x %in% y) & all(y %in% x)
    },
    import = c(".__js__", ".Random.seed"),
    pkgs = c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base"),
    test_addin = function(code, func) {
      rstudioapi::selectionSet(code);
      rstudioapi:::setSelectionRanges(rstudioapi::document_range(c(0, 0), c(10000, 10000)));
      func()
    }
  )

  # For good measure
  helpers$cleanup()

  a = 123
  b = list(a = a, goat = "baah")
  options(job.mainsession = TRUE)  # Test whether this is imported to job sessions


  #########################
  # TEST DEFAULT BEHAVIOR #
  #########################
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
  helpers$wait_for_job("default")
  expect_identical(default$vars, c(".__js__", "a", "b", "helpers"))
  expect_identical(default$pkgs, c("rstudioapi", "testthat", helpers$pkgs))
  expect_identical(default$a_copy, a)
  expect_identical(default$b_copy, b)
  expect_true(default$attached_rstudioapi)
  expect_identical(names(default), c(".call", "b_copy", "pkgs", "b", "opts", "a_copy", "vars", "attached_rstudioapi"))
  expect_true(is.null(default$opts$job.mainsession) == FALSE & default$opts$device == options("device"))

  # Cleanup
  rm(default, envir = globalenv())


  #############
  # TEST ARGS #
  #############
  returned = job::job(with_args = {
    vars = ls(all.names = TRUE)
    pkgs = .packages()
    b_copy = b
    helpers = list(i_am = "different")  # change value of imported
    attached_rstudioapi = exists("isAvailable")
    opts = options()
  }, import = c(b, helpers), packages = c("job"), title = "something weird: #/(¤", opts = list(job.newopt = 59))

  # Check result
  helpers$wait_for_job("with_args")
  expect_true(is.character(returned))
  expect_identical(with_args$vars, c(".__js__", "b", "helpers"))
  expect_identical(with_args$pkgs, c("job", helpers$pkgs))
  expect_identical(with_args$b_copy, b)
  expect_true(with_args$attached_rstudioapi == FALSE)
  expect_identical(names(with_args), c(".call", "b_copy", "pkgs", "opts", "helpers", "vars", "attached_rstudioapi"))
  expect_true(is.null(with_args$opts$job.mainsession) == TRUE & with_args$opts$job.newopt == 59)

  # Cleanup
  rm(returned)
  rm(with_args, envir = globalenv())


  ##############
  # TEST EMPTY #
  ##############
  job::job(empty = {
    vars = ls(all.names = TRUE)
    pkgs = .packages()
    opts = options()
  }, import = NULL, packages = NULL, opts = NULL)

  helpers$wait_for_job("empty")
  expect_identical(empty$vars, ".__js__")
  expect_identical(empty$pkgs, helpers$pkgs)
  expect_identical(names(empty), c(".call", "pkgs", "opts", "vars"))
  expect_true(is.null(empty$opts$job.mainsession) == TRUE)
  rm(empty, envir = globalenv())


  ####################
  # TEST JOB::EXPORT #
  ####################

  job::job(ex_all = {
    q = 555
    job::export("all")
  })

  helpers$wait_for_job("ex_all")
  expect_true(ex_all$q == 555)
  expect_identical(names(ex_all), c(".call", "a", "b", "helpers", "q"))
  rm(ex_all, envir = globalenv())

  job::job(ex_none = {
    q = 555
    job::export(NULL)
  })

  helpers$wait_for_job("ex_none")
  expect_true(names(ex_none) == ".call")
  rm(ex_none, envir = globalenv())


  job::job(ex_some = {
    q = 555
    stuff = "don't return me"
    job::export(c(a, q, helpers))
  })

  helpers$wait_for_job("ex_some")
  expect_identical(names(ex_some), c(".call", "a", "helpers", "q"))
  rm(ex_some, envir = globalenv())


  ##########################
  # TEST UNNAMED ARGUMENTS #
  ##########################
  # These two jobs are identical
  job::job(ex_unnamed = {
    a = 123
    vars = ls(all.names = TRUE)
    pkgs = .packages()
    opts = options()
  }, c(a), c("rstudioapi"), list(job.newopt = 59))

  helpers$wait_for_job("ex_unnamed")
  expect_identical(ex_unnamed$vars, c(".__js__", "a"))
  expect_identical(ex_unnamed$pkgs, c("rstudioapi"), helpers$pkgs)
  expect_null(ex_unnamed$opts$job.mainsession)
  expect_identical(ex_unnamed$opts$job.newopt == 59)
  rm(ex_unnamed, envir = globalenv())


  job::job(ex_onenamed = {
    a = 123
    vars = ls(all.names = TRUE)
    pkgs = .packages()
    opts = options()
  }, c(a), packages = c("rstudioapi"), list(job.newopt = 59))

  helpers$wait_for_job("ex_onenamed")
  expect_identical(ex_onenamed$vars, c(".__js__", "a"))
  expect_identical(ex_onenamed$pkgs, c("rstudioapi", helpers$pkgs))
  expect_null(ex_onenamed$opts$job.mainsession)
  expect_identical(ex_onenamed$opts$job.newopt == 59)
  rm(ex_onenamed, envir = globalenv())


  #########################
  # TEST RETURN TO GLOBAL #
  #########################
  job::job({
    a = 10  # Try assigning new value
    newvar1 = 1
    newvar5 = 5 * 3
  }, packages = NULL)  # for speed

  helpers$wait_for_job("newvar1")
  expect_true(get("a", envir = globalenv()) == 10)
  expect_true(newvar1 == 1 & newvar5 == 15)
  expect_identical(ls(envir = globalenv()), c("a", "newvar1", "newvar5"))  # b and helpers are in local env.

  rm("a", envir = globalenv())
  rm(newvar1, envir = globalenv())
  rm(newvar5, envir = globalenv())


  ############
  # CLEAN UP #
  ############
  helpers$cleanup()
  rm(helpers)
} else {
  expect_error(job::job({a = 1}), pattern = "must be called from within RStudio.")
}
