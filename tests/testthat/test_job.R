if (rstudioapi::isAvailable()) {
  # Remove everything
  rm(list = ls(all.names = TRUE))
  rm(list = ls(all.names = TRUE, envir = globalenv()), envir = globalenv())
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE))

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
      suppressWarnings(rm(list = c("a", "b", "default", "with_args1", "with_args2", "returned",  "empty1", "empty2", "attached_start"), envir = parent.frame()))
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

  #a = 123
  #b = list(a = a, goat = "baah")
  options(job.mainsession = TRUE)  # Test whether this is imported to job sessions


  #########################
  # TEST DEFAULT BEHAVIOR #
  #########################
  test_that("Default usage with named chunk", {
    # Set env
    a = 123
    b = list(a = a, goat = "baah")

    # Launch job
    job::job(default = {
      vars = ls(all.names = TRUE)
      pkgs = .packages()
      a_copy = a
      b_copy = b
      a = 123  # same value as imported; do not return
      b = list(a = a, goat = "peep")  # imported, but new value; return
      attached_rstudioapi = exists("isAvailable")
      opts = options()
      print("output!")
    })

    # Check result
    helpers$wait_for_job("default")
    expect_identical(default$vars, c(".__js__", "a", "b"))
    expect_identical(default$pkgs, c("rstudioapi", "testthat", helpers$pkgs))
    expect_identical(default$a_copy, a)
    expect_identical(default$b_copy, b)
    expect_true(default$attached_rstudioapi)
    expect_identical(names(default), c(".call", "b_copy", "pkgs", "b", "opts", "a_copy", "vars", "attached_rstudioapi"))
    expect_true(is.null(default$opts$job.mainsession) == FALSE & default$opts$device == options("device"))

    # Cleanup
    rm(default, envir = globalenv())
  })



  #############
  # TEST ARGS #
  #############
  test_that("Set arguments in job::job()", {
    # Set env
    a = 123
    b = list(a = a, goat = "baah")
    q = "some value"

    # Launch job
    returned = job::job(with_args1 = {
      vars = ls(all.names = TRUE)
      pkgs = .packages()
      b_copy = b
      q = list(i_am = "different")  # change value of imported
      attached_rstudioapi = exists("isAvailable")
      opts = options()
      print("output!")
    }, import = c(b, q), packages = c("job"), title = "something weird: #/(¤", opts = list(job.newopt = 59))

    # Check result
    helpers$wait_for_job("with_args1")
    expect_true(is.character(returned))
    expect_identical(with_args1$vars, c(".__js__", "b", "q"))
    expect_identical(with_args1$pkgs, c("job", helpers$pkgs))
    expect_identical(with_args1$b_copy, b)
    expect_true(with_args1$attached_rstudioapi == FALSE)
    expect_identical(names(with_args1), c(".call", "b_copy", "pkgs", "opts", "vars", "q", "attached_rstudioapi"))
    expect_true(is.null(with_args1$opts$job.mainsession) == TRUE & with_args1$opts$job.newopt == 59)

    # Cleanup
    rm(with_args1, envir = globalenv())
  })



  ##############################
  # TEST empty() WITH ARGS #
  ##############################
  test_that("Set arguments in job::empty()", {
    # Set env
    a = 123
    b = list(a = a, goat = "baah")
    q = "some value"

    # Launch job
    returned = job::empty(with_args2 = {
      vars = ls(all.names = TRUE)
      pkgs = .packages()
      b_copy = b
      q = list(i_am = "different")  # change value of imported
      attached_rstudioapi = exists("isAvailable")
      opts = options()
      print("output!")
    }, import = c(b, q), packages = c("job"), title = "something weird: #/(¤", opts = list(job.newopt = 59))

    # Check result
    helpers$wait_for_job("with_args2")
    expect_true(is.character(returned))
    expect_identical(with_args2$vars, c(".__js__", "b", "q"))
    expect_identical(with_args2$pkgs, c("job", helpers$pkgs))
    expect_identical(with_args2$b_copy, b)
    expect_true(with_args2$attached_rstudioapi == FALSE)
    expect_identical(names(with_args2), c(".call", "b_copy", "pkgs", "opts", "vars", "q", "attached_rstudioapi"))
    expect_true(is.null(with_args2$opts$job.mainsession) == TRUE & with_args2$opts$job.newopt == 59)

    # Cleanup
    rm(with_args2, envir = globalenv())
  })


  ##############
  # TEST EMPTY #
  ##############
  test_that("Empty job via job::job()", {
    # Set env
    a = 123
    b = list(a = a, goat = "baah")

    # Launch job
    job::job(empty1 = {
      vars = ls(all.names = TRUE)
      pkgs = .packages()
      opts = options()
      print("output!")
    }, import = NULL, packages = NULL, opts = NULL)

    # Check results
    helpers$wait_for_job("empty1")
    expect_identical(empty1$vars, ".__js__")
    expect_identical(empty1$pkgs, helpers$pkgs)
    expect_identical(names(empty1), c(".call", "pkgs", "opts", "vars"))
    expect_true(is.null(empty1$opts$job.mainsession) == TRUE)

    # Cleanup
    rm(empty1, envir = globalenv())
  })

  test_that("Empty job via job::empty()", {
    # Set env
    a = 123
    b = list(a = a, goat = "baah")

    # Launch job
    job::empty(empty2 = {
      vars = ls(all.names = TRUE)
      pkgs = .packages()
      opts = options()
      print("output!")
    })

    # Check results
    helpers$wait_for_job("empty2")
    expect_identical(empty2$vars, ".__js__")
    expect_identical(empty2$pkgs, helpers$pkgs)
    expect_identical(names(empty2), c(".call", "pkgs", "opts", "vars"))
    expect_true(is.null(empty2$opts$job.mainsession) == TRUE)

    # Cleanup
    rm(empty2, envir = globalenv())
  })


  ####################
  # TEST JOB::EXPORT #
  ####################
  test_that("Default + export('all')", {
    # Set env
    a = 123

    # Launch job
    job::job(ex_all = {
      q = 555
      print("output!")
      job::export("all")
    })

    # Check results
    helpers$wait_for_job("ex_all")
    expect_identical(ex_all$q, 555)
    expect_identical(names(ex_all), c(".call", "a", "q"))

    # Cleanup
    rm(ex_all, envir = globalenv())
  })

  test_that("Default + export(NULL)", {
    # Set env
    a = 123

    # Launch job
    job::job(ex_none = {
      q = 555
      print("output!")
      job::export(NULL)
    })

    # Check results
    helpers$wait_for_job("ex_none")
    expect_identical(names(ex_none), ".call")

    # Cleanup
    rm(ex_none, envir = globalenv())
  })

  test_that("Default + export(c(some, vars))", {
    # Set env
    a = 123
    b = list(a = a, goat = "baah")

    # Launch job
    job::job(ex_some = {
      q = 555
      stuff = "don't return me"
      print("output!")
      job::export(c(a, q))
    })

    # Check results
    helpers$wait_for_job("ex_some")
    expect_identical(names(ex_some), c(".call", "a", "q"))

    # Cleanup
    rm(ex_some, envir = globalenv())
  })

  test_that("Default + export('new')", {
    # Set env
    a = 123

    # Launch job
    job::job(ex_new = {
      a = 444
      q = 555
      print("output!")
      job::export("new")
    })

    # Check results
    helpers$wait_for_job("ex_new")
    expect_identical(ex_new$q, 555)
    expect_identical(names(ex_new), c(".call", "q"))

    # Cleanup
    rm(ex_new, envir = globalenv())
  })





  ##########################
  # TEST UNNAMED ARGUMENTS #
  ##########################
  # These two jobs are identical
  test_that("All args are unnamed", {
    # Set env
    a = 123
    b = list(a = a, goat = "baah")

    # Launch job
    job::job(ex_unnamed = {
      a = 123
      vars = ls(all.names = TRUE)
      pkgs = .packages()
      opts = options()
      print("output!")
    }, c(a), c("rstudioapi"), list(job.newopt = 59))

    # Check results
    helpers$wait_for_job("ex_unnamed")
    expect_identical(ex_unnamed$vars, c(".__js__", "a"))
    expect_identical(ex_unnamed$pkgs, c("rstudioapi", helpers$pkgs))
    expect_null(ex_unnamed$opts$job.mainsession)
    expect_identical(ex_unnamed$opts$job.newopt, 59)

    # Cleanup
    rm(ex_unnamed, envir = globalenv())
  })

  test_that("Some args are unnamed", {
    # Set env
    a = 123
    b = list(a = a, goat = "baah")

    # Launch job
    job::job(ex_onenamed = {
      a = 123
      vars = ls(all.names = TRUE)
      pkgs = .packages()
      opts = options()
      print("output!")
    }, c(a), packages = c("rstudioapi"), list(job.newopt = 59))

    # Check results
    helpers$wait_for_job("ex_onenamed")
    expect_identical(ex_onenamed$vars, c(".__js__", "a"))
    expect_identical(ex_onenamed$pkgs, c("rstudioapi", helpers$pkgs))
    expect_null(ex_onenamed$opts$job.mainsession)
    expect_identical(ex_onenamed$opts$job.newopt, 59)

    # Cleanup
    rm(ex_onenamed, envir = globalenv())
  })


  #########################
  # TEST RETURN TO GLOBAL #
  #########################
  test_that("Return to global", {
    job::job({
      a = 10  # Try assigning new value
      newvar1 = 1
      newvar5 = 5 * 3
      print("output!")
    }, packages = NULL)  # for speed

    helpers$wait_for_job("newvar1")
    expect_identical(get("a", envir = globalenv()), 10)
    expect_true(newvar1 == 1 & newvar5 == 15)
    #expect_identical(ls(envir = globalenv()), c("a", "newvar1", "newvar5"))  # b and helpers are in local env.

    rm(a, envir = globalenv())
    rm(newvar1, envir = globalenv())
    rm(newvar5, envir = globalenv())
  })



  ############
  # CLEAN UP #
  ############
  helpers$cleanup()
  rm(helpers)
} else {
  expect_error(job::job({a = 1}), pattern = "You must run this from the RStudio main session.")
}
