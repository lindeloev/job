if (rstudioapi::isAvailable()) {
  # Remove everything
  rm(list = ls(all.names = TRUE))
  rm(list = ls(all.names = TRUE, envir = globalenv()), envir = globalenv())
  if (length(sessionInfo()$otherPkgs) > 0)
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
    pkgs = c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")
  )

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
    expect_identical(default$vars, c(".__jobsettings__", "a", "b"))
    expect_identical(default$pkgs, c("rstudioapi", "testthat", helpers$pkgs))
    expect_identical(default$a_copy, a)
    expect_identical(default$b_copy, b)
    expect_true(default$attached_rstudioapi)
    expect_identical(names(default), c("b_copy", "pkgs", "b", ".jobcode", "opts", "a_copy", "vars", "attached_rstudioapi"))
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
    expect_identical(with_args1$vars, c(".__jobsettings__", "b", "q"))
    expect_identical(with_args1$pkgs, c("job", helpers$pkgs))
    expect_identical(with_args1$b_copy, b)
    expect_true(with_args1$attached_rstudioapi == FALSE)
    expect_identical(names(with_args1), c("b_copy", "pkgs", ".jobcode", "opts", "vars", "q", "attached_rstudioapi"))
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
    expect_identical(with_args2$vars, c(".__jobsettings__", "b", "q"))
    expect_identical(with_args2$pkgs, c("job", helpers$pkgs))
    expect_identical(with_args2$b_copy, b)
    expect_true(with_args2$attached_rstudioapi == FALSE)
    expect_identical(names(with_args2), c("b_copy", "pkgs", ".jobcode", "opts", "vars", "q", "attached_rstudioapi"))
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
    expect_identical(empty1$vars, ".__jobsettings__")
    expect_identical(empty1$pkgs, helpers$pkgs)
    expect_identical(names(empty1), c("pkgs", ".jobcode", "opts", "vars"))
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
    expect_identical(empty2$vars, ".__jobsettings__")
    expect_identical(empty2$pkgs, helpers$pkgs)
    expect_identical(names(empty2), c("pkgs", ".jobcode", "opts", "vars"))
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
    job::job(export_all = {
      q = 555
      print("output!")
      job::export("all")
    })

    # Check results
    helpers$wait_for_job("export_all")
    expect_identical(export_all$q, 555)
    expect_identical(names(export_all), c("a", ".jobcode", "q"))

    # Cleanup
    rm(export_all, envir = globalenv())
  })


  test_that("Default + export(NULL)", {
    # Set env
    a = 123

    # Launch job
    job::job(export_none = {
      q = 555
      print("output!")
      job::export(NULL)
    })

    # Check results
    helpers$wait_for_job("export_none")
    expect_identical(names(export_none), ".jobcode")

    # Cleanup
    rm(export_none, envir = globalenv())
  })


  test_that("Default + export(c(some, vars))", {
    # Set env
    a = 123
    b = list(a = a, goat = "baah")

    # Launch job
    job::job(export_some = {
      q = 555
      stuff = "don't return me"
      print("output!")
      job::export(c(a, q))
    })

    # Check results
    helpers$wait_for_job("export_some")
    expect_identical(names(export_some), c("a", ".jobcode", "q"))

    # Cleanup
    rm(export_some, envir = globalenv())
  })


  test_that("Default + export('new')", {
    # Set env
    a = 123

    # Launch job
    job::job(export_new = {
      a = 444
      q = 555
      print("output!")
      job::export("new")
    })

    # Check results
    helpers$wait_for_job("export_new")
    expect_identical(export_new$q, 555)
    expect_identical(names(export_new), c(".jobcode", "q"))

    # Cleanup
    rm(export_new, envir = globalenv())
  })


  test_that("Default + export('changed', file = 'myfile.RData')", {
    # Set env
    a = 123

    # Launch job
    job::job(export_file = {
      a = 123
      q = 555
      print("output!")
      job::export(file = "mydata.RData")
    })

    # Check results
    helpers$wait_for_job("export_file")
    expect_identical(names(export_file), c(".jobcode"))

    export_env = new.env()
    load("mydata.RData", envir = export_env)
    expect_identical(names(export_env), c("q"))
    expect_identical(export_env$q, 555)

    # Cleanup
    rm(export_file, envir = globalenv())
    file.remove("mydata.RData")
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
    job::job(args_unnamed = {
      a = 123
      vars = ls(all.names = TRUE)
      pkgs = .packages()
      opts = options()
      print("output!")
    }, c(a), c("rstudioapi"), list(job.newopt = 59))

    # Check results
    helpers$wait_for_job("args_unnamed")
    expect_identical(args_unnamed$vars, c(".__jobsettings__", "a"))
    expect_identical(args_unnamed$pkgs, c("rstudioapi", helpers$pkgs))
    expect_null(args_unnamed$opts$job.mainsession)
    expect_identical(args_unnamed$opts$job.newopt, 59)

    # Cleanup
    rm(args_unnamed, envir = globalenv())
  })

  test_that("Some args are unnamed", {
    # Set env
    a = 123
    b = list(a = a, goat = "baah")

    # Launch job
    job::job(args_onenamed = {
      a = 123
      vars = ls(all.names = TRUE)
      pkgs = .packages()
      opts = options()
      print("output!")
    }, c(a), packages = c("rstudioapi"), list(job.newopt = 59))

    # Check results
    helpers$wait_for_job("args_onenamed")
    expect_identical(args_onenamed$vars, c(".__jobsettings__", "a"))
    expect_identical(args_onenamed$pkgs, c("rstudioapi", helpers$pkgs))
    expect_null(args_onenamed$opts$job.mainsession)
    expect_identical(args_onenamed$opts$job.newopt, 59)

    # Cleanup
    rm(args_onenamed, envir = globalenv())
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
  rm(list = ls(all.names = TRUE))
  rm(list = ls(all.names = TRUE, envir = globalenv()))
} else {
  expect_error(job::job({a = 1}), pattern = "You must run this from the RStudio main session.")
}
