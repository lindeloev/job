if (rstudioapi::isAvailable()) {
  library(rstudioapi)
  a = 123
  b = list(a = a, goat = "baah")

  # Helpers to be called inside the jobs
  in_job = list()
  in_job$equal_sets = function(x, y) {
    all(x %in% y) & all(y %in% x)
  }
  in_job$vars = c("in_job", "importnames__", "wd__")
  in_job$packages = c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")



  #########################
  # TEST DEFAULT BEHAVIOR #
  #########################
  job::job(default_result = {
    vars = ls()
    pkgs = .packages()
    a_copy = a
    b_copy = b
    attached_rstudioapi = exists("isAvailable")
  })

  # Result is not returned immediately
  expect_true(exists("default_result") == FALSE)

  # Check result
  Sys.sleep(10)  # First is slower
  expect_true(in_job$equal_sets(default_result$vars, c("a", "b", in_job$vars)))
  expect_true(in_job$equal_sets(default_result$pkgs, c("job", "rstudioapi", "tinytest", in_job$packages)))
  expect_identical(default_result$a_copy, a)
  expect_identical(default_result$b_copy, b)
  expect_true(default_result$attached_rstudioapi)
  expect_true(in_job$equal_sets(names(default_result), c(".call", "vars", "pkgs", "a_copy", "b_copy", "attached_rstudioapi")))
  rm(default_result)

  #############
  # TEST ARGS #
  #############
  returned = job::job(with_args = {
    vars = ls()
    pkgs = .packages()
    b_copy = b
    attached_rstudioapi = exists("isAvailable")
  }, import = c(b, in_job), packages = c("job"), title = "something weird: #/(¤")


  # Result is not returned immediately
  expect_true(exists("with_args") == FALSE)

  # Check result
  Sys.sleep(3)
  expect_null(returned)
  expect_true(in_job$equal_sets(with_args$vars, c("b", in_job$vars)))
  expect_true(in_job$equal_sets(with_args$pkgs, c("job", in_job$packages)))
  expect_identical(with_args$b_copy, b)
  expect_true(with_args$attached_rstudioapi == FALSE)
  expect_true(in_job$equal_sets(names(with_args), c(".call", "vars", "pkgs", "b_copy", "attached_rstudioapi")))
  rm(returned)
  rm(with_args)


  ################
  # TEST BLANK #
  ################
  job::job(blank_slate = {
    vars = ls()
    pkgs = .packages()
  }, import = NULL, packages = NULL)

  Sys.sleep(3)
  expect_true(in_job$equal_sets(blank_slate$vars, c("importnames__", "wd__")))
  expect_true(in_job$equal_sets(blank_slate$pkgs, in_job$packages))
  rm(blank_slate)


  ##################
  # TEST NO RETURN #
  ##################

  attached_start = ls()

  job::job({
    a = 1
  }, import = NULL, packages = NULL)  # for speed

  expect_true(in_job$equal_sets(c(attached_start, "attached_start"), ls()))
  rm(attached_start)


  ############
  # CLEAN UP #
  ############
  rm(list = c("a", "b", "in_job"))
} else {
  expect_error(job::job({a = 1}), pattern = "Jobs can only be created if job")
}
