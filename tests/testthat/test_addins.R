# Currently has to be run manually
if (rstudioapi::isAvailable() & FALSE) {
  # Set env
  library(testthat)
  rm(list = ls(all.names = TRUE))
  a = 5

  # Simulate user behavior in console
  launch_addin = function(code, func) {
    rstudioapi::sendToConsole(code, execute = FALSE, focus = TRUE)  # paste to console
    rstudioapi::setSelectionRanges(rstudioapi::document_range(c(0, 0), c(10000, 10000)))  # Select it
    func()  # Run addin func
    selectionSet("")  # blank when done
  }

  # Test jobaddin_selection()
  launch_addin("qqq = a * 3; sss = c(1,3,5)", job:::jobaddin_selection)
  Sys.sleep(3)
  expect_identical(qqq, a * 3)
  expect_identical(sss, c(1, 3, 5))
  rm(qqq)
  rm(sss)

  # Test jobaddin_selection_empty()
  launch_addin("a_exists = exists('a')", job:::jobaddin_selection_empty)
  Sys.sleep(3)
  expect_false(a_exists)
  expect_identical(a, 5)  # Quick check that a kept its value
  expect_identical(ls(all.names = TRUE), c("a", "a_exists", "launch_addin"))
  rm(a_exists)

  # Final cleanup
  rm(list = ls())
}
