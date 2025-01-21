library(testthat)

test_file <- tempfile() #Create temporary folder for the tests


test_that("check_if_update behaves as expected", {

  # Case 1: file does not exist
  expect_error(check_if_update("non_existent_file.txt", 7), "File NOT FOUND")

  # Case 2: File exists, but it's very old (should issue a warning)
  file.create(test_file)
  Sys.setFileTime(test_file, Sys.time() - days(10))
  expect_warning(check_if_update(test_file, 7), "File is outdated")

  # Caso 3: File was recently updated (should not issue warnings, only message)
  Sys.setFileTime(test_file, Sys.time() - days(3))
  expect_message(check_if_update(test_file, 7), "File is up to date")
})

# clean temporary fodler
unlink(test_file)

