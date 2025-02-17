library(testthat)
library(readxl)
library(openxlsx)
library(janitor)
library(purrr)

test_that("read_excel_allsheets reads all sheets with default engine (readxl)", {
  # Create a temporary test file
  temp_file <- tempfile(fileext = ".xlsx")
  write.xlsx(list(Sheet1 = data.frame(A = 1:3, B = letters[1:3]),
                  Sheet2 = data.frame(X = 4:6, Y = letters[4:6])),
             file = temp_file)

  # Test the function
  result <- read_excel_allsheets(temp_file)

  # Check if the result contains both data frames
  expect_named(result, c("sheet1", "sheet2"))
  expect_equal(ncol(result$sheet1), 2)
  expect_equal(nrow(result$sheet1), 3)
})

test_that("read_excel_allsheets reads specific sheets", {
  temp_file <- tempfile(fileext = ".xlsx")
  write.xlsx(list(Sheet1 = data.frame(A = 1:3, B = letters[1:3]),
                  Sheet2 = data.frame(X = 4:6, Y = letters[4:6])),
             file = temp_file)

  # Reading only the first sheet
  result <- read_excel_allsheets(temp_file, sheet_names = "Sheet1")

  # Check if only the specified sheet was read
  expect_named(result, "sheet1")
  expect_equal(ncol(result$sheet1), 2)
  expect_equal(nrow(result$sheet1), 3)
})

test_that("read_excel_allsheets works with openxlsx engine", {
  temp_file <- tempfile(fileext = ".xlsx")
  write.xlsx(list(Sheet1 = data.frame(A = 1:3, B = letters[1:3]),
                  Sheet2 = data.frame(X = 4:6, Y = letters[4:6])),
             file = temp_file)

  result <- read_excel_allsheets(temp_file, engine = "openxlsx")

  # Check the sheet names and data
  expect_named(result, c("sheet1", "sheet2"))
  expect_equal(ncol(result$sheet1), 2)
  expect_equal(nrow(result$sheet1), 3)
})

test_that("read_excel_allsheets returns data frames when tibble = FALSE", {
  temp_file <- tempfile(fileext = ".xlsx")
  write.xlsx(list(Sheet1 = data.frame(A = 1:3, B = letters[1:3])),
             file = temp_file)

  result <- read_excel_allsheets(temp_file, tibble = FALSE)

  # Check that the result is a data frame
  expect_true(is.data.frame(result$sheet1))
})

test_that("read_excel_allsheets returns tibbles when tibble = TRUE", {
  temp_file <- tempfile(fileext = ".xlsx")
  write.xlsx(list(Sheet1 = data.frame(A = 1:3, B = letters[1:3])),
             file = temp_file)

  result <- read_excel_allsheets(temp_file, tibble = TRUE)

  # Check that the result is a tibble
  expect_true(tibble::is_tibble(result$sheet1))
})

test_that("read_excel_allsheets throws an error for invalid engine", {
  temp_file <- tempfile(fileext = ".xlsx")
  write.xlsx(list(Sheet1 = data.frame(A = 1:3, B = letters[1:3])),
             file = temp_file)

  # Test if the function throws an error for an invalid engine
  expect_error(read_excel_allsheets(temp_file, engine = "invalid_engine"))
})

test_that("read_excel_allsheets correctly uses the guess_max argument", {
  temp_file <- tempfile(fileext = ".xlsx")

  # Create a data frame with mixed data in the same column
  df <- data.frame(
    Column1 = c(rep("text", 10), 100),
    stringsAsFactors = FALSE
  )
  write.xlsx(list(Sheet1 = df), file = temp_file)

  # Without setting guess_max, the column may be read as all character
  result <- read_excel_allsheets(temp_file, guess_max = 5)
  expect_true(is.character(result$sheet1$column1[11]))

  # With a higher guess_max, the numeric value should be detected
  result <- read_excel_allsheets(temp_file, guess_max = 20)
  expect_true(is.numeric(result$sheet1$column1[11]))
})
