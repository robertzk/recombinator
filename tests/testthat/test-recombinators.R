context("recombinators")

test_that("heterogeneous recombinator can parse a simple example correctly", {
  pre_dataframe <-
    list(list(variable_one = 1, variable_two = "a"),
         list(variable_one = 2, variable_three = 1))
  df <- data.frame(variable_one = c(1,2), variable_two = c("a", NA),
                   variable_three = c(NA, 1), stringsAsFactors = FALSE)
  expect_identical(heterogeneous_recombinator(pre_dataframe), df)
})

test_that("homogeneous recombinator can parse a simple example correctly", {
  pre_dataframe <- list(c("variable_one", "variable_two"), list(1, "a"), list(2, "b"))
  df <- data.frame(variable_one = c(1,2), variable_two = c("a", "b"), stringsAsFactors = FALSE)
  expect_identical(homogeneous_recombinator(pre_dataframe), df)
})

test_that("recombinator calls to heterogeneous recombinator correctly", {
  pre_dataframe <-
    list(list(variable_one = 1, variable_two = "a"),
         list(variable_one = 2, variable_three = 1))
  df <- data.frame(variable_one = c(1,2), variable_two = c("a", NA),
                   variable_three = c(NA, 1), stringsAsFactors = FALSE)
  expect_identical(recombinator(pre_dataframe), df)
})

test_that("recombinator calls to heterogeneous recombinator correctly #2", {
  pre_dataframe <- list(one = list(1, 2), two = list(2, 3))
  df <- data.frame(one = c(1, 2), two = c(2, 3))
  expect_identical(recombinator(pre_dataframe), df)
})

test_that("recombinator calls to homogeneous recombinator correctly #1", {
  pre_dataframe <- list(c("variable_one", "variable_two"), list(1, "a"), list(2, "b"))
  df <- data.frame(variable_one = c(1,2), variable_two = c("a", "b"), stringsAsFactors = FALSE)
  expect_identical(recombinator(pre_dataframe), df)
})

test_that("recombinator calls to homogeneous recombinator correctly #2.  Issue #6", {
  pre_dataframe <- list(list("variable_one", "variable_two"), list(1, "a"), list(2, "b"))
  df <- data.frame(variable_one = c(1,2), variable_two = c("a", "b"), stringsAsFactors = FALSE)
  expect_identical(recombinator(pre_dataframe), df)
})

test_that("names that are different than make.names are nonstandard", {
  expect_warning(warn_on_nonstandard_names(list(`x:2` = 2)))
})

test_that("names that aren't different than make.names are not nonstandard", {
  expect_identical(NULL, warn_on_nonstandard_names(list(`x2` = 2)))
})

test_that("a list with no names is not unstandard", {
  expect_identical(NULL, warn_on_nonstandard_names(list(2)))
})

test_that("recombinator warns about nonstandard names", {
  expect_warning(homogeneous_recombinator(list(`x:2` = 2)))
})


