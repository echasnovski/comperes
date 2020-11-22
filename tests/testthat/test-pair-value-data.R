context("pair-value-data")


# Input data --------------------------------------------------------------
long_tbl <- tibble::tibble(
  key_1 = c(1, 1, 2, NA),
  key_2 = c(2, 1, NA, NA),
  val = 1:4
)

# styler: off
mat_data <- matrix(
  c(2L, 1L, NA,
    NA, NA, 3L,
    NA, NA, 4L),
  nrow = 3, dimnames = list(c("1", "2", NA), c("1", "2", NA)),
  byrow = TRUE
)
# styler: on


# long_to_mat -------------------------------------------------------------
test_that("long_to_mat works", {
  output_1 <- long_to_mat(long_tbl, "key_1", "key_2", "val")
  output_ref_1 <- mat_data

  expect_identical(output_1, output_ref_1)

  # Handling length zero `value` (which should be default)
  expect_identical(
    long_to_mat(long_tbl, "key_1", "key_2", value = NULL),
    long_to_mat(long_tbl, "key_1", "key_2", value = character(0))
  )

  expect_message(
    output_2 <- long_to_mat(long_tbl, "key_1", "key_2"),
    "Us.*val"
  )
  expect_identical(output_2, output_ref_1)

  expect_message(
    output_3 <- long_to_mat(long_tbl[, -3], "key_1", "key_2"),
    "No.*dummy"
  )

  output_ref_3 <- output_ref_1 <- matrix(
    rep(NA, 9),
    nrow = 3, dimnames = list(c("1", "2", NA), c("1", "2", NA)),
    byrow = TRUE
  )

  expect_identical(output_3, output_ref_3)
})

test_that("long_to_mat handles not NULL `fill`", {
  # Handling not NULL `fill`
  output <- long_to_mat(long_tbl, "key_1", "key_2", "val", fill = 0)

  # styler: off
  output_ref <- matrix(
    c(2, 1, 0,
      0, 0, 3,
      0, 0, 4),
    nrow = 3, dimnames = list(c("1", "2", NA), c("1", "2", NA)),
    byrow = TRUE
  )
  # styler: on

  expect_identical(output, output_ref)
})

test_that("long_to_mat works with list values", {
  input <- long_tbl
  input$val <- list(1, 2, 3, 4)

  output_1 <- long_to_mat(input, "key_1", "key_2", "val")
  # styler: off
  output_ref_1 <- matrix(
    list(   2,    1, NULL,
            NULL, NULL,    3,
            NULL, NULL,    4),
    nrow = 3, dimnames = list(c("1", "2", NA), c("1", "2", NA)),
    byrow = TRUE
  )
  # styler: on

  expect_identical(output_1, output_ref_1)

  # Not NULL `fill`
  output_2 <- long_to_mat(input, "key_1", "key_2", "val", fill = "a")
  # styler: off
  output_ref_2 <- matrix(
    list(  2,   1, "a",
         "a", "a",   3,
         "a", "a",   4),
    nrow = 3, dimnames = list(c("1", "2", NA), c("1", "2", NA)),
    byrow = TRUE
  )
  # styler: on

  expect_identical(output_2, output_ref_2)
})

test_that("long_to_mat can work silently", {
  expect_silent(long_to_mat(long_tbl, "key_1", "key_2", silent = TRUE))
  expect_silent(long_to_mat(long_tbl[, -3], "key_1", "key_2", silent = TRUE))
})

test_that("long_to_mat handles factors", {
  # Mixing factor and non-factor
  input_fac_1 <- long_tbl[1:2, ]
  input_fac_1$key_1 <- factor(input_fac_1$key_1, levels = 3:1)

  output_1 <- long_to_mat(input_fac_1, "key_1", "key_2", "val")
  output_ref_1 <- matrix(
    c(rep(NA, 4), 2L, 1L),
    nrow = 3,
    dimnames = list(c("3", "2", "1"), c("1", "2")),
    byrow = TRUE
  )

  expect_identical(output_1, output_ref_1)

  # Both are factors
  input_fac_2 <- input_fac_1
  input_fac_2$key_2 <- factor(input_fac_2$key_2, levels = 1:3)

  output_2 <- long_to_mat(input_fac_2, "key_1", "key_2", "val")
  output_ref_2 <- matrix(
    c(rep(NA, 6), 2L, 1L, NA),
    nrow = 3,
    dimnames = list(c("3", "2", "1"), c("1", "2", "3")),
    byrow = TRUE
  )

  expect_identical(output_2, output_ref_2)
})

test_that("long_to_mat correctly orders row and column names", {
  input <- tibble::tibble(
    key_1 = c(10, 1, 12, 2),
    key_2 = c(1, 10, 10, 1),
    val = 1:4
  )

  output <- long_to_mat(input, "key_1", "key_2", "val")
  # styler: off
  output_ref <- matrix(
    c(NA, 2L,
      4L, NA,
      1L, NA,
      NA, 3L),
    nrow = 4, dimnames = list(c("1", "2", "10", "12"), c("1", "10")),
    byrow = TRUE
  )
  # styler: on

  expect_identical(output, output_ref)
})

test_that("long_to_mat takes first pair among duplicated", {
  input <- tibble::tibble(
    key_1 = c(1, 1, 1, 1),
    key_2 = c(1, 2, 1, 2),
    val = 1:4
  )

  output <- long_to_mat(input, "key_1", "key_2", "val")
  output_ref <- matrix(
    c(1L, 2L),
    nrow = 1, dimnames = list(c("1"), c("1", "2")),
    byrow = TRUE
  )

  expect_identical(output, output_ref)
})

test_that("long_to_mat throws errors", {
  expect_error(
    long_to_mat(long_tbl, 1, "key_2", "val"),
    "row_key.*single.*string"
  )
  expect_error(
    long_to_mat(long_tbl, "key_1", c("key_2", "key_3"), "val"),
    "col_key.*single.*string"
  )
  expect_error(
    long_to_mat(long_tbl, "key_1", "key_2", list("a")),
    "value.*single.*string"
  )
})


# mat_to_long -------------------------------------------------------------
test_that("mat_to_long works", {
  output <- mat_to_long(mat_data, "key_1", "key_2", "val")
  output_ref <- tibble::tibble(
    key_1 = rep(c("1", "2", NA), each = 3),
    key_2 = rep(c("1", "2", NA), times = 3),
    val = c(2L, 1L, NA, NA, NA, 3L, NA, NA, 4L)
  )

  expect_identical(output, output_ref)
})

test_that("mat_to_long drops by only value column", {
  output <- mat_to_long(mat_data, "key_1", "key_2", "val", drop = TRUE)
  output_ref <- tibble::tibble(
    key_1 = c("1", "1", "2", NA),
    key_2 = c("1", "2", NA, NA),
    val = c(2L, 1L, 3L, 4L)
  )

  expect_identical(output, output_ref)
})

test_that("mat_to_long throws errors", {
  expect_error(
    mat_to_long(mat_data, 1, "key_2", "val"),
    "row_key.*single.*string"
  )
  expect_error(
    mat_to_long(mat_data, "key_1", c("key_2", "key_3"), "val"),
    "col_key.*single.*string"
  )
  expect_error(
    mat_to_long(mat_data, "key_1", "key_2", list("a")),
    "value.*single.*string"
  )
})
