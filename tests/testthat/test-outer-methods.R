context("outer-methods")


# Input data --------------------------------------------------------------
input_longcr <- tibble::tibble(
  game = 1:10,
  player = 11:20,
  score = 101:110
)
input_longcr <- add_class(input_longcr, "longcr")

input_widecr <- tibble::tibble(
  game = 1:10,
  player1 = 1:10,
  score1 = 11:20,
  player2 = 2:11,
  score2 = 12:21
)
input_widecr <- add_class(input_widecr, "widecr")

input_join <- tibble::tibble(
  game = 10:6,
  value1 = -(1:5)
)

input_h2h_long <- tibble::tibble(
  player1 = c(1, 1, 2, 2),
  player2 = c(1, NA, 1, NA),
  value1 = 1:4,
  value2 = -(1:4)
)
input_h2h_long <- add_class(input_h2h_long, "h2h_long")

input_h2h_mat <- matrix(
  c(1, NA,
    0, 2),
  nrow = 2, dimnames = list(c("1", "2"), c("1", "2")),
  byrow = TRUE
)
class(input_h2h_mat) <- c("h2h_mat", "matrix")


# select.longcr -----------------------------------------------------------
test_that("select.longcr works", {
  expect_is(dplyr::select(input_longcr, everything()), "longcr")
})


# rename.longcr -----------------------------------------------------------
test_that("rename.longcr works", {
  expect_is(dplyr::rename(input_longcr, game = game), "longcr")
})


# mutate.longcr -----------------------------------------------------------
test_that("mutate.longcr works", {
  expect_is(dplyr::mutate(input_longcr, game = game), "longcr")
})


# summarise.longcr --------------------------------------------------------
test_that("summarise.longcr works", {
  output <- dplyr::summarise(input_longcr, n = n())

  expect_false(class(output)[1] == "longcr")
})


# group_by.longcr ---------------------------------------------------------
test_that("group_by.longcr works", {
  output <- dplyr::group_by(input_longcr, game)

  expect_is(output, "longcr")
  expect_is(output, "grouped_df")
})


# ungroup.longcr ----------------------------------------------------------
test_that("ungroup.longcr works", {
  input_longcr %>%
    dplyr::group_by(game) %>%
    dplyr::ungroup() %>%
    expect_is("longcr")
})


# distinct.longcr ---------------------------------------------------------
test_that("distinct.longcr works", {
  expect_is(dplyr::distinct(input_longcr, game), "longcr")
})


# do.longcr ---------------------------------------------------------------
test_that("do.longcr works", {
  input_longcr %>%
    dplyr::group_by(game) %>%
    dplyr::do(n = nrow(.)) %>%
    expect_is("longcr")
})


# arrange.longcr ----------------------------------------------------------
test_that("arrange.longcr works", {
  expect_is(dplyr::arrange(input_longcr, game), "longcr")
})


# filter.longcr -----------------------------------------------------------
test_that("filter.longcr works", {
  expect_is(dplyr::filter(input_longcr, game %% 2 == 0), "longcr")
})


# slice.longcr ------------------------------------------------------------
test_that("slice.longcr works", {
  expect_is(dplyr::slice(input_longcr, 1:2), "longcr")
})


# inner_join.longcr -------------------------------------------------------
test_that("inner_join.longcr works", {
  input_longcr %>%
    dplyr::inner_join(y = input_join, by = "game") %>%
    expect_is("longcr")
})


# left_join.longcr --------------------------------------------------------
test_that("left_join.longcr works", {
  input_longcr %>%
    dplyr::left_join(y = input_join, by = "game") %>%
    expect_is("longcr")
})


# right_join.longcr -------------------------------------------------------
test_that("right_join.longcr works", {
  input_longcr %>%
    dplyr::right_join(y = input_join, by = "game") %>%
    expect_is("longcr")
})


# full_join.longcr --------------------------------------------------------
test_that("full_join.longcr works", {
  input_longcr %>%
    dplyr::full_join(y = input_join, by = "game") %>%
    expect_is("longcr")
})


# semi_join.longcr --------------------------------------------------------
test_that("semi_join.longcr works", {
  input_longcr %>%
    dplyr::semi_join(y = input_join, by = "game") %>%
    expect_is("longcr")
})


# anti_join.longcr --------------------------------------------------------
test_that("anti_join.longcr works", {
  input_longcr %>%
    dplyr::anti_join(y = input_join, by = "game") %>%
    expect_is("longcr")
})


# [.longcr ----------------------------------------------------------------
test_that("[.longcr works", {
  expect_is(input_longcr[1:2, ], "longcr")
  expect_is(input_longcr[, 1:3], "longcr")
})


# print.longcr ------------------------------------------------------------
test_that("print.longcr works", {
  input <- input_longcr
  class(input) <- class(tibble::tibble())

  tibble_output_ref <- capture_output(print(input))

  expect_output(
    print(as_longcr(input_longcr)),
    paste0("# A longcr object:\n.*", tibble_output_ref)
  )
})


# select.widecr -----------------------------------------------------------
test_that("select.widecr works", {
  expect_is(dplyr::select(input_widecr, everything()), "widecr")
})


# rename.widecr -----------------------------------------------------------
test_that("rename.widecr works", {
  expect_is(dplyr::rename(input_widecr, game = game), "widecr")
})


# mutate.widecr -----------------------------------------------------------
test_that("mutate.widecr works", {
  expect_is(dplyr::mutate(input_widecr, game = game), "widecr")
})


# summarise.widecr --------------------------------------------------------
test_that("summarise.widecr works", {
  output <- dplyr::summarise(input_widecr, n = n())

  expect_false(class(output)[1] == "widecr")
})


# group_by.widecr ---------------------------------------------------------
test_that("group_by.widecr works", {
  output <- dplyr::group_by(input_widecr, game)

  expect_is(output, "widecr")
  expect_is(output, "grouped_df")
})


# ungroup.widecr ----------------------------------------------------------
test_that("ungroup.widecr works", {
  input_widecr %>%
    dplyr::group_by(game) %>%
    dplyr::ungroup() %>%
    expect_is("widecr")
})


# distinct.widecr ---------------------------------------------------------
test_that("distinct.widecr works", {
  expect_is(dplyr::distinct(input_widecr, game), "widecr")
})


# do.widecr ---------------------------------------------------------------
test_that("do.widecr works", {
  input_widecr %>%
    dplyr::group_by(game) %>%
    dplyr::do(n = nrow(.)) %>%
    expect_is("widecr")
})


# arrange.widecr ----------------------------------------------------------
test_that("arrange.widecr works", {
  expect_is(dplyr::arrange(input_widecr, game), "widecr")
})


# filter.widecr -----------------------------------------------------------
test_that("filter.widecr works", {
  expect_is(dplyr::filter(input_widecr, game %% 2 == 0), "widecr")
})


# slice.widecr ------------------------------------------------------------
test_that("slice.widecr works", {
  expect_is(dplyr::slice(input_widecr, 1:2), "widecr")
})


# inner_join.widecr -------------------------------------------------------
test_that("inner_join.widecr works", {
  input_widecr %>%
    dplyr::inner_join(y = input_join, by = "game") %>%
    expect_is("widecr")
})


# left_join.widecr --------------------------------------------------------
test_that("left_join.widecr works", {
  input_widecr %>%
    dplyr::left_join(y = input_join, by = "game") %>%
    expect_is("widecr")
})


# right_join.widecr -------------------------------------------------------
test_that("right_join.widecr works", {
  input_widecr %>%
    dplyr::right_join(y = input_join, by = "game") %>%
    expect_is("widecr")
})


# full_join.widecr --------------------------------------------------------
test_that("full_join.widecr works", {
  input_widecr %>%
    dplyr::full_join(y = input_join, by = "game") %>%
    expect_is("widecr")
})


# semi_join.widecr --------------------------------------------------------
test_that("semi_join.widecr works", {
  input_widecr %>%
    dplyr::semi_join(y = input_join, by = "game") %>%
    expect_is("widecr")
})


# anti_join.widecr --------------------------------------------------------
test_that("anti_join.widecr works", {
  input_widecr %>%
    dplyr::anti_join(y = input_join, by = "game") %>%
    expect_is("widecr")
})


# [.widecr ----------------------------------------------------------------
test_that("[.widecr works", {
  expect_is(input_widecr[1:2, ], "widecr")
  expect_is(input_widecr[, 1:5], "widecr")
})


# print.widecr ------------------------------------------------------------
test_that("print.widecr works", {
  input <- input_widecr
  class(input) <- class(tibble::tibble())

  tibble_output_ref <- capture_output(print(input))

  expect_output(
    print(input_widecr),
    paste0("# A widecr object:\n.*", tibble_output_ref)
  )
})


# select.h2h_long -----------------------------------------------------------
test_that("select.h2h_long works", {
  expect_is(dplyr::select(input_h2h_long, everything()), "h2h_long")
})


# rename.h2h_long -----------------------------------------------------------
test_that("rename.h2h_long works", {
  expect_is(dplyr::rename(input_h2h_long, value = value1), "h2h_long")
})


# mutate.h2h_long -----------------------------------------------------------
test_that("mutate.h2h_long works", {
  expect_is(dplyr::mutate(input_h2h_long, value = value1), "h2h_long")
})


# summarise.h2h_long --------------------------------------------------------
test_that("summarise.h2h_long works", {
  output <- dplyr::summarise(input_h2h_long, n = n())

  expect_false(class(output)[1] == "h2h_long")
})


# group_by.h2h_long ---------------------------------------------------------
test_that("group_by.h2h_long works", {
  output <- dplyr::group_by(input_h2h_long, player1)

  expect_is(output, "h2h_long")
  expect_is(output, "grouped_df")
})


# ungroup.h2h_long ----------------------------------------------------------
test_that("ungroup.h2h_long works", {
  input_h2h_long %>%
    dplyr::group_by(player1) %>%
    dplyr::ungroup() %>%
    expect_is("h2h_long")
})


# distinct.h2h_long ---------------------------------------------------------
test_that("distinct.h2h_long works", {
  expect_is(dplyr::distinct(input_h2h_long, player1), "h2h_long")
})


# do.h2h_long ---------------------------------------------------------------
test_that("do.h2h_long works", {
  input_h2h_long %>%
    dplyr::group_by(player1) %>%
    dplyr::do(n = nrow(.)) %>%
    expect_is("h2h_long")
})


# arrange.h2h_long ----------------------------------------------------------
test_that("arrange.h2h_long works", {
  expect_is(dplyr::arrange(input_h2h_long, player1), "h2h_long")
})


# filter.h2h_long -----------------------------------------------------------
test_that("filter.h2h_long works", {
  expect_is(dplyr::filter(input_h2h_long, player1 %% 2 == 0), "h2h_long")
})


# slice.h2h_long ------------------------------------------------------------
test_that("slice.h2h_long works", {
  expect_is(dplyr::slice(input_h2h_long, 1:2), "h2h_long")
})


# inner_join.h2h_long -------------------------------------------------------
test_that("inner_join.h2h_long works", {
  input_h2h_long %>%
    dplyr::inner_join(y = input_join, by = "value1") %>%
    expect_is("h2h_long")
})


# left_join.h2h_long --------------------------------------------------------
test_that("left_join.h2h_long works", {
  input_h2h_long %>%
    dplyr::left_join(y = input_join, by = "value1") %>%
    expect_is("h2h_long")
})


# right_join.h2h_long -------------------------------------------------------
test_that("right_join.h2h_long works", {
  input_h2h_long %>%
    dplyr::right_join(y = input_join, by = "value1") %>%
    expect_is("h2h_long")
})


# full_join.h2h_long --------------------------------------------------------
test_that("full_join.h2h_long works", {
  input_h2h_long %>%
    dplyr::full_join(y = input_join, by = "value1") %>%
    expect_is("h2h_long")
})


# semi_join.h2h_long --------------------------------------------------------
test_that("semi_join.h2h_long works", {
  input_h2h_long %>%
    dplyr::semi_join(y = input_join, by = "value1") %>%
    expect_is("h2h_long")
})


# anti_join.h2h_long --------------------------------------------------------
test_that("anti_join.h2h_long works", {
  input_h2h_long %>%
    dplyr::anti_join(y = input_join, by = "value1") %>%
    expect_is("h2h_long")
})


# [.h2h_long ----------------------------------------------------------------
test_that("[.h2h_long works", {
  expect_is(input_h2h_long[1:2, ], "h2h_long")
  expect_is(input_h2h_long[, 1:2], "h2h_long")
})


# print.h2h_long ------------------------------------------------------------
test_that("print.h2h_long works", {
  input <- input_h2h_long
  class(input) <- class(tibble::tibble())

  tibble_output_ref <- capture_output(print(input))

  expect_output(
    print(input_h2h_long),
    paste0("# A long format of Head-to-Head values:\n.*", tibble_output_ref)
  )
})


# [.h2h_mat ----------------------------------------------------------------
test_that("[.h2h_mat works", {
  expect_is(input_h2h_mat[1:2, ], "h2h_mat")
  expect_is(input_h2h_mat[, 1:2], "h2h_mat")
})

test_that("[.h2h_mat works with only one argument", {
  expect_identical(input_h2h_mat[c(1, 3)], c(1, NA))

  output <- c(1, 2)
  names(output) <- c("1", "2")

  expect_identical(diag(input_h2h_mat), output)
})


# print.h2h_mat ------------------------------------------------------------
test_that("print.h2h_mat works", {
  input <- input_h2h_mat
  class(input) <- "matrix"

  matrix_output_ref <- capture_output(print(input))

  expect_false(grepl("h2h_mat", matrix_output_ref))
  expect_output(
    print(input_h2h_mat),
    paste0("# A matrix format of Head-to-Head values:\n.*", matrix_output_ref)
  )
})
