context("outer-methods")


# Input data --------------------------------------------------------------
input_longcr <- dplyr::tibble(
  game = 1:10,
  player = 11:20,
  score = 101:110
)
input_longcr <- add_class(input_longcr, "longcr")

input_widecr <- dplyr::tibble(
  game = 1:10,
  player1 = 1:10,
  score1 = 11:20,
  player2 = 2:11,
  score2 = 12:21
)
input_widecr <- add_class(input_widecr, "widecr")

input_join <- dplyr::tibble(
  game = 10:6,
  extra = -(1:5)
)


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
  expect_is(dplyr::summarise(input_longcr, n = n()), "longcr")
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
  class(input) <- class(dplyr::tibble())

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
  expect_is(dplyr::summarise(input_widecr, n = n()), "widecr")
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
  class(input) <- class(dplyr::tibble())

  tibble_output_ref <- capture_output(print(input))

  expect_output(
    print(as_widecr(input_widecr)),
    paste0("# A widecr object:\n.*", tibble_output_ref)
  )
})
