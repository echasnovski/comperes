context("utils")


# add_name_prefix ---------------------------------------------------------
test_that("add_name_prefix works", {
  input <- tibble::tibble(x = 1:2, y = 3:4)

  expect_identical(add_name_prefix(input, ""), input)

  output <- add_name_prefix(input, "a_")
  output_ref <- tibble::tibble(a_x = input$x, a_y = input$y)

  expect_identical(output, output_ref)
})


# first_col_name ----------------------------------------------------------
test_that("first_col_name works", {
  input <- data.frame(x = 1, y = 2)
  input_no_col <- input[, -(1:2)]

  expect_identical(first_col_name(input), "x")
  expect_identical(first_col_name(input_no_col), NULL)

  # Appropriate messaging
  expect_silent(first_col_name(input, silent = TRUE))
  expect_silent(first_col_name(input_no_col, silent = TRUE))

  expect_message(first_col_name(input, silent = FALSE), "Us.*x")
  expect_message(
    first_col_name(input_no_col, silent = FALSE),
    "[Nn]o.*Us.*dummy"
  )

  expect_message(
    first_col_name(input, silent = FALSE, target_name = "new value"),
    "new value"
  )
})


# miss_value --------------------------------------------------------------
test_that("miss_value works", {
  expect_identical(miss_value(), NA)
  expect_identical(miss_value(type = "list"), list(NULL))
  expect_identical(miss_value(fill = 0), 0)
})


# levels2 -----------------------------------------------------------------
test_that("levels2 works", {
  input_fac <- factor(c("a", "b", "a"), levels = c("b", "a"))

  expect_identical(levels2(input_fac), c("b", "a"))

  input_int <- c(10, 1, 2, NA, 11)

  expect_identical(levels2(input_int), c("1", "2", "10", "11", NA))
  expect_identical(levels2(input_int, na.last = FALSE),
                   c(NA, "1", "2", "10", "11"))
  expect_identical(levels2(input_int, na.last = NA),
                   c("1", "2", "10", "11"))
})


# add_class ---------------------------------------------------------------
test_that("add_class works", {
  input <- 1:10
  class(input) <- "class1"
  output <- input
  class(output) <- c("class2", "class1")

  expect_identical(add_class(input, "class2"), output)
})


# add_class_cond ----------------------------------------------------------
test_that("add_class_cond works", {
  input <- 1:10
  class(input) <- "class1"
  output <- input
  class(output) <- c("class2", "class1")

  expect_identical(add_class_cond(input, "class2"), output)
  expect_identical(add_class_cond(input, "class1"), input)
})


# remove_class_cond -------------------------------------------------------
test_that("remove_class_cond works", {
  input <- 1:10
  class(input) <- c("class2", "class1")
  output <- input
  class(output) <- c("class1")

  expect_identical(remove_class_cond(input, "class2"), output)
  expect_identical(remove_class_cond(input, "class1"), input)
})


# reconstruct -------------------------------------------------------------
test_that("reconstruct works", {
  df <- mtcars
  class(df) <- c("some", "data.frame")

  output <- reconstruct(mtcars, df)
  expect_identical(output, df)
})


# get_formatC_width -----------------------------------------------------
test_that("get_formatC_width works", {
  expect_equal(get_formatC_width(1:9), 1)
  expect_equal(get_formatC_width(1:10), 2)
  expect_equal(get_formatC_width(1:99), 2)
})


# renamecreate_columns ----------------------------------------------------
test_that("renamecreate_columns works", {
  input <- data.frame(x = 1:10, y = 2:11, z = 3:12)
  info <- data.frame(target = c("a", "b", "c"), original = c("x", NA, "y"),
                     stringsAsFactors = FALSE)
  output <- data.frame(a = 1:10, c = 2:11, z = 3:12, b = rep(NA_integer_, 10))

  expect_identical(renamecreate_columns(df = input, info = info,
                                        fill = NA_integer_),
                   output)
})


# reduce_full_join --------------------------------------------------------
test_that("reduce_full_join works", {
  input <- list(
    tibble::tibble(
      game = 1:10,
      player1 = 11:20,
      score1 = 101:110
    ),
    tibble::tibble(
      game = 1:5,
      player2 = 12:16,
      score2 = 102:106
    )
  )
  output <- tibble::tibble(
    game = 1:10,
    player1 = 11:20,
    score1 = 101:110,
    player2 = c(12:16, rep(NA, 5)),
    score2 = c(102:106, rep(NA, 5))
  )

  expect_identical(reduce_full_join(input, by = "game"), output)
  expect_identical(reduce_full_join(input[1], by = "game"), input[[1]])
})


# get_matchups ---------------------------------------------------------
test_that("get_matchups works", {
  cr_data <- data.frame(
    game = rep(1:2, each = 2),
    player = rep(1:2, times = 2),
    score = 31:34,
    scoreSP = 44:41
  )

  output <- get_matchups(cr_data)
  output_ref <- tibble::tibble(
    game = rep(1:2, each = 4),
    player1 = rep(rep(1:2, each = 2), times = 2),
    score1 = rep(31:34, each = 2),
    player2 = rep(1:2, times = 4),
    score2 = c(rep(31:32, times = 2), rep(33:34, times = 2))
  )

  expect_equivalent(output, output_ref)
  expect_true(is_widecr(output))
})


# assert_single_string ----------------------------------------------------
test_that("assert_single_string works", {
  expect_silent(assert_single_string())
  expect_silent(assert_single_string(a = "a"))
  expect_silent(assert_single_string(a = "a", "b"))

  expect_error(assert_single_string(a = 1), "a.*single.*string")
  expect_error(assert_single_string(a = "a", b = 1), "b.*single.*string")
  expect_error(assert_single_string(a = "a", 1), "single.*string")
})


# assert_used_value_col ---------------------------------------------------
test_that("assert_used_value_col works", {
  expect_message(assert_used_value_col(NULL), "[Nn]o.*dummy")
  expect_message(assert_used_value_col(character(0)), "[Nn]o.*dummy")

  expect_message(assert_used_value_col("value_column"), "Us.*value_column")

  expect_message(
    assert_used_value_col(NULL, target_name = "new value"),
    "new value"
  )
  expect_message(
    assert_used_value_col("value_column", target_name = "new value"),
    "Us.*new value"
  )
})


# assert_used_names -------------------------------------------------------
test_that("assert_used_names works", {
  info <- data.frame(original = c("gameId", "playerId", "scoreId"),
                     target = c("game", "player", "score"),
                     stringsAsFactors = FALSE)
  expect_message(
    assert_used_names(info, prefix = "prefix: "),
    "prefix: .*not.*matched.*gameId.*game.*playerId.*player.*scoreId.*score"
  )

  info$original <- info$target
  expect_silent(assert_used_names(info))

  info$original[2] <- NA
  expect_message(
    assert_used_names(info, prefix = "prefix: "),
    "prefix: .*not.*found.*NA.*player"
  )
})
