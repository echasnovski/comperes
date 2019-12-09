context("head-to-head")


# Input data --------------------------------------------------------------
cr_data <- data.frame(
  game =   c(1,  1,  1, 2, 2, 3, 3, 4),
  player = c(1, NA, NA, 1, 2, 2, 1, 2),
  score = as.numeric(1:8),
  scoreSP = -(1:8)
)

output_long <- tibble::tibble(
  player1 = rep(c(1, 2, NA), each = 3),
  player2 = rep(c(1, 2, NA), times = 3),
  mean_score1 = c(4, 5.5, 1, 5.5, 19/3, NA, 2.5, NA, 2.5),
  sum_score = c(24, 22, 7, 22, 38, NA, 7, NA, 20)
)
class(output_long) <- c("h2h_long", class(tibble::tibble()))

output_mat <- matrix(
  c(  4,  5.5,   1,
    5.5, 19/3,  NA,
    2.5,   NA, 2.5),
  nrow = 3, dimnames = list(c("1", "2", NA), c("1", "2", NA)),
  byrow = TRUE
)

matrix_class <- class(matrix(1:2, nrow = 1))


# h2h_long ----------------------------------------------------------------
test_that("h2h_long works", {
  output_1 <- h2h_long(
    cr_data,
    mean_score1 = mean(score1), sum_score = sum(score1 + score2)
  )

  expect_is(output_1, "h2h_long")
  expect_equal(output_1, output_long)

  output_2 <- h2h_long(cr_data)
  output_ref_2 <- output_long[, 1:2]

  expect_is(output_2, "h2h_long")
  expect_equal(output_2, output_ref_2)
})

test_that("h2h_long handles `player` as factor", {
  input <- cr_data
  input$player <- factor(input$player, levels = c(1, 2, 3))

  output <- h2h_long(input, sum_score = sum(score1 + score2))
  output_ref <- tibble::tibble(
    player1 = factor(rep(c(1, 2, 3), each = 3), levels = c(1, 2, 3)),
    player2 = factor(rep(c(1, 2, 3), times = 3), levels = c(1, 2, 3)),
    sum_score = c(24, 22, NA, 22, 38, rep(NA, 4))
  )
  class(output_ref) <- c("h2h_long", class(tibble::tibble()))

  expect_is(output, "h2h_long")
  expect_equal(output, output_ref)
})

test_that("h2h_long handles unnamed Head-to-Head functions", {
  output <- h2h_long(cr_data, sum(score1))
  output_ref <- h2h_long(cr_data, sum_score1 = sum(score1))
  colnames(output_ref) <- c("player1", "player2", "sum(score1)")

  expect_equal(output, output_ref)
})

test_that("h2h_long handles not NULL `fill`", {
  output <- h2h_long(
    cr_data,
    mean_score1 = mean(score1), sum_score = sum(score1 + score2),
    fill = list(mean_score1 = 0, sum_score = -1)
  )
  output_ref <- output_long
  output_ref$mean_score1[c(6, 8)] <- 0
  output_ref$sum_score[c(6, 8)] <- -1

  expect_equal(output, output_ref)
})


# to_h2h_long -------------------------------------------------------------
test_that("to_h2h_long works", {
  output_1 <- output_mat %>% to_h2h_long(value = "mean_score1")
  output_ref_1 <- output_long[, 1:3]
  output_ref_1$player1 <- as.character(output_ref_1$player1)
  output_ref_1$player2 <- as.character(output_ref_1$player2)

  expect_is(output_1, "h2h_long")
  expect_equal(output_1, output_ref_1)

  # `value` can handle different names
  expect_equal(
    output_mat %>% to_h2h_long(value = "new_val") %>% colnames(),
    c("player1", "player2", "new_val")
  )

  # Dropping missing values in `value` column
  output_2 <- output_mat %>% to_h2h_long(value = "mean_score1", drop = TRUE)
  output_ref_2 <- output_ref_1[-c(6, 8), ]

  expect_equal(output_2, output_ref_2)
})


# as_tibble.h2h_long --------------------------------------------------------
test_that("as_tibble.h2h_long removes `h2h_long` class", {
  input <- output_long
  output_ref <- input
  class(output_ref) <- class(tibble::tibble())

  expect_identical(tibble::as_tibble(input), output_ref)
})


# h2h_mat -----------------------------------------------------------------
test_that("h2h_mat works", {
  expect_equal(
    cr_data %>% h2h_mat(mean_score1 = mean(score1)),
    cr_data %>% h2h_mat(mean(score1))
  )

  output_1 <- cr_data %>% h2h_mat(mean_score1 = mean(score1))
  output_ref_1 <- output_mat
  class(output_ref_1) <- c("h2h_mat", matrix_class)

  expect_equal(output_1, output_ref_1)

  output_2 <- cr_data %>% h2h_mat()
  output_ref_2 <- matrix(
    rep(NA, 9), nrow = 3,
    dimnames = list(c("1", "2", NA), c("1", "2", NA)),
    byrow = TRUE
  )
  class(output_ref_2) <- c("h2h_mat", matrix_class)

  expect_is(output_2, "h2h_mat")
  expect_equal(output_2, output_ref_2)
})

test_that("h2h_mat handles `player` as factor", {
  input <- cr_data
  input$player <- factor(input$player, levels = c(1, 2, 3))

  output <- h2h_mat(input, sum(score1 + score2))
  output_ref <- matrix(
    c(24, 22, NA,
      22, 38, NA,
      NA, NA, NA),
    nrow = 3, dimnames = list(c("1", "2", "3"), c("1", "2", "3")),
    byrow = TRUE
  )
  class(output_ref) <- c("h2h_mat", matrix_class)

  expect_equal(output, output_ref)
})

test_that("h2h_mat allows multiple Head-to-Head functions", {
  expect_silent(h2h_mat(cr_data))

  expect_message(
    h2h_mat(cr_data, mean_score1 = mean(score1),
            sum_score = sum(score1 + score2)),
    "mean_score1"
  )

  # Ensure that only first function is evaluated
  capt_output <- capture_error(
    h2h_mat(cr_data, mean_score1 = mean(score1),
            error = stop())
  )
  expect_identical(capt_output, NULL)
})

test_that("h2h_mat handles not NULL `fill`", {
  output <- cr_data %>% h2h_mat(mean_score1 = mean(score1), fill = 0)
  output_ref <- output_mat
  output_ref[cbind(c(3, 2), c(2, 3))] <- 0
  class(output_ref) <- c("h2h_mat", matrix_class)

  expect_equal(output, output_ref)
})


# to_h2h_mat --------------------------------------------------------------
test_that("to_h2h_mat works", {
  output <- to_h2h_mat(output_long, value = "mean_score1")
  output_ref <- output_mat
  class(output_ref) <- c("h2h_mat", matrix_class)

  expect_is(output, "h2h_mat")
  expect_equal(output, output_ref)
})

test_that("to_h2h_mat gives messages", {
  expect_message(to_h2h_mat(output_long), "mean_score1")
  expect_message(to_h2h_mat(output_long[, 1:2]), "dummy")
})

test_that("to_h2h_mat handles not NULL `fill`", {
  output <- output_long[-1, ] %>% to_h2h_mat(fill = 0)
  output_ref <- output_mat
  output_ref[1, 1] <- 0
  class(output_ref) <- c("h2h_mat", matrix_class)

  expect_equal(output, output_ref)
})


# h2h_funs ----------------------------------------------------------------
test_that("h2h_funs can be used with !!!", {
  output <- h2h_long(cr_data, !!!h2h_funs[c("num_wins", "num_wins2")])

  expect_true(tibble::is_tibble(output))
  expect_equal(
    colnames(output),
    c("player1", "player2", "num_wins", "num_wins2")
  )
})


# num_wins ----------------------------------------------------------------
test_that("num_wins works", {
  score_1 <- c(1, NA, 2, 1, 0)
  score_2 <- c(2, 1, 1, 1, 10^(-17))

  expect_equal(num_wins(score_1, score_2), 1)
  expect_equal(num_wins(score_1, score_2, half_for_draw = TRUE), 2)
  expect_equal(num_wins(score_1, score_2, na.rm = FALSE), NA_real_)
})
