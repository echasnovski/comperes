context("head-to-head")


# Input data --------------------------------------------------------------
cr_data <- data.frame(
  game = rep(1:2, each = 2),
  player = rep(1:2, times = 2),
  score = 31:34,
  scoreSP = 44:41
)


# get_h2h ------------------------------------------------------------
test_that("get_h2h works", {
  output_ref_1 <- matrix(c(0, -1, 1, 0), ncol = 2, dimnames = list(1:2, 1:2))
  class(output_ref_1) <- c("h2h", "matrix")

  output_1 <- get_h2h(
    cr_data = cr_data, h2h_fun = h2h_mean_score_diff,
    players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h
  )

  expect_identical(output_1, output_ref_1)

  output_ref_2 <- output_ref_1 * 2
  output_2 <- get_h2h(
    cr_data = cr_data, h2h_fun = h2h_sum_score_diff,
    players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h
  )

  expect_identical(output_2, output_ref_2)

  expect_silent(get_h2h(
    cr_data = cr_data, h2h_fun = h2h_mean_score_diff,
    players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h,
    extrArg = TRUE
  ))

  # Test to ensure `get_h2h()` works with `h2h_num()` as it is the only current
  # `h2h_*()` which actually treats its first argument as data frame
  output_ref_3 <- matrix(c(2, 2, 2, 2), ncol = 2, dimnames = list(1:2, 1:2))
  class(output_ref_3) <- c("h2h", "matrix")

  output_3 <- get_h2h(
    cr_data = cr_data, h2h_fun = h2h_num,
    players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h
  )

  expect_identical(output_3, output_ref_3)
})

test_that("get_h2h works when 'transpose' is TRUE", {
  output_ref <- matrix(c(0, 1, -1, 0), ncol = 2, dimnames = list(1:2, 1:2))
  class(output_ref) <- c("h2h", "matrix")

  output <- get_h2h(
    cr_data = cr_data, h2h_fun = h2h_mean_score_diff,
    players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h, transpose = TRUE
  )

  expect_identical(output, output_ref)
})

test_that("get_h2h works with not NULL 'self_play'", {
  output_ref <- matrix(c(1, -1, 1, 2), ncol = 2, dimnames = list(1:2, 1:2))
  class(output_ref) <- c("h2h", "matrix")

  output <- get_h2h(
    cr_data = cr_data, h2h_fun = h2h_mean_score_diff,
    players = NULL, absent_players = players_drop,
    absent_h2h = fill_h2h, self_play = 1:2
  )

  expect_identical(output, output_ref)
})
