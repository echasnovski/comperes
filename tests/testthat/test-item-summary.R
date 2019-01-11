context("item-summary")


# Input data --------------------------------------------------------------
input <- data.frame(
  game = rep(1:20, each = 2),
  player = rep(1:10, times = 4),
  score = 31:70,
  season = rep(1:2, each = 20)
)


# summarise_item ----------------------------------------------------------
test_that("summarise_item works with item of length zero", {
  output <- summarise_item(input, character(0), mean_score = mean(score))
  output_ref <- tibble::tibble(mean_score = 50.5)

  expect_equal(output, output_ref)
})

test_that("summarise_item works with item of length one", {
  output <- summarise_item(
    tbl = input, item = "game",
    mean_score = mean(score), sum_score = sum(score)
  )
  output_ref <- tibble::tibble(
    game = 1:20,
    mean_score = seq(from = 31.5, to = 69.5, by = 2),
    sum_score = 63L + (0:19)*4L
  )

  expect_equal(output, output_ref)
})

test_that("summarise_item works with item length more than one", {
  output <- summarise_item(
    tbl = input, item = c("season", "player"),
    mean_score = mean(score), sum_score = sum(score)
  )
  output_ref <- tibble::tibble(
    season = rep(1:2, each = 10),
    player = rep(1:10, times = 2),
    mean_score = as.numeric(c(36:45, 56:65)),
    sum_score = c(72L + (0:9)*2L, 112L + (0:9)*2L)
  )

  expect_equal(output, output_ref)
})

test_that("summarise_item works with no functions supplied", {
  output_ref <- tibble::tibble(game = 1:20)

  expect_identical(summarise_item(tbl = input, item = "game"), output_ref)
})

test_that("summarise_item works with argument `.prefix`", {
  output <- summarise_item(
    tbl = input, item = "game",
    mean_score = mean(score), sum_score = sum(score),
    # For checking prefix addition to unnamed elements
    NA,
    .prefix = "game_"
  )
  output_ref <- tibble::tibble(
    game = 1:20,
    game_mean_score = seq(from = 31.5, to = 69.5, by = 2),
    game_sum_score = 63L + (0:19)*4L,
    game_NA = rep(NA, 20)
  )

  expect_equal(output, output_ref)

  # Check that it works with zero-length item
  output <- summarise_item(input, character(0), mean_score = mean(score),
                           .prefix = "a_")
  output_ref <- tibble::tibble(a_mean_score = 50.5)
  expect_equal(output, output_ref)
})

test_that("summarise_item throws errors", {
  expect_error(summarise_item(tbl = input, item = 1, mean_score = mean(score)),
               "item.*char")
})


# summarise_game ----------------------------------------------------------
test_that("summarise_game works", {
  expect_equal(summarise_game(input), tibble::tibble(game = 1:20))
})


# summarise_player --------------------------------------------------------
test_that("summarise_player works", {
  expect_equal(summarise_player(input), tibble::tibble(player = 1:10))
})


# summarize_item ----------------------------------------------------------
test_that("summarize_item works", {
  expect_equal(summarize_item(input, "game"), tibble::tibble(game = 1:20))
})


# summarize_game ----------------------------------------------------------
test_that("summarize_game works", {
  expect_equal(summarize_game(input), tibble::tibble(game = 1:20))
})


# summarize_player --------------------------------------------------------
test_that("summarize_player works", {
  expect_equal(summarize_player(input), tibble::tibble(player = 1:10))
})


# summary_funs ------------------------------------------------------------
test_that("summary_funs can be used in summarise_item", {
  output <- summarise_game(input, !!!summary_funs["min_score"])
  output_ref <- summarise_game(input, min_score = min(score))

  expect_identical(output, output_ref)
})


# join_item_summary -------------------------------------------------------
test_that("join_item_summary works", {
  output <- join_item_summary(input, "season", season_mean_score = mean(score))
  output_ref <- input
  output_ref$season_mean_score <- c(rep(40.5, 20), rep(60.5, 20))

  expect_equal(output, output_ref)
})

test_that("join_item_summary works with no functions supplied", {
  expect_identical(input, join_item_summary(input, "game"))
})


# join_game_summary -------------------------------------------------------
test_that("join_game_summary works", {
  output <- join_game_summary(input, game_mean_score = mean(score))
  output_ref <- input
  output_ref$game_mean_score <- rep(seq(from = 31.5, to = 69.5, by = 2),
                                    each = 2)

  expect_equal(output, output_ref)
})


# join_player_summary -----------------------------------------------------
test_that("join_player_summary works", {
  output <- join_player_summary(input, player_mean_score = mean(score))
  output_ref <- input
  output_ref$player_mean_score <- rep(as.numeric(46:55), 4)

  expect_equal(output, output_ref)
})
