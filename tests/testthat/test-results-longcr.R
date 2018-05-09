context("results-longcr")


# Input data --------------------------------------------------------------
input <- data.frame(
  playerscoregame_ID = rep(1:5, times = 2),
  gameId = rep(1:5, each = 2),
  scoreS = 31:40,
  scoreSP = 41:50
)

input_good <- data.frame(
  game = 1:10,
  player = 11:20,
  score = 101:110
)

input_widecr <- tibble::tibble(
  player1 = 11:20,
  score1 = 101:110,
  player2 = 12:21,
  score2 = 102:111,
  game = 2:11,
  otherCol = -(1:10)
)
input_widecr <- add_class(input_widecr, "widecr")


# is_longcr ---------------------------------------------------------------
test_that("is_longcr works", {
  expect_true(is_longcr(as_longcr(input, repair = TRUE)))
})


# as_tibble.longcr --------------------------------------------------------
test_that("as_tibble.longcr removes `longcr` class", {
  input_1 <- input_good
  class(input_1) <- c("longcr", "data.frame")
  output_ref <- tibble::as_tibble(input_good)

  expect_identical(tibble::as_tibble(input_1), output_ref)

  input_2 <- tibble::as_tibble(input_good)
  class(input_2) <- c("longcr", class(tibble::tibble()))

  expect_identical(tibble::as_tibble(input_2), output_ref)
})


# as_longcr.default -------------------------------------------------------
test_that("as_longcr.default handles simple repairing", {
  output_ref_1 <- tibble::tibble(
    game = input$gameId,
    player = input$playerscoregame_ID,
    score = input$scoreS,
    scoreSP = input$scoreSP
  )
  output_ref_1 <- add_class(output_ref_1, "longcr")

  expect_identical(as_longcr(input, repair = TRUE), output_ref_1)
  expect_identical(as_longcr(unclass(input), repair = TRUE), output_ref_1)
})

test_that("as_longcr.default makes exact matching first during repairing", {
  input <- data.frame(
    PlayerRS = "a", gameSS = "b", extra = -1,
    score_player = 10, player = 1,
    stringsAsFactors = FALSE
  )
  output <- as_longcr(input, repair = TRUE)
  output_ref <- tibble::tibble(
    game = "b", player = 1, score = 10,
    PlayerRS = "a", extra = -1
  )
  class(output_ref) <- c("longcr", class(tibble::tibble()))

  expect_identical(output, output_ref)
})

test_that("as_longcr.default handles missing columns correctly", {
  output_ref_2 <- tibble::tibble(
    game = input$gameId,
    player = rep(NA_integer_, nrow(input)),
    score = input$scoreS,
    scoreSP = input$scoreSP
  )
  output_ref_2 <- add_class(output_ref_2, "longcr")

  expect_message(as_longcr(input[, -1], repair = TRUE), "not found.*player")
  expect_identical(suppressMessages(as_longcr(input[, -1], repair = TRUE)),
                   output_ref_2)
})

test_that("as_longcr.default works properly on good inputs", {
  output_ref_good <- tibble::as_tibble(input_good)
  output_ref_good <- add_class(output_ref_good, "longcr")

  expect_identical(as_longcr(input_good, repair = TRUE), output_ref_good)
  expect_silent(as_longcr(input_good, repair = TRUE))
})

test_that("as_longcr.default removes duplicated 'game'-'player' pairs", {
  input_good_extra_row <- dplyr::bind_rows(input_good, input_good[10, ])

  expect_identical(as_longcr(input_good_extra_row, repair = TRUE),
                   as_longcr(input_good, repair = TRUE))
})

test_that("as_longcr.default preserves column types", {
  input_types <- input
  output_ref_types <- tibble::tibble(
    game = input$gameId,
    player = input$playerscoregame_ID,
    score = input$scoreS,
    scoreSP = input$scoreSP
  )
  output_ref_types <- add_class(output_ref_types, "longcr")

  input_types1 <- input_types
  output_ref_types1 <- output_ref_types
  input_types1$gameId <- factor(input_types1$gameId)
  output_ref_types1$game <- factor(output_ref_types1$game)
  expect_identical(as_longcr(input_types1, repair = TRUE), output_ref_types1,
                   info = "Factor 'game'")

  input_types2 <- input_types
  output_ref_types2 <- output_ref_types
  input_types2$gameId <- as.character(input_types2$gameId)
  output_ref_types2$game <- as.character(output_ref_types2$game)
  expect_identical(as_longcr(input_types2, repair = TRUE), output_ref_types2,
                   info = "Character 'game'")

  input_types3 <- input_types
  output_ref_types3 <- output_ref_types
  input_types3$playerscoregame_ID  <- factor(input_types3$playerscoregame_ID)
  output_ref_types3$player <- factor(output_ref_types3$player)
  expect_identical(as_longcr(input_types3, repair = TRUE), output_ref_types3,
                   info = "Factor 'player'")

  input_types4 <- input_types
  output_ref_types4 <- output_ref_types
  input_types4$playerscoregame_ID  <-
    as.character(input_types4$playerscoregame_ID)
  output_ref_types4$player <- as.character(output_ref_types4$player)
  expect_identical(as_longcr(input_types4, repair = TRUE), output_ref_types4,
                   info = "Character 'player'")

  input_types5 <- input_types
  output_ref_types5 <- output_ref_types
  input_types5$scoreS <- as.character(input_types5$scoreS)
  output_ref_types5$score <- as.character(output_ref_types5$score)
  expect_identical(as_longcr(input_types5, repair = TRUE), output_ref_types5,
                   info = "Character 'score'")

  input_types6 <- input_types
  output_ref_types6 <- output_ref_types
  list_scores <- lapply(1:10, function(i) {
    c(points = 100 + i, type = i %% 2)
  })
  input_types6$scoreS <- I(list_scores)
  class(input_types6$scoreS) <- NULL
  output_ref_types6$score <- list_scores
  expect_identical(as_longcr(input_types6, repair = TRUE), output_ref_types6,
                   info = "List-column 'score'")
})

test_that("as_longcr.default works without repairing", {
  output_ref_3 <- tibble::as_tibble(input)
  output_ref_3 <- add_class(output_ref_3, "longcr")

  expect_identical(as_longcr(input, repair = FALSE), output_ref_3)
})

test_that("as_longcr.default handles extra arguments", {
  expect_silent(as_longcr(input_good, repair = TRUE, extraArg = 1))
  expect_silent(as_longcr(input_good, repair = FALSE, extraArg = 1))
})


# as_longcr.widecr --------------------------------------------------------
test_that("as_longcr.widecr does simple converting", {
  output_ref <- tibble::tibble(
    game = rep(2:11, each = 2),
    player = c(11L, rep(12:20, each = 2), 21L),
    score = c(101L, rep(102:110, each = 2), 111L),
    otherCol = rep(-(1:10), each = 2)
  )
  output_ref <- add_class(output_ref, "longcr")

  output <- as_longcr(input_widecr, repair = TRUE)

  expect_identical(output, output_ref)
})

test_that("as_longcr.widecr orders by game and pair id during repair", {
  input_widecr_1 <- tibble::tibble(
    player1 = 12:13, score1 = 101:102,
    player2 = 11:12, score2 = 102:103,
    game = 2:3,
    otherCol = -(1:2)
  )
  input_widecr_1 <- add_class(input_widecr_1, "widecr")

  output_ref <- tibble::tibble(
    game = rep(2:3, each = 2),
    player = c(12L, 11L, 13L, 12L),
    score = c(101L, 102L, 102L, 103L),
    otherCol = rep(-(1:2), each = 2)
  )
  output_ref <- add_class(output_ref, "longcr")

  output <- as_longcr(input_widecr_1, repair = TRUE)

  expect_equal(output, output_ref)
})

test_that("as_longcr.widecr preserves column types", {
  input_types <- input_widecr
  output_ref_types <- tibble::tibble(
    game = rep(2:11, each = 2),
    player = c(11L, rep(12:20, each = 2), 21L),
    score = c(101L, rep(102:110, each = 2), 111L),
    otherCol = rep(-(1:10), each = 2)
  )
  output_ref_types <- add_class(output_ref_types, "longcr")

  input_types1 <- input_types
  output_ref_types1 <- output_ref_types
  input_types1$game <- factor(input_types1$game, levels = 2:11)
  output_ref_types1$game <- factor(output_ref_types1$game, levels = 2:11)
  expect_identical(as_longcr(input_types1, repair = TRUE), output_ref_types1,
                   info = "Factor 'game'")

  input_types2 <- input_types
  output_ref_types2 <- output_ref_types
  input_types2$game <- as.character(input_types2$game)
  output_ref_types2$game <- as.character(output_ref_types2$game)
  output_ref_types2 <- output_ref_types2[order(output_ref_types2$game,
                                               output_ref_types2$player), ]
  output_ref_types2 <- add_class_cond(output_ref_types2, "longcr")
  expect_identical(as_longcr(input_types2, repair = TRUE), output_ref_types2,
                   info = "Character 'game'")

  input_types3 <- input_types
  output_ref_types3 <- output_ref_types
  input_types3$player1 <- factor(input_types3$player1, levels = 11:21)
  input_types3$player2 <- factor(input_types3$player2, levels = 11:21)
  output_ref_types3$player <- factor(output_ref_types3$player, levels = 11:21)
  expect_identical(as_longcr(input_types3, repair = TRUE), output_ref_types3,
                   info = "Factor 'player'")

  input_types4 <- input_types
  output_ref_types4 <- output_ref_types
  input_types4$player1 <- as.character(input_types4$player1)
  input_types4$player2 <- as.character(input_types4$player2)
  output_ref_types4$player <- as.character(output_ref_types4$player)
  expect_identical(as_longcr(input_types4, repair = TRUE), output_ref_types4,
                   info = "Character 'player'")

  input_types5 <- input_types
  output_ref_types5 <- output_ref_types
  input_types5$score1 <- as.character(input_types5$score1)
  input_types5$score2 <- as.character(input_types5$score2)
  output_ref_types5$score <- as.character(output_ref_types5$score)
  expect_identical(as_longcr(input_types5, repair = TRUE), output_ref_types5,
                   info = "Character 'score'")

  input_types6 <- input_types
  output_ref_types6 <- output_ref_types
  list_scores <- lapply(1:10, function(i) {
    c(points = 100 + i, type = i %% 2)
  })
  input_types6$score1 <- I(list_scores)
  class(input_types6$score1) <- NULL
  input_types6$score2 <- I(list_scores)
  class(input_types6$score2) <- NULL
  output_ref_types6$score <- I(rep(list_scores, each = 2))
  class(output_ref_types6$score) <- NULL
  expect_identical(as_longcr(input_types6, repair = TRUE), output_ref_types6,
                   info = "List-column 'score'")
})

test_that("as_longcr.widecr removes duplicated 'game'-'player'
          if repair is TRUE", {
  input_dupl <- input_widecr
  input_dupl$player2[1] <- 11L

  output_ref_dupl <- tibble::tibble(
    game = rep(2:11, each = 2),
    player = c(11L, rep(12:20, each = 2), 21L),
    score = c(101L, rep(102:110, each = 2), 111L),
    otherCol = rep(-(1:10), each = 2)
  )
  output_ref_dupl <- output_ref_dupl[-2, ]
  output_ref_dupl <- add_class_cond(output_ref_dupl, "longcr")

  expect_identical(as_longcr(input_dupl, repair = TRUE), output_ref_dupl)
})

test_that("as_longcr.widecr works without column 'game'", {
  output_ref_longcr_from_widecr <- tibble::tibble(
    game = rep(2:11, each = 2),
    player = c(11L, rep(12:20, each = 2), 21L),
    score = c(101L, rep(102:110, each = 2), 111L),
    otherCol = rep(-(1:10), each = 2)
  )
  output_ref_longcr_from_widecr <- add_class(output_ref_longcr_from_widecr,
                                             "longcr")

  input_widecr_nogame <- input_widecr[, setdiff(colnames(input_widecr), "game")]
  input_widecr_nogame <- add_class_cond(input_widecr_nogame, "widecr")

  output_ref_longcr_from_widecr_nogame <- output_ref_longcr_from_widecr
  output_ref_longcr_from_widecr_nogame$game <- rep(1:10, each = 2)

  expect_identical(as_longcr(input_widecr_nogame),
                   output_ref_longcr_from_widecr_nogame)
})

test_that("as_longcr.widecr throws error on corrupted widecr object", {
  input_widecr_corrupt <- input_widecr[, -1]
  input_widecr_corrupt <- add_class_cond(input_widecr_corrupt, "widecr")

  expect_error(as_longcr(input_widecr_corrupt), "not.*widecr")
})


# as_longcr.longcr --------------------------------------------------------
test_that("as_longcr.longcr works", {
  as_longcr_res <- as_longcr(input_good)
  expect_identical(as_longcr(as_longcr_res, repair = TRUE), as_longcr_res)

  class(as_longcr_res) <- "longcr"
  expect_error(as_longcr(as_longcr_res, repair = TRUE), "not.*longcr")
})


# repair_longcr -----------------------------------------------------------
test_that("repair_longcr works", {
  # Most essential tests are provided in as_longcr() methods
  input <- tibble::as_tibble(input_good)
  input <- add_class(input, "longcr")

  expect_identical(repair_longcr(input), input)
})
