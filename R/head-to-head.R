#' Compute Head-to-Head matrix
#'
#' This page describes methods of computing Head-to-Head matrices.
#'
#' @param cr_data Competition results in format ready for [to_longcr()].
#' @param h2h_fun Head-to-Head function (see Details).
#' @param players Vector of players for which Head-to-Head is computed.
#' @param absent_players Function which performs actions on Head-to-Head matrix
#'   dealing with players which absent in `cr_data`.
#' @param absent_h2h Function which performs actions on Head-to-Head matrix
#'   dealing with absent Head-to-Head records for some pairs of players.
#' @param transpose Whether to transpose Head-to-Head matrix.
#' @param self_play Values for self-play matchups.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details Head-to-Head value is a measure of a quality of direct
#' confrontation between two players. It is assumed that this value can be
#' computed based only on the players' scores in their common games. If it is
#' not true for some case then competition results should be changed by
#' transformation or addition of more information (in form of extra columns or
#' extra field in `score` column(s) making list-column(s)).
#'
#' `get_h2h()` performs computation of Head-to-Head matrix: square matrix with
#' number of rows (and columns) equal to number of players for which it is
#' computed. Head-to-Head values are computed based only on
#' [matchups][head-to-head-helpers] (pairs of players from one game) between
#' players from argument `players`. __Note__ to be careful with Head-to-Head
#' values of players with themselves: it can be inaccurate if `players` is not
#' `NULL` because it will be based on possibly undesirable data.
#'
#' The following algorithm is used:
#' 1. Compute for every present in `cr_data` matchup between players from
#' `players` its Head-to-Head value via `h2h_fun` based on these players'
#' scores in common games. `h2h_fun` should accept two arguments:
#'     - A tibble of [matchups][head-to-head-helpers] data. Structure is like
#'     [widecr][results-widecr] with two players, i.e. with columns `game`,
#'     `player1`, `score1`, `player2`, `score2` (in input of `h2h_fun()` columns
#'     `player1` and `player2` will contain constant values).
#'     - Argument `...` for easier use of `get_h2h()`.
#' Also `h2h_fun` should return a single value.
#' __Note__ that order of the players in matchups matters. So, for example,
#' matchup "player1"-"player2" is considered different from "player2"-"player1"
#' in order to except not symmetrical Head-to-Head values.
#' For absent in `cr_data` matchups `NA_real_`s are produced.
#'
#' 1. Perform actions via `absent_players`. It should, based on Head-to-Head
#' matrix, do something with data of players that have not enough games played.
#' For no actions use [skip_action()]. For other options see [Head-to-Head
#' helpers][head-to-head-helpers].
#'
#' 1. Perform actions via `absent_h2h`. It should do something with those
#' entries of Head-to-Head matrix which are `NA`. For no actions use
#' [skip_action()]. For other options see [Head-to-Head
#' helpers][head-to-head-helpers].
#'
#' 1. If `transpose` is `TRUE` do transposition of Head-to-Head matrix. This
#' option is added to minimize the need in almost duplicated `h2h_fun`s.
#'
#' 1. If `self_play` is not `NULL` replace values on diagonal of Head-to-Head
#' matrix with `self_play`.
#'
#' If argument `players` is `NULL` then Head-to-Head matrix is computed for all
#' present in `cr_data` players. __Note__ that `players` can contain values that
#' are not present in `cr_data`: in this case rows and columns in Head-to-Head
#' matrix for these values will contain only `NA`s (before applying
#' `absent_players`).
#'
#' @return An object of class `h2h` which is a square matrix of Head-to-Head
#'   values. Rows correspond to `player1` and columns to `player2` (as in input
#'   for `h2h_fun`). Row and column names are made with `as.character()` on
#'   players used for computation.
#'
#' @examples
#' set.seed(1002)
#' cr_data <- data.frame(
#'   game = rep(1:5, each = 3),
#'   player = rep(1:5, times = 3),
#'   score = rnorm(15)
#' )
#'
#' # Compute Head-to-Head matrix with mean score difference.
#' get_h2h(
#'   cr_data = cr_data, h2h_fun = h2h_mean_score_diff,
#'   players = NULL, absent_players = players_drop,
#'   absent_h2h = fill_h2h
#' )
#'
#' # Use arguments for post modification
#' get_h2h(
#'   cr_data = cr_data, h2h_fun = h2h_mean_score_diff,
#'   transpose = TRUE
#' )
#' get_h2h(
#'   cr_data = cr_data, h2h_fun = h2h_mean_score_diff,
#'   self_play = 1
#' )
#'
#' @seealso [Head-to-Head functions][head-to-head-functions], [Head-to-Head
#'   helpers][head-to-head-helpers]
#' @name head-to-head
NULL

#' @rdname head-to-head
#' @export
get_h2h <- function(cr_data, h2h_fun, players = NULL,
                    absent_players = players_drop, absent_h2h = fill_h2h,
                    transpose = FALSE, self_play = NULL,
                    ...) {
  cr <- to_longcr(cr_data, ...)
  players <- get_cr_players(cr_data = cr, players = players, ...)

  h2h_long <- get_cr_matchups(cr_data = cr) %>%
    filter(
      .data$player1 %in% players,
      .data$player2 %in% players
    ) %>%
    group_by(.data$player1, .data$player2) %>%
    do(data.frame(h2hVal = h2h_fun(.data, ...))) %>%
    ungroup() %>%
    mutate(
      player1 = factor(.data$player1, levels = players),
      player2 = factor(.data$player2, levels = players)
    ) %>%
    tidyr::complete(!!! rlang::syms(c("player1", "player2"))) %>%
    mutate(
      player1 = as.character(.data$player1),
      player2 = as.character(.data$player2)
    )

  players <- as.character(players)
  res <- matrix(NA_real_, nrow = length(players), ncol = length(players),
                dimnames = list(players, players))
  res[as.matrix(h2h_long[, c("player1", "player2")])] <- h2h_long$h2hVal

  res <- res %>%
    absent_players(...) %>%
    absent_h2h(...)

  class(res) <- c("h2h", "matrix")

  if (!is.null(self_play)) {
    diag(res) <- self_play
  }

  if (transpose) {
    t(res)
  } else {
    res
  }
}
