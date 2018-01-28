#' Specific functions for computing Head-to-Head matrices
#'
#' Specific functions for computing Head-to-Head matrices.
#'
#' @param matchup_data Data of matchups described in [Head-to-Head
#'   computation][head-to-head] as input for specific `h2h_fun`.
#' @param half_for_draw Whether to count half points for draws in
#'   `h2h_num_wins()`.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details This is a list of currently implemented `h2h_fun`s:
#'   - `h2h_mean_score_diff()` - computes mean score difference of `player2`
#'   compared to `player1`.
#'   - `h2h_mean_score_diff_pos()` - equivalent to `h2h_mean_score_diff()` but
#'   returns 0 if result is negative.
#'   - `h2h_mean_score()` - computes mean score of `player2`.
#'   - `h2h_sum_score_diff()` - computes sum of score differences of
#'     `player2` compared to `player1`.
#'   - `h2h_sum_score_diff_pos()` - equivalent to `h2h_sum_score_diff()` but
#'   returns 0 if result is negative.
#'   - `h2h_sum_score()` - computes sum of scores of `player2`.
#'   - `h2h_num_wins()` - computes number of matchups `player2` scored __more__
#'   than `player1`. If `half_for_draw` is `TRUE` then it also adds half number
#'   of matchups where they had draw. __Note__ that it excludes matchups of the
#'   players with themselves.
#'   - `h2h_num()` - computes number of matchups.
#'
#' @return All `h2h_*` are implementations of `h2h_fun` and return a
#'   single Head-to-Head value.
#'
#' @examples # Initializing data
#' cr_data <- data.frame(
#'   game = rep(1:5, each = 2),
#'   player = rep(1:5, times = 2),
#'   score = c(31:39, 39)
#' )
#' matchup_data <- get_cr_matchups(cr_data)
#'
#' # Computing Head-to-Head matrices
#' get_h2h(cr_data, h2h_num_wins)
#' get_h2h(cr_data, h2h_num_wins, half_for_draw = TRUE)
#' get_h2h(cr_data, h2h_num_wins, half_for_draw = TRUE, fill = 0)
#'
#' # Computing Head-to-Head values based on matchups
#' h2h_mean_score_diff(matchup_data[2, ])
#' h2h_mean_score_diff(matchup_data[19, ])
#'
#' @seealso [Head-to-Head computation][head-to-head], [Head-to-Head
#'   helpers][head-to-head-helpers]
#' @name head-to-head-functions
NULL

#' @rdname head-to-head-functions
#' @export
h2h_mean_score_diff <- function(matchup_data, ...) {
  mean(matchup_data$score2 - matchup_data$score1)
}

#' @rdname head-to-head-functions
#' @export
h2h_mean_score_diff_pos <- function(matchup_data, ...) {
  max(mean(matchup_data$score2 - matchup_data$score1), 0)
}

#' @rdname head-to-head-functions
#' @export
h2h_mean_score <- function(matchup_data, ...) {
  mean(matchup_data$score2)
}

#' @rdname head-to-head-functions
#' @export
h2h_sum_score_diff <- function(matchup_data, ...) {
  sum(matchup_data$score2 - matchup_data$score1)
}

#' @rdname head-to-head-functions
#' @export
h2h_sum_score_diff_pos <- function(matchup_data, ...) {
  max(sum(matchup_data$score2 - matchup_data$score1), 0)
}

#' @rdname head-to-head-functions
#' @export
h2h_sum_score <- function(matchup_data, ...) {
  sum(matchup_data$score2)
}

#' @rdname head-to-head-functions
#' @export
h2h_num_wins <- function(matchup_data, half_for_draw = FALSE, ...) {
  not_identity <- matchup_data$player1 != matchup_data$player2
  score1 <- matchup_data$score1[not_identity]
  score2 <- matchup_data$score2[not_identity]
  sum(score2 > score1) + half_for_draw * 0.5 * sum(score2 == score1)
}

#' @rdname head-to-head-functions
#' @export
h2h_num <- function(matchup_data, ...) {
  nrow(matchup_data)
}
