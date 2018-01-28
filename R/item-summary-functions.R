#' Specific functions for computing item summary
#'
#' This page describes specific functions for computing item summary.
#'
#' @param cr_data Competition results in format ready for [to_longcr()].
#' @param prefix Possible prefix to add to item summary's names.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details Implementations of `summary_fun` should be able to take
#'   `prefix` and `...` as arguments.
#'
#' @return All `summary_fun`s return a named vector of item summaries
#'   (names are given before adding prefix). This is a list of currently
#'   implemented `summary_fun`s:
#'   - `summary_mean_sd_score()` - returns a vector:
#'       - `meanScore` - mean score.
#'       - `sdScore` - standard deviation of scores.
#'   - `summary_min_max_score()` - returns a vector:
#'       - `minScore` - minimum score.
#'       - `maxScore` - maximum score.
#'   - `summary_sum_score()` - returns a vector:
#'       - `sumScore` - sum of all present scores.
#'   - `summary_num_games()` - returns a vector:
#'       - `numGames` - number of unique games.
#'   - `summary_num_players()` - returns a vector:
#'       - `numPlayers` - number of unique players.
#'
#' @examples
#' cr_data <- data.frame(
#'   game = rep(1:20, each = 2),
#'   player = rep(1:10, times = 4),
#'   score = 31:70,
#'   season = rep(1:2, each = 20)
#' )
#'
#' # Computing item summary
#' summary_mean_sd_score(cr_data)
#' summary_min_max_score(cr_data)
#' summary_min_max_score(cr_data, prefix = "total_")
#'
#' @seealso [Item summary computation][item-summary], [Item summary
#'   addition][item-summary-add]
#'
#' @name item-summary-functions
NULL

#' @rdname item-summary-functions
#' @export
summary_mean_sd_score <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- c(mean(cr$score), stats::sd(cr$score))
  names(res) <- paste0(prefix, c("meanScore", "sdScore"))

  res
}

#' @rdname item-summary-functions
#' @export
summary_min_max_score <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- c(min(cr$score), max(cr$score))
  names(res) <- paste0(prefix, c("minScore", "maxScore"))

  res
}

#' @rdname item-summary-functions
#' @export
summary_sum_score <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- sum(cr$score)
  names(res) <- paste0(prefix, c("sumScore"))

  res
}

#' @rdname item-summary-functions
#' @export
summary_num_games <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- length(unique(cr$game))
  names(res) <- paste0(prefix, c("numGames"))

  res
}

#' @rdname item-summary-functions
#' @export
summary_num_players <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- length(unique(cr$player))
  names(res) <- paste0(prefix, c("numPlayers"))

  res
}
