#' Compute item summary
#'
#' This page describes functionality for computing item summary, i.e. some
#' summary measurements (of arbitrary nature) of item (one or more columns)
#' present in competition results.
#'
#' @param cr_data Competition results in format ready for [to_longcr()].
#' @param item Character vector of columns to group by.
#' @param summary_fun Function to compute item summary (see Details).
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details Argument `item` defines on which columns grouping is made for
#'   computing item summary. Basically `get_item_summary()` applies
#'   `summary_fun` to groups of `cr_data` defined by `item`.
#'
#'   `summary_fun` is a function that takes competition results of a particular
#'   item (game, player, their combination, etc.) and produces named vector of
#'   item summary. Also it should take `prefix` and `...` as argument for easier
#'   use. See [Item summary functions][item-summary-functions] for more details.
#'
#'   One can control the names of the summaries by adding prefix stored in
#'   `prefix` as extra argument.
#'
#' @return If `summary_fun` is `NULL` then `get_item_summary()` returns a
#'   [tibble][tibble::tibble] with columns named as stored in argument `item`
#'   and which has all unique values of particular item in `cr_data`. If not
#'   `NULL` then there will be extra columns for every summary value that
#'   `summary_fun` produces.
#'
#'   `get_game_summary()` and `get_player_summary()` are wrappers for
#'   `get_item_summary()` with `item` equals to `"game"` and `"player"`
#'   accordingly.
#'
#' @examples
#' cr_data <- data.frame(
#'   game = rep(1:20, each = 2),
#'   player = rep(1:10, times = 4),
#'   score = 31:70,
#'   season = rep(1:2, each = 20)
#' )
#'
#' # Computing summaries.
#' get_game_summary(cr_data = cr_data, summary_fun = summary_min_max_score)
#' get_player_summary(cr_data = cr_data, summary_fun = summary_min_max_score)
#' get_item_summary(
#'   cr_data = cr_data, item = c("season", "player"),
#'   summary_fun = summary_min_max_score
#' )
#'
#' # Varying prefix of the summary columns.
#' get_item_summary(
#'   cr_data = cr_data, item = c("season", "player"),
#'   summary_fun = summary_mean_sd_score, prefix = "seasonPlayer_"
#' )
#'
#' @seealso [Item summary functions][item-summary-functions], [Item summary
#'   addition][item-summary-add]
#'
#' @name item-summary
NULL

#' @rdname item-summary
#' @export
get_item_summary <- function(cr_data, item, summary_fun = NULL, ...) {
  cr <- cr_data %>%
    to_longcr(repair = TRUE)

  if (is.null(summary_fun)) {
    res <- distinct(cr, rlang::UQS(rlang::syms(item)))
  } else {
    res <- cr %>%
      group_by(rlang::UQS(rlang::syms(item))) %>%
      do(dplyr::as_tibble(as.list(summary_fun(.data, ...)))) %>%
      ungroup()
  }

  class(res) <- class(dplyr::tibble())

  res
}

#' @rdname item-summary
#' @export
get_game_summary <- function(cr_data, summary_fun = NULL, ...) {
  get_item_summary(cr_data = cr_data,
                   item = "game",
                   summary_fun = summary_fun,
                   ...)
}

#' @rdname item-summary
#' @export
get_player_summary <- function(cr_data, summary_fun = NULL, ...) {
  get_item_summary(cr_data = cr_data,
                   item = "player",
                   summary_fun = summary_fun,
                   ...)
}
