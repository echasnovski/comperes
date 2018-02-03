# Compute item summary ----------------------------------------------------
#' Compute item summary
#'
#' Functions for computing item summary, i.e. some summary measurements (of
#' arbitrary nature) of item (one or more columns) present in data frame.
#'
#' @param tbl Data frame.
#' @param item Character vector of columns to group by.
#' @param ... Name-value pairs of summary functions (as in [dplyr::summarise]).
#' @param .args A named list of additional arguments to be added to all function
#'   calls (as in [dplyr::funs]).
#' @param .prefix A string to be added to all summary functions' names.
#'
#' @details Basically, `summarise_item()` performs the following steps:
#' - Group `tbl` by columns stored in `item`.
#' - Apply dplyr's `summarise()`.
#' - Ungroup result.
#' - Convert to [tibble][tibble::tibble].
#'
#' @return Output of `summarise()` as not grouped `tibble`.
#'
#' @examples
#' ncaa2005 %>%
#'   dplyr::mutate(game_type = game %% 2) %>%
#'   summarise_item(c("game_type", "player"), mean_score = mean(score))
#'
#' ncaa2005 %>%
#'   summarise_game(mean_score = mean(score), min_score = min(score))
#'
#' @seealso Common item [summary functions][summary_funs] for competition
#' results.
#'
#' [Join item summary][item-summary-join]
#'
#' @name item-summary
NULL

#' @rdname item-summary
#' @export
summarise_item <- function(tbl, item, ..., .args = list(), .prefix = "") {
  dots <- funs(..., .args = .args)
  names(dots) <- paste0(rep(.prefix, length.out = length(dots)),
                        names(dots))

  tbl %>%
    group_by(!!! syms(item)) %>%
    summarise(!!! dots) %>%
    ungroup() %>%
    as_tibble()
}

#' @rdname item-summary
#' @export
summarise_game <- function(tbl, ..., .args = list(), .prefix = "") {
  summarise_item(tbl, "game", ..., .args = .args, .prefix = .prefix)
}

#' @rdname item-summary
#' @export
summarise_player <- function(tbl, ..., .args = list(), .prefix = "") {
  summarise_item(tbl, "player", ..., .args = .args, .prefix = .prefix)
}

#' @rdname item-summary
#' @export
summarize_item <- summarise_item

#' @rdname item-summary
#' @export
summarize_game <- summarise_game

#' @rdname item-summary
#' @export
summarize_player <- summarise_player


# Common item summary functions -------------------------------------------
#' Common item summary functions
#'
#' List of commonly used functions for summarising competition results.
#'
#' @details `summary_funs` is a named list of [quosures][rlang::quo()]
#'   representing commonly used expressions of summary functions for summarising
#'   competition results with [summarise_item()]. Names of the elements will be
#'   used as summary names. It is designed primarily to be used with [long
#'   format][results-longcr] of competition results. To use them inside
#'   `summarise_item()` use [unquoting][rlang::quasiquotation] mechanism from
#'   rlang package.
#'
#' Currently present functions:
#' - __min_score__ - `min(score)`.
#' - __max_score__ - `max(score)`.
#' - __mean_score__ - `mean(score)`.
#' - __median_score__ - `median(score)`.
#' - __sd_score__ - `sd(score)`.
#' - __sum_score__ - `sum(score)`.
#' - __num_games__ - `length(unique(game))`.
#' - __num_players__ - `length(unique(player))`.
#'
#' __Note__ that it is generally better to subset `summary_funs` using names
#' rather than indices because the order of elements might change in future
#' versions.
#'
#' @examples
#' ncaa2005 %>% summarise_game(!!! summary_funs, .prefix = "game_")
#'
#' @seealso [Compute item summary][item-summary], [Join item
#'   summary][item-summary-join]
#'
#' @export
summary_funs <- list(
  min_score = quo(min(score)),
  max_score = quo(max(score)),
  mean_score = quo(mean(score)),
  median_score = quo(median(score)),
  sd_score = quo(sd(score)),
  sum_score = quo(sum(score)),
  num_games = quo(length(unique(game))),
  num_players = quo(length(unique(player)))
)


# Join item summary -------------------------------------------------------
#' Join item summary
#'
#' Functions for joining summary data to data frame. They perform respective
#' variant of [summarise item functions][item-summary] and then [left
#' join][dplyr::left_join()] to the input its result (by `item` columns).
#'
#' @inheritParams item-summary
#'
#' @return Result of `left_join()` to the input data frame.
#'
#' @examples
#' ncaa2005 %>% join_player_summary(player_mean_score = mean(score))
#'
#' @seealso [Compute item summary][item-summary]
#'
#' Common item [summary functions][summary_funs] for competition results.
#'
#' @name item-summary-join
NULL

#' @rdname item-summary-join
#' @export
join_item_summary <- function(tbl, item, ..., .args = list(),
                              .prefix = "") {
  item_summary <- summarise_item(tbl, item, ..., .args = .args,
                                 .prefix = .prefix)

  left_join(x = tbl, y = item_summary, by = item)
}

#' @rdname item-summary-join
#' @export
join_game_summary <- function(tbl, ..., .args = list(), .prefix = "") {
  join_item_summary(tbl, "game", ..., .args = .args, .prefix = .prefix)
}

#' @rdname item-summary-join
#' @export
join_player_summary <- function(tbl, ..., .args = list(), .prefix = "") {
  join_item_summary(tbl, "player", ..., .args = .args, .prefix = .prefix)
}
