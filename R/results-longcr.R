#' Long format of competition results
#'
#' Functions for converting data of competition results to long format.
#'
#' @param cr_data Data of competition results (convertable to tabular).
#' @param repair Whether to repair input.
#' @param ... Additional arguments to be passed to or from methods.
#' @param x An object to print.
#'
#' @section Long format of competition results:
#' It is assumed that competition consists from multiple games (matches,
#' comparisons, etc.). One game can consist from __variable__ number of
#' players. Inside a game all players are treated equally.
#' In every game every player has some score: the value of arbitrary nature
#' that fully characterizes player's performance in particular game (in most
#' cases it is some numeric value).
#'
#' `longcr` inherits from [tibble][tibble::tibble]. Data should have at least
#' three columns with the following names:
#' - `game` - game identifier.
#' - `player` - player identifier.
#' - `score` - score of particular player in particular game.
#'
#' Extra columns are allowed. __Note__ that if object is converted to
#' [widecr][results-widecr], they will be dropped. So it is better to store
#' extra information about "game"-"player" pair as list-column "score" which
#' will stay untouched.
#'
#' @details `to_longcr()` is S3 method for converting data to `longcr`. When
#'   using __default__ method if `repair` is `TRUE` it also tries to fix
#'   possible problems with the following actions:
#' - Detect first columns with names containing "game", "player" or "score"
#' (ignoring case). If there are many matching names for one output name then
#' the first one is used. In case of imperfect match, message is given.
#' - If some legitimate names aren't detected respective columns are created and
#' filled with `NA_integer_`. Also a message is given.
#' - If in one game some player listed more than once the first record is taken.
#' - Return the tibble with at least 3 appropriate columns and column names.
#'
#' If `repair` is `FALSE` it converts `cr_data` to [tibble][tibble::tibble] and
#' adds `longcr` class to it.
#'
#' When applying `to_longcr()` to __`widecr`__ object, conversion is made:
#' - If there is column `game` then it is used as game identifier. Else treat
#' every row as separate game data.
#' - Every "player"-"score" pair for every game is converted to separate row
#' with adding the appropriate extra columns.
#' - Result is arranged by `game` and `player` in increasing order.
#' - If `repair` is `TRUE` then repair is done as in `to_longcr.default()`.
#'
#' For appropriate __`longcr`__ objects `to_longcr()` returns its input and
#' throws error otherwise.
#'
#' @return `is_longcr()` returns `TRUE` if its argument is appropriate object of
#'   class `longcr`.
#'
#' `to_longcr()` returns an object of class `longcr`.
#'
#' @examples # Repairing example
#' cr_data <- data.frame(
#'   playerscoregame_ID = rep(1:5, times = 2),
#'   gameId = rep(1:5, each = 2),
#'   scoreS = 31:40,
#'   scoreSS = 41:50
#' )
#' cr_data_long <- to_longcr(cr_data, repair = TRUE)
#' is_longcr(cr_data_long)
#'
#' @name results-longcr
#' @seealso [Wide format][results-widecr]
NULL

#' @rdname results-longcr
#' @export
is_longcr <- function(cr_data) {
  (class(cr_data)[1] == "longcr") &&
    (inherits(x = cr_data, what = "tbl_df")) &&
    (length(setdiff(c("game", "player", "score"), colnames(cr_data))) == 0)
}

#' @rdname results-longcr
#' @export
to_longcr <- function(cr_data, repair = TRUE, ...) {
  UseMethod("to_longcr")
}

#' @export
to_longcr.default <- function(cr_data, repair = TRUE, ...) {
  res <- dplyr::as_tibble(cr_data)
  if (repair) {
    res <- repair_longcr(res, ...)
  }
  res <- add_class(res, "longcr")

  res
}

#' @export
to_longcr.widecr <- function(cr_data, repair = TRUE, ...) {
  if (!is_widecr(cr_data)) {
    stop("Input is not appropriate object of class widecr.")
  }

  if (!("game" %in% colnames(cr_data))) {
    cr_data$game <- seq_len(nrow(cr_data))
  }

  column_info <-
    data.frame(
      name = colnames(cr_data),
      stringsAsFactors = FALSE
    ) %>%
    tidyr::extract(
      col = .data$name, into = c("group", "pair"),
      regex = "(player|score)([0-9]+)", remove = FALSE
    ) %>%
    arrange(.data$pair, .data$group)

  extra_columns <- column_info %>%
    filter(.data$name != "game",
      !(.data$group %in% c("player", "score"))
    ) %>%
    "$"("name")

  res <- split(column_info, column_info$pair) %>%
    lapply(function(pair_info) {
      pair_names <- pair_info$name
      names(pair_names) <- pair_info$group

      cr_data %>%
        select(!!! rlang::syms(c("game", pair_names, extra_columns)))
    }) %>%
    bind_rows() %>%
    arrange(.data$game, .data$player)

  if (repair) {
    res <- repair_longcr(cr_data = res)
  }
  class(res) <- c("longcr", class(dplyr::tibble()))

  res
}

#' @export
to_longcr.longcr <- function(cr_data, repair = TRUE, ...) {
  if (!is_longcr(cr_data)) {
    stop("Input is not appropriate object of class longcr.")
  }

  cr_data
}

repair_longcr <- function(cr_data, ...) {
  longcr_colnames <- c("game", "player", "score")
  longcr_pattern <- paste0(longcr_colnames, collapse = "|")

  names_cr <- tolower(colnames(cr_data))
  matched_which <- which(grepl(pattern = longcr_pattern, x = names_cr))
  names_cr_extracted <-
    regexpr(
      pattern = longcr_pattern,
      text = names_cr[matched_which]
    ) %>%
    regmatches(x = names_cr[matched_which])

  matched_inds <- match(x = longcr_colnames, table = names_cr_extracted)
  repair_info <-
    data.frame(
      target = longcr_colnames,
      original = names(cr_data)[matched_which[matched_inds]],
      stringsAsFactors = FALSE
    )

  assert_used_names(repair_info, prefix = "to_longcr: ")

  res <- renamecreate_columns(cr_data, repair_info, fill = NA_integer_) %>%
    select(.data$game, .data$player, .data$score,
           everything())

  not_dupl_records <- (!duplicated(res[, c("game", "player")])) |
    is.na(res$game) | is.na(res$player)

  res[not_dupl_records, ]
}

#' @rdname results-longcr
#' @export
print.longcr <- function(x, ...) {
  cat("# A longcr object:\n")
  class(x) <- class(x)[-1]
  print(x, ...)
}
