#' Long format of competition results
#'
#' Functions for dealing with competition results in long format.
#'
#' @param cr_data Data of competition results (convertible to tabular).
#' @param repair Whether to repair input.
#' @param ... Additional arguments to be passed to or from methods.
#' @param x Object to be converted to [tibble][tibble::tibble].
#'
#' @section Long format of competition results:
#' It is assumed that competition consists from multiple games (matches,
#' comparisons, etc.). One game can consist from __variable__ number of
#' players. Inside a game all players are treated equally.
#' In every game every player has some score: the value of arbitrary nature
#' that fully characterizes player's performance in particular game (in most
#' cases it is some numeric value).
#'
#' `longcr` inherits from `tibble`. Data should have at least three columns with
#' the following names:
#' - `game` - game identifier.
#' - `player` - player identifier.
#' - `score` - score of particular player in particular game.
#'
#' Extra columns are allowed. __Note__ that if object is converted to
#' [widecr], they will be dropped. So it is better to store
#' extra information about "game"-"player" pair as list-column "score" which
#' will stay untouched.
#'
#' @section Repairing:
#' Option `repair = TRUE` (default) in `as_longcr()` means that its result is
#' going to be repaired with following actions:
#' - Detect columns exactly matching "game", "player" or "score". Those are used
#' in the output. If all are detected matched columns are put in the beginning.
#' Other columns are preserved.
#' - If not all columns were exactly matched, detect first columns with names
#' containing "game", "player" or "score" (ignoring case). If there are many
#' matching names for one output name then the first one is used. In case of
#' imperfect match, message is given. All other columns are treated as "extra".
#' - If some legitimate names aren't detected, respective columns are created
#' and filled with `NA_integer_`. Also a message is given.
#' - If in one game some player listed more than once, the first record is
#' taken.
#' - Return the tibble with at least 3 appropriate for `longcr` columns and
#' column names.
#'
#' @details `as_longcr()` is S3 method for converting data to `longcr`. When
#' using __default__ method if `repair` is `TRUE` it also tries to fix possible
#' problems (see "Repairing"). If `repair` is `FALSE` it converts `cr_data` to
#' [tibble][tibble::tibble] and adds `longcr` class to it.
#'
#' When applying `as_longcr()` to proper (check via [is_widecr()] is made)
#' __`widecr`__ object, conversion is made:
#' - If there is column `game` then it is used as game identifier. Else
#' treat every row as separate game data.
#' - Every "player"-"score" pair for every game is converted to separate row
#' with adding the appropriate extra columns.
#' - Result is arranged by `game` and identifier of a "player"-"score" pair
#' (extra symbols after "player" and "score" strings in input column names) in
#' increasing order.
#' - If `repair` is `TRUE` then repair is done.
#'
#' For appropriate __`longcr`__ objects `as_longcr()` returns its input and
#' throws error otherwise.
#'
#' @return `is_longcr()` returns `TRUE` if its argument is appropriate object of
#'   class `longcr`: it should inherit classes `longcr`, `tbl_df` (in other
#'   words, to be [tibble][tibble::tibble]) and have "game", "player", "score"
#'   among column names.
#'
#' `as_longcr()` returns an object of class `longcr`.
#'
#' [as_tibble()][tibble::as_tibble()] applied to `longcr` object drops `longcr`
#' class.
#'
#' @examples # Repairing example
#' cr_data <- data.frame(
#'   playerscoregame_ID = rep(1:5, times = 2),
#'   gameId = rep(1:5, each = 2),
#'   scoreS = 31:40,
#'   scoreSS = 41:50
#' )
#' cr_data_long <- as_longcr(cr_data, repair = TRUE)
#'
#' is_longcr(cr_data_long)
#'
#' as_tibble(cr_data_long)
#'
#' @name longcr
#' @seealso [Wide format][widecr]
NULL

#' @rdname longcr
#' @export
is_longcr <- function(cr_data) {
  inherits(x = cr_data, what = "longcr") &&
    inherits(x = cr_data, what = "tbl_df") &&
    all(c("game", "player", "score") %in% colnames(cr_data))
}

#' @rdname longcr
#' @export
as_longcr <- function(cr_data, repair = TRUE, ...) {
  UseMethod("as_longcr")
}

#' @rdname longcr
#' @export
as_tibble.longcr <- function(x, ...) {
  tibble::as_tibble(remove_class_cond(x, "longcr"), ...)
}

#' @export
as_longcr.default <- function(cr_data, repair = TRUE, ...) {
  res <- tibble::as_tibble(cr_data)
  if (repair) {
    res <- repair_longcr(res, ...)
  }
  res <- add_class(res, "longcr")

  res
}

#' @export
as_longcr.widecr <- function(cr_data, repair = TRUE, ...) {
  if (!is_widecr(cr_data)) {
    stop("Input is not appropriate object of class widecr.", call. = FALSE)
  }

  if (!("game" %in% colnames(cr_data))) {
    cr_data$game <- seq_len(nrow(cr_data))
  }

  column_info <- tibble::tibble(name = colnames(cr_data)) %>%
    tidyr::extract(
      col = .data$name, into = c("group", "pair"),
      regex = "(player|score)([0-9]+)", remove = FALSE
    ) %>%
    arrange(.data$pair, .data$group)

  extra_columns <- column_info %>%
    filter(
      .data$name != "game",
      !(.data$group %in% c("player", "score"))
    ) %>%
    pull("name")

  res <- split(column_info, column_info$pair) %>%
    lapply(function(pair_info) {
      pair_names <- pair_info$name
      names(pair_names) <- pair_info$group

      cr_data %>%
        select(!!!rlang::syms(c("game", pair_names, extra_columns))) %>%
        mutate(..pair = pair_info$pair[1])
    }) %>%
    bind_rows() %>%
    arrange(.data$game, .data[["..pair"]]) %>%
    select(-.data[["..pair"]])

  if (repair) {
    res <- repair_longcr(cr_data = res)
  }
  class(res) <- c("longcr", class(tibble::tibble()))

  res
}

#' @export
as_longcr.longcr <- function(cr_data, repair = TRUE, ...) {
  if (!is_longcr(cr_data)) {
    stop("Input is not appropriate object of class longcr.", call. = FALSE)
  }

  cr_data
}

repair_longcr <- function(cr_data, ...) {
  if (is_longcr(cr_data)) {
    return(cr_data)
  }

  longcr_colnames <- c("game", "player", "score")

  # Exact matching
  exact_pattern <- paste0("^", longcr_colnames, "$", collapse = "|")
  exact_inds <- which(grepl(pattern = exact_pattern, x = colnames(cr_data),
                            ignore.case = TRUE))
  if (length(exact_inds) > 0) {
    res <- cr_data[, exact_inds]
    cr_data <- cr_data[, -exact_inds]
  } else {
    res <- cr_data[, integer(0)]
  }

  # Relaxed matching
  names_cr <- tolower(colnames(cr_data))
  left_longcr_colnames <- setdiff(longcr_colnames, colnames(res))
  longcr_pattern <- paste0(left_longcr_colnames, collapse = "|")

  matched_which <- which(grepl(pattern = longcr_pattern, x = names_cr))
  names_cr_extracted <-
    regexpr(
      pattern = longcr_pattern,
      text = names_cr[matched_which]
    ) %>%
    regmatches(x = names_cr[matched_which])

  matched_inds <- match(x = left_longcr_colnames, table = names_cr_extracted)
  repair_info <-
    data.frame(
      target = left_longcr_colnames,
      original = names(cr_data)[matched_which[matched_inds]],
      stringsAsFactors = FALSE
    )

  assert_used_names(repair_info, prefix = "as_longcr: ")

  res <- res %>%
    bind_cols(
      renamecreate_columns(cr_data, repair_info, fill = NA_integer_)
    ) %>%
    select(.data$game, .data$player, .data$score,
           everything())

  not_dupl_records <- (!duplicated(res[, c("game", "player")])) |
    is.na(res$game) | is.na(res$player)

  res[not_dupl_records, ]
}
