#' Wide format of competition results
#'
#' Functions for dealing with competition results in wide format.
#'
#' @param cr_data Data of competition results (convertible to tabular).
#' @param repair Whether to repair input.
#' @param ... Additional arguments to be passed to or from methods.
#' @param x Object to be converted to [tibble][tibble::tibble].
#'
#' @section Wide format of competition results:
#' It is assumed that competition consists from multiple games (matches,
#' comparisons, etc.). One game can consist only from __constant__ number
#' of players. Inside a game all players are treated equally.
#' In every game every player has some score: the value of arbitrary nature
#' that fully characterizes player's performance in particular game (in most
#' cases it is some numeric value).
#'
#' `widecr` inherits from `tibble`. Data should be organized in pairs of columns
#' "player"-"score". Identifier of a pair should go after respective keyword and
#' consist only from digits. For example: player1, score1, player2, score2.
#' Order doesn't matter. Extra columns are allowed.
#'
#' To account for R standard string ordering, identifiers of pairs should be
#' formatted with leading zeros (when appropriate). For example: player01,
#' score01, ..., player10, score10.
#'
#' Column `game` for game identifier is optional. If present it will be used in
#' conversion to `longcr` format via [as_longcr()].
#'
#' @section Repairing:
#' Option `repair = TRUE` (default) in `as_widecr()` means that its result is
#' going to be repaired with following actions:
#' - Detect columns with names containing "player" or "score" (ignoring case).
#' All other columns are treated as "extra".
#' - Extract first occurrence of "player" or "score" (ignoring case) from names
#' of detected columns. Everything after extracted word is treated as identifier
#' of "player"-"score" pair.
#' - Convert these identifiers to numeric form with
#' `as.integer(as.factor(...))`.
#' - Convert identifiers once again to character form with possible leading
#' zeros (to account for R standard string ordering).
#' - Spread pairs to appropriate columns with possible column adding (which
#' were missed in original pairs based on information of pair identifier) with
#' `NA_integer_`.
#' - __Note__ that if there is column `game` (exactly matched) it is placed as
#' first column.
#' __Note__ that the order (and numeration) of pairs can change.
#'
#' @details `as_widecr()` is S3 method for converting data to `widecr`. When
#'   using __default__ method if `repair` is `TRUE` it also tries to fix
#' possible problems (see "Repairing"). If `repair` is `FALSE` it converts
#' `cr_data` to [tibble][tibble::tibble] and adds `widecr` class to it.
#'
#' When applying `as_widecr()` to proper (check via [is_longcr()] is made)
#' __`longcr`__ object, conversion is made:
#' - All columns except "game", "player" and "score" are dropped.
#' - Conversion from long to wide format is made. The number of "player"-"score"
#' pairs is taken as the maximum number of players in game. If not all games are
#' played between the same number of players then there will be `NA`'s in some
#' pairs. Column `game` is preserved in output and is used for arranging in
#' increasing order.
#'
#' For appropriate __`widecr`__ objects `as_widecr` returns its input  and
#' throws error otherwise.
#'
#' @return `is_widecr()` returns `TRUE` if its argument is appropriate object
#' of class `widecr`: it should inherit classes `widecr`, `tbl_df` (in other
#' words, to be [tibble][tibble::tibble]) and have complete pairs of
#' "player"-"score" columns where pair is detected by __digits__ after strings
#' "player" and "score" respectively. Columns of "player" and "score" types
#' shouldn't have any extra symbols except type name and digits after it. All
#' other columns are considered as "extra columns".
#'
#' `as_widecr()` returns an object of class `widecr`.
#'
#' [as_tibble()][tibble::as_tibble()] applied to `widecr` object drops `widecr`
#' class.
#'
#' @examples
#' cr_data <- data.frame(
#'   playerA = 1:10,
#'   playerB = 2:11,
#'   scoreC = 11:20,
#'   scoreB = 12:21,
#'   otherColumn =  101:110
#' )
#' cr_data_wide <- as_widecr(cr_data, repair = TRUE)
#'
#' is_widecr(cr_data_wide)
#'
#' as_tibble(cr_data_wide)
#'
#' @name widecr
#' @seealso [Long format][longcr]
NULL

#' @rdname widecr
#' @export
is_widecr <- function(cr_data) {
  if (!(inherits(x = cr_data, what = "tbl_df"))) {
    return(FALSE)
  }
  names_cr <- tolower(colnames(cr_data))
  names_df <- tibble::tibble(
    name = names_cr[grepl("^(player|score)[0-9]+$", x = names_cr)]
  )

  if (nrow(names_df) == 0) {
    return(FALSE)
  }

  names_df <- names_df %>%
    tidyr::extract(
      col = .data$name, into = c("group", "id"),
      regex = ".*(player|score)([0-9]+)",
      remove = TRUE
    ) %>%
    mutate(
      group = factor(.data$group, levels = c("player", "score")),
      id = factor(.data$id),
      name = interaction(.data$group, .data$id, sep = "")
    )

  inherits(x = cr_data, what = "widecr") &&
    setequal(
      unique(as.character(names_df$name)),
      levels(names_df$name)
    )
}


#' @rdname widecr
#' @export
as_widecr <- function(cr_data, repair = TRUE, ...) {
  UseMethod("as_widecr")
}

#' @rdname widecr
#' @export
as_tibble.widecr <- function(x, ...) {
  tibble::as_tibble(remove_class_cond(x, "widecr"), ...)
}

#' @export
as_widecr.default <- function(cr_data, repair = TRUE, ...) {
  res <- tibble::as_tibble(cr_data)
  if (repair) {
    res <- repair_widecr(res, ...)
  }
  res <- add_class(res, "widecr")

  res
}

#' @export
as_widecr.longcr <- function(cr_data, repair = TRUE, ...) {
  if (!is_longcr(cr_data)) {
    stop("Input is not appropriate object of class longcr.", call. = FALSE)
  }

  res <- cr_data %>%
    select(.data$game, .data$player, .data$score) %>%
    group_by(.data$game) %>%
    mutate(in_game_id = seq_len(dplyr::n())) %>%
    ungroup() %>%
    mutate(
      in_game_id = formatC(
        .data$in_game_id, width = get_formatC_width(.data$in_game_id),
        format = "d", flag = "0"
      )
    )

  res <- split(res, res$in_game_id) %>%
    lapply(function(game_data) {
      pair_id <- game_data$in_game_id[1]
      player_name <- paste0("player", pair_id)
      score_name <- paste0("score", pair_id)

      game_data %>%
        rename(
          !!player_name := .data$player,
          !!score_name := .data$score
        ) %>%
        select(-.data$in_game_id)
    }) %>%
    reduce_full_join(by = "game") %>%
    arrange(.data$game) %>%
    select(.data$game, everything())

  if (repair) {
    res <- repair_widecr(res)
  }
  class(res) <- c("widecr", class(tibble::tibble()))

  res
}

#' @export
as_widecr.widecr <- function(cr_data, repair = TRUE, ...) {
  if (!is_widecr(cr_data)) {
    stop("Input is not appropriate object of class widecr.", call. = FALSE)
  }

  cr_data
}

repair_widecr <- function(cr_data, ...) {
  if (is_widecr(cr_data)) {
    return(cr_data)
  }

  repair_info <- tibble::tibble(original_lower = tolower(colnames(cr_data))) %>%
    tidyr::extract(
      col = .data$original_lower, into = c("group", "pair"),
      regex = ".*(player|score)(.*)", remove = TRUE
    ) %>%
    mutate(original = colnames(cr_data)) %>%
    filter(.data$group %in% c("player", "score"))

  if (nrow(repair_info) == 0) {
    warning("Neither 'player' nor 'score' columns are detected.")
    return(cr_data)
  }

  repair_info <- repair_info %>%
    mutate(
      group = factor(.data$group, levels = c("player", "score")),
      pair = as.integer(factor(.data$pair))
    ) %>%
    tidyr::complete(!!!rlang::syms(c("group", "pair"))) %>%
    mutate(
      group = as.character(.data$group),
      pair = formatC(.data$pair, width = get_formatC_width(.data$pair),
                     format = "d", flag = "0")
    ) %>%
    arrange(.data$pair, .data$group) %>%
    tidyr::unite(col = "target", !!!rlang::syms(c("group", "pair")), sep = "")

  assert_used_names(repair_info, prefix = "as_widecr: ")

  res <- renamecreate_columns(cr_data, repair_info, fill = NA_integer_)

  if ("game" %in% colnames(res)) {
    res <- res %>%
      select(.data$game,
             !!!rlang::syms(repair_info$target),
             everything())
  } else {
    res <- res %>%
      select(!!!rlang::syms(repair_info$target),
             everything())
  }

  res
}
