# General -----------------------------------------------------------------
#' @export
tibble::as_tibble

add_name_prefix <- function(tbl, prefix = "", except = NULL) {
  if (identical(prefix, "") || (length(prefix) == 0)) {
    return(tbl)
  }

  rename_cols <- setdiff(colnames(tbl), except)
  if (length(rename_cols) > 0) {
    rename_list <- as.list(rename_cols)
    names(rename_list) <- paste0(prefix, rename_cols)

    tbl <- tbl %>%
      rename(!!!rename_list)
  }

  tbl
}

first_col_name <- function(tbl, silent = FALSE, target_name = "value") {
  if (ncol(tbl) == 0) {
    res <- NULL
  } else {
    res <- colnames(tbl)[1]
  }

  if (!silent) {
    assert_used_value_col(res, target_name)
  }

  res
}

miss_value <- function(type = NULL, fill = NULL) {
  if (!identical(fill, NULL)) {
    fill
  } else {
    if (identical(type, "list")) {
      list(NULL)
    } else {
      NA
    }
  }
}

#' Levels of vector
#'
#' Extension of [levels()] function. If `levels(x)` is not `NULL`, it is
#' returned. Otherwise, character representation of unique sorted values is
#' returned (with `NA` treated based on `na.last` as in [sort()]).
#'
#' @param x An object of interest.
#' @param na.last Argument for controlling the treatment of `NA`s. See [sort()].
#'
#' @examples
#' fac_vec <- factor(c("a", "b"), levels = c("a", "b", "c"))
#' levels2(fac_vec)
#'
#' levels2(c(10, 1, 2, NA, 11))
#' @export
levels2 <- function(x, na.last = TRUE) {
  if (identical(levels(x), NULL)) {
    as.character(sort(unique(x), na.last = na.last))
  } else {
    as.character(levels(x))
  }
}


# Operations with class ---------------------------------------------------
add_class <- function(x, class_chr) {
  class(x) <- c(class_chr, class(x))

  x
}

add_class_cond <- function(x, class_chr) {
  if (class(x)[1] != class_chr) {
    class(x) <- c(class_chr, class(x))
  }

  x
}

remove_class_cond <- function(x, class_chr) {
  if (class(x)[1] == class_chr) {
    class(x) <- class(x)[-1]
  }

  x
}

reconstruct <- function(new, old) {
  class(new) <- class(old)

  new
}


# Competition results -----------------------------------------------------
get_formatC_width <- function(vec) {
  floor(log10(length(unique(vec)))) + 1
}

renamecreate_columns <- function(df, info, fill = NA_integer_) {
  # info is a data.frame that should consist from two columns:
  # target - names of target columns (which will be repaired into);
  # original - names of original columns (which will be repaired from).
  # If original is NA then new column with corresponded target name is
  # created with values from 'fill'.
  res <- df
  absent_original <- is.na(info$original)
  if (any(absent_original)) {
    res[, info$target[absent_original]] <- rep(list(rep(fill, nrow(df))))
  }
  if (any(!absent_original)) {
    colnames(res)[match(info$original[!absent_original], colnames(res))] <-
      info$target[!absent_original]
  }

  res
}

reduce_full_join <- function(x, by) {
  reduce_f <- function(x, y) {
    full_join(x = x, y = y, by = by)
  }

  Reduce(f = reduce_f, x = x)
}

#' Get matchups from competition results
#'
#' This function powers computing Head-to-Head values (both [long][h2h_long] and
#' [matrix][h2h_mat]).
#'
#' @param cr_data Competition results ready for [as_longcr()].
#'
#' @details `get_matchups()` returns a [tibble][tibble::tibble] of all
#' matchups (pairs of players from one game) __actually present__ in `cr_data`
#' (including matchups of players with themselves).
#' It has following columns:
#' - `game` - game identifier of matchup.
#' - `player1` - identifier of first player in matchup.
#' - `score1` - score of the first player in matchup.
#' - `player2` - identifier of second player in matchup.
#' - `score2` - score of the second player in matchup.
#'
#' __Important notes__:
#' - Matchups are not symmetrical: matchup "player1"-"player2" is considered
#' different from "player2"-"player1" in order to except more advanced, not
#' symmetrical Head-to-Head values.
#' - Missing values in `player` column after conversion to `longcr` are treated
#' as separate players. It allows operating with games where multiple players'
#' identifiers are not known. However, when computing Head-to-Head values they
#' treated as single player.
#'
#' @return A [widecr] for games with two players.
#'
#' @examples
#' get_matchups(ncaa2005)
#' @seealso [Long format][h2h_long] of Head-to-Head values.
#'
#' [Matrix format][h2h_mat] of Head-to-Head values.
#'
#' @export
get_matchups <- function(cr_data) {
  cr <- cr_data %>%
    as_longcr(repair = TRUE) %>%
    select(.data$game, .data$player, .data$score)
  class(cr) <- class(tibble::tibble())

  left_join(x = cr, y = cr, by = "game", suffix = c("1", "2")) %>%
    as_widecr()
}


# Assertions --------------------------------------------------------------
assert_single_string <- function(...) {
  dots <- quos(..., .named = TRUE)

  for (nm in names(dots)) {
    value <- rlang::eval_tidy(dots[[nm]])
    if (!(is.character(value) && length(value) == 1)) {
      stop("`", nm, "` should be a single string.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

assert_used_value_col <- function(x, target_name = "value") {
  if (length(x) == 0) {
    message("No ", target_name, " found. Using dummy vector.")
  } else {
    message("Using ", x, " as ", target_name, ".")
  }

  invisible(TRUE)
}

assert_used_names <- function(info, prefix = "") {
  # info is a data.frame that should consist from two columns:
  # target - names of used columns;
  # original - names of original columns.
  absent_original <- is.na(info$original)

  target <- info$target[!absent_original]
  original <- info$original[!absent_original]
  if (any(!absent_original) && any(target != original)) {
    unmatched <- target != original
    used_names_message <-
      paste0(original[unmatched], " -> ", target[unmatched], collapse = "\n  ")
    message(
      prefix, "Some matched names are not perfectly matched:\n  ",
      used_names_message, "\n"
    )
  }

  if (any(absent_original)) {
    message(
      prefix,
      sprintf(
        "Next columns are not found. Creating with NAs.\n  %s",
        paste0(info$target[absent_original], collapse = ", ")
      ),
      "\n"
    )
  }

  invisible(TRUE)
}
