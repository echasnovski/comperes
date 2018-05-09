#' Convert between long pair-value data and matrix
#'
#' Functions for conversion between long pair-value data (data frame with
#' columns for pair identifiers and value column) and matrix.
#'
#' @param tbl Data frame with pair-value data.
#' @param row_key String name of column for first key in pair.
#' @param col_key String name of column for second key in pair.
#' @param value String name of column for value (or `NULL` for `long_to_mat()`).
#' @param fill Value to fill for missing pairs.
#' @param silent Use `TRUE` to omit message about guessed value column (see
#'   Details).
#' @param mat Matrix with pair-value data.
#' @param drop Use `TRUE` to drop rows with missing value (see Details).
#'
#' @details Pair-value data is commonly used in description of pairs of objects.
#' Pair is described by two keys (usually integer or character) and value is an
#' object of arbitrary nature.
#'
#' In __long format__ there are at least three columns: for first key in pair,
#' for second key and for value (might be more). In __matrix format__ pair-value
#' data is represented as matrix of values with row names as character
#' representation of first key, column names - second key.
#'
#' `long_to_mat()` works as follows:
#' - Pair identifiers are taken from columns with names `row_key` (to be used as
#' row names) and `col_key` (to be used as column names). Unique identifiers
#' (and future dimension names) are determined with [levels2()]. This is a way
#' to target function on specific set of pairs by using factor columns. __Note__
#' that `NA`s are treated as single unknown key and put on last place (in case
#' of non-factor).
#' - Values are taken from column with name `value`. __Note__ that if `value`
#' has length 0 (typically `NULL`) then `long_to_mat()` will take first
#' non-key column. If there is no such column, it will use vector of dummy
#' values (`NA`s or `fill`s). In both cases a message is given if `silent =
#' FALSE`.
#' - Output is a matrix with described row and column names. Value of pair
#' "key_1" and "key_2" is stored at intersection of row "key_1" and "key_2".
#' __Note__ that in case of duplicated pairs the value from first occurrence is
#' taken.
#'
#' `mat_to_long()` basically performs inverse operation to `long_to_mat()` but
#' pair identifiers are always character. If `drop = TRUE` it drops rows with
#' values (but not keys) being missing.
#'
#' @return `long_to_mat()` returns a matrix with selected values where row names
#'   indicate first key in pair, col names - second.
#'
#' `mat_to_long()` returns a `tibble` with three columns: the
#' one for first key in pair, the one for second, and the one for value.
#'
#' @examples
#' long_data <- data.frame(
#'   key_1 = c("a", "a", "b"),
#'   key_2 = c("c", "d", "c"),
#'   val = 1:3,
#'   stringsAsFactors = FALSE
#' )
#'
#' mat_data <- long_data %>% long_to_mat("key_1", "key_2", "val")
#' print(mat_data)
#'
#' # Converts to tibble
#' mat_data %>% mat_to_long("new_key_1", "new_key_2", "new_val")
#'
#' # Drops rows with valuus missing
#' mat_data %>% mat_to_long("new_key_1", "new_key_2", "new_val", drop = TRUE)
#'
#' @name convert-pair-value
NULL

#' @rdname convert-pair-value
#' @export
long_to_mat <- function(tbl, row_key, col_key, value = NULL,
                        fill = NULL, silent = FALSE) {
  assert_single_string(row_key, col_key)
  row <- tbl[[row_key]]
  col <- tbl[[col_key]]

  if (length(value) == 0) {
    val_name <- tbl %>%
      select(-one_of(row_key, col_key)) %>%
      first_col_name(silent = silent)

    if (identical(val_name, NULL)) {
      mat_elem <- miss_value(NULL, fill)
      val <- rep(mat_elem, nrow(tbl))
    } else {
      val <- tbl[[val_name]]
      mat_elem <- miss_value(class(val), fill)
    }
  } else {
    assert_single_string(value)
    val <- tbl[[value]]
    mat_elem <- miss_value(class(val), fill)
  }

  row_names <- levels2(row, na.last = TRUE)
  col_names <- levels2(col, na.last = TRUE)
  res <- matrix(
    mat_elem, nrow = length(row_names), ncol = length(col_names),
    dimnames = list(row_names, col_names)
  )
  # For repairing in case `val` is list
  res_attrs <- attributes(res)

  # Used to handle NAs
  row_inds <- match(as.character(row), row_names)
  col_inds <- match(as.character(col), col_names)

  # rev() is used to pick first value in case of duplicated pair
  inds_2d <- cbind(rev(row_inds), rev(col_inds))
  used_inds <- stats::complete.cases(inds_2d)
  inds_2d <- inds_2d[used_inds, ]
  res[inds_2d] <- rev(val)[used_inds]
  attributes(res) <- res_attrs

  res
}

#' @rdname convert-pair-value
#' @export
mat_to_long <- function(mat, row_key, col_key, value, drop = FALSE) {
  assert_single_string(row_key, col_key, value)

  rows <- rep(rownames(mat), each = ncol(mat))
  cols <- rep(colnames(mat), times = nrow(mat))
  vals <- c(t(mat))

  res <- tibble::tibble(row = rows, col = cols, val = vals)
  colnames(res) <- c(row_key, col_key, value)

  if (isTRUE(drop)) {
    res <- res %>% tidyr::drop_na(one_of(value))
  }

  res
}
