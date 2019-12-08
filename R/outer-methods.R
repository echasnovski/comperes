# Methods for longcr ------------------------------------------------------
select.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

rename.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

mutate.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

summarise.longcr <- function(.data, ...) {
  remove_class_cond(NextMethod(), "longcr")
}

group_by.longcr <- function(.data, ..., add = FALSE) {
  add_class_cond(NextMethod(), "longcr")
}

ungroup.longcr <- function(x, ...) {
  add_class_cond(NextMethod(), "longcr")
}

distinct.longcr <- function(.data, ..., .keep_all = FALSE) {
  reconstruct(NextMethod(), .data)
}

do.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

arrange.longcr <- function(.data, ..., .by_group = FALSE) {
  reconstruct(NextMethod(), .data)
}

filter.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

slice.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

inner_join.longcr <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

left_join.longcr <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

right_join.longcr <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

full_join.longcr <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

semi_join.longcr <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

anti_join.longcr <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
`[.longcr` <- function(x, i, j, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
print.longcr <- function(x, ...) {
  cat("# A longcr object:\n")

  NextMethod()
}

# Methods for widecr ------------------------------------------------------
select.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

rename.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

mutate.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

summarise.widecr <- function(.data, ...) {
  remove_class_cond(NextMethod(), "widecr")
}

group_by.widecr <- function(.data, ..., add = FALSE) {
  add_class_cond(NextMethod(), "widecr")
}

ungroup.widecr <- function(x, ...) {
  add_class_cond(NextMethod(), "widecr")
}

distinct.widecr <- function(.data, ..., .keep_all = FALSE) {
  reconstruct(NextMethod(), .data)
}

do.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

arrange.widecr <- function(.data, ..., .by_group = FALSE) {
  reconstruct(NextMethod(), .data)
}

filter.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

slice.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

inner_join.widecr <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

left_join.widecr <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

right_join.widecr <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

full_join.widecr <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

semi_join.widecr <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

anti_join.widecr <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
`[.widecr` <- function(x, i, j, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
print.widecr <- function(x, ...) {
  cat("# A widecr object:\n")

  NextMethod()
}


# Methods for h2h_long ----------------------------------------------------
select.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

rename.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

mutate.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

summarise.h2h_long <- function(.data, ...) {
  remove_class_cond(NextMethod(), "h2h_long")
}

group_by.h2h_long <- function(.data, ..., add = FALSE) {
  add_class_cond(NextMethod(), "h2h_long")
}

ungroup.h2h_long <- function(x, ...) {
  add_class_cond(NextMethod(), "h2h_long")
}

distinct.h2h_long <- function(.data, ..., .keep_all = FALSE) {
  reconstruct(NextMethod(), .data)
}

do.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

arrange.h2h_long <- function(.data, ..., .by_group = FALSE) {
  reconstruct(NextMethod(), .data)
}

filter.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

slice.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

inner_join.h2h_long <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

left_join.h2h_long <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

right_join.h2h_long <- function(x, y, by = NULL, copy = FALSE,
                                suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

full_join.h2h_long <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

semi_join.h2h_long <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

anti_join.h2h_long <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
`[.h2h_long` <- function(x, i, j, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
print.h2h_long <- function(x, ...) {
  cat("# A long format of Head-to-Head values:\n")

  NextMethod()
}


# Methods for h2h_mat -----------------------------------------------------
#' @export
`[.h2h_mat` <- function(x, i, j, ...) {
  res <- NextMethod()

  if (is.matrix(res)) {
    res <- add_class_cond(res, "h2h_mat")
  }

  res
}

#' @export
print.h2h_mat <- function(x, ...) {
  cat("# A matrix format of Head-to-Head values:\n")

  y <- remove_class_cond(x, "h2h_mat")

  print(y)
}
