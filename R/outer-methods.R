# To ensure `filter` from `dplyr` (and not from `stats`)
#' @export
dplyr::filter

# Methods for longcr ------------------------------------------------------
#' @export
select.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
rename.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
mutate.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
summarise.longcr <- function(.data, ...) {
  remove_class_cond(NextMethod(), "longcr")
}

#' @export
group_by.longcr <- function(.data, ..., add = FALSE) {
  add_class_cond(NextMethod(), "longcr")
}

#' @export
ungroup.longcr <- function(x, ...) {
  add_class_cond(NextMethod(), "longcr")
}

#' @export
distinct.longcr <- function(.data, ..., .keep_all = FALSE) {
  reconstruct(NextMethod(), .data)
}

#' @export
do.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
arrange.longcr <- function(.data, ..., .by_group = FALSE) {
  reconstruct(NextMethod(), .data)
}

#' @export
filter.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
slice.longcr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
inner_join.longcr <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
left_join.longcr <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
right_join.longcr <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
full_join.longcr <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
semi_join.longcr <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
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
#' @export
select.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
rename.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
mutate.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
summarise.widecr <- function(.data, ...) {
  remove_class_cond(NextMethod(), "widecr")
}

#' @export
group_by.widecr <- function(.data, ..., add = FALSE) {
  add_class_cond(NextMethod(), "widecr")
}

#' @export
ungroup.widecr <- function(x, ...) {
  add_class_cond(NextMethod(), "widecr")
}

#' @export
distinct.widecr <- function(.data, ..., .keep_all = FALSE) {
  reconstruct(NextMethod(), .data)
}

#' @export
do.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
arrange.widecr <- function(.data, ..., .by_group = FALSE) {
  reconstruct(NextMethod(), .data)
}

#' @export
filter.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
slice.widecr <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
inner_join.widecr <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
left_join.widecr <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
right_join.widecr <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
full_join.widecr <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
semi_join.widecr <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
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
#' @export
select.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
rename.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
mutate.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
summarise.h2h_long <- function(.data, ...) {
  remove_class_cond(NextMethod(), "h2h_long")
}

#' @export
group_by.h2h_long <- function(.data, ..., add = FALSE) {
  add_class_cond(NextMethod(), "h2h_long")
}

#' @export
ungroup.h2h_long <- function(x, ...) {
  add_class_cond(NextMethod(), "h2h_long")
}

#' @export
distinct.h2h_long <- function(.data, ..., .keep_all = FALSE) {
  reconstruct(NextMethod(), .data)
}

#' @export
do.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
arrange.h2h_long <- function(.data, ..., .by_group = FALSE) {
  reconstruct(NextMethod(), .data)
}

#' @export
filter.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
slice.h2h_long <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
inner_join.h2h_long <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
left_join.h2h_long <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
right_join.h2h_long <- function(x, y, by = NULL, copy = FALSE,
                                suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
full_join.h2h_long <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
semi_join.h2h_long <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
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
