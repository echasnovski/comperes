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
  NextMethod()
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
  NextMethod()
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
