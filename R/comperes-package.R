#' comperes: Manage Competition Results
#'
#' `comperes` offers a set of tools for storing and managing competition
#' results. __Competition__ is understood as a set of __games__ (abstract event)
#' in which __players__ (abstract entity) gain some abstract __scores__. The
#' most natural example is sport results, however not the only one. For example,
#' product rating can be considered as a competition between products as
#' "players". Here a "game" is a customer that reviews a set of products by
#' rating them with numerical "score" (stars, points, etc.).
#'
#' This package provides the following functionality:
#' - __Store and convert__ competition results:
#'     - In [long format][longcr] as a [tibble][tibble::tibble] with one row per
#'     game-player pair.
#'     - In [wide format][widecr] as a `tibble` with one row per game with fixed
#'     amount of players.
#' - __Summarise__:
#'     - Compute [item summaries][item-summary] with functions using
#'     [dplyr](https://dplyr.tidyverse.org)'s grammar of data manipulation.
#'     - Compute and [join][item-summary-join] item summaries to data for easy
#'     transformation.
#'     - Use [common item summary functions][summary_funs] with
#'     [rlang](https://CRAN.R-project.org/package=rlang)'s
#'     [unquoting][rlang::quasiquotation] mechanism.
#' - __Compute Head-to-Head values__ (a summary statistic of direct
#' confrontation between two players) with functions also using dplyr's grammar:
#'     - Store output in [long format][h2h_long] as a `tibble` with one row per
#'     pair of players.
#'     - Store output in [matrix format][h2h_mat] as a matrix with rows and
#'     columns describing players and entries - Head-to-Head values.
#'     - Use [common Head-to-Head functions][h2h_funs] with rlang's unquoting
#'     mechanism.
#'
#' To learn more about `comperes` browse vignettes with
#' `browseVignettes(package = "comperes")`.
#'
#' @import dplyr
"_PACKAGE"
