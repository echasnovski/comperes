#' Competition results with games between two players
#'
#' Functions for competition results with games between two players.
#'
#' @param cr_data Competition results in format ready for [as_longcr()].
#'
#' @details Pairgames is a term for competition results with games between two
#' players.
#'
#' `to_pairgames()` is a function that converts competition results into
#' pairwise games: it drops games with one player and for every game with 3 and
#' more players this function transforms it into set of separate games between
#' unordered pairs of players. In other words the result is a set of unordered
#' [matchups][get_matchups()] (__as different games__) between different
#' players.
#'
#' __Important notes__:
#' - New game identifiers are integers, order of which respects order of
#' games stored in `cr_data` (based on first occurrence in long format). There
#' is no particular order in subgames of games with 3 and more players.
#' - Order in which players are assigned to `player1` or `player2` column in
#' general shouldn't agree with any order in `cr_data`.
#' - Any column except `game`, `player` and `score` will be dropped after
#' conversion to [longcr].
#' - `NA` and `NaN` in `players` are allowed. They are treated as different
#' players.
#' - `to_pairgames()` is rather compute-intensive and can take much time for
#' competition results with many games.
#'
#' @return `to_pairgames()` returns a competition results of pairwise games as
#' [widecr] object with two players.
#'
#' `is_pairgames()` returns a boolean value of whether `cr_data` contains only
#' games between two players.
#'
#' @examples
#' cr_data <- data.frame(
#'   game = c(rep(1:5, each = 3), 6),
#'   player = c(rep(1:5, times = 3), 1),
#'   score = 101:116,
#'   extraCol = -(1:16)
#' )
#'
#' to_pairgames(cr_data)
#'
#' # Missing values
#' cr_data_na <- data.frame(
#'   game = rep(1L, 3),
#'   player = c(1, NA, NA),
#'   score = 1:3
#' )
#' to_pairgames(cr_data_na)
#'
#' # Checks
#' is_pairgames(cr_data)
#' is_pairgames(to_pairgames(cr_data))
#'
#' @name pairgames
NULL

#' @rdname pairgames
#' @export
to_pairgames <- function(cr_data) {
  cr <- cr_data %>%
    as_longcr(repair = TRUE) %>%
    select(.data$game, .data$player, .data$score)

  multiple_players_games <- cr %>%
    count(.data$game) %>%
    filter(.data$n > 1)

  # In raw pairgames game identifier is formed from 'game' and '..subGame'
  raw_pairgames <- cr %>%
    semi_join(y = multiple_players_games, by = "game") %>%
    tidyr::nest(-.data$game) %>%
    mutate(data = lapply(.data$data, function(game_res) {
      cr_pairs <- utils::combn(nrow(game_res), 2)

      game_res %>%
        slice(c(cr_pairs)) %>%
        mutate(..subGame = rep(seq_len(ncol(cr_pairs)), each = 2))
    })) %>%
    tidyr::unnest(.data$data)

  # Compute new game identifiers
  pairgames_ids <- raw_pairgames %>%
    distinct(.data$game, .data[["..subGame"]]) %>%
    mutate(..pairgameId = 1:n())

  raw_pairgames %>%
    left_join(y = pairgames_ids, by = c("game", "..subGame")) %>%
    select(-.data$game, -.data[["..subGame"]]) %>%
    select(game = .data[["..pairgameId"]], everything()) %>%
    as_longcr(repair = FALSE) %>%
    as_widecr(repair = FALSE)
}


#' @rdname pairgames
#' @export
is_pairgames <- function(cr_data) {
  cr_data %>%
    as_longcr(repair = TRUE) %>%
    count(.data$game) %>%
    summarise(isAllTwo = all(.data$n == 2)) %>%
    "[["("isAllTwo")
}
