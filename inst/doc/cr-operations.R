## ----setup, include = FALSE----------------------------------------------
library(dplyr)
library(comperes)

## ----longcr, echo = FALSE------------------------------------------------
print(ncaa2005, n = 6)

## ----widecr, echo = FALSE------------------------------------------------
print(to_widecr(ncaa2005, repair = FALSE), n = 3)

## ----to_pairgames, echo = TRUE-------------------------------------------
cr_data <- data.frame(game = rep(1, 3), player = 11:13, score = 101:103)
to_pairgames(cr_data)

## ----get_h2h, echo = TRUE------------------------------------------------
get_h2h(ncaa2005, h2h_fun = h2h_num_wins)

## ----get_h2h players, echo = TRUE----------------------------------------
get_h2h(ncaa2005, h2h_fun = h2h_num, players = c("Duke", "Miami"))

## ----get_h2h extra players, echo = TRUE----------------------------------
get_h2h(
  ncaa2005,
  h2h_fun = h2h_num,
  players = c("Duke", "Miami", "Extra"),
  absent_players = skip_action
)

# Use extra argument 'fill' to supply value for 'fill_h2h'
get_h2h(
  ncaa2005,
  h2h_fun = h2h_num,
  players = c("Duke", "Miami", "Extra"),
  absent_players = skip_action,
  absent_h2h = fill_h2h, fill = 0
)

## ----get_item_summary, echo = TRUE---------------------------------------
get_item_summary(ncaa2005, item = "player",
                 summary_fun = summary_min_max_score)
get_item_summary(ncaa2005, item = "game",
                 summary_fun = NULL)

## ----get_item_summary wrappers, echo = TRUE, eval = FALSE----------------
#  # The same as previous code
#  get_player_summary(ncaa2005, summary_fun = summary_min_max_score)
#  get_game_summary(ncaa2005, summary_fun = NULL)

## ----get_item_summary multiple item, echo = TRUE-------------------------
ncaa2005 %>%
  mutate(season = rep(1:2, each = 10)) %>%
  get_item_summary(item = c("season", "player"),
                   summary_fun = summary_min_max_score)

## ----add_item_summary, echo = TRUE---------------------------------------
ncaa2005 %>%
  add_item_summary(item = "game",
                   summary_fun = summary_mean_sd_score) %>%
  mutate(score = abs(score - meanScore)) %>%
  print(n = 6)

## ----add_item_summary wrappers, echo = TRUE, eval = FALSE----------------
#  # The same as previous example
#  ncaa2005 %>%
#    add_game_summary(summary_fun = summary_mean_sd_score) %>%
#    mutate(score = abs(score - meanScore)) %>%
#    print(n = 6)

