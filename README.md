
<!-- README.md is generated from README.Rmd. Please edit that file -->
comperes: Manage Competition Results
====================================

[![Build Status](https://travis-ci.org/echasnovski/comperes.svg?branch=master)](https://travis-ci.org/echasnovski/comperes) [![codecov](https://codecov.io/gh/echasnovski/comperes/branch/master/graph/badge.svg)](https://codecov.io/gh/echasnovski/comperes)

`comperes` offers a pipe (`%>%`) friendly set of tools for storing and managing competition results. Understanding of **competition** is quite general: it is a set of **games** (abstract event) in which **players** (abstract entity) gain some abstract **scores** (typically numeric). The most natural example is sport results, however not the only one. For example, product rating can be considered as a competition between products as "players". Here a "game" is a customer that reviews a set of products by rating them with numerical "score" (stars, points, etc.).

This package leverages [dplyr](http://dplyr.tidyverse.org)'s grammar of data manipulation. Only basic knowledge is enough to use `comperes`.

Overview
--------

`comperes` provides the following functionality:

-   **Store and convert** competition results:
    -   In *long format* as a [tibble](http://tibble.tidyverse.org) with one row per game-player pair. Functions: `as_longcr()`, `is_longcr()`.
    -   In *wide format* as a `tibble` with one row per game with fixed amount of players. Functions: `as_widecr()`, `is_widecr()`.
-   **Summarise**:
    -   Compute *item summaries* with functions using `dplyr`'s grammar. Functions: `summarise_item()`, `summarise_game()`, `summarise_player()`.
    -   Compute and *join* item summaries to data for easy transformation. Functions: `join_item_summary()`, `join_game_summary()`, `join_player_summary()`.
    -   Use *common item summary functions* with [rlang](http://rlang.tidyverse.org/)'s [unquoting](http://rlang.r-lib.org/reference/quasiquotation.html) mechanism. Example: `. %>% summarise_player(!!!summary_funs["mean_score"])`.
-   **Compute Head-to-Head values** (a summary statistic of direct confrontation between two players) with functions also using `dplyr`'s grammar:
    -   Store output in *long format* as a `tibble` with one row per pair of players. Function: `h2h_long()`.
    -   Store output in *matrix format* as a matrix with rows and columns describing players and entries - Head-to-Head values. Function: `h2h_mat()`.
    -   Use *common Head-to-Head functions* with rlang's unquoting mechanism. Example: `. %>% h2h_mat(!!!h2h_funs["num_wins"])`.

Installation
------------

You can install `comperes` from CRAN with:

``` r
install.packages("comperes")
```

To install the most recent development version from GitHub use:

``` r
# install.packages("devtools")
devtools::install_github("echasnovski/comperes")
```

Examples
--------

### Store and Convert

We will be using `ncaa2005`, data from `comperes` package. It is an example competition results (hereafter - results) of an isolated group of Atlantic Coast Conference teams provided in book ["Who's \#1"](https://www.amazon.com/Whos-1-Science-Rating-Ranking/dp/069116231X) by Langville and Meyer. It looks like this:

``` r
library(comperes)
ncaa2005
#> # A longcr object:
#> # A tibble: 20 x 3
#>    game player score
#>   <int> <chr>  <int>
#> 1     1 Duke       7
#> 2     1 Miami     52
#> 3     2 Duke      21
#> 4     2 UNC       24
#> 5     3 Duke       7
#> 6     3 UVA       38
#> # ... with 14 more rows
```

This is an object of class `longcr` which describes results in long form (*each row represents the score of particular player in particular game*). Because in this competition a game always consists from two players, more natural way to look at `ncaa2005` is in wide format:

``` r
as_widecr(ncaa2005)
#> # A widecr object:
#> # A tibble: 10 x 5
#>    game player1 score1 player2 score2
#>   <int> <chr>    <int> <chr>    <int>
#> 1     1 Duke         7 Miami       52
#> 2     2 Duke        21 UNC         24
#> 3     3 Duke         7 UVA         38
#> 4     4 Duke         0 VT          45
#> 5     5 Miami       34 UNC         16
#> 6     6 Miami       25 UVA         17
#> # ... with 4 more rows
```

This converted `ncaa2005` into an object of `widecr` class which describes results in wide format (*each row represents scores of all players in particular game*). All `comperes` functions expect either a data frame with results structured in long format or one of supported classes: `longcr`, `widecr`.

### Summarise

With `compere` the following summaries are possible:

``` r
ncaa2005 %>%
  summarise_player(min_score = min(score), mean_score = mean(score))
#> # A tibble: 5 x 3
#>   player min_score mean_score
#>   <chr>      <dbl>      <dbl>
#> 1 Duke           0       8.75
#> 2 Miami         25      34.5 
#> 3 UNC            3      12.5 
#> 4 UVA            5      18.5 
#> 5 VT             7      33.5

# Using list of common summary functions
library(rlang)
ncaa2005 %>%
  summarise_game(!!!summary_funs[c("sum_score", "num_players")])
#> # A tibble: 10 x 3
#>    game sum_score num_players
#>   <int>     <int>       <int>
#> 1     1        59           2
#> 2     2        45           2
#> 3     3        45           2
#> 4     4        45           2
#> 5     5        50           2
#> 6     6        42           2
#> # ... with 4 more rows
```

Supplied list of common summary functions has 8 entries, which are quoted expressions to be used in `dplyr` grammar:

``` r
summary_funs
#> $min_score
#> min(score)
#> 
#> $max_score
#> max(score)
#> 
#> $mean_score
#> mean(score)
#> 
#> $median_score
#> median(score)
#> 
#> $sd_score
#> sd(score)
#> 
#> $sum_score
#> sum(score)
#> 
#> $num_games
#> length(unique(game))
#> 
#> $num_players
#> length(unique(player))

ncaa2005 %>% summarise_player(!!!summary_funs)
#> # A tibble: 5 x 9
#>   player min_score max_score mean_score median_score sd_score sum_score
#>   <chr>      <dbl>     <dbl>      <dbl>        <dbl>    <dbl>     <int>
#> 1 Duke           0        21       8.75          7       8.81        35
#> 2 Miami         25        52      34.5          30.5    12.3        138
#> 3 UNC            3        24      12.5          11.5     9.40        50
#> 4 UVA            5        38      18.5          15.5    14.0         74
#> 5 VT             7        52      33.5          37.5    19.9        134
#> # ... with 2 more variables: num_games <int>, num_players <int>
```

To modify scores based on the rest of results one can use `join_*_summary()` functions:

``` r
suppressPackageStartupMessages(library(dplyr))
ncaa2005_mod <- ncaa2005 %>%
  join_player_summary(player_mean_score = mean(score)) %>%
  join_game_summary(game_mean_score = mean(score)) %>%
  mutate(score = player_mean_score - game_mean_score)

ncaa2005_mod
#> # A longcr object:
#> # A tibble: 20 x 5
#>    game player score player_mean_score game_mean_score
#>   <int> <chr>  <dbl>             <dbl>           <dbl>
#> 1     1 Duke   -20.8              8.75            29.5
#> 2     1 Miami    5               34.5             29.5
#> 3     2 Duke   -13.8              8.75            22.5
#> 4     2 UNC    -10               12.5             22.5
#> 5     3 Duke   -13.8              8.75            22.5
#> 6     3 UVA     -4               18.5             22.5
#> # ... with 14 more rows

ncaa2005_mod %>% summarise_player(mean_score = mean(score))
#> # A tibble: 5 x 2
#>   player mean_score
#>   <chr>       <dbl>
#> 1 Duke       -15.5 
#> 2 Miami       11.4 
#> 3 UNC         -5   
#> 4 UVA         -2.12
#> 5 VT          11.2
```

This code modifies `score` to be average player score minus average game score. Negative values indicate poor game performance.

### Head-to-Head

Computation of Head-to-Head performance is done with `h2h_long()` (output is a tibble; allows multiple Head-to-Head values per pair of players) or `h2h_mat()` (output is a matrix; only one value per pair of players).

Head-to-Head functions should be supplied in `dplyr` grammar but for players' matchups: direct confrontation between **ordered** pairs of players (including playing with themselves) stored in wide format:

``` r
ncaa2005 %>% get_matchups()
#> # A widecr object:
#> # A tibble: 40 x 5
#>    game player1 score1 player2 score2
#>   <int> <chr>    <int> <chr>    <int>
#> 1     1 Duke         7 Duke         7
#> 2     1 Duke         7 Miami       52
#> 3     1 Miami       52 Duke         7
#> 4     1 Miami       52 Miami       52
#> 5     2 Duke        21 Duke        21
#> 6     2 Duke        21 UNC         24
#> # ... with 34 more rows
```

Typical Head-to-Head computation is done like this:

``` r
ncaa2005 %>%
  h2h_long(
    mean_score_diff = mean(score1 - score2),
    num_wins = sum(score1 > score2)
  )
#> # A long format of Head-to-Head values:
#> # A tibble: 25 x 4
#>   player1 player2 mean_score_diff num_wins
#>   <chr>   <chr>             <dbl>    <int>
#> 1 Duke    Duke                  0        0
#> 2 Duke    Miami               -45        0
#> 3 Duke    UNC                  -3        0
#> 4 Duke    UVA                 -31        0
#> 5 Duke    VT                  -45        0
#> 6 Miami   Duke                 45        1
#> # ... with 19 more rows

ncaa2005 %>% h2h_mat(mean(score1 - score2))
#> # A matrix format of Head-to-Head values:
#>       Duke Miami UNC UVA  VT
#> Duke     0   -45  -3 -31 -45
#> Miami   45     0  18   8  20
#> UNC      3   -18   0   2 -27
#> UVA     31    -8  -2   0 -38
#> VT      45   -20  27  38   0
```

Supplied list of common Head-to-Head functions has 9 entries, which are also quoted expressions:

``` r
h2h_funs
#> $mean_score_diff
#> mean(score1 - score2)
#> 
#> $mean_score_diff_pos
#> max(mean(score1 - score2), 0)
#> 
#> $mean_score
#> mean(score1)
#> 
#> $sum_score_diff
#> sum(score1 - score2)
#> 
#> $sum_score_diff_pos
#> max(sum(score1 - score2), 0)
#> 
#> $sum_score
#> sum(score1)
#> 
#> $num_wins
#> num_wins(score1, score2, half_for_draw = FALSE)
#> 
#> $num_wins2
#> num_wins(score1, score2, half_for_draw = TRUE)
#> 
#> $num
#> n()

ncaa2005 %>% h2h_long(!!!h2h_funs)
#> # A long format of Head-to-Head values:
#> # A tibble: 25 x 11
#>   player1 player2 mean_score_diff mean_score_diff… mean_score
#>   <chr>   <chr>             <dbl>            <dbl>      <dbl>
#> 1 Duke    Duke                  0                0       8.75
#> 2 Duke    Miami               -45                0       7   
#> 3 Duke    UNC                  -3                0      21   
#> 4 Duke    UVA                 -31                0       7   
#> 5 Duke    VT                  -45                0       0   
#> 6 Miami   Duke                 45               45      52   
#> # ... with 19 more rows, and 6 more variables: sum_score_diff <int>,
#> #   sum_score_diff_pos <dbl>, sum_score <int>, num_wins <dbl>,
#> #   num_wins2 <dbl>, num <int>
```

To compute Head-to-Head for only subset of players or include values for players that are not in the results, use factor `player` column:

``` r
ncaa2005 %>%
  mutate(player = factor(player, levels = c("Duke", "Miami", "Extra"))) %>%
  h2h_mat(!!!h2h_funs["num_wins"], fill = 0)
#> # A matrix format of Head-to-Head values:
#>       Duke Miami Extra
#> Duke     0     0     0
#> Miami    1     0     0
#> Extra    0     0     0
```
