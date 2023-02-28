# comperes 0.2.7

* Maintenance release in reaction to a planned update of `dplyr` to 1.1.1.

# comperes 0.2.6

* Maintenance release in reaction to a planned update of `dplyr` to 1.1.0.

# comperes 0.2.5

* Maintenance release in reaction to a planned update of `tibble`.

# comperes 0.2.4

* Maintenance release in reaction to `dplyr` version 1.0.0.

# comperes 0.2.3

* Maintenance release in reaction to R 4.0.0.

# comperes 0.2.2

* React to `dplyr` version 0.8.0.

# comperes 0.2.1

## Bug fixes

* `is_widecr()` now returns `TRUE` on data frames with columns containing "score" or "player" in their names but which are note followed by digits (#2).

## Other

* Tweak `levels2()` for better performance.
* React to `rlang` version 0.3.0.

# comperes 0.2.0 (2018-05-09)

Initial CRAN release

# comperes 0.1.0.9001

* Update "item summary" API.
* Rename `to_longcr` and `to_widecr` to `as_longcr` and `as_widecr` for consistency with other R packages. Slightly tweak their logic.
* Rename `get_cr_matchups()` to `get_matchups()`.
* Completely update API for computation of Head-to-Head values.
* Remove `skip_action()` function.
* Add `hp_survey` data.

# comperes 0.1.0.9000 (2017-06-18)

## Features

* Add 'self_play' argument to `get_h2h`.

## Bug fixes

* Replace usage of '1:ncol' to 'seq_len(ncol)';

# comperes 0.1.0 (2017-06-13)

Initial release.
