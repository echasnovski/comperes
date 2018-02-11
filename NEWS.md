# comperes 0.1.0.9001

- Update "item summary" API.
- Rename `to_longcr` and `to_widecr` to `as_longcr` and `as_widecr` for consistency with other R packages. Slightly tweak their logic.
- Rename `get_cr_matchups()` to `get_matchups()`.
- Completely update API for computation of Head-to-Head values.


# comperes 0.1.0.9000 (2017-06-18)

## Features

- Add 'self_play' argument to `get_h2h`.

## Bug fixes

- Replace usage of '1:ncol' to 'seq_len(ncol)';


# comperes 0.1.0 (2017-06-13)

Initial release.
