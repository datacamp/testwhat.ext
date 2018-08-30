# Changelog

## 0.3.0

- Add roxygen related tests again (disable one that causes issues with Travis)
- Include new functions to verify roxygen related tasks
- Fix `parse_roxy()`; it only works with `roxygen2` version 6.1.0 from now on.

## 0.2.0

### Added

- 3 functions for testing `lavaan` model strings, which is very hard to do with `testwhat`'s API:

  + `check_lavaan_df()`: Turn lavaan model into its data frame version.
  + `check_lavaan_pattern()`: Run grepl on lavaan model strings.
  + `check_lavaan_uses()`: Check that the student specified a specific piece of an edge formula

- Documentation website, built with `pkgdown` is now hosted at https://datacamp.github.io/testwhat.ext.
  CI with Travis is setup: every time the `master` branch changes, the documentation rebuilds.

### Removed

Temporarily disabled roxygen-related tests (which is the reason for the drop in coverage).
More context in https://github.com/datacamp/testwhat.ext/issues/11.
