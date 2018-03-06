# The test style here is different to the ones in testwhat itself.
# This is to avoid using Rbackend, and make it clearer when things go wrong.

# The testwhat DC_reporter overrides all error messages to testwhat:::sct_failed_msg
# So for tests with expect_error(), there is no point in passing the regexp arg.

FN_WITHOUT_ROXY <- "geomean <- function(x, na.rm = FALSE) {
exp(mean(log(x), na.rm = na.rm))
}"

FN_WITH_ROXY <- "#' Geometric mean
#'
#' Calculate a geometric mean.
#' @param x A numeric vector of positive numbers.
#' @param na.rm Logical. If \\code{TRUE}, remove missing values before calculating.
#' @return The geometric mean of \\code{x}.
#' @examples
#' geomean(rlnorm(100, log(5), 0.1)) # more or less 5
#' @export
geomean <- function(x, na.rm = FALSE) {
  exp(mean(log(x), na.rm = na.rm))
}"

ANOTHER_FN_WITH_ROXY <- "#' Hypotenuse
#'
#' Calculate a hypotenuse.
#' @param x A numeric vector of non-negative numbers.
#' @param y A numeric vector of non-negative numbers.
#' @return The hypotenuse of \\code{x}.
#' @examples
#' hypotenuse(c(3, 5), c(4, 12))
#' hypotenuse('a', 'b')          # throws an error
#' @export
hypotenuse <- function(x, y) {
  sqrt(x ^ 2 + y ^ 2)
}"

YET_ANOTHER_FN_WITH_ROXY <- "#' Harmonic mean
#'
#' Calculate a harmonic mean.
#' @param x A numeric vector of non-zero numbers.
#' @param na.rm Logical. If \\code{TRUE}, remove missing values before calculating.
#' @return The harmonic mean of \\code{x}.
#' @export
harmmean <- function(x, na.rm = FALSE) {
  1 / mean(1 / x, na.rm = na.rm)
}"

FN_WITH_IMPORTS <- "#' Psychedelic dotplot
#'
#' Draw a dot plot with a psychedelic color scheme.
#' @param data A data frame.
#' @param formula A formula to specify the variables on the x and y axes.
#' @return An dotplot object of class \\code{trellis}.
#' @details This demonstrates using @importFrom and @import.
#' It imports all of the tidyverse just for the sheer horror.
#' @importFrom grDevices hcl
#' @importFrom lattice dotplot
#' @importFrom lattice panel.fill panel.dotplot
#' @import colorspace
#' @import tidyverse
#' @examples
#' psychedelic_dot_plot(ChickWeight, Time ~ weight | Diet)
#' @export
psychedelic_dot_plot <- function(data, formula) {
  dotplot(
    formula,
    data,
    panel = function(x, y, ...) {
      panel.fill(col = hcl(runif(1, 0, 360), 100, 100))
      cols <- rainbow_hcl(length(x), c = 100, l = 75)
      panel.dotplot(x, y, ..., col = cols)
    }
  )
}
"

PACKAGE_DOCS <- "#' Base R 2
#'
#' Because all the best packages have a sequel.
#' @docType package
#' @name base2
'_PACKAGE'" # The value here is special

# check_has_roxy ----------------------------------------------------------

context("check_has_roxy")

test_that(
 "test check_has_roxy() passes on a function with roxygen code", {
   # Solution code not considered
   state <- setup_state(stu_code = FN_WITH_ROXY)
   expect_pass(
     state %>%
       parse_roxy() %>%
       check_has_roxy()
   )
  }
)

test_that(
  "test check_has_roxy() fails on a function without roxygen code", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITHOUT_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_has_roxy()
    )
  }
)

# check_has_roxy_element --------------------------------------------------

context("check_has_roxy_element")

test_that(
  "test check_has_roxy_element() passes on a function with that roxygen element", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_has_roxy_element('title')
    )
  }
)

test_that(
  "test check_has_roxy_element() fails on a function without roxygen code", {
    state <- setup_state(stu_code = FN_WITHOUT_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_has_roxy_element('title')
    )
  }
)

test_that(
  "test check_has_roxy_element() fails on a function without that roxygen element", {
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_has_roxy_element('qwerty')
    )
  }
)

test_that(
  "test check_has_roxy_element() passes on package documentation", {
    # Solution code not considered
    state <- setup_state(stu_code = PACKAGE_DOCS)
    state %>%
      parse_roxy() %>%
      check_has_roxy_element('docType')
  }
)

# check_roxy_element_equals -----------------------------------------------

context("check_roxy_element_equals")

test_that(
  "test check_roxy_element_equals() passes on a function with a correct roxygen element", {
    state <- setup_state(stu_code = FN_WITH_ROXY, sol_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_roxy_element_equals('title')
    )
  }
)

test_that(
  "test check_roxy_element_equals() fails on a function without roxygen code", {
    state <- setup_state(stu_code = FN_WITHOUT_ROXY, sol_code = FN_WITH_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_element_equals('title')
    )
  }
)

test_that(
  "test check_roxy_element_equals() fails on a function with an incorrect roxygen element", {
    state <- setup_state(stu_code = ANOTHER_FN_WITH_ROXY, sol_code = FN_WITH_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_element_equals('title')
    )
  }
)

# check_roxy_element_matches ----------------------------------------------

context("check_roxy_element_matches")

test_that(
  "test check_roxy_element_matches() passes on a function with a regex-matching roxygen element", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_roxy_element_matches('examples', 'geomean\\(.*\\)')
    )
  }
)

test_that(
  "test check_roxy_element_matches() passes on a function with a fixed-matching roxygen element", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_roxy_element_matches('description', 'geometric mean', fixed = TRUE)
    )
  }
)

test_that(
  "test check_roxy_element_matches() fails on a function without roxygen code", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITHOUT_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_element_matches('description', 'geometric mean', fixed = TRUE)
    )
  }
)

test_that(
  "test check_roxy_element_matches() fails on a function with a mismatched roxygen element", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_element_matches('description', 'arithmetic mean', fixed = TRUE)
    )
  }
)

# check_has_roxy_param ----------------------------------------------------

context("check_has_roxy_param")

test_that(
  "test check_has_roxy_param() passes on a function with that roxygen param", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_has_roxy_param('na.rm')
    )
  }
)

test_that(
  "test check_has_roxy_param() fails on a function without roxygen code", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITHOUT_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_has_roxy_param('na.rm')
    )
  }
)

test_that(
  "test check_has_roxy_param() fails on a function without that roxygen param", {
   # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_has_roxy_param('y')
    )
  }
)

# check_roxy_param_matches ------------------------------------------------

context("check_roxy_param_matches")

test_that(
  "test check_roxy_param_matches() passes on a function with that roxygen param", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_roxy_param_matches('na.rm', '[lL]ogical.*remove missing')
    )
  }
)

test_that(
  "test check_roxy_param_matches() passes on a function with a fixed-matching roxygen param", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_roxy_param_matches('na.rm', 'remove missing values', fixed = TRUE)
    )
  }
)

test_that(
  "test check_roxy_param_matches() fails on a function without roxygen code", {
    # lst <- list()
    # # Solution code not considered
    # lst$DC_SCT <- "ex() %>% parse_roxy() %>% check_roxy_param_matches('x', 'numeric vector')"
    # lst$DC_CODE <- FN_WITHOUT_ROXY
    # output <- test_it(lst)
    # fails(output)
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITHOUT_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_param_matches('x', 'numeric vector')
    )
  }
)

test_that(
  "test check_roxy_param_matches() fails on a function with a mismatched roxygen element", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_param_matches('x', 'character vector', fixed = TRUE)
    )
  }
)

# check_roxy_imports_package ----------------------------------------------

context("check_roxy_imports_package")

test_that(
  "test check_roxy_imports_package() passes on a function with that roxygen import", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_IMPORTS)
    state %>%
      parse_roxy() %>%
      check_roxy_imports_package('colorspace')
  }
)


test_that(
  "test check_roxy_imports_package() fails on a function without that roxygen import", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_IMPORTS)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_imports_package('lattice') # this is in @importFrom not @import
    )
  }
)

# check_roxy_imports_from_package -----------------------------------------

context("check_roxy_imports_from_package")

test_that(
  "test check_roxy_imports_from_package() passes on a function with that roxygen importFrom", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_IMPORTS)
    state %>%
      parse_roxy() %>%
      check_roxy_imports_from_package('lattice')
  }
)

test_that(
  "test check_roxy_imports_from_package() fails on a function with that roxygen importFrom", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_IMPORTS)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_imports_from_package('colorspace') # this is in @import not @importFrom
    )
  }
)


# check_roxy_imports_object_from_package ----------------------------------------

context("check_roxy_imports_object_from_package")

test_that(
  "test check_roxy_imports_object_from_package() passes on a function with that objected imported from the package", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_IMPORTS)
    state %>%
      parse_roxy() %>% {
        check_roxy_imports_object_from_package(., 'grDevices', 'hcl')
        check_roxy_imports_object_from_package(., 'lattice', 'dotplot')
        check_roxy_imports_object_from_package(., 'lattice', 'panel.fill')
        check_roxy_imports_object_from_package(., 'lattice', 'panel.dotplot')
      }
  }
)

test_that(
  "test check_roxy_imports_object_from_package() fails on a function without that objected imported from the package", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_IMPORTS)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_imports_object_from_package('lattice', 'xyplot')
    )
  }
)

# check_roxy_examples_run -------------------------------------------------

context("check_roxy_examples_run")

test_that(
  "test check_roxy_examples_run() passes on a function with runnable roxygen examples", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_roxy_examples_run()
    )
  }
)

test_that(
  "test check_roxy_examples_run() fails on a function with no roxygen examples", {
    # Solution code not considered
    state <- setup_state(stu_code = YET_ANOTHER_FN_WITH_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_examples_run()
    )
  }
)

test_that(
  "test check_roxy_examples_run() fails on a function with non-runnable roxygen examples", {
    # Solution code not considered
    state <- setup_state(stu_code = ANOTHER_FN_WITH_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_examples_run()
    )
  }
)

test_that(
  "test check_roxy_examples_run() passes on a function with roxygen examples that only run when pre-exercise code is run", {
    # Solution code not considered
    state <- setup_state(
      pre_ex_code = ".libPaths(Sys.getenv('R_LIBS_USER')); library('colorspace'); library('lattice')",
      stu_code = FN_WITH_IMPORTS
    )
    state %>%
      parse_roxy() %>%
      check_roxy_examples_run()
  }
)

# check_roxy_examples_result_equals ---------------------------------------

context("check_roxy_examples_result_equals")

test_that(
  "test check_roxy_examples_result_equals() passes on a function with a correct roxygen element", {
    state <- setup_state(stu_code = FN_WITH_ROXY, sol_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_roxy_examples_result_equals()
    )
  }
)

test_that(
  "test check_roxy_examples_result_equals() fails on a function with an incorrect roxygen element", {
    state <- setup_state(
      stu_code = sub("log(5)", "log(10)", FN_WITH_ROXY, fixed = TRUE),
      sol_code = FN_WITH_ROXY
    )
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_examples_result_equals()
    )
  }
)

# check_roxy_example_matches ----------------------------------------------

context("check_roxy_example_matches")

test_that(
  "test check_roxy_example_matches() passes on a function with matching roxygen example", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_roxy_example_matches('geomean\\(.*\\)')
    )
  }
)

test_that(
  "test check_roxy_example_matches() passes on a function with a fixed-matching roxygen element", {
    # Solution code not considered
    state <- setup_state(stu_code = FN_WITH_ROXY)
    expect_pass(
      state %>%
        parse_roxy() %>%
        check_roxy_example_matches('geomean(', fixed = TRUE)
    )
  }
)

test_that(
  "test check_roxy_example_matches() fails on a function without roxygen examples", {
    # Solution code not considered
    state <- setup_state(stu_code = YET_ANOTHER_FN_WITH_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_example_matches('geomean(', fixed = TRUE)
    )
  }
)

test_that(
  "test check_roxy_example_matches() fails on a function with a mismatched roxygen element", {
    state <- setup_state(stu_code = ANOTHER_FN_WITH_ROXY)
    expect_error(
      state %>%
        parse_roxy() %>%
        check_roxy_example_matches('geomean(', fixed = TRUE)
    )
  }
)
