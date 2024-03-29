% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-roxygen.R
\name{check_has_roxy}
\alias{check_has_roxy}
\alias{check_has_roxy_element}
\alias{check_roxy_element_equals}
\alias{check_roxy_element_matches}
\alias{check_has_roxy_param}
\alias{check_roxy_param_matches}
\alias{check_roxy_imports_package}
\alias{check_roxy_imports_from_package}
\alias{check_roxy_imports_object_from_package}
\alias{check_roxy_examples_run}
\alias{check_roxy_examples_result_equals}
\alias{check_roxy_example_matches}
\title{Check the roxygen comments}
\usage{
check_has_roxy(state, index = 1L, missing_msg = NULL, append = TRUE)

check_has_roxy_element(
  state,
  element,
  index = 1L,
  missing_msg = NULL,
  append = TRUE
)

check_roxy_element_equals(
  state,
  element,
  index = 1L,
  incorrect_msg = NULL,
  append = TRUE
)

check_roxy_element_matches(
  state,
  element,
  regex,
  fixed = FALSE,
  times = 1L,
  index = 1L,
  not_typed_msg = NULL,
  append = TRUE
)

check_has_roxy_param(
  state,
  param_name,
  index = 1L,
  missing_msg = NULL,
  append = TRUE
)

check_roxy_param_matches(
  state,
  param_name,
  regex,
  fixed = FALSE,
  index = 1L,
  not_typed_msg = NULL,
  append = TRUE
)

check_roxy_imports_package(
  state,
  pkg_name,
  index = 1L,
  missing_msg = NULL,
  append = TRUE
)

check_roxy_imports_from_package(
  state,
  pkg_name,
  index = 1L,
  missing_msg = NULL,
  append = TRUE
)

check_roxy_imports_object_from_package(
  state,
  pkg_name,
  object_name,
  index = 1L,
  missing_msg = NULL,
  append = TRUE
)

check_roxy_examples_run(
  state,
  index = 1L,
  not_runnable_msg = NULL,
  append = TRUE
)

check_roxy_examples_result_equals(
  state,
  index = 1L,
  incorrect_msg = NULL,
  append = TRUE
)

check_roxy_example_matches(
  state,
  regex,
  fixed = FALSE,
  index = 1L,
  not_typed_msg = NULL,
  append = TRUE
)
}
\arguments{
\item{state}{The state of the exercise, as returned from \code{\link{parse_roxy}}.}

\item{index}{A positive integer or a string naming a function. This describes
which roxygen element in the code to check.}

\item{missing_msg}{Optional string. Used to override the feedback message
in the event of failure.}

\item{append}{For compatibility; currently unused.}

\item{element}{String naming the element of the roxygen block to check.}

\item{incorrect_msg}{Optional string. Used to override the feedback message
in the event of failure.}

\item{regex}{String providing a regular expression for the solution code to
match. See See \code{testwhat}'s \code{check_code} function.}

\item{fixed}{Logical. If \code{TRUE}, regex is treated as a fixed string, not
a regular expression. See \code{testwhat}'s \code{check_code} function.}

\item{times}{Positive integer. Denotes the number of times the string in
\code{regex} should be matched.}

\item{not_typed_msg}{Optional string. Used to override the feedback message
in the event of failure.}

\item{param_name}{String naming a parameter for the function.}

\item{pkg_name}{String naming an R package to import from.}

\item{object_name}{String naming an object to import from another package.}

\item{not_runnable_msg}{Optional string. Used to override the feedback message
in the event of failure.
incorrect_msg}
}
\value{
This function is invoked for the side effect of registering feedback
in the event of a failed test. See \code{\link{check_that}} for details of
the return value and feedback mechanism.
}
\description{
Check that the roxygen comments provided by the student are correct.
}
\details{
\code{check_has_roxy} checks that the \code{index} block of roxygen
is present.
\code{check_has_roxy_element} checks that the \code{element} element of the
\code{index} block of roxygen is present.
\code{check_roxy_element_equals} checks that the \code{element} element of
the \code{index} block of roxygen is equal to the value in the solution code.
\code{check_roxy_element_matches} checks that the \code{element} element of
the \code{index} block of roxygen matches a regular expression or string.
\code{check_roxy_param_equals} checks that the \code{param_name} parameter of
the \code{index} block of roxygen is equal to the value in the solution code.
\code{check_roxy_param_matches} checks that the \code{param_name} element of
the \code{index} block of roxygen matches a regular expression or string.
\code{check_roxy_example_results} checks that the final result of running the
\code{examples} in the \code{index} block of roxygen is equal to the value in
the solution code.
\code{check_roxy_example_matches} check that the \code{examples} of the
\code{index} block of roxygen matches a regular expression or string.

Only \code{check_roxy_element_equals} and \code{check_roxy_example_results}
require the solution. The other checks can safely be run with
'___BLOCK_SOLUTION_EXEC___'.
}
\examples{
\dontrun{
  # Always begin by calling parse_roxy() on the exercise state.
  ex() \%>\% parse_roxy() \%>\% {
    check_has_roxy(.)
    check_has_roxy_element(., 'title')
    check_roxy_element_equals(., 'description', 'This is a mean function')
    check_roxy_element_matches(., 'return', 'integer +vector')
    check_roxy_param_equals(., 'x', 'A numeric vector.')
    check_roxy_param_matches(., 'na.rm', '[Ll]ogical.*missing.')
    check_roxy_example_results(., 10)
    check_roxy_example_matches(., 'mean\\\\\\\\(.*\\\\\\\\)')
  }
}

}
