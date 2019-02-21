#' Write code lines to file and get lint msgs
#'
#' Writes a character vector of R code to a file and runs
#' \code{\link[lintr]{lintr}} on it.
#' @param lines A character vector of R code.
#' @return A character vector of lint messages.
#' @importFrom lintr lint
lint_from_code <- function(lines) {
  tf <- tempfile()
  writeLines(lines, tf);
  vapply(
    lintr::lint(tf),
    function(lint) {
      paste0(
        "L", lint$line_number,
        "C", lint$column_number,
        " ", lint$message
      )
    },
    character(1)
  )
}


#' Make a lint exercise state
#'
#' Convert the parse data to lint messages.
#' @param state An exercise state, as returned by \code{ex()}.
#' @return A child state.
#' @importFrom testwhat ChildState
#' @export
make_lint_state <- function(state) {
  childState <- ChildState$new(state)
  childState$set(
    student_pd = lint_from_code(childState$get("student_code")),
    solution_pd = lint_from_code(childState$get("solution_code"))
  )
  return(invisible(childState))
}


#' Check that there is no lint
#'
#' Check that the student's code is lint-free
#' @param state The state of the exercise, as returned from
#' \code{\link{make_lint_state}}.
#' @return The child state.
#' @importFrom testwhat is_gte is_true check_that
#' @export
check_is_lint_free <- function(state) {
  student_lint <- state$get("student_pd")
  n_lints <- length(student_lint)
  #
  has_lint_feedback_msg <- sprintf(
    "You have %d lints left to fix. The messages are: \n%s",
    n_lints,
    paste(student_lint, collapse = "\t")
  )
  check_that(is_true(n_lints == 0), feedback = has_lint_feedback_msg)
  return(invisible(state))
}
