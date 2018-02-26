# Use internal fn without :::
eval_parse <- testwhat.ext:::eval_parse

#' Setup exercise state
#'
#' Sets up the exercise state from student, solution, and pre-exercise code.
#' @param stu_code A character vector of student code.
#' @param sol_code A character vector of solution code.
#' @param pre_ex_code A character vector of pre-exercise code.
#' @param output A character vector of solution code output.
#' @return An exercise state of class \code{RootState}.
#' It also has the side effect of setting the state of global testwhat object,
#' \code{testwhat:::tw}, to this state, and the reporter to a new
#' \code{DC_reporter}.
#' @noRd
setup_state <- function(stu_code, sol_code = "", pre_ex_code = "", output = "") {
  if (is.character(output)) {
    output <- list(list(type = "output", payload = output))
  }

  tw <<- testwhat:::tw

  sol_env <- new_env()
  stu_env <- new_env()

  eval_parse(pre_ex_code, sol_env)
  eval_parse(sol_code, sol_env)
  eval_parse(pre_ex_code, stu_env)
  eval_parse(stu_code, stu_env)

  tw$clear()

  state <- testwhat:::RootState$new(
    pec           = pre_ex_code,
    student_code  = stu_code,
    student_pd    = testwhat:::build_pd(stu_code),
    student_env   = stu_env,
    solution_code = sol_code,
    solution_pd   = testwhat:::build_pd(sol_code),
    solution_env  = sol_env,
    output_list   = output,
    test_env      = new_env()
  )

  # testwhat will access the reporter and state from the tw object
  rep <- testwhat:::DC_reporter$new()
  tw$set(state = state, reporter = rep, stack = TRUE)

  state
}

#' Create a new environment
#'
#'  Create a new environment whose parent environment is the global environment.
#'  @return An environment.
#'  @examples
#'  # Create a new environment
#'  (e <- new_env())
#'
#'  # The parent environment is the global environment
#'  identical(parent.env(e), globalenv())
#'  @noRd
new_env <- function() {
  new.env(parent = globalenv())
}
