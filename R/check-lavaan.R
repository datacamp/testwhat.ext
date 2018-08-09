#' Turn lavaan model into its data frame version.
#'
#' Chain this function onto \code{check_object}.
#' Use this function first before using \code{\link{check_lavaan_uses}}.
#'
#' @param state state to start from (should be state produced by \code{check_object}).
#' @param invalid_model_msg if specified, this overrides the automatically generated feedback message in case the targeted variable does not validly represent a lavaan model.
#' @param append whether or not to append the invalid model message to earlier messages that describe the object.
#'
#' @examples
#' \dontrun{
#'
#' # example
#' model <- 'a ~ x1'
#'
#' # sct
#' ex() %>%
#'   check_object("model") %>%
#'   check_lavaan_df()
#' }
#'
#' @importFrom testwhat check_that
#' @export
check_lavaan_df <- function(state, invalid_model_msg = NULL, append = TRUE) {

  if (!inherits(state, "ObjectState")) {
    stop("check_lavaan_df can only be used on the state produced by check_object")
  }

  if(is.null(invalid_model_msg)) {
    invalid_model_msg <- "Make sure it is a valid lavaan model."
  }

  stu_obj <- state$get("student_object")
  sol_obj <- state$get("solution_object")

  tryCatch(
    sol_df <- lavaan::lavaanify(sol_obj),
    error = function(e) {
      stop("The targeted object in the solution does not validly represent a lavaan model.")
    }
  )

  state$add_details(message = invalid_model_msg,
                    append = append, pd = NULL)

  tryCatch(
    stu_df <- lavaan::lavaanify(stu_obj),
    error = function(e) {
      check_that(FALSE, feedback = state$details)
    }
  )

  state$set(
    solution_object = sol_df,
    student_object = stu_df
  )

  state$set_details(message = "Check the lavaan model it represents.",
                    append = TRUE, pd = NULL)

  return(state)
}

#' Check that the student specified a specific piece of an edge formula
#'
#' \code{check_lavaan_uses} inspects the model data frame produced by \code{\link{check_lavaan_df}} according to the arguments you specify.
#' If it cannot find any rows in the data frame that match all of the arguments and corresponding values, the function will fail.
#'
#' To see which arguments you can pass to the ellipsis argument, use \code{lavaan::lavaanify} on a lavaan model. Examples of arguments are \code{lhs}, \code{op}, and \code{rhs}, among others.
#'
#' @param state state to start from (has to be the state produced by \code{\link{check_lavaan_df}})
#' @param incorrect_msg feedback message in case the specified columns and values were not found in the lavaan data frame.
#' @param append whether or not to append the \code{incorrect_msg} message to earlier messages that describe the object.
#' @param ... named args are columns of lavaanify output, and their values are used to subset that output.
#'
#' @examples
#' \dontrun{
#'
#' # example 1
#' # lavaan::lavaanify(model) produces df with 3 rows
#' model <- 'a ~ x1'
#'
#' # sct
#' ex() %>%
#'   check_object("model") %>%
#'   check_lavaan_df() %>%
#'   check_lavaan_uses(lhs = 'a', op = '~', rhs = 'x1')
#'
#' # example 2
#' # lavaan::lavaanify(model) produces df with 6 rows
#' model <- 'a ~ x1 + x3'
#'
#' # sct
#' ex() %>%
#'   check_object("model") %>%
#'   check_lavaan_df() %>%
#'   check_lavaan_uses(lhs = 'a', op = '~', rhs = 'x1')
#'
#' # sct
#' ex() %>%
#'   check_object("model") %>%
#'   check_lavaan_df() %>%
#'   check_lavaan_uses(lhs = 'a', op = '~', rhs = 'x3')
#'
#' }
#'
#' @importFrom testwhat check_that
#' @export
check_lavaan_uses <- function(state, incorrect_msg, append = TRUE, ...) {
  args <- list(...)

  # get object state (should we check it's a string or DF?)
  stu_obj <- state$get("student_object")
  sol_obj <- state$get("solution_object")

  if (!inherits(sol_obj, "data.frame")) {
    stop("check_lavaan_uses() can only be used on the state produced by check_lavaan_df().")
  }

  # select rows that match args
  indx <- apply(sol_obj[,names(args), drop = FALSE] == args, 1, all)
  if (!any(indx)) stop("check_lavaan_uses fails on the targeted model in the solution.")

  state$add_details(message = incorrect_msg,
                    append = append,
                    pd = NULL)

  indx <- apply(stu_obj[,names(args), drop = FALSE] == args, 1, all)
  check_that(any(indx), feedback = state$details)
  return(state)
}

match_data_frame <- function(rows_to_find, X) {
  start <- nrow(X)
  stop <- start + nrow(rows_to_find)
  duplicated(rbind(X, rows_to_find))[start:stop]
}

#' Run \code{grepl} on lavaan model strings.
#'
#' If the pattern is not found, the function throws an error message (that you have to specify explicitly).
#'
#' @param state state to start from (should be state produced by \code{check_object}).
#' @param patt pattern to look for in the lavaan model string that was targeted with \code{check_object}.
#' @param not_string_msg if specified, this overrides the automatically generated message in case the targetted object is not a string.
#' @param incorrect_msg feedback message if calling the function specified in func for if the test fails
#' @param append whether or not to append the feedback messages to earlier messages that describe the object.
#'
#' @examples
#' \dontrun{
#'
#' # solution model
#' mod <- 'a =~ x1 + x2'
#' # student model
#  mod <- 'a =~ x1'
#'
#' # scts that pass
#' ex() %>%
#'   check_object("model") %>%
#'   check_lavaan_pattern("a")
#'
#' ex() %>%
#'   check_object("model") %>%
#'   check_lavaan_pattern("a\\s+=~\\s+x1")
#'
#' # scts that fail
#' ex() %>%
#'   check_object("model") %>%
#'   check_lavaan_pattern("x2")
#'
#' ex() %>%
#'   check_object("model") %>%
#'   check_lavaan_pattern("x1\\s\\+\\s+x2")
#' }
#'
#' @export
check_lavaan_pattern <- function(state, patt, not_string_msg = NULL, incorrect_msg, append = TRUE) {

  if (!inherits(state, "ObjectState")) {
    stop("check_lavaan_df can only be used on the state produced by check_object")
  }

  stu_obj <- state$get("student_object")
  sol_obj <- state$get("solution_object")


  if (!isTRUE(typeof(sol_obj) == "character")) {
    stop("the solution object you're targetting is not a string")
  }

  if (!isTRUE(grepl(patt, sol_obj))) {
    stop("the pattern you specified was not found in the lavaan model of the solution.")
  }

  if (is.null(not_string_msg)) {
    not_string_msg <- "It is not a string so it can't represent a lavaan model."
  }

  state$add_details(message = not_string_msg,
                    append = append,
                    pd = NULL)

  check_that(is_equal(typeof(stu_obj), "character"),
             feedback = state$details)

  state$set_details(message = incorrect_msg,
                    append = append,
                    pd = NULL)

  check_that(grepl(patt, stu_obj), feedback = state$details)
  return(state)
}

# #' Check all model edges as specified in the output of lavaanify
# #'
# #' @param state state to start from
# #' @param name name of variable where lavaan model is specified
# #' @param ... named args are columns of lavaanify output, and their values are used to subset that output.
# #' @importFrom stringr str_interp
# #' @importFrom testwhat check_that
# #'
# #' @export
# check_lavaan_match <- function(state, name, msg, ...) {
#   args <- list(...)
#
#   # get object state (should we check it's a string or DF?)
#   obj_state <- testwhat::check_object(state, name)
#   stu_obj <- obj_state$get("student_object")
#   sol_obj <- obj_state$get("solution_object")
#
#   # subset solution using args
#   stu_df <- lavaan::lavaanify(stu_obj)
#   sol_df <- lavaan::lavaanify(sol_obj)
#
#   # select rows that match args
#   indx <- apply(sol_df[,names(args), drop = FALSE] == args, 1, all)
#
#   test_df <- sol_df[indx,]
#
#   # check that each row matches
#   matches <- match_data_frame(test_df, stu_df)
#   no_match <- test_df[which(matches),]
#
#   # loop over NAs, with report
#   for (el in 1:nrow(no_match)) {
#     testwhat::check_that(FALSE, msg)
#   }
#
#   return(obj_state)
# }
