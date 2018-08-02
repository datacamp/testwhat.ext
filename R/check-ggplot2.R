

# User-facing functionality -----------------------------------------------

#' Check a ggplot2 aesthetic
#'
#' Checks the aesthetics of ggplot2 plots.
#' @param state The state of the exercise.
#' @param plot A string naming the ggplot variable.
#' @param aesthetic A string naming the aesthetic. (Use British English names,
#' e.g., "colour".)
#' @param layer_index Positive integer of layer number.
#' @return This function is invoked for the side effect of registering feedback
#' in the event of a failed test. See \code{\link{check_that}} for details of
#' the return value and feedback mechanism.
#' @section Limitations
#' This currently does not support checking against \code{ggplot2::last_plot()}.
#' That means it only works when you assign the plot to a value. It also relies
#' heavily on rlang, which means it only works with ggplot2 version 3 or higher.
#' @examples
#' \dontrun{
#' # An simple plot:
#' carplot <- ggplot(cars, aes(speed, dist)) +
#'   geom_point()
#'
#' ex() %>% {
#'   check_aes(. "carplot", "speed")
#'   check_aes(. "carplot", "dist")
#' }
#' @importFrom testwhat test_correct
#' @importFrom magrittr %>%
#' @export
check_aes <- function(state, plot, aesthetic, layer_index = 1L) {
  test_correct(
    state %>% check_layer_aes_vs_layer_data(plot, aesthetic, layer_index),
    state %>% check_layer_aes_vs_global_data(plot, aesthetic, layer_index),
    state %>% check_global_aes_vs_layer_data(plot, aesthetic, layer_index),
    state %>% check_global_aes_vs_global_data(plot, aesthetic)
  )
}


# Lower-level checks ------------------------------------------------------

#' Check a ggplot2 aesthetic
check_global_aes_vs_global_data <- function(state, plot, aesthetic) {
  assert_has_ggplot2_v3()
  cmd <- sprintf(
    "rlang::eval_tidy(%s$mapping[[%s]], %s$data)",
    plot, aesthetic, plot
  )
  aes_error_msg <- sprintf(
    "Evaluating the %s aesthetic for plot %s threw an error.",
    aesthetic, plot
  )
  aes_incorrect_msg <- sprintf(
    "The %s aesthetic for plot %s has the wrong value.",
    aesthetic, plot
  )
  state %>%
    check_expr_result_equal(cmd, aes_error_msg, aes_incorrect_msg)
}

check_layer_aes_vs_global_data <- function(state, plot, aesthetic, layer_index = 1L) {
  assert_has_ggplot2_v3()
  cmd <- sprintf(
    "rlang::eval_tidy(%s$layers[[%d]]$mapping[[%s]], %s$data)",
    plot, layer_index, aesthetic, plot
  )
  aes_error_msg <- sprintf(
    "Evaluating the %s aesthetic for layer %d of plot %s threw an error.",
    aesthetic, layer_index, plot
  )
  aes_incorrect_msg <- sprintf(
    "The %s aesthetic for layer %d of plot %s has the wrong value.",
    aesthetic, layer_index, plot
  )
  state %>%
    check_expr_result_equal(cmd, aes_error_msg, aes_incorrect_msg)
}

check_global_aes_vs_layer_data <- function(state, plot, aesthetic, layer_index = 1L) {
  assert_has_ggplot2_v3()
  cmd <- sprintf(
    "rlang::eval_tidy(%s$mapping[[%s]], %s$layers[[%d]]$data)",
    plot, layer_index, aesthetic, plot
  )
  aes_error_msg <- sprintf(
    "Evaluating the %s aesthetic against data in layer %d of plot %s threw an error.",
    aesthetic, layer_index, plot
  )
  aes_incorrect_msg <- sprintf(
    "The %s aesthetic (tested against data in layer %d) of plot %s has the wrong value.",
    aesthetic, layer_index, plot
  )
  state %>%
    check_expr_result_equal(cmd, aes_error_msg, aes_incorrect_msg)
}

check_layer_aes_vs_layer_data <- function(state, plot, aesthetic, layer_index = 1L) {
  assert_has_ggplot2_v3()
  cmd <- sprintf(
    "rlang::eval_tidy(%s$layers[[%d]]$mapping[[%s]], %s$layers[[%d]]$data)",
    plot, layer_index, aesthetic, plot
  )
  aes_error_msg <- sprintf(
    "Evaluating the %s aesthetic for layer %d of plot %s threw an error.",
    aesthetic, layer_index, plot
  )
  aes_incorrect_msg <- sprintf(
    "The %s aesthetic for layer %d of plot %s has the wrong value.",
    aesthetic, layer_index, plot
  )
  state %>%
    check_expr_result_equal(cmd, aes_error_msg, aes_incorrect_msg)
}


# Utilities ---------------------------------------------------------------

#' @importFrom utils packageVersion
assert_has_ggplot2_v3 <- function() {
  if(!requireNamespace("ggplot2") || packageVersion("ggplot2") < "3.0.0") {
    stop("check_aes() is designed for use with ggplot2 v3.0.0 or higher.")
  }
}

#' @importFrom magrittr %>%
#' @importFrom testwhat check_expr
#' @importFrom testwhat check_result
#' @importFrom testwhat check_equal
check_expr_result_equal <- function(state, cmd, error_msg, incorect_msg) {
  state %>%
    check_expr(cmd) %>%
    check_result(error_msg = error_msg) %>%
    check_equal(incorrect_msg = incorrect_msg)
}

