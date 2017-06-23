#' Example high-level SCT for checking object equality
#'
#' @param state state to start from
#' @param names one or more object names to test for equality
#' @importFrom testwhat check_object check_equal %>%
#'
#' @export
check_object2 <- function(state, names) {
  for (el in names)
    #1 %>% (function(a) a + 1)
    state %>% check_object(el) %>% check_equal()

  return(state)
}

#' Example high-level SCT for checking a function
#'
#' @param state state to start from
#' @param name passed to check_function
#' @param index passed to check_function
#' @importFrom testwhat check_function check_arg check_equal %>%
#'
#' @export
check_function2 <- function(state, name, index, args) {
  fun_state <- check_function(state, name, index)

  for (arg in args) fun_state %>% check_arg(arg) %>% check_equal()

  return(state)
}
