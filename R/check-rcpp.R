#' Parse cpp files with embedded R
#'
#' Extracts the embedded R and Cpp portions from a cpp file.
#'
#' @param state An exercise state, as returned by \code{ex()}.
#' @return A child state.
#'
#' @examples
#' \dontrun{
#' # Example solution cpp
#' #include <Rcpp.h>
#' using namespace Rcpp ;
#'
#' // [[Rcpp::export]]
#' int answer(){
#'   return 42 ;
#' }
#'
#' /*** R
#' x <- answer()
#' x
#' */
#'
#' # SCT
#' ex() %>% check_cpp(.) %>% check_code(., "return//s+42//s+;")
#' ex() %>% check_embedded_r(.) %>% check_function("answer") %>% check_result()
#'
#' }
#'
#' @rdname check_rcpp

#' @name check_rcpp
#' @importFrom testwhat build_pd ChildState
#' @export
check_embedded_r <- function(state) {
  childState <- ChildState$new(state)
  childState$set(
    student_code = parse_cpp_script(state$get("student_code"))[["r"]],
    solution_code = parse_cpp_script(state$get("solution_code"))[["r"]]
  )
  childState$set(
    student_pd = build_pd(childState$get("student_code")),
    solution_pd = build_pd(childState$get("solution_code"))
  )
  return(childState)
}

#' @name check_rcpp
#' @importFrom testwhat ChildState
#' @export
check_cpp <- function(state) {
  childState <- ChildState$new(state)
  childState$set(
    student_code = parse_cpp_script(state$get("student_code"))[["cpp"]],
    solution_code = parse_cpp_script(state$get("solution_code"))[["cpp"]],
    student_pd = NULL, # cpp can't be parsed atm
    solution_pd = NULL # cpp can't be parsed atm
  )
  return(childState)
}

seq_int <- function(lo, hi) {
  if(hi < lo) return(integer())
  seq.int(lo, hi, by = 1)
}

parse_cpp_script <- function(code, flatten = TRUE) {
  code_lines <- strsplit(code, split = "\n")[[1]]
  start_line <- which(grepl(" */\\*{3} +R", code_lines))
  end_line <- which(grepl(" *\\*/", code_lines))

  r_lines <- Map(seq_int, start_line + 1, end_line - 1)
  r_chunks <- code_lines[unlist(r_lines, use.names = FALSE)]

  cpp_lines <- setdiff(seq_along(code_lines), unlist(Map(seq_int, start_line, end_line)))
  cpp_chunks <- code_lines[cpp_lines]

  return(list(r = paste(r_chunks, collapse = "\n"),
              cpp = paste(cpp_chunks, collapse = "\n")))
}

#' Check whether CPP function was properly exported
#'
#' This check function uses check_code with an advanced regex pattern.
#'
#' @param state A child state that focuses on the cpp portion of an exercise
#'   submission.
#' @param return_type A character string denoting the return type of the
#'   function that should have been exported.
#' @param name A character string denoting name of the function that should have
#'   been exported.
#' @param not_exported_msg An optional character string with a message that is
#'   shown if the function was not exported properly.
#' @examples
#' \dontrun{
#' # Example solution cpp
#' #include <Rcpp.h>
#' using namespace Rcpp ;
#'
#' // [[Rcpp::export]]
#' int answer(){
#'   return 42 ;
#' }
#'
#' /*** R
#' x <- answer()
#' x
#' */
#'
#' # SCT
#' ex() %>% check_cpp() %>% check_cpp_function_exported("int", "answer")
#' }
#' @importFrom testwhat check_code
#' @export
check_cpp_function_exported <- function(state, return_type, name, not_exported_msg = NULL) {
  if (is.null(not_exported_msg)) {
    not_exported_msg <- sprintf("Did you properly export the function `%s %s()`?", return_type, name)
  }
  patt <- sprintf("//\\s*\\[\\[\\s*?Rcpp::export\\s*\\]\\]\\s*\\n+\\s*%s\\s+%s\\s*\\(", return_type, name)
  state %>% check_code(regex = patt, missing_msg = not_exported_msg)
}
