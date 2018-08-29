# roxygen2 ----------------------------------------------------------------

#' Extract roxygen details from a file
#'
#' Parses an R file and extracts the roxygen tags. Mostly just a wrapper
#' around \code{roxygen2::parse_text}.
#'
#' @param lines A character vector of code lines.
#' @return A list of lists. Each top level element corresponds to a roxygen
#' block. Each second level element corresponds to a roxygen tag within that
#' block.
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' @noRd
extract_roxygen_from_code <- function(lines) {
  # registry setup inferred from body of roxygenize()
  registry <- c(
    roxygen2::roclet_tags(roxygen2::roclet_find("rd")),
    roxygen2::roclet_tags(roxygen2::roclet_find("namespace")),
    include = roxygen2::tag_value
  )
  # Parse the file
  roxy <- roxygen2::parse_text(lines, new.env(), registry)
  # Unclass object to fix the print method
  roxy <- lapply(
    roxy,
    function(x) {
      # This object doesn't print properly
      if(!is.null(x$object)) {
        x$object <- unclass(x$object)
      }
      x
    }
  )
  # Flatten the param element for easier manipulation later
  roxy <- lapply(
    roxy,
    function(x) {
      params <- x[names(x) == "param"]
      if(length(params) == 0L) return(x)
      x$param <- lapply(
        params,
        function(paramsi) paramsi$description
      ) %>%
        setNames(
          vapply(
            params,
            function(paramsi) paramsi$name,
            character(1)
          )
        )
      x
    }
  )
  # For convenience, it's nice to have elements named after
  # the function that they are describing
  names(roxy) <- vapply(
    roxy,
    function(x) {
      if(!is.null(x$object$alias)) {
        x$object$alias
      } else {
        ""
      }
    },
    character(1L)
  )
  roxy
}

#' Parse roxygen2 comments
#'
#' Parses roxygen2 comments and updates the state.
#' @param state An exercise state, as returned by \code{ex()}.
#' @return A child state.
#' @details The function extracts the roxygen2 comments from the state then
#' parses them.
#'
#' @importFrom testwhat ChildState
#' @export
parse_roxy <- function(state) {
  childState <- ChildState$new(state)
  childState$set(
    student_pd = extract_roxygen_from_code(childState$get("student_code")),
    solution_pd = extract_roxygen_from_code(childState$get("solution_code"))
  )
  return(invisible(childState))
}

# DESCRIPTION -------------------------------------------------------------

#' Extract DESCRIPTION details from a character vector
#'
#' Parses a package DESCRIPTION file and extracts the tags. Mostly just a
#' wrapper around \code{\link[base]{read.dcf}}.
#' @param lines A character vector of lines of a DESCRIPTION file.
#' @return A list of DESCRIPTION fields. They are all character vectors.
#' @examples
#' # Base package
#' desc_lines <- readLines(system.file("DESCRIPTION"))
#' read_dcf(desc_lines)
#'
#' # This package
#' desc_lines <- readLines(system.file("DESCRIPTION", package = "testwhat"))
#' read_dcf(desc_lines)
#' @noRd
extract_description_from_code <- function(lines) {
  tc <- textConnection(lines)
  on.exit(close(tc))
  dcf <- read.dcf(tc)
  desc <- setNames(as.list(dcf), colnames(dcf))
  # desc$Version <- if(!is.null(desc$Version)) {
  #   as.numeric_version(desc$Version)
  # }
  # desc$Date <- if(!is.null(desc$Date)) {
  #   as.Date(desc$Date)
  # }
  # desc$`Authors@R` <- if(!is.null(desc$`Authors@R`)) {
  #   eval(parse(text = desc$`Authors@R`))
  # }
  desc
}

#' Parse DESCRIPTION
#'
#' Parses a package DESCRIPTION file and updates the state.
#' @param state An exercise state, as returned by \code{ex()}.
#' @return A child state.
#' @details The function extracts the DESCRIPTION fields from the state then
#' parses them.
#'
#' @importFrom testwhat ChildState
#' @export
parse_desc <- function(state) {
  childState <- ChildState$new(state)
  childState$set(
    student_pd = extract_description_from_code(childState$get("student_code")),
    solution_pd = extract_description_from_code(childState$get("solution_code"))
  )
  return(invisible(childState))
}


