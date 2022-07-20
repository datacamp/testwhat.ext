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

  # Prevent roxygen2 looking for the non-existent DESCRIPTION file
  # in package-level documentation. See
  # https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html#documenting-packages
  # and roxygen2:::find_data() and roxygen2:::find_data_for_package()
  lines <- sub("[\r\n] *['\"]_PACKAGE['\"] *(\n|\r|$)", "\nNULL\n", lines)

  # Parse the file
  roxy <- roxygen2::parse_text(lines, new.env())

  # Unclass object to fix the print method
  roxy <- roxy %>%
    lapply(unclass_roxy_object)

  # Reshape the @param, @import, and @importFrom elements
  # for easier manipulation later
  roxy <- roxy %>%
    lapply(reshape_roxy_params) %>%
    lapply(reshape_roxy_import) %>%
    lapply(reshape_roxy_import_from)

  # For convenience, it's nice to have elements named after
  # the function that they are describing
  names(roxy) <- roxy %>%
    vapply(get_roxy_object_alias, character(1L))
  roxy
}

unclass_roxy_object <- function(x) {
  # This object doesn't print properly
  if(!is.null(x$object)) {
    x$object <- unclass(x$object)
  }
  x
}

reshape_roxy_params <- function(x) {
  params <- x[names(x) == "param"]
  if(length(params) == 0L) return(x)
  param_names <- vapply(
    params,
    function(param) param$name,
    character(1)
  )
  param_descriptions <- lapply(
    params,
    function(param) param$description
  )
  x$param <- setNames(param_descriptions, param_names)
  x
}

reshape_roxy_import <- function(x) {
  imports <- x[names(x) == "import"]
  if(length(imports) == 0L) return(x)
  # Need to remove multiple existing elements and recreate
  x[names(x) == "import"] <- NULL

  x$import <- unlist(imports, use.names = FALSE)
  x
}

reshape_roxy_import_from <- function(x) {
  import_froms <- x[names(x) == "importFrom"]
  if(length(import_froms) == 0L) return(x)
  # Need to remove multiple existing elements and recreate
  x[names(x) == "importFrom"] <- NULL

  pkg_names <- vapply(
    import_froms,
    function(import_from) import_from[1L],
    character(1)
  )
  object_names <- lapply(
    import_froms,
    function(import_from) import_from[-1L]
  )
  x$importFrom <- setNames(object_names, pkg_names) %>%
    concatenate_elements_with_same_name()
  x
}

concatenate_elements_with_same_name <- function(lst) {
  lst %>%
    split(names(lst)) %>%
    lapply(unlist, recursive = FALSE, use.names = FALSE)
}

get_roxy_object_alias <- function(x) {
  if(!is.null(x$object$alias)) {
    x$object$alias
  } else {
    ""
  }
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


