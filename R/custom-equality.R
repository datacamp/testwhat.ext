#' Custom equality checks
#'
#' Custom equality checks to make it easier to check weird objects.
#' @param x The student's object.
#' @param y The solution object.
#' @return \code{TRUE} or \code{FALSE}, depending on whether or not the two
#' objects are considered the same. See details.
#' @details
#' The functions performs the following checks.
#' \describe{
#' \item{\code{are_classes_equal()}}{Identical classes.}
#' \item{\code{are_lengths_equal()}}{Identical length.}
#' \item{\code{are_dims_equal()}}{Identical dimensions. For objects with
#' \code{NULL} dimension, their length is used.}
#' \item{\code{are_igraphs_equal()}}{\code{igraphs} with identical edges and
#' directedness.}
#' \item{\code{are_corpora_equal()}}{Corpora with identical contents.}
#' }
#' @name equality
NULL

#' @rdname equality
#' @export
are_classes_equal <- function(x, y) {
  identical(class(x), class(y))
}

#' @rdname equality
#' @export
are_lengths_equal <- function(x, y) {
  identical(length(x), length(y))
}

DIM <- function(x)
{
  dim_x <- dim(x)
  if (is.null(dim_x)) {
    return(length(x))
  }
  dim_x
}

#' @rdname equality
#' @export
are_dims_equal <- function(x, y) {
  identical(DIM(x), DIM(y))
}

#' @importFrom igraph is_igraph
#' @importFrom igraph as_data_frame
#' @importFrom igraph is_directed
#' @rdname equality
#' @export
are_igraphs_equal <- function(x, y) {
  if(!is_igraph(y)) {
    stop("The solution object should be an `igraph` if you compare using `igraph_equals()`.")
  }
  are_classes_equal(x, y) &
    identical(as_data_frame(x), as_data_frame(y)) &
    identical(is_directed(x), is_directed(y))
}

#' @importFrom NLP content
#' @rdname equality
#' @export
are_corpora_equal <- function(x, y) {
  if(!inherits(y, "Corpus")) {
    stop("The solution object should be a `Corpus` if you compare using `corpus_equals()`.")
  }
  are_classes_equal(x, y) &
    identical(lapply(x, content), lapply(y, content))
}
