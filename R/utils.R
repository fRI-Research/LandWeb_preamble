#' Extract boundary polygon(s) for forest management area(s)
#'
#' @param ml    A \code{map} object containing 'FMA Boundary Updated'
#'              \code{SpatialPolygonsDataFrame} object with FMA boundaries
#'
#' @param name  A character (regex) string to match.
#'
#' @return \code{SpatialPolygonsDataFrame}
#'
#' @export
#'
extractFMA <- function(ml, name) {
  if (is.null(ml$`FMA Boundaries Updated`)) stop("'FMA Boundaries Updated' not found")
  ml$`FMA Boundaries Updated`[grepl(name, ml$`FMA Boundaries Updated`$Name), ]
}
