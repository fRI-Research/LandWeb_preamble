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

#' Plot boudary polygon(s) for forest management areas
#'
#' @param x        \code{SpatialPolygons*} object corresponding to the FMA to be plotted
#'
#' @param provs    \code{SpatialPolygons*} object corresponding to the provincial
#'                 (or territorial) boundaries to plot
#'
#' @param caribou  Optional \code{SpatialPolygons*} object corresponding to caribou boundaries
#'
#' @param xsr      Optional \code{SpatialPolygons*} object corresponding to a buffered studyArea
#'
#' @param title    Character string to use for plot title
#'
#' @param png      Optional. If non-NULL, must be a valid file path to a write a png
#'
#' @export
#' @importFrom graphics dev.off png
#' @importFrom sp crs plot.SpatialPointsDataFrame spTransform
plotFMA <- function(x, provs, caribou = NULL, xsr = NULL, title = NULL, png = NULL) {
  if (!is.null(png)) png(filename = png, width = 1200, height = 800)
  plot(spTransform(provs, crs(x)))
  plot(x[, "Name"], main = title, col = "lightblue", add = TRUE)
  if (!is.null(caribou)) plot(caribou, col = "magenta", add = TRUE)
  if (!is.null(xsr)) plot(xsr, add = TRUE)
  if (!is.null(png)) dev.off()
}
