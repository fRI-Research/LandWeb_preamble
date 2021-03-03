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
  if (is.null(ml[["FMA Boundaries Updated"]])) stop("'FMA Boundaries Updated' not found")
  ml[["FMA Boundaries Updated"]][grepl(name, ml[["FMA Boundaries Updated"]][["Name"]]), ]
}

#' @rdname extractFMA
extractFMU <- function(ml, name) {
  if (is.null(ml[["AB FMU Boundaries"]])) stop("'AB FMU Boundaries' not found")
  ml[["AB FMU Boundaries"]][ml[["AB FMU Boundaries"]][["FMU_NAME"]] == name, ]
}

#' Join reporting polygons and intersect their features
#'
#' Join two reporting polygons, preserving their features;
#' concatenate the \code{Name.*} fields into a single \code{Name} field;
#' concatenate the \code{shinyLabel.*} fields into a single \code{shinyLabel} field.
#' E.g., if \code{x} and \code{y} each contain 2 features, the resulting object will contain 4
#' features (corresponding to \code{x1.y1}, \code{x1.y2}, \code{x2.y1}, and \code{x2.y2}).
#'
#' @param x,y a \code{SpatialPolygonsDataFrame}
#'
#' @return a \code{SpatialPolygonsDataFrame}
#'
#' @importFrom sf as st_as_sf st_join
joinReportingPolygons <- function(x, y) {
  if (is.null(x[["Name"]]) && !is.null(x[["Name.1"]]) && !is.null(x[["Name.2"]]) &&
      is.null(x[["shinyLabel"]]) && !is.null(x[["shinyLabel.1"]]) && !is.null(x[["shinyLabel.2"]])) {
    z <- x

    z[["Name"]] <- paste(z[["Name.2"]], z[["Name.1"]])
    z[["Name.1"]] <- z[["Name.2"]] <- NULL

    z[["shinyLabel"]] <- paste(z[["shinyLabel.2"]], z[["shinyLabel.1"]])
    z[["shinyLabel.1"]] <- z[["shinyLabel.2"]] <- NULL
  } else {
    z <- sf::st_intersection(fixErrors(sf::st_set_precision(sf::st_as_sf(x), 1e5)),
                             fixErrors(sf::st_set_precision(sf::st_as_sf(y), 1e5)))

    ## sfc_GEOMETRY may itself contain points, so filter them out
    z <- suppressWarnings(sf::st_collection_extract(z, "POLYGON"))

    z[["Name"]] <- paste(z[["Name"]], z[["Name.1"]])
    z[["Name.1"]] <- NULL

    z[["shinyLabel"]] <- paste(z[["shinyLabel"]], z[["shinyLabel.1"]])
    z[["shinyLabel.1"]] <- NULL

    z <- as(z, "Spatial")
  }

  return(z)
}

#' Plot boundary polygon(s) for forest management areas
#'
#' @param x        \code{SpatialPolygons*} object corresponding to the FMA to be plotted
#'
#' @param provs    \code{SpatialPolygons*} object corresponding to the provincial
#'                 (or territorial) boundaries to plot
#'
#' @param caribou  Optional \code{SpatialPolygons*} object corresponding to caribou boundaries
#'
#' @param xsr      Optional \code{SpatialPolygons*} object corresponding to a buffered \code{studyArea}
#'
#' @param title    Character string to use for plot title
#'
#' @param png      Optional. If non-NULL, must be a valid file path to a write a png
#'
#' @export
#' @importFrom graphics dev.off png
#' @importFrom sp crs plot spTransform
#' @rdname plotFMA
plotFMA <- function(x, provs, caribou = NULL, xsr = NULL, title = NULL, png = NULL) {
  provs <- spTransform(provs, crs(x))

  ## regular boring old plot
  if (!is.null(png)) png(filename = png, width = 1200, height = 800)
  sp::plot(provs)
  sp::plot(x[, "Name"], main = title, col = "lightblue", add = TRUE)
  if (!is.null(caribou)) sp::plot(caribou, col = "magenta", add = TRUE)
  if (!is.null(xsr)) sp::plot(xsr, add = TRUE)
  if (!is.null(png)) dev.off()

  ## sexy ggplot version
  x_gg <- plotGG(x, provs, caribou, png)

  if (!is.null(png)) {
    png2 <- gsub("[.]png", "_gg.png", png)
    ggsave(png2, x_gg, width = 6, height = 8) ## a bit slow...
  }
}

#' @export
#' @importFrom sp plot spTransform
#' @rdname plotFMA
plotLandWeb <- function(x, provs, caribou = NULL, xsr = NULL, title = NULL, png = NULL) {
  provs <- spTransform(provs, crs(x))

  ## regular boring old plot
  if (!is.null(png)) png(filename = png, width = 1800, height = 1200)
  sp::plot(provs)
  sp::plot(x, main = title, col = "lightblue", add = TRUE)
  if (!is.null(caribou)) sp::plot(caribou, col = "magenta", add = TRUE)
  if (!is.null(xsr)) sp::plot(xsr, add = TRUE)
  if (!is.null(png)) dev.off()

  ## sexy ggplot version
  x_gg <- plotGG(x, provs, caribou, png)

  if (!is.null(png)) {
    png2 <- gsub("[.]png", "_gg.png", png)
    ggsave(png2, x_gg, width = 6, height = 8) ## a bit slow...
  }
}

#' @export
#' @importFrom dplyr left_join
#' @importFrom ggplot2 aes coord_equal element_blank fortify geom_path geom_polygon ggsave hue_pal theme
#' @importFrom scales hue_pal show_col
#' @importFrom reproducible Cache
#' @rdname plotFMA
plotGG <- function(x, provs, caribou = NULL, png = NULL) {
  ## sexy ggplot version
  x@data$id <- rownames(x@data)
  x_pnts <- fortify(x, region = "id")
  x_df <- left_join(x_pnts, x@data, by = "id")

  provs@data$id <- rownames(provs@data)
  provs_pnts <- Cache(fortify, model = provs, region = "id") ## really freakin' slow!
  provs_df <- left_join(provs_pnts, provs@data, by = "id")

  #show_col(hue_pal()(16)) ## show some ggplot colours to choose from
  x_gg <- ggplot(provs_df) +
    aes(long, lat, group = group) +
    geom_polygon() +
    geom_path(color = "white") +
    coord_equal() +
    geom_polygon(data = x_df, color = "white", fill = hue_pal()(16)[11])

  if (!is.null(caribou)) {
    caribou@data$id <- rownames(caribou@data)
    caribou_pnts <- fortify(caribou, region = "id")
    caribou_df <- left_join(caribou_pnts, caribou@data, by = "id")

    x_gg <- x_gg + geom_polygon(data = caribou_df, color = "white", fill = hue_pal()(16)[15])
  }

  x_gg <- x_gg + theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
                       axis.text.x = element_blank(), axis.text.y = element_blank())

  return(x_gg)
}
