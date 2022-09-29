provAB <- function(ml, studyAreaName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]

  id <- which(ml[["Provincial Boundaries"]][["NAME_1"]] == "Alberta")
  AB <- ml[["Provincial Boundaries"]][id, ]
  raster::shapefile(AB, filename = file.path(dataDir, "AB_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  AB.ansr <- ml[["Alberta Natural Subregions"]]

  AB[["Name"]] <- AB[["NAME_1"]]
  AB.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                            studyArea = AB, useSAcrs = TRUE,
                            filename2 = file.path(dataDir, "AB_caribou.shp"),
                            overwrite = TRUE) %>%
    joinReportingPolygons(., AB)

  ml <- mapAdd(AB, ml, layerName = "AB", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "AB", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(AB.ansr, ml, layerName = "AB ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "AB ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(AB.caribou, ml, layerName = "AB Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "AB Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## AB FMU boundaries (replaces previously added FMU map, for use as reporting polygon)
  ml <- mapAdd(map = ml, layerName = "AB FMU Boundaries",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/open?id=1OH3b5pwjumm1ToytDBDI6jthVe2pp0tS", # 2020-06
               analysisGroupReportingPolygon = "AB FMU Boundaries", isStudyArea = FALSE,
               columnNameForLabels = "FMU_NAME", filename2 = NULL)
  ml[["AB FMU Boundaries"]][["Name"]] <- ml[["AB FMU Boundaries"]][["shinyLabel"]]

  ## AB Land Use Framework Planning Regions
  # TODO: clean up these polygons?
  ml <- mapAdd(map = ml, layerName = "AB Land Use Framework Planning Regions",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1RnLGnuX0r9EGJ11YL2mov7n-Vgke0uTC",
               analysisGroupReportingPolygon = "AB Land Use Framework Planning Regions", isStudyArea = FALSE,
               columnNameForLabels = "LUF_NAME", filename2 = NULL)
  ml[["AB Land Use Framework Planning Regions"]][["Name"]] <- ml[["AB Land Use Framework Planning Regions"]][["shinyLabel"]]

  ## AB regional planning units
  ml <- mapAdd(map = ml, layerName = "AB SUBR Bistcho Lake",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1taLQF-J69y7qweOsfDJ2f2wvTTvP_n6C",
               analysisGroupReportingPolygon = "AB SUBR Bistcho Lake", isStudyArea = FALSE,
               columnNameForLabels = "SUBR_NAME", filename2 = NULL)
  ml[["AB SUBR Bistcho Lake"]][["Name"]] <- ml[["AB SUBR Bistcho Lake"]][["shinyLabel"]]

  ml <- mapAdd(map = ml, layerName = "AB SUBR Cold Lake",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1jy4u2OyhnLjj1Wp_t27pPz_MsmMhPoSg",
               analysisGroupReportingPolygon = "AB SUBR Cold Lake", isStudyArea = FALSE,
               columnNameForLabels = "SUBR_NAME", filename2 = NULL)
  ml[["AB SUBR Cold Lake"]][["Name"]] <- ml[["AB SUBR Cold Lake"]][["shinyLabel"]]

  ml <- mapAdd(map = ml, layerName = "AB SUBR Upper Smoky",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1T2FgfHdHy41GsLnfSxWW0hpEqwo3f5VF",
               analysisGroupReportingPolygon = "AB SUBR Upper Smoky", isStudyArea = FALSE,
               columnNameForLabels = "SUBR_NAME", filename2 = NULL)
  ml[["AB SUBR Upper Smoky"]][["Name"]] <- ml[["AB SUBR Upper Smoky"]][["shinyLabel"]]

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  AB_sr <- postProcess(ml[["LandWeb Study Area"]],
                       studyArea = amc::outerBuffer(AB, bufferDist),
                       useSAcrs = TRUE,
                       filename2 = file.path(dataDir, "AB_SR.shp"),
                       overwrite = TRUE)

  plotFMA(AB, provs = ab, caribou = AB.caribou, xsr = AB_sr,
          title = "Alberta",
          png = file.path(dataDir, "AB.png"))
  #plotFMA(AB, provs = ab, caribou = AB.caribou, xsr = AB_sr,
  #        title = "Alberta", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(AB_sr, ml, isStudyArea = TRUE, layerName = "AB SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
