provMB <- function(ml, runName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  mb <- canProvs[canProvs$NAME_1 == "Manitoba", ]

  id <- which(ml[["Provincial Boundaries"]][["NAME_1"]] == "Manitoba")
  MB <- ml[["Provincial Boundaries"]][id, ]
  raster::shapefile(MB, filename = file.path(dataDir, "MB_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  MB[["Name"]] <- MB[["NAME_1"]]
  MB.natler <- postProcess(ml[["National Ecoregions"]],
                           studyArea = MB, useSAcrs = TRUE,
                           filename2 = file.path(dataDir, "SK_NATLER.shp")) %>%
    joinReportingPolygons(., MB)

  MB.caribou <- postProcess(ml[["MB Caribou Ranges"]],
                            studyArea = MB, useSAcrs = TRUE,
                            filename2 = file.path(dataDir, "MB_caribou.shp"),
                            overwrite = TRUE) %>%
    joinReportingPolygons(., MB)

  ml <- mapAdd(MB, ml, layerName = "MB", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MB", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(MB.natler, ml, layerName = "MB NATLER", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MB NATLER",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(MB.caribou, ml, layerName = "MB Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MB Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  MB_sr <- postProcess(ml[["LandWeb Study Area"]],
                       studyArea = amc::outerBuffer(MB, bufferDist),
                       useSAcrs = TRUE,
                       filename2 = file.path(dataDir, "MB_SR.shp"),
                       overwrite = TRUE)

  plotFMA(MB, provs = mb, caribou = MB.caribou, xsr = MB_sr,
          title = "Manitoba",
          png = file.path(dataDir, "MB.png"))
  # plotFMA(MB, provs = mb, caribou = MB.caribou, xsr = MB_sr,
  #         title = "Manitoba", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(MB_sr, ml, isStudyArea = TRUE, layerName = "MB SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
