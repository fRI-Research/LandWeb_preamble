fmaMistik <- function(ml, studyAreaName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ## reportingPolygons
  absk <- canProvs[canProvs$NAME_1 %in% c("Alberta", "Saskatchewan"), ]
  mistik <- extractFMA(ml, "Mistik")
  raster::shapefile(mistik, filename = file.path(dataDir, "Mistik.shp"), overwrite = TRUE)

  mistik.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                studyArea = mistik, useSAcrs = TRUE,
                                filename2 = file.path(dataDir, "Mistik_caribou.shp"),
                                overwrite = TRUE) %>%
    joinReportingPolygons(., mistik)

  ml <- mapAdd(mistik, ml, layerName = "Mistik", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Mistik", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(mistik.caribou, ml, layerName = "Mistik Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Mistik Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  mistik_sr <- postProcess(ml[["LandWeb Study Area"]],
                           studyArea = amc::outerBuffer(mistik, bufferDist),
                           useSAcrs = TRUE,
                           filename2 = file.path(dataDir, "Mistik_SR.shp"),
                           overwrite = TRUE)

  plotFMA(mistik, provs = absk, caribou = mistik.caribou, xsr = mistik_sr, title = "Mistik",
          png = file.path(dataDir, "Mistik.png"))
  #plotFMA(mistik, provs = absk, caribou = mistik.caribou, xsr = mistik_sr,
  #        title = "Mistik", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(mistik_sr, ml, isStudyArea = TRUE, layerName = "Mistik SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
