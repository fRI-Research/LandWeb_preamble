provSK <- function(ml, runName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  sk <- canProvs[canProvs$NAME_1 == "Saskatchewan", ]

  id <- which(ml[["Provincial Boundaries"]][["NAME_1"]] == "Saskatchewan")
  SK <- ml[["Provincial Boundaries"]][id, ]
  shapefile(SK, filename = file.path(dataDir, "SK_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  SK[["Name"]] <- SK[["NAME_1"]]
  SK.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                            studyArea = SK, useSAcrs = TRUE,
                            filename2 = file.path(dataDir, "SK_caribou.shp"),
                            overwrite = TRUE) %>%
    joinReportingPolygons(., SK)

  ml <- mapAdd(SK, ml, layerName = "SK", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SK", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(SK.caribou, ml, layerName = "SK Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SK Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  SK_sr <- postProcess(ml[["LandWeb Study Area"]],
                       studyArea = amc::outerBuffer(SK,bufferDist),
                       useSAcrs = TRUE,
                       filename2 = file.path(dataDir, "SK_SR.shp"),
                       overwrite = TRUE)

  plotFMA(SK, provs = sk, caribou = SK.caribou, xsr = SK_sr,
          title = "Saskatchewan",
          png = file.path(dataDir, "SK.png"))
  #plotFMA(SK, provs = sk, caribou = SK.caribou, xsr = SK_sr,
  #        title = "Saskatchewan", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(SK_sr, ml, isStudyArea = TRUE, layerName = "SK SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
