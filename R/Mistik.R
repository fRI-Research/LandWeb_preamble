fmaMistik <- function(ml, runName, dataDir, canProvs, asStudyArea = FALSE) {
  dataDirMistik <- file.path(dataDir, "Mistik") %>% checkPath(create = TRUE)

  ## reportingPolygons
  absk <- canProvs[canProvs$NAME_1 %in% c("Alberta", "Saskatchewan"), ]
  mistik <- extractFMA(ml, "Mistik")
  shapefile(mistik, filename = file.path(dataDirMistik, "Mistik.shp"), overwrite = TRUE)

  mistik.caribou <- postProcess(ml$`LandWeb Caribou Ranges`,
                                studyArea = mistik, useSAcrs = TRUE,
                                filename2 = file.path(dataDirMistik, "Mistik_caribou.shp"),
                                overwrite = TRUE) %>%
    joinReportingPolygons(., mistik)

  ml <- mapAdd(mistik, ml, layerName = "Mistik", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Mistik", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(mistik.caribou, ml, layerName = "Mistik Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Mistik Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  mistik_sr <- postProcess(ml$`LandWeb Study Area`,
                          studyArea = amc::outerBuffer(mistik, 50000), # 50 km buffer
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDirMistik, "Mistik_SR.shp"),
                          overwrite = TRUE)

  plotFMA(mistik, provs = absk, caribou = mistik.caribou, xsr = mistik_sr, title = "Mistik",
          png = file.path(dataDirMistik, "Mistik.png"))
  #plotFMA(mistik, provs = absk, caribou = mistik.caribou, xsr = mistik_sr,
  #        title = "Mistik", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(mistik_sr, ml, isStudyArea = TRUE, layerName = "Mistik SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
