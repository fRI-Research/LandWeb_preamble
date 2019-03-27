fmaMistik <- function(ml, runName, dataDir, canProvs) {
  dataDirMistik <- file.path(dataDir, "Mistik") %>% checkPath(create = TRUE)


  ## reportingPolygons
  absk <- canProvs[canProvs$NAME_1 %in% c("Alberta", "Saskatchewan"), ]
  mistik <- extractFMA(ml, "Mistik")
  #plot(spTransform(absk, crs(mistik)))
  #plot(mistik[, "Name"], main = "Mistik full", col = "lightblue", add = TRUE)

  mistik.sp <- as(mistik, "SpatialPolygons")

  shapefile(mistik, filename = file.path(dataDirMistik, "Mistik.shp"), overwrite = TRUE)

  mistik.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                studyArea = mistik.sp, useSAcrs = TRUE,
                                filename2 = file.path(dataDirMistik, "Mistik_caribou.shp"),
                                overwrite = TRUE)
  #plot(mistik.caribou, col = "magenta", add = TRUE)

  ml <- mapAdd(mistik, ml, layerName = "Mistik", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Mistik", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(mistik.caribou, ml, layerName = "Mistik Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Mistik Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
  names(ml$`Mistik Caribou`) <- gsub("[.]1", "", names(ml$`Mistik Caribou`))

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  mistik_sr <- postProcess(ml$`LandWeb Study Area`,
                          studyArea = amc::outerBuffer(mistik, 50000), # 50 km buffer
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDirMistik, "Mistik_SR.shp"),
                          overwrite = TRUE)
  #plot(mistik_sr)

  ml <- mapAdd(mistik_sr, ml, isStudyArea = TRUE, layerName = "Mistik SR",
               useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
               columnNameForLabels = "NSN", filename2 = NULL)

  return(ml)
}
