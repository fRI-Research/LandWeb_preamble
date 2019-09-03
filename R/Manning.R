fmaManning <- function(ml, runName, dataDir, canProvs, asStudyArea = FALSE) {
  dataDirManning <- file.path(dataDir, "Manning") %>% checkPath(create = TRUE)

  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  manning <- extractFMA(ml, "Manning")
  shapefile(manning, filename = file.path(dataDirManning, "Manning_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  manning.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                              studyArea = manning, useSAcrs = TRUE,
                              filename2 = file.path(dataDirManning, "Manning_ANSR.shp"),
                              overwrite = TRUE) %>%
    joinReportingPolygons(., manning)
  manning.caribou <- postProcess(ml$`LandWeb Caribou Ranges`,
                                 studyArea = manning, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirManning, "Manning_caribou.shp"),
                                 overwrite = TRUE) %>%
    joinReportingPolygons(., manning)

  ml <- mapAdd(manning, ml, layerName = "Manning", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Manning", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(manning.ansr, ml, layerName = "Manning ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Manning ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(manning.caribou, ml, layerName = "Manning Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Manning Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  manning_sr <- postProcess(ml$`LandWeb Study Area`,
                            studyArea = amc::outerBuffer(manning, 50000), # 50 km buffer
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDirManning, "Manning_SR.shp"),
                            overwrite = TRUE)

  plotFMA(manning, provs = ab, caribou = manning.caribou, xsr = manning_sr,
          title = "Manning", png = file.path(dataDirManning, "Manning.png"))
  #plotFMA(manning, provs = ab, caribou = manning.caribou, xsr = manning_sr,
  #        title = "Manning", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(manning_sr, ml, isStudyArea = TRUE, layerName = "Manning SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
