provMB <- function(ml, studyAreaName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  mb <- canProvs[canProvs$NAME_1 == "Manitoba", ]

  id <- which(ml[["Provincial Boundaries"]][["NAME_1"]] == "Manitoba")
  MB <- ml[["Provincial Boundaries"]][id, ]
  raster::shapefile(MB, filename = file.path(dataDir, "MB_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  MB[["Name"]] <- MB[["NAME_1"]]

  MB.natlez <- postProcess(ml[["National Ecozones"]],
                           studyArea = MB, useSAcrs = TRUE,
                           filename2 = file.path(dataDir, "MB_NATLEZ.shp")) %>%
    joinReportingPolygons(., MB)

  MB.natler <- postProcess(ml[["National Ecoregions"]],
                           studyArea = MB, useSAcrs = TRUE,
                           filename2 = file.path(dataDir, "MB_NATLER.shp")) %>%
    joinReportingPolygons(., MB)

  MB.caribou <- postProcess(ml[["MB Caribou Ranges"]],
                            studyArea = MB, useSAcrs = TRUE,
                            filename2 = file.path(dataDir, "MB_caribou.shp"),
                            overwrite = TRUE) %>%
    joinReportingPolygons(., MB)

  MB.fmla1 <- prepInputs(url = "https://drive.google.com/file/d/1cHt9irGx9PhoMhP3oSrTG758-vrzog--/",
                         studyArea = MB, useSAcrs = TRUE,
                         filename2 = file.path(dataDir, "MB_FMLA.shp"))
  MB.fmla1$Name <- MB.fmla1$FMLNO
  MB.fmla1$shinyLabel <- MB.fmla1$FMLNO
  MB.fmla1 <- joinReportingPolygons(MB.fmla1, MB)
  MB.fmla1$FML_NAME <- MB.fmla1$FMLNO

  MB.fmla23 <- prepInputs(url = "https://drive.google.com/file/d/18Uwivkqt97WYAse_cM2uC7Op-GmXUo6V/",
                          studyArea = MB, useSAcrs = TRUE,
                          filename2 = file.path(dataDir, "MB_FMLA.shp"))
  MB.fmla23$Name <- MB.fmla23$FML_NAME
  MB.fmla23$shinyLabel <- MB.fmla23$FML_NAME
  MB.fmla23 <- joinReportingPolygons(MB.fmla23, MB)

  MB.fmla <- bind(MB.fmla1, MB.fmla23)

  MB.fmu <- prepInputs(url = "https://drive.google.com/file/d/18IXDtSyrakMS5QKRi6IU4UWOwhR2fYxn/",
                       studyArea = MB, useSAcrs = TRUE,
                       filename2 = file.path(dataDir, "MB_FMU.shp"))
  MB.fmu$Name <- MB.fmu$SEC_NAME
  MB.fmu$shinyLabel <- MB.fmu$SEC_NAME
  MB.fmu <- joinReportingPolygons(MB.fmu, MB)

  ml <- mapAdd(MB, ml, layerName = "MB", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MB", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(MB.natlez, ml, layerName = "MB NATLEZ", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MB NATLEZ",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(MB.natler, ml, layerName = "MB NATLER", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MB NATLER",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(MB.caribou, ml, layerName = "MB Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MB Caribou",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(MB.fmla, ml, layerName = "MB FMLA", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MB FMLA",
               columnNameForLabels = "FML_NAME", filename2 = NULL)
  ml <- mapAdd(MB.fmu, ml, layerName = "MB FMS", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "MB FMS",
               columnNameForLabels = "SEC_NAME", filename2 = NULL)

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
