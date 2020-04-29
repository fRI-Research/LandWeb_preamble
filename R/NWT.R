fmaNWT <- function(ml, runName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  nwt <- canProvs[canProvs$NAME_1 %in% c("Northwest Territories"), ]
  fmanwt <- extractFMA(ml, "Fort Resolution|Fort Providence")
  shapefile(fmanwt, filename = file.path(dataDir, "FMA_NWT.shp"), overwrite = TRUE)

  fmanwt_FP <- extractFMA(ml, "Fort Providence") # FMANWT2
  shapefile(fmanwt_FP, filename = file.path(dataDir, "FMA_NWT_FP.shp"), overwrite = TRUE)

  fmanwt_FR <- extractFMA(ml, "Fort Resolution") # FMANWT1
  shapefile(fmanwt_FR, filename = file.path(dataDir, "FMA_NWT_FR.shp"), overwrite = TRUE)

  if (grepl("FMANWT2", runName)) {
    ## reportingPolygons
    fmanwt_FP.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                     studyArea = fmanwt_FP, useSAcrs = TRUE,
                                     filename2 = file.path(dataDir, "FMA_NWT_FP_caribou.shp"),
                                     overwrite = TRUE) %>%
      joinReportingPolygons(., fmanwt_FP)

    ml <- mapAdd(fmanwt_FP, ml, layerName = "FMANWT FP", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "FMANWT FP", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(fmanwt_FP.caribou, ml, layerName = "FMANWT FP Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "FMANWT FP Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    fmanwt_FP_sr <- postProcess(ml[["LandWeb Study Area"]],
                                studyArea = amc::outerBuffer(fmanwt_FP, bufferDist),
                                useSAcrs = TRUE,
                                filename2 = file.path(dataDir, "FMA_NWT_FP_SR.shp"),
                                overwrite = TRUE)

    plotFMA(fmanwt_FP, provs = nwt, caribou = fmanwt_FP.caribou, xsr = fmanwt_FP_sr,
            title = "Fort Providence", png = file.path(dataDir, "FMA_NWT_FP.png"))
    #plotFMA(fmanwt_FP, provs = nwt, caribou = fmanwt_FP.caribou, xsr = fmanwt_FP_sr,
    #        title = "Fort Resolution", png = NULL)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(fmanwt_FP_sr, ml, isStudyArea = TRUE, layerName = "FMANWT FP SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }
  } else {
    ## reportingPolygons
    fmanwt_FR.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                     studyArea = fmanwt_FR, useSAcrs = TRUE,
                                     filename2 = file.path(dataDir, "FMA_NWT_FR_caribou.shp"),
                                     overwrite = TRUE) %>%
      joinReportingPolygons(., fmanwt_FR)

    ml <- mapAdd(fmanwt_FR, ml, layerName = "FMANWT FR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "FMANWT FR", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(fmanwt_FR.caribou, ml, layerName = "FMANWT FR Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "FMANWT FR Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    fmanwt_FR_sr <- postProcess(ml[["LandWeb Study Area"]],
                                studyArea = amc::outerBuffer(fmanwt_FR, bufferDist),
                                useSAcrs = TRUE,
                                filename2 = file.path(dataDir, "FMA_NWT_FR_SR.shp"),
                                overwrite = TRUE)

    plotFMA(fmanwt_FR, provs = nwt, caribou = fmanwt_FR.caribou, xsr = fmanwt_FR_sr,
            title = "Fort Resolution", png = file.path(dataDir, "FMA_NWT_FR.png"))
    #plotFMA(fmanwt_FR, provs = nwt, caribou = fmanwt_FR.caribou, xsr = fmanwt_FR_sr,
    #        title = "Fort Resolution", png = NULL)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(fmanwt_FR_sr, ml, isStudyArea = TRUE, layerName = "FMANWT FR SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }
  }
  return(ml)
}
