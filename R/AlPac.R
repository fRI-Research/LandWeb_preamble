fmaAlpac <- function(ml, runName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  alpac <- extractFMA(ml, "ALPAC")
  raster::shapefile(alpac, filename = file.path(dataDir, "Alpac.shp"), overwrite = TRUE)

  ## reportingPolygons
  alpac.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                            studyArea = alpac, useSAcrs = TRUE,
                            filename2 = file.path(dataDir, "Alpac_ANSR.shp"),
                            overwrite = TRUE) %>%
    joinReportingPolygons(., alpac)
  alpac.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                               studyArea = alpac, useSAcrs = TRUE,
                               filename2 = file.path(dataDir, "Alpac_caribou.shp"),
                               overwrite = TRUE) %>%
    joinReportingPolygons(., alpac)

  ml <- mapAdd(alpac, ml, layerName = "Alpac", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Alpac", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(alpac.ansr, ml, layerName = "Alpac ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Alpac ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(alpac.caribou, ml, layerName = "Alpac Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Alpac Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  alpac_sr <- postProcess(ml[["LandWeb Study Area"]],
                          studyArea = amc::outerBuffer(alpac, bufferDist),
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDir, "Alpac_SR.shp"),
                          overwrite = TRUE)

  plotFMA(alpac, provs = ab, caribou = alpac.caribou, xsr = alpac_sr, title = "Alpac",
          png = file.path(dataDir, "Alpac.png"))
  #plotFMA(alpac, provs = ab, caribou = alpac.caribou, xsr = alpac_sr, title = "Alpac", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(alpac_sr, ml, isStudyArea = TRUE, layerName = "Alpac SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
