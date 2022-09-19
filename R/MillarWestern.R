fmaMillarWestern <- function(ml, runName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  mw <- extractFMA(ml, "Millar Western Forest Products")
  raster::shapefile(mw, filename = file.path(dataDir, "Millar_Western.shp"), overwrite = TRUE)

  ## reportingPolygons
  mw.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                         studyArea = mw, useSAcrs = TRUE,
                         filename2 = file.path(dataDir, "Millar_Western_ANSR.shp"),
                         overwrite = TRUE) %>%
    joinReportingPolygons(., mw)

  mw.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                            studyArea = mw, useSAcrs = TRUE,
                            filename2 = file.path(dataDir, "Millar_Western_caribou.shp"),
                            overwrite = TRUE) %>%
    joinReportingPolygons(., mw)

  ml <- mapAdd(mw, ml, layerName = "Millar Western", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Millar Western", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(mw.ansr, ml, layerName = "Millar Western ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Millar Western ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(mw.caribou, ml, layerName = "Millar Western Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Millar Western Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  mw_sr <- postProcess(ml[["LandWeb Study Area"]],
                       studyArea = amc::outerBuffer(mw, bufferDist),
                       useSAcrs = TRUE,
                       filename2 = file.path(dataDir, "Millar_Western_SR.shp"),
                       overwrite = TRUE)

  plotFMA(mw, provs = ab, caribou = mw.caribou, xsr = mw_sr, title = "Millar Western",
          png = file.path(dataDir, "Millar_Western.png"))
  #plotFMA(mw, provs = ab, caribou = mw.caribou, xsr = mw_sr, title = "Millar Western", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(mw_sr, ml, isStudyArea = TRUE, layerName = "Millar Western SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
