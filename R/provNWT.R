provNWT <- function(ml, runName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  nwt <- canProvs[canProvs$NAME_1 %in% c("Northwest Territories"), ]

  id <- which(ml[["Provincial Boundaries"]][["NAME_1"]] == "Northwest Territories")
  NWT <- ml[["Provincial Boundaries"]][id, ]
  raster::shapefile(NWT, filename = file.path(dataDir, "NWT_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  NWT[["Name"]] <- NWT[["NAME_1"]]
  NWT.nwter <- postProcess(ml[["Northwest Territories Ecoregions"]],
                           studyArea = NWT, useSAcrs = TRUE,
                           filename2 = file.path(dataDir, "NWT_NWTER.shp")) %>%
    joinReportingPolygons(., NWT)
  NWT.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                             studyArea = NWT, useSAcrs = TRUE,
                             filename2 = file.path(dataDir, "NWT_caribou.shp"),
                             overwrite = TRUE) %>%
    joinReportingPolygons(., NWT)

  ml <- mapAdd(NWT, ml, layerName = "NWT", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "NWT", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(NWT.nwter, ml, layerName = "NWT NWTER", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "NWT NWTER",
               columnNameForLabels = "ECO4_NAM_1", filename2 = NULL)
  ml <- mapAdd(NWT.caribou, ml, layerName = "NWT Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "NWT Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  NWT_sr <- postProcess(ml[["LandWeb Study Area"]],
                        studyArea = amc::outerBuffer(NWT, bufferDist),
                        useSAcrs = TRUE,
                        filename2 = file.path(dataDir, "NWT_SR.shp"),
                        overwrite = TRUE)

  plotFMA(NWT, provs = nwt, caribou = NWT.caribou, xsr = NWT_sr,
          title = "Northwest Territories",
          png = file.path(dataDir, "NWT.png"))
  #plotFMA(NWT, provs = nwt, caribou = NWT.caribou, xsr = NWT_sr,
  #        title = "Northwest Territories", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(NWT_sr, ml, isStudyArea = TRUE, layerName = "NWT SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
