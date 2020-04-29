fmaVanderwell <- function(ml, runName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ## NOTE: Vanderwell has 2 FMAS (close enough we can run all together):
  ## - one shared vith Tolko/WestFraser (Tolko_AB_S)
  ## - the other is just south of the first
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  vanderwell <- extractFMA(ml, "Vanderwell Contractors")
  shapefile(vanderwell, filename = file.path(dataDir, "Vanderwell_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  vanderwell.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                                 studyArea = vanderwell, useSAcrs = TRUE,
                                 filename2 = file.path(dataDir, "Vanderwell_ANSR.shp"),
                                 overwrite = TRUE) %>%
    joinReportingPolygons(., vanderwell)
  vanderwell.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                    studyArea = vanderwell, useSAcrs = TRUE,
                                    filename2 = file.path(dataDir, "Vanderwell_caribou.shp"),
                                    overwrite = TRUE) %>%
    joinReportingPolygons(., vanderwell)

  ml <- mapAdd(vanderwell, ml, layerName = "Vanderwell", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Vanderwell", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(vanderwell.ansr, ml, layerName = "Vanderwell ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Vanderwell ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(vanderwell.caribou, ml, layerName = "Vanderwell Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Vanderwell Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  vanderwell_sr <- postProcess(ml[["LandWeb Study Area"]],
                               studyArea = amc::outerBuffer(vanderwell, bufferDist),
                               useSAcrs = TRUE,
                               filename2 = file.path(dataDir, "Vanderwell_SR.shp"),
                               overwrite = TRUE)

  plotFMA(vanderwell, provs = ab, caribou = vanderwell.caribou, xsr = vanderwell_sr,
          title = "Vanderwell Contractors",
          png = file.path(dataDir, "Vanderwell.png"))
  #plotFMA(vanderwell, provs = ab, caribou = vanderwell.caribou, xsr = vanderwell_sr,
  #        title = "Vanderwell Contractors", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(vanderwell_sr, ml, isStudyArea = TRUE, layerName = "Vanderwell SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
