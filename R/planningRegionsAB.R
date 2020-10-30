processPSR <- function(ml, study_area_name, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  psr <- extractPSR(ml, study_area_name)                                                # extractPSR
  shapefile(psr, filename = file.path(dataDir, paste0(study_area_name, ".shp")), overwrite = TRUE)

  ## reportingPolygons
  psr.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                          studyArea = psr, useSAcrs = TRUE,
                          filename2 = file.path(dataDir, paste0(study_area_name, "_ANSR.shp")),
                          overwrite = TRUE) %>%
    joinReportingPolygons(., psr)
  psr.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                             studyArea = psr, useSAcrs = TRUE,
                             filename2 = file.path(dataDir, paste0(study_area_name, "_caribou.shp")),
                             overwrite = TRUE) %>%
    joinReportingPolygons(., psr)
  psr.caribou.joined <- SpatialPolygonsDataFrame(aggregate(psr.caribou),
                                                 data.frame(Name = "Caribou",
                                                            shinyLabel = "Caribou",
                                                            stringsAsFactors = FALSE)) %>%
    joinReportingPolygons(., psr)

  ml <- mapAdd(psr, ml, layerName = study_area_name, useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = study_area_name, isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(psr.ansr, ml, layerName = paste0(study_area_name, " ANSR"), useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = paste0(study_area_name, " ANSR"),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(psr.caribou, ml, layerName = paste0(study_area_name, " Caribou"), useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = paste0(study_area_name, " Caribou"),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(psr.caribou.joined, ml, layerName = paste0(study_area_name, " Caribou Joined"), useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = paste0(study_area_name, " Caribou Joined"),
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  psr_sr <- postProcess(ml[["LandWeb Study Area"]],
                        studyArea = amc::outerBuffer(psr, bufferDist),
                        useSAcrs = TRUE,
                        filename2 = file.path(dataDir, paste0(study_area_name, "_SR.shp")),
                        overwrite = TRUE)

  plotPSR(psr, provs = ab, caribou = psr.caribou, xsr = psr_sr, title = study_area_name,  #plotPSR
          png = file.path(dataDir, paste0(study_area_name, ".png")))

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(psr_sr, ml, isStudyArea = TRUE, layerName = paste0(study_area_name, " SR"),
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
