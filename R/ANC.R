fmaANC <- function(ml, runName, dataDir, canProvs, asStudyArea = FALSE) {
  dataDirANC <- file.path(dataDir, "ANC") %>% checkPath(create = TRUE)

  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  anc <- extractFMA(ml, "ANC")
  shapefile(anc, filename = file.path(dataDirANC, "ANC.shp"), overwrite = TRUE)

  ## reportingPolygons
  anc.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                          studyArea = anc, useSAcrs = TRUE,
                          filename2 = file.path(dataDirANC, "ANC_ANSR.shp"),
                          overwrite = TRUE) %>%
    joinReportingPolygons(., anc)
  anc.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                             studyArea = anc, useSAcrs = TRUE,
                             filename2 = file.path(dataDirANC, "ANC_caribou.shp"),
                             overwrite = TRUE) %>%
    joinReportingPolygons(., anc)
  anc.caribou.joined <- SpatialPolygonsDataFrame(aggregate(anc.caribou),
                                                 data.frame(Name = "A La Peche & Little Smoky",
                                                            shinyLabel = "A La Peche & Little Smoky"))

  ml <- mapAdd(anc, ml, layerName = "ANC", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "ANC", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(anc.ansr, ml, layerName = "ANC ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "ANC ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(anc.caribou, ml, layerName = "ANC Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "ANC Caribou",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(anc.caribou.joined, ml, layerName = "ANC Caribou Joined", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "ANC Caribou Joined",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  anc_sr <- postProcess(ml[["LandWeb Study Area"]],
                        studyArea = amc::outerBuffer(anc, 50000), # 50 km buffer
                        useSAcrs = TRUE,
                        filename2 = file.path(dataDirANC, "ANC_SR.shp"),
                        overwrite = TRUE)

  plotFMA(anc, provs = ab, caribou = anc.caribou, xsr = anc_sr, title = "ANC",
          png = file.path(dataDirANC, "ANC.png"))
  #plotFMA(anc, provs = ab, caribou = anc.caribou, xsr = anc_sr, title = "ANC", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(anc_sr, ml, isStudyArea = TRUE, layerName = "ANC SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
