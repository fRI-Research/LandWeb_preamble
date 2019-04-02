fmaNWT <- function(ml, runName, dataDir, canProvs, asStudyArea = FALSE) {
  dataDirFMANWT <- file.path(dataDir, "FMANWT") %>% checkPath(create = TRUE)

  nwt <- canProvs[canProvs$NAME_1 %in% c("Northwest Territories"), ]
  fmanwt <- extractFMA(ml, "Fort Resolution")
  fmanwt.sp <- as(fmanwt, "SpatialPolygons")
  shapefile(fmanwt, filename = file.path(dataDirFMANWT, "FMANWT.shp"), overwrite = TRUE)

  ## reportingPolygons
  fmanwt.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                studyArea = fmanwt.sp, useSAcrs = TRUE,
                                filename2 = file.path(dataDirFMANWT, "FMANWT_caribou.shp"),
                                overwrite = TRUE)

  ml <- mapAdd(fmanwt, ml, layerName = "FMANWT", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "FMANWT", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(fmanwt.caribou, ml, layerName = "FMANWT Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "FMANWT Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
  names(ml$`FMANWT Caribou`) <- gsub("[.]1", "", names(ml$`FMANWT Caribou`))

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  fmanwt_sr <- postProcess(ml$`LandWeb Study Area`,
                          studyArea = amc::outerBuffer(fmanwt, 50000), # 50 km buffer
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDirFMANWT, "FMANWT_SR.shp"),
                          overwrite = TRUE)

  plotFMA(fmanwt, provs = nwt, caribou = fmanwt.caribou, xsr = fmanwt_sr,
          title = "Fort Resolution", png = file.path(dataDirFMANWT, "FMA_NWT.png"))
  #plotFMA(fmanwt, provs = nwt, caribou = fmanwt.caribou, xsr = fmanwt_sr,
  #        title = "Fort Resolution", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(fmanwt_sr, ml, isStudyArea = TRUE, layerName = "FMANWT SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}