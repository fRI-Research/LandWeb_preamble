fmaEdsonFP <- function(ml, runName, dataDir, canProvs) {
  dataDirEdsonFP <- file.path(dataDir, "EdsonFP") %>% checkPath(create = TRUE)

  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  edson <- extractFMA(ml, "Edson")
  edson.sp <- as(edson, "SpatialPolygons")
  shapefile(edson, filename = file.path(dataDirEdsonFP, "EdsonFP.shp"), overwrite = TRUE)

  ## reportingPolygons
  edson.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                            studyArea = edson.sp, useSAcrs = TRUE,
                            filename2 = file.path(dataDirEdsonFP, "EdsonFP_ANSR.shp"),
                            overwrite = TRUE)
  ## NOTE: no caribou ranges intersect with this FMA

  ml <- mapAdd(edson, ml, layerName = "EdsonFP", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "EdsonFP", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(edson.ansr, ml, layerName = "EdsonFP ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "EdsonFP ANSR",
               columnNameForLabels = "Name", filename2 = NULL)

  ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
  names(ml$`EdsonFP ANSR`) <- gsub("[.]1", "", names(ml$`EdsonFP ANSR`))

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  edson_sr <- postProcess(ml$`LandWeb Study Area`,
                          studyArea = amc::outerBuffer(edson, 50000), # 50 km buffer
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDirEdsonFP, "EdsonFP_SR.shp"),
                          overwrite = TRUE)

  ml <- mapAdd(edson_sr, ml, isStudyArea = TRUE, layerName = "EdsonFP SR",
               useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
               columnNameForLabels = "NSN", filename2 = NULL)

  plotFMA(edson, provs = ab, caribou = edson.caribou, xsr = edson_sr,
          title = "EdsonFP", png = file.path(dataDirEdsonFP, "EdsonFP.png"))
  #plotFMA(edson, provs = ab, caribou = edson.caribou, xsr = edson_sr, title = "EdsonFP", png = NULL)

  return(ml)
}
