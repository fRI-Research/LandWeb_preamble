fmaManning <- function(ml, runName, dataDir, canProvs) {
  dataDirManning <- file.path(dataDir, "Manning") %>% checkPath(create = TRUE)

  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  manning <- extractFMA(ml, "Manning")
  manning.sp <- as(manning, "SpatialPolygons")
  shapefile(manning, filename = file.path(dataDirManning, "Manning_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  manning.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                              studyArea = manning.sp, useSAcrs = TRUE,
                              filename2 = file.path(dataDirManning, "Manning_ANSR.shp"),
                              overwrite = TRUE)
  manning.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                 studyArea = manning.sp, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirManning, "Manning_caribou.shp"),
                                 overwrite = TRUE)

  ml <- mapAdd(manning, ml, layerName = "Manning", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Manning", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(manning.ansr, ml, layerName = "Manning ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Manning ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(manning.caribou, ml, layerName = "Manning Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Manning Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
  names(ml$`Manning ANSR`) <- gsub("[.]1", "", names(ml$`Manning ANSR`))
  names(ml$`Manning Caribou`) <- gsub("[.]1", "", names(ml$`Manning Caribou`))

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  manning_sr <- postProcess(ml$`LandWeb Study Area`,
                            studyArea = amc::outerBuffer(manning, 50000), # 50 km buffer
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDirManning, "Manning_SR.shp"),
                            overwrite = TRUE)

  ml <- mapAdd(manning_sr, ml, isStudyArea = TRUE, layerName = "Manning SR",
               useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
               columnNameForLabels = "NSN", filename2 = NULL)
  plotFMA(manning, provs = ab, caribou = manning.caribou, xsr = manning_sr,
          title = "Manning", png = file.path(dataDirManning, "Manning.png"))
  #plotFMA(manning, provs = ab, caribou = manning.caribou, xsr = manning_sr,
  #        title = "Manning", png = NULL)

  return(ml)
}
