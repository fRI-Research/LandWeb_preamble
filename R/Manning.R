fmaManning <- function(ml, runName, dataDir, canProvs) {
  dataDirManning <- file.path(dataDir, "Manning") %>% checkPath(create = TRUE)

  ## reportingPolygons
  manning <- ml$`FMA Boundaries Updated`[grepl("Manning", ml$`FMA Boundaries Updated`$Name), ]
  #plot(manning, main = "Manning", col = "lightblue")
  manning.sp <- as(manning, "SpatialPolygons")
  shapefile(manning, filename = file.path(dataDirManning, "Manning_full.shp"), overwrite = TRUE)

  manning.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                              studyArea = manning.sp, useSAcrs = TRUE,
                              filename2 = file.path(dataDirManning, "Manning_ANSR.shp"),
                              overwrite = TRUE)
  #plot(manning.ansr, add = TRUE)

  manning.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                 studyArea = manning.sp, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirManning, "Manning_caribou.shp"),
                                 overwrite = TRUE)
  #plot(manning.caribou, col = "magenta", add = TRUE)

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
  #plot(manning_sr)

  ml <- mapAdd(manning_sr, ml, isStudyArea = TRUE, layerName = "Manning SR",
               useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
               columnNameForLabels = "NSN", filename2 = NULL)

  return(ml)
}
