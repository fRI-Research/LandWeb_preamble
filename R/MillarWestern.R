fmaMillarWestern <- function(ml, runName, dataDir, canProvs) {
  dataDirMillarWestern <- file.path(dataDir, "MillarWestern") %>% checkPath(create = TRUE)

  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  mw <- ml$`FMA Boundaries Updated`[grepl("Millar Western", ml$`FMA Boundaries Updated`$Name), ]
  #plot(spTransform(ab, crs(mw)))
  #plot(mw[, "Name"], main = "Millar Western full", col = "lightblue", add = TRUE)

  mw.sp <- as(mw, "SpatialPolygons")

  shapefile(mw, filename = file.path(dataDirMillarWestern, "Millar_Western.shp"), overwrite = TRUE)

  ## reportingPolygons
  mw.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                          studyArea = mw.sp, useSAcrs = TRUE,
                          filename2 = file.path(dataDirMillarWestern, "Millar_Western_ANSR.shp"),
                          overwrite = TRUE)
  #plot(mw.ansr, add = TRUE)

  mw.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                               studyArea = mw.sp, useSAcrs = TRUE,
                               filename2 = file.path(dataDirMillarWestern, "Millar_Western_caribou.shp"),
                               overwrite = TRUE)
  #plot(mw.caribou, col = "magenta", add = TRUE)

  ml <- mapAdd(mw, ml, layerName = "Millar Western", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Millar Western", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(mw.ansr, ml, layerName = "Millar Western ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Millar Western ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(mw.caribou, ml, layerName = "Millar Western Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Millar Western Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
  names(ml$`Millar Western ANSR`) <- gsub("[.]1", "", names(ml$`Millar Western ANSR`))
  names(ml$`Millar Western Caribou`) <- gsub("[.]1", "", names(ml$`Millar Western Caribou`))

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  mw_sr <- postProcess(ml$`LandWeb Study Area`,
                          studyArea = amc::outerBuffer(mw, 50000), # 50 km buffer
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDirMillarWestern, "Millar_Western_SR.shp"),
                          overwrite = TRUE)
  #plot(mw_sr)

  ml <- mapAdd(mw_sr, ml, isStudyArea = TRUE, layerName = "Millar Western SR",
               useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
               columnNameForLabels = "NSN", filename2 = NULL)

  return(ml)
}
