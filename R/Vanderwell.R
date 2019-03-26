fmaVanderwell <- function(ml, runName, dataDir, canProvs) {
  dataDirVanderwell <- file.path(dataDir, "Vanderwell") %>% checkPath(create = TRUE)

  ## NOTE: Vanderwell has 2 FMAS (close enough we can run all together):
  ## - one shared vith Tolko/WestFraser (Tolko_AB_S)
  ## - the other is just south of the first
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  vanderwell <- ml$`FMA Boundaries Updated`[grepl("Vanderwell Contractors",
                                          ml$`FMA Boundaries Updated`$Name), ]
  vanderwell.sp <- as(vanderwell, "SpatialPolygons")
  #plot(spTransform(ab, crs(vanderwell)))
  #plot(vanderwell[, "Name"], main = "Vanderwell full", col = "lightblue", add = TRUE)

  shapefile(vanderwell, filename = file.path(dataDirVanderwell, "Vanderwell_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  vanderwell.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                                 studyArea = vanderwell.sp, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirVanderwell, "Vanderwell_ANSR.shp"),
                                 overwrite = TRUE)
  #plot(vanderwell.ansr, col = "magenta", add = TRUE)
  vanderwell.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                    studyArea = vanderwell.sp, useSAcrs = TRUE,
                                    filename2 = file.path(dataDirVanderwell, "Vanderwell_caribou.shp"),
                                    overwrite = TRUE)
  #plot(vanderwell.caribou, col = "magenta", add = TRUE)

  ml <- mapAdd(vanderwell, ml, layerName = "Vanderwell", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Vanderwell", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(vanderwell.ansr, ml, layerName = "Vanderwell ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Vanderwell ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(vanderwell.caribou, ml, layerName = "Vanderwell Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Vanderwell Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
  names(ml$`Vanderwell ANSR`) <- gsub("[.]1", "", names(ml$`Vanderwell ANSR`))
  names(ml$`Vanderwell Caribou`) <- gsub("[.]1", "", names(ml$`Vanderwell Caribou`))

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  vanderwell_sr <- postProcess(ml$`LandWeb Study Area`,
                          studyArea = amc::outerBuffer(vanderwell, 50000), # 50 km buffer
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDirVanderwell, "Vanderwell_SR.shp"),
                          overwrite = TRUE)
  #plot(vanderwell_sr)

  ml <- mapAdd(vanderwell_sr, ml, isStudyArea = TRUE, layerName = "Vanderwell SR",
               useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
               columnNameForLabels = "NSN", filename2 = NULL)

  return(ml)
}
