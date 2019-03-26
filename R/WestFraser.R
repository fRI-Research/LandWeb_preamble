fmaWestFraser <- function(ml, runName, dataDir, canProvs) {
  dataDirWestFraser <- file.path(dataDir, "WestFraser") %>% checkPath(create = TRUE)

  ## There are multiple parts to the WestFraser FMAs:
  ## - also includes Tolko_AB_S
  ## - also includes Blue Ridge
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  wf <- ml$`FMA Boundaries Updated`[grepl("West Fraser|Blue Ridge",
                                          ml$`FMA Boundaries Updated`$Name), ]
  #plot(spTransform(ab, crs(wf)))
  #plot(wf[, "Name"], main = "WestFraser full", col = "lightblue", add = TRUE)

  shapefile(wf, filename = file.path(dataDirWestFraser, "WestFraser_full.shp"), overwrite = TRUE)

  if (grepl("BlueRidge", runName)) {
    ## reportingPolygons
    wf_br <- wf[grepl("Blue Ridge", wf$Name), ] ## first subpolygon
    wf_br.sp <- as(wf_br, "SpatialPolygons")
    #plot(wf_br, col = "blue", add = TRUE)
    shapefile(wf_br, filename = file.path(dataDirWestFraser, "WestFraser_BlueRidge.shp"), overwrite = TRUE)

    wf_br.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                             studyArea = wf_br.sp, useSAcrs = TRUE,
                             filename2 = file.path(dataDirWestFraser, "WestFraser_BlueRidge_ANSR"),
                             overwrite = TRUE)
    #plot(wf_br.ansr, col = "magenta", add = TRUE)

    ## NOTE: no intersecting caribou areas

    ml <- mapAdd(wf_br, ml, layerName = "West Fraser Blue Ridge", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser Blue Ridge", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_br.ansr, ml, layerName = "West Fraser Blue Ridge ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser Blue Ridge ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`West Fraser Blue Ridge ANSR`) <- gsub("[.]1", "", names(ml$`West Fraser Blue Ridge ANSR`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    wf_br_sr <- postProcess(ml$`LandWeb Study Area`,
                           studyArea = amc::outerBuffer(wf_br, 50000), # 50 km buffer
                           useSAcrs = TRUE,
                           filename2 = file.path(dataDirWestFraser, "WestFraser_BlueRidge_SR.shp"),
                           overwrite = TRUE)
    #plot(wf_br_sr, add = TRUE)

    ml <- mapAdd(wf_br_sr, ml, isStudyArea = TRUE, layerName = "West Fraser Blue Ridge SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  } else if (grepl("WestFraser_N", runName)) {
    ## reportingPolygons
    wf_n <- wf[c(2:3, 6), ]
    wf_n.sp <- as(wf_n, "SpatialPolygons")
    #plot(wf_n, col = "purple", add = TRUE)
    shapefile(wf_n, filename = file.path(dataDirWestFraser, "WestFraser_N.shp"), overwrite = TRUE)

    wf_n.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                             studyArea = wf_n.sp, useSAcrs = TRUE,
                             filename2 = file.path(dataDirWestFraser, "WestFraser_N_ANSR"),
                             overwrite = TRUE)
    #plot(wf_n.ansr, col = "magenta", add = TRUE)

    wf_n.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                 studyArea = wf_n.sp, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirWestFraser, "WestFraser_N_caribou.shp"),
                                 overwrite = TRUE)
    #plot(wf_n.caribou, col = "magenta", add = TRUE)

    ml <- mapAdd(wf_n, ml, layerName = "West Fraser N", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser N", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_n.ansr, ml, layerName = "West Fraser N ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser N ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_n.caribou, ml, layerName = "West Fraser N Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser N Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`West Fraser N ANSR`) <- gsub("[.]1", "", names(ml$`West Fraser N ANSR`))
    names(ml$`West Fraser N Caribou`) <- gsub("[.]1", "", names(ml$`West Fraser N Caribou`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    wf_n_sr <- postProcess(ml$`LandWeb Study Area`,
                            studyArea = amc::outerBuffer(wf_n, 50000), # 50 km buffer
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDirWestFraser, "WestFraser_N_SR.shp"),
                            overwrite = TRUE)
    #plot(wf_n_sr, add = TRUE)

    ml <- mapAdd(wf_n_sr, ml, isStudyArea = TRUE, layerName = "West Fraser N SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  } else if (grepl("WestFraser_S", runName)) {
    ## reportingPolygons
    wf_s <- wf[3:4,]
    wf_s.sp <- as(wf_s, "SpatialPolygons")
    #plot(wf_s, col = "orange", add = TRUE)
    shapefile(wf_s, filename = file.path(dataDirWestFraser, "WestFraser_S.shp"), overwrite = TRUE)

    wf_s.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                             studyArea = wf_s.sp, useSAcrs = TRUE,
                             filename2 = file.path(dataDirWestFraser, "WestFraser_S_ANSR"),
                             overwrite = TRUE)
    #plot(wf_s.ansr, col = "magenta", add = TRUE)

    wf_s.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                studyArea = wf_s.sp, useSAcrs = TRUE,
                                filename2 = file.path(dataDirWestFraser, "WestFraser_S_caribou.shp"),
                                overwrite = TRUE)
    #plot(wf_s.caribou, col = "magenta", add = TRUE)

    ml <- mapAdd(wf_s, ml, layerName = "West Fraser S", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser S", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_s.ansr, ml, layerName = "West Fraser S ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser S ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_s.caribou, ml, layerName = "West Fraser S Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser S Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`West Fraser S ANSR`) <- gsub("[.]1", "", names(ml$`West Fraser S ANSR`))
    names(ml$`West Fraser S Caribou`) <- gsub("[.]1", "", names(ml$`West Fraser S Caribou`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    wf_s_sr <- postProcess(ml$`LandWeb Study Area`,
                           studyArea = amc::outerBuffer(wf_s, 50000), # 50 km buffer
                           useSAcrs = TRUE,
                           filename2 = file.path(dataDirWestFraser, "WestFraser_S_SR.shp"),
                           overwrite = TRUE)
    #plot(wf_s_sr, add = TRUE)

    ml <- mapAdd(wf_s_sr, ml, isStudyArea = TRUE, layerName = "West Fraser S SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  } ## TODO: add "All" option

  return(ml)
}
