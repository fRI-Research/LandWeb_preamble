fmaLP <- function(ml, runName, dataDir, canProvs) {
  dataDirLP <- file.path(dataDir, "LP") %>% checkPath(create = TRUE)

  ## There are 3 parts to the LP FMA: 2 in BC and one in MB.
  manitoba <- canProvs[canProvs$NAME_1 %in% c("Manitoba"), ]
  west <- canProvs[canProvs$NAME_1 %in% c("British Columbia", "Alberta", "Saskatchewan"), ]
  lp <- ml$`FMA Boundaries Updated`[grepl("Fort St\\. John|Dawson Creek|Mountain",
                                          ml$`FMA Boundaries Updated`$Name), ]
  #plot(spTransform(west, crs(lp)))
  #plot(spTransform(manitoba, crs(lp)), add = TRUE)
  #plot(lp[, "Name"], main = "LP full", col = "lightblue", add = TRUE)

  shapefile(lp, filename = file.path(dataDirLP, "LP_full.shp"), overwrite = TRUE)

  if (grepl("LP_BC", runName)) {
    ## reportingPolygons
    lp_bc <- ml$`FMA Boundaries Updated`[grepl("Fort St\\. John|Dawson Creek",
                                               ml$`FMA Boundaries Updated`$Name), ]
    lp_bc.sp <- as(lp_bc, "SpatialPolygons")
    #plot(lp_bc, col = "purple", add = TRUE)
    shapefile(lp_bc, filename = file.path(dataDirLP, "LP_BC.shp"), overwrite = TRUE)

    lp_bc.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                 studyArea = lp_bc.sp, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirLP, "LP_BC_caribou.shp"),
                                 overwrite = TRUE)
    #plot(lp_bc.caribou, col = "magenta", add = TRUE)

    ml <- mapAdd(lp_bc, ml, layerName = "LP BC", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP BC", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(lp_bc.caribou, ml, layerName = "LP BC Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP BC Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`LP BC Caribou`) <- gsub("[.]1", "", names(ml$`LP BC Caribou`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    lp_bc_sr <- postProcess(ml$`LandWeb Study Area`,
                            studyArea = amc::outerBuffer(lp_bc, 50000), # 50 km buffer
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDirLP, "LP_BC_SR.shp"),
                            overwrite = TRUE)
    #plot(lp_bc_sr, add = TRUE)

    ml <- mapAdd(lp_bc_sr, ml, isStudyArea = TRUE, layerName = "LP BC SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  } else if (grepl("LP_MB", runName)) {
    ## reportingPolygons
    lp_mb <- ml$`FMA Boundaries Updated`[grepl("Mountain", ml$`FMA Boundaries Updated`$Name), ]
    lp_mb.sp <- as(lp_mb, "SpatialPolygons")
    #plot(lp_mb, col = "purple", add = TRUE)
    shapefile(lp_mb, filename = file.path(dataDirLP, "LP_MB.shp"), overwrite = TRUE)

    lp_mb.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                 studyArea = lp_mb.sp, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirLP, "LP_MB_caribou.shp"),
                                 overwrite = TRUE)
    #plot(lp_mb.caribou, col = "magenta", add = TRUE)

    ml <- mapAdd(lp_mb, ml, layerName = "LP MB", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP MB", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(lp_mb.caribou, ml, layerName = "LP MB Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP MB Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`LP MB Caribou`) <- gsub("[.]1", "", names(ml$`LP MB Caribou`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    lp_mb_sr <- postProcess(ml$`LandWeb Study Area`,
                            studyArea = amc::outerBuffer(lp_mb, 50000), # 50 km buffer
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDirLP, "LP_MB_SR.shp"),
                            overwrite = TRUE)
    #plot(lp_mb_sr, add = TRUE)

    ml <- mapAdd(lp_mb_sr, ml, isStudyArea = TRUE, layerName = "LP MB SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
