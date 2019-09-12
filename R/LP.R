fmaLP <- function(ml, runName, dataDir, canProvs, asStudyArea = FALSE) {
  dataDirLP <- file.path(dataDir, "LP") %>% checkPath(create = TRUE)

  ## There are 3 parts to the LP FMA: 2 in BC and one in MB.
  west <- canProvs[canProvs$NAME_1 %in% c("British Columbia", "Alberta",
                                          "Saskatchewan", "Manitoba"), ]
  lp <- extractFMA(ml, "Fort St\\. John|Dawson Creek|Mountain")
  shapefile(lp, filename = file.path(dataDirLP, "LP_full.shp"), overwrite = TRUE)

  if (grepl("LandWeb|LP_BC", runName)) {
    ## reportingPolygons
    lp_bc <- extractFMA(ml, "Fort St\\. John|Dawson Creek")
    shapefile(lp_bc, filename = file.path(dataDirLP, "LP_BC.shp"), overwrite = TRUE)

    lp_bc.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                 studyArea = lp_bc, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirLP, "LP_BC_caribou.shp"),
                                 overwrite = TRUE) %>%
      joinReportingPolygons(., lp_bc)

    ml <- mapAdd(lp_bc, ml, layerName = "LP BC", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP BC", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(lp_bc.caribou, ml, layerName = "LP BC Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP BC Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    lp_bc_sr <- postProcess(ml[["LandWeb Study Area"]],
                            studyArea = amc::outerBuffer(lp_bc, 50000), # 50 km buffer
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDirLP, "LP_BC_SR.shp"),
                            overwrite = TRUE)

    plotFMA(lp_bc, provs = west, caribou = lp_bc.caribou, xsr = lp_bc_sr, title = "LP BC",
            png = file.path(dataDirLP, "LP_BC.png"))
    #plotFMA(lp_bc, provs = west, caribou = lp_bc.caribou, xsr = lp_bc_sr, title = "LP BC", png = NULL)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(lp_bc_sr, ml, isStudyArea = TRUE, layerName = "LP BC SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }
  }

  if (grepl("LandWeb|LP_MB", runName)) {
    ## reportingPolygons
    lp_mb <- extractFMA(ml, "Mountain")
    shapefile(lp_mb, filename = file.path(dataDirLP, "LP_MB.shp"), overwrite = TRUE)

    lp_mb.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                 studyArea = lp_mb, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirLP, "LP_MB_caribou.shp"),
                                 overwrite = TRUE) %>%
      joinReportingPolygons(., lp_mb)

    ml <- mapAdd(lp_mb, ml, layerName = "LP MB", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP MB", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(lp_mb.caribou, ml, layerName = "LP MB Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "LP MB Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    lp_mb_sr <- postProcess(ml[["LandWeb Study Area"]],
                            studyArea = amc::outerBuffer(lp_mb, 50000), # 50 km buffer
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDirLP, "LP_MB_SR.shp"),
                            overwrite = TRUE)

    plotFMA(lp_mb, provs = west, caribou = lp_mb.caribou, xsr = lp_mb_sr, title = "LP MB",
            png = file.path(dataDirLP, "LP_MB.png"))
    #plotFMA(lp_mb, provs = west, caribou = lp_mb.caribou, xsr = lp_mb_sr, title = "LP MB", png = NULL)
    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(lp_mb_sr, ml, isStudyArea = TRUE, layerName = "LP MB SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }
  }

  return(ml)
}
