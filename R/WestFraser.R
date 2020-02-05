fmaWestFraser <- function(ml, runName, dataDir, canProvs, asStudyArea = FALSE) {
  dataDirWestFraser <- file.path(dataDir, "WestFraser") %>% checkPath(create = TRUE)

  ## There are multiple parts to the WestFraser FMAs:
  ## - also includes Tolko_AB_S
  ## - also includes Blue Ridge
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  wf <- extractFMA(ml, "West Fraser|Blue Ridge")
  shapefile(wf, filename = file.path(dataDirWestFraser, "WestFraser_full.shp"), overwrite = TRUE)

  if (grepl("LandWeb|BlueRidge", runName)) {
    ## reportingPolygons
    wf_br <- wf[grepl("Blue Ridge", wf$Name), ] ## first subpolygon
    shapefile(wf_br, filename = file.path(dataDirWestFraser, "WestFraser_BlueRidge.shp"), overwrite = TRUE)

    wf_br.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                              studyArea = wf_br, useSAcrs = TRUE,
                              filename2 = file.path(dataDirWestFraser, "WestFraser_BlueRidge_ANSR"),
                              overwrite = TRUE) %>%
      joinReportingPolygons(., wf_br)
    ## NOTE: no intersecting caribou areas

    ml <- mapAdd(wf_br, ml, layerName = "West Fraser Blue Ridge", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser Blue Ridge", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_br.ansr, ml, layerName = "West Fraser Blue Ridge ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser Blue Ridge ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    wf_br_sr <- postProcess(ml[["LandWeb Study Area"]],
                            studyArea = amc::outerBuffer(wf_br, 25000), # 25 km buffer
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDirWestFraser, "WestFraser_BlueRidge_SR.shp"),
                            overwrite = TRUE)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(wf_br_sr, ml, isStudyArea = TRUE, layerName = "West Fraser Blue Ridge SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }

    plotFMA(wf_br, provs = ab, caribou = NULL, xsr = wf_br_sr, title = "West Fraser Blue Ridge",
            png = file.path(dataDirWestFraser, "WestFraser_BR.png"))
    # plotFMA(wf_br, provs = ab, caribou = NULL, xsr = wf_br_sr,
    #         title = "West Fraser Blue Ridge", png = NULL)
  }

  if (grepl("LandWeb|WestFraser_N", runName)) {
    ## reportingPolygons
    wf_n <- wf[c(2:3, 6), ] ## 3 FMAs: Slave Lake; shared w/ Tolko; shared vith Tolko & Vanderwell.
    shapefile(wf_n, filename = file.path(dataDirWestFraser, "WestFraser_N.shp"), overwrite = TRUE)

    wf_n.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                             studyArea = wf_n, useSAcrs = TRUE,
                             filename2 = file.path(dataDirWestFraser, "WestFraser_N_ANSR"),
                             overwrite = TRUE) %>%
      joinReportingPolygons(., wf_n)
    wf_n.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                studyArea = wf_n, useSAcrs = TRUE,
                                filename2 = file.path(dataDirWestFraser, "WestFraser_N_caribou.shp"),
                                overwrite = TRUE) %>%
      joinReportingPolygons(., wf_n)

    ml <- mapAdd(wf_n, ml, layerName = "West Fraser N", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser N", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_n.ansr, ml, layerName = "West Fraser N ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser N ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_n.caribou, ml, layerName = "West Fraser N Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser N Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    wf_n_sr <- postProcess(ml[["LandWeb Study Area"]],
                            studyArea = amc::outerBuffer(wf_n, 25000), # 25 km buffer
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDirWestFraser, "WestFraser_N_SR.shp"),
                            overwrite = TRUE)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(wf_n_sr, ml, isStudyArea = TRUE, layerName = "West Fraser N SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }

    plotFMA(wf_n, provs = ab, caribou = NULL, xsr = wf_n_sr, title = "West Fraser North",
            png = file.path(dataDirWestFraser, "WestFraser_N.png"))
    # plotFMA(wf_n, provs = ab, caribou = NULL, xsr = wf_s_sr,
    #         title = "West Fraser North", png = NULL)
  }

  if (grepl("LandWeb|WestFraser_S", runName)) {
    ## reportingPolygons
    wf_s <- wf[4:5, ] ## Hinton and Edson FMAs
    shapefile(wf_s, filename = file.path(dataDirWestFraser, "WestFraser_S.shp"), overwrite = TRUE)

    wf_s.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                             studyArea = wf_s, useSAcrs = TRUE,
                             filename2 = file.path(dataDirWestFraser, "WestFraser_S_ANSR"),
                             overwrite = TRUE) %>%
      joinReportingPolygons(., wf_s)
    wf_s.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                studyArea = wf_s, useSAcrs = TRUE,
                                filename2 = file.path(dataDirWestFraser, "WestFraser_S_caribou.shp"),
                                overwrite = TRUE) %>%
      joinReportingPolygons(., wf_s)

    ml <- mapAdd(wf_s, ml, layerName = "West Fraser S", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser S", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_s.ansr, ml, layerName = "West Fraser S ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser S ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_s.caribou, ml, layerName = "West Fraser S Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser S Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    wf_s_sr <- postProcess(ml[["LandWeb Study Area"]],
                           studyArea = amc::outerBuffer(wf_s, 25000), # 25 km buffer
                           useSAcrs = TRUE,
                           filename2 = file.path(dataDirWestFraser, "WestFraser_S_SR.shp"),
                           overwrite = TRUE)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(wf_s_sr, ml, isStudyArea = TRUE, layerName = "West Fraser S SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }

    plotFMA(wf_s, provs = ab, caribou = NULL, xsr = wf_s_sr, title = "West Fraser South",
            png = file.path(dataDirWestFraser, "WestFraser_S.png"))
    # plotFMA(wf_s, provs = ab, caribou = NULL, xsr = wf_s_sr,
    #         title = "West Fraser South", png = NULL)

  } ## TODO: add "All" option

  return(ml)
}
