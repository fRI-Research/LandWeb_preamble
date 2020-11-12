fmaWeyCo <- function(ml, runName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ## There are 3 parts to the WeyCo FMA: two in AB and one in SK
  absk <- canProvs[canProvs$NAME_1 %in% c("Alberta", "Saskatchewan"), ]
  weyco <- extractFMA(ml, "Weyerhaeuser|Pasquia-Porcupine")
  shapefile(weyco, filename = file.path(dataDir, "WeyCo_full.shp"), overwrite = TRUE)

  if (grepl("LandWeb|WeyCo_GP", runName)) {
    ## reportingPolygons
    weyco_gp <- extractFMA(ml, "Weyerhaeuser Company Limited \\(Grande Prairie\\)")
    shapefile(weyco_gp, filename = file.path(dataDir, "WeyCo_GP.shp"), overwrite = TRUE)

    weyco_gp.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                                 studyArea = weyco_gp, useSAcrs = TRUE,
                                 filename2 = file.path(dataDir, "WeyCo_GP_ANSR.shp"),
                                 overwrite = TRUE) %>%
      joinReportingPolygons(., weyco_gp)

    weyco_gp.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                    studyArea = weyco_gp, useSAcrs = TRUE,
                                    filename2 = file.path(dataDir, "WeyCo_GP_Caribou.shp"),
                                    overwrite = TRUE) %>%
      joinReportingPolygons(., weyco_gp)

    ml <- mapAdd(weyco_gp, ml, layerName = "WeyCo GP", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo GP", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(weyco_gp.ansr, ml, layerName = "WeyCo GP ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo GP ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(weyco_gp.caribou, ml, layerName = "WeyCo GP Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo GP Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    weyco_gp_sr <- postProcess(ml[["LandWeb Study Area"]],
                               studyArea = amc::outerBuffer(weyco_gp, bufferDist),
                               useSAcrs = TRUE,
                               filename2 = file.path(dataDir, "WeyCo_GP_SR.shp"),
                               overwrite = TRUE)

    plotFMA(weyco_gp, provs = absk, caribou = weyco_gp.caribou, xsr = weyco_gp_sr,
            title = "Weyerhaeuser Company Limited (Grande Prairie)",
            png = file.path(dataDir, "WeyCo_GP.png"))
    #plotFMA(weyco_gp, provs = absk, caribou = weyco_gp.caribou, xsr = weyco_gp_sr,
    #        title = "Weyerhaeuser Company Limited (Grande Prairie)", png = NULL)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(weyco_gp_sr, ml, isStudyArea = TRUE, layerName = "WeyCo GP SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }
  }

  if (grepl("LandWeb|WeyCo_PT|WeyCo_Pembina", runName)) {
    ## reportingPolygons
    weyco_pt <- extractFMA(ml, "Weyerhaeuser Company Limited \\(Pembina Timberland\\)")
    shapefile(weyco_pt, filename = file.path(dataDir, "WeyCo_PT.shp"), overwrite = TRUE)

    weyco_pt.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                                 studyArea = weyco_pt, useSAcrs = TRUE,
                                 filename2 = file.path(dataDir, "WeyCo_PT_ANSR.shp"),
                                 overwrite = TRUE) %>%
      joinReportingPolygons(., weyco_pt)
    ## NOTE: no caribou areas intersect with this FMA

    ml <- mapAdd(weyco_pt, ml, layerName = "WeyCo PT", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo PT", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(weyco_pt.ansr, ml, layerName = "WeyCo PT ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo PT ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    weyco_pt_sr <- postProcess(ml[["LandWeb Study Area"]],
                               studyArea = amc::outerBuffer(weyco_pt, bufferDist),
                               useSAcrs = TRUE,
                               filename2 = file.path(dataDir, "WeyCo_PT_SR.shp"),
                               overwrite = TRUE)

    plotFMA(weyco_pt, provs = absk, caribou = NULL, xsr = weyco_pt_sr,
            title = "Weyerhaeuser Company Limited (Pembina Timberland)",
            png = file.path(dataDir, "WeyCo_PT.png"))
    #plotFMA(weyco_pt, provs = absk, caribou = NULL, xsr = weyco_pt_sr,
    #        title = "Weyerhaeuser Company Limited (Pembina Timberland)", png = NULL)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(weyco_pt_sr, ml, isStudyArea = TRUE, layerName = "WeyCo PT SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }
  }

  if (grepl("LandWeb|WeyCo_SK", runName)) {
    ## reportingPolygons
    weyco_sk <- extractFMA(ml, "Pasquia-Porcupine")
    shapefile(weyco_sk, filename = file.path(dataDir, "WeyCo_SK.shp"), overwrite = TRUE)

    weyco_sk.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                    studyArea = weyco_sk, useSAcrs = TRUE,
                                    filename2 = file.path(dataDir, "WeyCo_SK_Caribou.shp"),
                                    overwrite = TRUE) %>%
      joinReportingPolygons(., weyco_sk)

    ml <- mapAdd(weyco_sk, ml, layerName = "WeyCo SK", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo SK", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(weyco_sk.caribou, ml, layerName = "WeyCo SK Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo SK Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    weyco_sk_sr <- postProcess(ml[["LandWeb Study Area"]],
                               studyArea = amc::outerBuffer(weyco_sk, bufferDist),
                               useSAcrs = TRUE,
                               filename2 = file.path(dataDir, "WeyCo_SK_SR.shp"),
                               overwrite = TRUE)

    plotFMA(weyco_sk, provs = absk, caribou = weyco_sk.caribou, xsr = weyco_sk_sr,
            title = "Weyerhaeuser Company Limited (Pasquia-Porcupine)",
            png = file.path(dataDir, "WeyCo_SK.png"))
    #plotFMA(weyco_sk, provs = absk, caribou = weyco_sk.caribou, xsr = weyco_sk_sr,
    #        title = "Weyerhaeuser Company Limited (Pasquia-Porcupine)", png = NULL)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(weyco_sk_sr, ml, isStudyArea = TRUE, layerName = "WeyCo SK SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }
  }

  return(ml)
}
