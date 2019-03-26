fmaWeyCo <- function(ml, runName, dataDir, canProvs) {
  dataDirWeyCo <- file.path(dataDir, "WeyCo") %>% checkPath(create = TRUE)

  ## There are 3 parts to the WeyCo FMA: 2 in BC and one in MB.
  absk <- canProvs[canProvs$NAME_1 %in% c("Alberta", "Saskatchewan"), ]
  weyco <- ml$`FMA Boundaries Updated`[grepl("Weyerhaeuser|Pasquia-Porcupine",
                                          ml$`FMA Boundaries Updated`$Name), ]
  #plot(spTransform(absk, crs(weyco)))
  #plot(weyco[, "Name"], main = "WeyCo full", col = "lightblue", add = TRUE)

  shapefile(weyco, filename = file.path(dataDirWeyCo, "WeyCo_full.shp"), overwrite = TRUE)

  if (grepl("WeyCo_GP", runName)) {
    ## reportingPolygons
    weyco_gp <- ml$`FMA Boundaries Updated`[grepl("Weyerhaeuser Company Limited \\(Grande Prairie\\)",
                                                  ml$`FMA Boundaries Updated`$Name), ]
    weyco_gp.sp <- as(weyco_gp, "SpatialPolygons")
    #plot(weyco_gp, col = "purple", add = TRUE)
    shapefile(weyco_gp, filename = file.path(dataDirWeyCo, "WeyCo_GP.shp"), overwrite = TRUE)

    weyco_gp.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                                 studyArea = weyco_gp.sp, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirWeyCo, "WeyCo_GP_ANSR.shp"),
                                 overwrite = TRUE)
    #plot(weyco_gp.ansr, add = TRUE)

    ml <- mapAdd(weyco_gp, ml, layerName = "WeyCo GP", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo GP", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(weyco_gp.ansr, ml, layerName = "WeyCo GP ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo GP ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`WeyCo PT ANSR`) <- gsub("[.]1", "", names(ml$`WeyCo PT ANSR`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    weyco_gp_sr <- postProcess(ml$`LandWeb Study Area`,
                               studyArea = amc::outerBuffer(weyco_gp, 50000), # 50 km buffer
                               useSAcrs = TRUE,
                               filename2 = file.path(dataDirWeyCo, "WeyCo_GP_SR.shp"),
                               overwrite = TRUE)
    #plot(weyco_gp_sr)

    ml <- mapAdd(weyco_gp_sr, ml, isStudyArea = TRUE, layerName = "WeyCo GP SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  } else if (grepl("WeyCo_PT|WeyCo_Pembina", runName)) {
    ## reportingPolygons
    weyco_pt <- ml$`FMA Boundaries Updated`[grepl("Weyerhaeuser Company Limited \\(Pembina Timberland\\)",
                                                  ml$`FMA Boundaries Updated`$Name), ]
    weyco_pt.sp <- as(weyco_pt, "SpatialPolygons")
    #plot(weyco_pt, col = "purple", add = TRUE)
    shapefile(weyco_pt, filename = file.path(dataDirWeyCo, "WeyCo_PT.shp"), overwrite = TRUE)

    weyco_pt.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                                 studyArea = weyco_pt.sp, useSAcrs = TRUE,
                                 filename2 = file.path(dataDirWeyCo, "WeyCo_PT_ANSR.shp"),
                                 overwrite = TRUE)
    #plot(weyco_pt.ansr, add = TRUE)

    ml <- mapAdd(weyco_pt, ml, layerName = "WeyCo PT", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo PT", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(weyco_pt.ansr, ml, layerName = "WeyCo PT ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo PT ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`WeyCo PT ANSR`) <- gsub("[.]1", "", names(ml$`WeyCo PT ANSR`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    weyco_pt_sr <- postProcess(ml$`LandWeb Study Area`,
                               studyArea = amc::outerBuffer(weyco_pt, 50000), # 50 km buffer
                               useSAcrs = TRUE,
                               filename2 = file.path(dataDirWeyCo, "WeyCo_PT_SR.shp"),
                               overwrite = TRUE)
    #plot(weyco_pt_sr)

    ml <- mapAdd(weyco_pt_sr, ml, isStudyArea = TRUE, layerName = "WeyCo PT SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  } else if (grepl("WeyCo_SK", runName)) {
    ## reportingPolygons
    weyco_sk <- ml$`FMA Boundaries Updated`[grepl("Pasquia-Porcupine",
                                                  ml$`FMA Boundaries Updated`$Name), ]
    weyco_sk.sp <- as(weyco_sk, "SpatialPolygons")
    #plot(weyco_sk, col = "purple", add = TRUE)
    shapefile(weyco_sk, filename = file.path(dataDirWeyCo, "WeyCo_SK.shp"), overwrite = TRUE)

    weyco_sk.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                    studyArea = weyco_sk.sp, useSAcrs = TRUE,
                                    filename2 = file.path(dataDirWeyCo, "WeyCo_SK_Caribou.shp"),
                                    overwrite = TRUE)
    #plot(weyco_sk.caribou, add = TRUE)

    ml <- mapAdd(weyco_sk, ml, layerName = "WeyCo SK", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo SK", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(weyco_sk.caribou, ml, layerName = "WeyCo SK Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "WeyCo SK Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`WeyCo SK Caribou`) <- gsub("[.]1", "", names(ml$`WeyCo SK Caribou`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    weyco_sk_sr <- postProcess(ml$`LandWeb Study Area`,
                               studyArea = amc::outerBuffer(weyco_sk, 50000), # 50 km buffer
                               useSAcrs = TRUE,
                               filename2 = file.path(dataDirWeyCo, "WeyCo_SK_SR.shp"),
                               overwrite = TRUE)
    #plot(weyco_sk_sr)

    ml <- mapAdd(weyco_sk_sr, ml, isStudyArea = TRUE, layerName = "WeyCo SK SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
