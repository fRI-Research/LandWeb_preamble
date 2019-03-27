fmaTolko <- function(ml, runName, dataDir, canProvs) {
  dataDirTolko <- file.path(dataDir, "Tolko") %>% checkPath(create = TRUE)

  ## There are 3 parts to the Tolko FMA in AB and one in SK
  bcabsk <- canProvs[canProvs$NAME_1 %in% c("British Columbia", "Alberta", "Saskatchewan"), ]
  tolko <- extractFMA(ml, "Tolko|Meadow Lake OSB")
  tolko.full <- maptools::unionSpatialPolygons(tolko, rep(1, 5))
  shapefile(tolko.full, filename = file.path(dataDirTolko, "Tolko_Full.shp"), overwrite = TRUE)

  if (grepl("Tolko_AB_N|tolko_AB_N", runName)) {
    ## reporting polygons
    tolko_ab_n <- tolko[4, ]
    tolko_ab_n.sp <- as(tolko_ab_n, "SpatialPolygons")
    shapefile(tolko_ab_n, filename = file.path(dataDirTolko, "Tolko_AB_N.shp"), overwrite = TRUE)

    tolko_ab_n.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                                   studyArea = tolko_ab_n.sp, useSAcrs = TRUE,
                                   filename2 = file.path(dataDirTolko, "Tolko_AB_N_ANSR.shp"),
                                   overwrite = TRUE)
    tolko_ab_n.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                      studyArea = tolko_ab_n.sp, useSAcrs = TRUE,
                                      filename2 = file.path(dataDirTolko, "Tolko_AB_N_caribou.shp"),
                                      overwrite = TRUE)

    ml <- mapAdd(tolko_ab_n, ml, layerName = "Tolko AB North", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB North", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_n.ansr, ml, layerName = "Tolko AB North ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB North ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_n.caribou, ml, layerName = "Tolko AB North Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB North Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`Tolko AB North ANSR`) <- gsub("[.]1", "", names(ml$`Tolko AB North ANSR`))
    names(ml$`Tolko AB North Caribou`) <- gsub("[.]1", "", names(ml$`Tolko AB North Caribou`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    tolko_ab_n_sr <- postProcess(ml$`LandWeb Study Area`,
                                 studyArea = amc::outerBuffer(tolko_ab_n, 50000), # 50 km buffer
                                 useSAcrs = TRUE,
                                 filename2 = file.path(dataDirTolko, "Tolko_AB_N_SR.shp"),
                                 overwrite = TRUE)

    ml <- mapAdd(tolko_ab_n_sr, ml, isStudyArea = TRUE, layerName = "Tolko AB North SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)

    plotFMA(tolko_ab_n, provs = bcabsk, caribou = tolko_ab_n.caribou, xsr = tolko_ab_n_sr,
            title = "Tolko AB N", png = file.path(dataDirTolko, "Tolko_AB_N.png"))
    #plotFMA(tolko_ab_n, provs = bcabsk, caribou = tolko_ab_n.caribou, xsr = tolko_ab_n_sr,
    #        title = "Tolko AB N", png = NULL)
  } else if (grepl("Tolko_AB_S|tolko_AB_S", runName)) {
    ## reportingPolygons
    tolko_ab_s <- tolko[c(2, 3, 5), ]
    tolko_ab_s.sp <- as(tolko_ab_s, "SpatialPolygons")
    shapefile(tolko_ab_s, filename = file.path(dataDirTolko, "Tolko_AB_S.shp"), overwrite = TRUE)

    tolko_ab_s.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                                   studyArea = tolko_ab_s.sp, useSAcrs = TRUE,
                                   filename2 = file.path(dataDirTolko, "Tolko_AB_S_ANSR.shp"),
                                   overwrite = TRUE)
    tolko_ab_s.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                      studyArea = tolko_ab_s.sp, useSAcrs = TRUE,
                                      filename2 = file.path(dataDirTolko, "Tolko_AB_S_caribou.shp"),
                                      overwrite = TRUE)

    ml <- mapAdd(tolko_ab_s, ml, layerName = "Tolko AB South", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB South", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_s.ansr, ml, layerName = "Tolko AB South ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB South ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_ab_s.caribou, ml, layerName = "Tolko AB South Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko AB South Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`Tolko AB South ANSR`) <- gsub("[.]1", "", names(ml$`Tolko AB South ANSR`))
    names(ml$`Tolko AB South Caribou`) <- gsub("[.]1", "", names(ml$`Tolko AB South Caribou`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    tolko_ab_s_sr <- postProcess(ml$`LandWeb Study Area`,
                                 studyArea = amc::outerBuffer(tolko_ab_s, 50000), # 50 km buffer
                                 useSAcrs = TRUE,
                                 filename2 = file.path(dataDirTolko, "Tolko_AB_S_SR.shp"),
                                 overwrite = TRUE)

    ml <- mapAdd(tolko_ab_s_sr, ml, isStudyArea = TRUE, layerName = "Tolko AB South SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)

    plotFMA(tolko_ab_s, provs = bcabsk, caribou = tolko_ab_s.caribou, xsr = tolko_ab_s_sr,
            title = "Tolko AB S", png = file.path(dataDirTolko, "Tolko_AB_S.png"))
    #plotFMA(tolko_ab_s, provs = bcabsk, caribou = tolko_ab_s.caribou, xsr = tolko_ab_s_sr,
    #        title = "Tolko AB S", png = NULL)
  } else if (grepl("Tolko_SK|tolko_SK", runName)) {
    ## reportingPolygons
    tolko_sk <- tolko[1, ]
    tolko_sk.sp <- as(tolko_sk, "SpatialPolygons")
    shapefile(tolko_sk, filename = file.path(dataDirTolko, "Tolko_SK.shp"), overwrite = TRUE)

    tolko_sk.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                                    studyArea = tolko_sk.sp, useSAcrs = TRUE,
                                    filename2 = file.path(dataDirTolko, "Tolko_SK_caribou.shp"),
                                    overwrite = TRUE)

    ml <- mapAdd(tolko_sk, ml, layerName = "Tolko SK", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko SK", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(tolko_sk.caribou, ml, layerName = "Tolko SK Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Tolko SK Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
    names(ml$`Tolko SK Caribou`) <- gsub("[.]1", "", names(ml$`Tolko SK Caribou`))

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    tolko_sk_sr <- postProcess(ml$`LandWeb Study Area`,
                               studyArea = amc::outerBuffer(tolko_sk, 50000), # 50 km buffer
                               useSAcrs = TRUE,
                               filename2 = file.path(dataDirTolko, "Tolko_SK_SR.shp"),
                               overwrite = TRUE)

    ml <- mapAdd(tolko_sk_sr, ml, isStudyArea = TRUE, layerName = "Tolko SK SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)

    plotFMA(tolko_sk, provs = bcabsk, caribou = tolko_sk.caribou, xsr = tolko_sk_sr,
            title = "Tolko AB SK", png = file.path(dataDirTolko, "Tolko_SK.png"))
    #plotFMA(tolko_sk, provs = bcabsk, caribou = tolko_sk.caribou, xsr = tolko_sk_sr,
    #        title = "Tolko AB SK", png = NULL)
  }

  return(ml)
}
