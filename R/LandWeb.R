allLandWeb <- function(ml, runName, dataDir, canProvs, asStudyArea = FALSE) {
  dataDirLandWeb <- file.path(dataDir, "FULL") %>% checkPath(create = TRUE)

  ## LandWeb area extends a bit into Yukon, Nunavut, Ontario, but not relevant here
  provs <- canProvs[canProvs$NAME_1 %in% c("British Columbia", "Alberta",
                                           "Saskatchewan", "Manitoba",
                                           "Northwest Territories"), ]
  ab <- canProvs[canProvs$NAME_1 %in% c("Alberta"), ]
  absk <- canProvs[canProvs$NAME_1 %in% c("Alberta", "Saskatchewan"), ]
  nwt <- canProvs[canProvs$NAME_1 %in% c("Northwest Territories"), ]
  west <- canProvs[canProvs$NAME_1 %in% c("British Columbia", "Alberta",
                                          "Saskatchewan", "Manitoba"), ]

  lw <- ml$`LandWeb Study Area`
  lw.sp <- as(lw, "SpatialPolygons")

  ## reportingPolygons
  lw.caribou <- postProcess(ml$`Boreal Caribou Ranges`,
                            studyArea = lw.sp, useSAcrs = TRUE,
                            filename2 = file.path(dataDirLandWeb, "LandWeb_caribou.shp"),
                            overwrite = TRUE)
  lw.provs <- postProcess(ml$`Provincial Boundaries`,
                          studyArea = lw.sp, useSAcrs = TRUE,
                          filename2 = file.path(dataDirLandWeb, "LandWeb_provinces.shp"),
                          overwrite = TRUE)

  ml <- mapAdd(lw, ml, layerName = "LandWeb", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "LandWeb", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL) ## TODO: losing a polygon in NWT
  ml$LandWeb <- lw ## TODO: workaround the problem with lost NWT poly

  ml <- mapAdd(lw.caribou, ml, layerName = "LandWeb Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "LandWeb Caribou",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(lw.provs, ml, layerName = "LandWeb Provinces", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "LandWeb Provinces",
               columnNameForLabels = "NAME_1", filename2 = NULL)

  ml <- fmaANC(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaDMI(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaEdsonFP(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaLP(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaManning(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaMillarWestern(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaMistik(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaNWT(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaSundreFP(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaTolko(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaVanderwell(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaWestFraser(ml, runName, dataDir, canProvs, asStudyArea = FALSE)
  ml <- fmaWeyCo(ml, runName, dataDir, canProvs, asStudyArea = FALSE)

  if (isTRUE(asStudyArea)) {
    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    ml <- mapAdd(lw, ml, isStudyArea = TRUE, layerName = "LandWeb SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  plotLandWeb(lw.provs, provs = provs, caribou = lw.caribou, xsr = NULL,
              title = "LandWeb Study Area", png = file.path(dataDirLandWeb, "LandWeb.png"))
  #plotLandWeb(lw.provs, provs = provs, caribou = lw.caribou, xsr = NULL,
  #            title = "LandWeb Study Area", png = NULL)

  return(ml)
}
