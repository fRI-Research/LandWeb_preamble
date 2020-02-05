provAB <- function(ml, runName, dataDir, canProvs, asStudyArea = FALSE) {
  dataDirAB <- file.path(dataDir, "AB") %>% checkPath(create = TRUE)

  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]

  id <- which(ml[["Provincial Boundaries"]][["NAME_1"]] == "Alberta")
  AB <- ml[["Provincial Boundaries"]][id, ]
  shapefile(AB, filename = file.path(dataDirAB, "AB_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  AB.ansr <- ml[["Alberta Natural Subregions"]]

  AB[["Name"]] <- AB[["NAME_1"]]
  AB.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                            studyArea = AB, useSAcrs = TRUE,
                            filename2 = file.path(dataDirAB, "AB_caribou.shp"),
                            overwrite = TRUE) %>%
    joinReportingPolygons(., AB)

  ml <- mapAdd(AB, ml, layerName = "AB", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "AB", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(AB.ansr, ml, layerName = "AB ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "AB ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(AB.caribou, ml, layerName = "AB Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "AB Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  AB_sr <- postProcess(ml[["LandWeb Study Area"]],
                       studyArea = amc::outerBuffer(AB, 25000), # 25 km buffer
                       useSAcrs = TRUE,
                       filename2 = file.path(dataDirAB, "AB_SR.shp"),
                       overwrite = TRUE)

  plotFMA(AB, provs = ab, caribou = AB.caribou, xsr = AB_sr,
          title = "Alberta",
          png = file.path(dataDirAB, "AB.png"))
  #plotFMA(AB, provs = ab, caribou = AB.caribou, xsr = AB_sr,
  #        title = "Alberta", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(AB_sr, ml, isStudyArea = TRUE, layerName = "AB SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
