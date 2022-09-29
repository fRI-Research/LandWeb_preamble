fmaSprayLake <- function(ml, studyAreaName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  spraylake <- extractFMA(ml, "Spray Lake")
  raster::shapefile(spraylake, filename = file.path(dataDir, "SprayLake.shp"), overwrite = TRUE)

  ## reportingPolygons
  spraylake.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                                 studyArea = spraylake, useSAcrs = TRUE,
                                 filename2 = file.path(dataDir, "SprayLake_ANSR.shp"),
                                 overwrite = TRUE) %>%
    joinReportingPolygons(., spraylake)
  ## NOTE: no caribou ranges intersect with this FMA

  ml <- mapAdd(spraylake, ml, layerName = "SprayLake", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SprayLake", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(spraylake.ansr, ml, layerName = "SprayLake ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SprayLake ANSR",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  spraylake_sr <- postProcess(ml[["LandWeb Study Area"]],
                               studyArea = amc::outerBuffer(spraylake, bufferDist),
                               useSAcrs = TRUE,
                               filename2 = file.path(dataDir, "SprayLake_SR.shp"),
                               overwrite = TRUE)

  plotFMA(spraylake, provs = ab, caribou = NULL, xsr = spraylake_sr,
          title = "SprayLake", png = file.path(dataDir, "SprayLake.png"))
  #plotFMA(spraylake, provs = ab, caribou = spraylake.caribou, xsr = spraylake_sr, title = "SprayLake", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(spraylake_sr, ml, isStudyArea = TRUE, layerName = "SprayLake SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
