fmaSundreFP <- function(ml, runName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ## There are 3 parts to the SundreFP FMA: 2 in BC and one in MB.
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  sundre <- extractFMA(ml, "Sundre Forest Products Inc.")
  shapefile(sundre, filename = file.path(dataDir, "SundreFP.shp"), overwrite = TRUE)

  ## reportingPolygons
  sundre.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                             studyArea = sundre, useSAcrs = TRUE,
                             filename2 = file.path(dataDir, "SundreFP_ANSR"),
                             overwrite = TRUE) %>%
    joinReportingPolygons(., sundre)

  ml <- mapAdd(sundre, ml, layerName = "SundreFP", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SundreFP", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(sundre.ansr, ml, layerName = "SundreFP ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SundreFP ANSR",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  sundre_sr <- postProcess(ml[["LandWeb Study Area"]],
                           studyArea = amc::outerBuffer(sundre, bufferDist),
                           useSAcrs = TRUE,
                           filename2 = file.path(dataDir, "SundreFP_SR.shp"),
                           overwrite = TRUE)

  plotFMA(sundre, provs = ab, caribou = NULL, xsr = sundre_sr,
          title = "Sundre Forest Products", png = file.path(dataDir, "SundreFP.png"))
  #plotFMA(sundre, provs = ab, caribou = sundre.caribou, xsr = sundre_sr,
  #        title = "Sundre Forest Products", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(sundre_sr, ml, isStudyArea = TRUE, layerName = "SundreFP SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
