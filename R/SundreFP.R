fmaSundreFP <- function(ml, runName, dataDir, canProvs) {
  dataDirSundreFP <- file.path(dataDir, "SundreFP") %>% checkPath(create = TRUE)

  ## There are 3 parts to the SundreFP FMA: 2 in BC and one in MB.
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  sundre <- extractFMA(ml, "Sundre Forest Products Inc.")
  sundre.sp <- as(sundre, "SpatialPolygons")
  shapefile(sundre, filename = file.path(dataDirSundreFP, "SundreFP.shp"), overwrite = TRUE)

  ## reportingPolygons
  sundre.ansr <- postProcess(ml$`Alberta Natural Subregions`,
                             studyArea = sundre.sp, useSAcrs = TRUE,
                             filename2 = file.path(dataDirSundreFP, "SundreFP_ANSR"),
                             overwrite = TRUE)

  ml <- mapAdd(sundre, ml, layerName = "SundreFP", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SundreFP", isStudyArea = TRUE,
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(sundre.ansr, ml, layerName = "SundreFP ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SundreFP ANSR",
               columnNameForLabels = "Name", filename2 = NULL)

  ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
  names(ml$`SundreFP ANSR`) <- gsub("[.]1", "", names(ml$`SundreFP ANSR`))

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  sundre_sr <- postProcess(ml$`LandWeb Study Area`,
                          studyArea = amc::outerBuffer(sundre, 50000), # 50 km buffer
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDirSundreFP, "SundreFP_SR.shp"),
                          overwrite = TRUE)

  ml <- mapAdd(sundre_sr, ml, isStudyArea = TRUE, layerName = "SundreFP SR",
               useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
               columnNameForLabels = "NSN", filename2 = NULL)

  plotFMA(sundre, provs = ab, caribou = sundre.caribou, xsr = sundre_sr,
          title = "Sundre Forest Products", png = file.path(dataDirSundreFP, "SundreFP.png"))
  #plotFMA(sundre, provs = ab, caribou = sundre.caribou, xsr = sundre_sr,
  #        title = "Sundre Forest Products", png = NULL)

  return(ml)
}
