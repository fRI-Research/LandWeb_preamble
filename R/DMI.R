fmaDMI <- function(ml, runName, dataDir, canProvs, asStudyArea = FALSE) {
  dataDirDMI <- file.path(dataDir, "DMI") %>% checkPath(create = TRUE)

  ## There are 3 parts to the DMI FMA: an East and two West areas (North and South)
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  dmi <- extractFMA(ml, "Mercer Peace River")
  dmi.full <- maptools::unionSpatialPolygons(dmi, rep(1, 2))
  shapefile(dmi.full, filename = file.path(dataDirDMI, "DMI_Full.shp"), overwrite = TRUE)

  dmi.e <- extractFMA(ml, "Mercer Peace River.*East")
  shapefile(dmi.e, filename = file.path(dataDirDMI, "DMI_East.shp"), overwrite = TRUE)

  dmi.w <- extractFMA(ml, "Mercer Peace River.*West")
  shapefile(dmi.w, filename = file.path(dataDirDMI, "DMI_West.shp"), overwrite = TRUE)

  dmi.nw <- disaggregate(dmi.w)[2, ]
  shapefile(dmi.nw, filename = file.path(dataDirDMI, "DMI_West_North.shp"), overwrite = TRUE)

  dmi.sw <- disaggregate(dmi.w)[1, ]
  shapefile(dmi.sw, filename = file.path(dataDirDMI, "DMI_West_South.shp"), overwrite = TRUE)

  ## reporting polygons
  dmi.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                          studyArea = dmi, useSAcrs = TRUE,
                          filename2 = file.path(dataDirDMI, "DMI_ANSR.shp")) %>%
    joinReportingPolygons(., dmi)

  dmi.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                             studyArea = dmi, useSAcrs = TRUE,
                             filename2 = file.path(dataDirDMI, "DMI_caribou.shp")) %>%
    joinReportingPolygons(., dmi)

  ml <- mapAdd(dmi, ml, layerName = "DMI Full", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI Full", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(dmi.ansr, ml, layerName = "DMI ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(dmi.caribou, ml, layerName = "DMI Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI Caribou",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(dmi.nw, ml, layerName = "DMI AB NW", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI AB NW",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(dmi.sw, ml, layerName = "DMI AB SW", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI AB SW",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(dmi.e, ml, layerName = "DMI AB E", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "DMI AB E",
               columnNameForLabels = "Name", filename2 = NULL)

  ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
  names(ml[["DMI ANSR"]]) <- gsub("[.]1", "", names(ml[["DMI ANSR"]]))
  names(ml[["DMI Caribou"]]) <- gsub("[.]1", "", names(ml[["DMI Caribou"]]))

  ## buffered study area (needs to have LTHFC data) ---------------------------#
  dmi_sr <- postProcess(ml[["LandWeb Study Area"]],
                        studyArea = amc::outerBuffer(dmi.full, 50000), ## 50 km buffer
                        useSAcrs = TRUE,
                        filename2 = file.path(dataDirDMI, "DMI_SR.shp"))

  plotFMA(dmi, provs = ab, caribou = dmi.caribou, xsr = dmi_sr,
          title = "Mercer Peace River Pulp Ltd.", png = file.path(dataDirDMI, "DMI.png"))
  #plotFMA(dmi, provs = ab, caribou = dmi.caribou, xsr = dmi_sr,
  #        title = "Mercer Peace River Pulp Ltd.", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(dmi_sr, ml, layerName = "DMI AB SR Full", isStudyArea = TRUE,
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
