fmu <- function(ml, runName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  fmuNum <- strsplit(runName, "_")[[1]][2]

  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  fmu <- extractFMU(ml, fmuNum)
  fmu[["Name"]] <- fmu[["FMU_NAME"]]
  shapefile(fmu, filename = file.path(dataDir, paste0("FMU_", fmuNum, ".shp")), overwrite = TRUE)

  ## reporting polygons
  fmu.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                          studyArea = fmu, useSAcrs = TRUE,
                          filename2 = file.path(dataDir, paste0("FMU", fmuNum, "_ANSR.shp"))) %>%
    joinReportingPolygons(., fmu)

  fmu.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                             studyArea = fmu, useSAcrs = TRUE,
                             filename2 = file.path(dataDir, paste0("FMU", fmuNum, "_caribou.shp"))) %>%
    joinReportingPolygons(., fmu)

  ml <- mapAdd(fmu, ml, layerName = "FMU", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "FMU", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(fmu.ansr, ml, layerName = "FMU ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "FMU ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(fmu.caribou, ml, layerName = "FMU Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "FMU Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  ## TODO: workaround problematic intersect() that changes Name to Name.1 and Name.2
  names(ml[["FMU ANSR"]]) <- gsub("[.]1", "", names(ml[["FMU ANSR"]]))
  names(ml[["FMU Caribou"]]) <- gsub("[.]1", "", names(ml[["FMU Caribou"]]))

  ## buffered study area (needs to have LTHFC data) ---------------------------#
  fmu_sr <- postProcess(ml[["LandWeb Study Area"]],
                        studyArea = amc::outerBuffer(fmu, bufferDist),
                        useSAcrs = TRUE,
                        filename2 = file.path(dataDir, paste0("FMU", fmuNum, "_SR.shp")))

  plotFMA(fmu, provs = ab, caribou = fmu.caribou, xsr = fmu_sr,
          title = "Mercer Peace River Pulp Ltd.", png = file.path(dataDir, paste0("FMU", fmuNum, ".png")))
  #plotFMA(fmu, provs = ab, caribou = fmu.caribou, xsr = fmu_sr,
  #        title = "Mercer Peace River Pulp Ltd.", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(fmu_sr, ml, layerName = "FMU AB SR Full", isStudyArea = TRUE,
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
