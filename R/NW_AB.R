NW_AB <- function(ml, studyAreaName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  AB <- canProvs[canProvs$NAME_1 == "Alberta", ]
  targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                     "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  nw_ab <- prepInputs(
    url = "https://drive.google.com/file/d/1kRb3fOSQEOTDZFhI_SEeF-Q7Mob0_cPy",
    targetFile = "NW_AB.shp",
    alsoExtract = "similar"
  ) |>
    sf::st_transform(targetCRS) |>
    sf::as_Spatial()

  ## make sure the reporting polygon has labels joinReportingPolygons expects
  if (!"Name" %in% names(nw_ab)) {
    nw_ab$Name <- "NW_AB"
  }
  if (!"shinyLabel" %in% names(nw_ab)) {
    nw_ab$shinyLabel <- rep_len(nw_ab$Name, nrow(nw_ab))
  }

  ## reportingPolygons
  nw_ab.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                            studyArea = nw_ab, useSAcrs = TRUE,
                            filename2 = file.path(dataDir, "NW_AB_ANSR.shp"),
                            overwrite = TRUE) %>%
    joinReportingPolygons(., nw_ab)

  nw_ab.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                               studyArea = nw_ab, useSAcrs = TRUE,
                               filename2 = file.path(dataDir, "NW_AB_caribou.shp"),
                               overwrite = TRUE) %>%
    joinReportingPolygons(., nw_ab)

  ml <- mapAdd(nw_ab, ml, layerName = "NW AB", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "NW AB", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(nw_ab.ansr, ml, layerName = "NW AB ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "NW AB ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(nw_ab.caribou, ml, layerName = "NW AB Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "NW AB Caribou",
               columnNameForLabels = "Name", filename2 = NULL)
  ## AB FMU boundaries (replaces previously added FMU map, for use as reporting polygon)
  ml <- mapAdd(map = ml, layerName = "AB FMU Boundaries",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1OH3b5pwjumm1ToytDBDI6jthVe2pp0tS", # 2025
               analysisGroupReportingPolygon = "AB FMU Boundaries", isStudyArea = FALSE,
               columnNameForLabels = "FMU_NAME", filename2 = NULL)
  ml[["AB FMU Boundaries"]][["Name"]] <- ml[["AB FMU Boundaries"]][["shinyLabel"]]

  nw_ab_sr <- postProcess(ml[["LandWeb Study Area"]],
                          studyArea = amc::outerBuffer(nw_ab, bufferDist),
                          useSAcrs = TRUE,
                          filename2 = file.path(dataDir, "NW_AB_SR.shp"),
                          overwrite = TRUE)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(nw_ab_sr, ml, isStudyArea = TRUE, layerName = "NW AB SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }
  return(ml)
}
