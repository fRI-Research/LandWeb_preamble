fmaManning <- function(ml, studyAreaName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  manning <- extractFMA(ml, "Manning")
  raster::shapefile(manning, filename = file.path(dataDir, "Manning_full.shp"), overwrite = TRUE)

  ## reportingPolygons
  manning.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                              studyArea = manning, useSAcrs = TRUE,
                              filename2 = file.path(dataDir, "Manning_ANSR.shp"),
                              overwrite = TRUE) %>%
    joinReportingPolygons(., manning)
  manning.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                 studyArea = manning, useSAcrs = TRUE,
                                 filename2 = file.path(dataDir, "Manning_caribou.shp"),
                                 overwrite = TRUE) %>%
    joinReportingPolygons(., manning)

  if (!grepl("LandWeb", studyAreaName)) {
    ## workaround issue with LBstatus when running full LandWeb area; TODO: diagnose and fix
    manning.lbstatus <- Cache({
      prepInputs(
        ## use custom clean version with non-polygon geometries removed
        url = "https://drive.google.com/file/d/1lY0p6Ms84paja9p1lmGXz5jaCgv2_VkY/",
        destinationPath = dataDir,
        targetFile = "Manning_LBStatus_clean.shp", alsoExtract = "similar",
        fun = "sf::st_read"
      )
    })
    manning.lbstatus <- manning.lbstatus[st_is_valid(manning.lbstatus), ] ## remove invalid geometries
    manning.lbstatus <- manning.lbstatus[!st_is_empty(manning.lbstatus), ] ## remove empty polygons
    manning.lbstatus <- Cache({
      mutate(manning.lbstatus, LBC_LBStat = LBC_LBStat, geometry = geometry, .keep = "used") |>
        group_by(LBC_LBStat) |>
        summarise(geometry = sf::st_union(geometry)) |>
        ungroup()
    })
    manning.lbstatus <- as_Spatial(manning.lbstatus)
    names(manning.lbstatus) <- "Name" ## rename LBC_LBStat to Name for use downstream
    manning.lbstatus[["shinyLabel"]] <- manning.lbstatus[["Name"]] ## need shinyLabel downstream
  } ## TODO: update this to match WF A/P additions spring 2023

  ml <- mapAdd(manning, ml, layerName = "Manning", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Manning", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(manning.ansr, ml, layerName = "Manning ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Manning ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(manning.caribou, ml, layerName = "Manning Caribou", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Manning Caribou",
               columnNameForLabels = "Name", filename2 = NULL)

  if (!grepl("LandWeb", studyAreaName)) {
    ml <- mapAdd(manning.lbstatus, ml, layerName = "Manning LBstatus", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Manning LBstatus",
                 columnNameForLabels = "Name", filename2 = NULL)
  }

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  manning_sr <- postProcess(ml[["LandWeb Study Area"]],
                            studyArea = amc::outerBuffer(manning, bufferDist),
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDir, "Manning_SR.shp"),
                            overwrite = TRUE)

  plotFMA(manning, provs = ab, caribou = manning.caribou, xsr = manning_sr,
          title = "Manning", png = file.path(dataDir, "Manning.png"))
  #plotFMA(manning, provs = ab, caribou = manning.caribou, xsr = manning_sr,
  #        title = "Manning", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(manning_sr, ml, isStudyArea = TRUE, layerName = "Manning SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
