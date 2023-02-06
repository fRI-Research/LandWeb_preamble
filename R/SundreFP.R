fmaSundreFP <- function(ml, studyAreaName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ## There are 3 parts to the SundreFP FMA: 2 in BC and one in MB.
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  sundre <- extractFMA(ml, "Sundre Forest Products Inc.")
  raster::shapefile(sundre, filename = file.path(dataDir, "SundreFP.shp"), overwrite = TRUE)

  ## reportingPolygons
  sundre.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                             studyArea = sundre, useSAcrs = TRUE,
                             filename2 = file.path(dataDir, "SundreFP_ANSR"),
                             overwrite = TRUE) %>%
    joinReportingPolygons(., sundre)

  sundre.lbstatus <- Cache({
    prepInputs(
      url = "https://drive.google.com/file/d/1FcIogFQ8veA25T1HEIgw-SyG_Dk21rw4/",
      destinationPath = dataDir,
      targetFile = "SFP_Landbase.shp", alsoExtract = "similar",
      fun = "sf::st_read"
    )
  })
  sundre.lbstatus <- sundre.lbstatus[st_is_valid(sundre.lbstatus), ] ## remove invalid geometries
  sundre.lbstatus <- sundre.lbstatus[!st_is_empty(sundre.lbstatus), ] ## remove empty polygons
  sundre.lbstatus <- Cache({
    mutate(sundre.lbstatus, LBC_LBStat = LBC_LBStat, geometry = geometry, .keep = "used") |>
      group_by(LBC_LBStat) |>
      summarise(geometry = sf::st_union(geometry)) |>
      ungroup()
  })

  sundre.lbstatus <- as_Spatial(sundre.lbstatus)
  names(sundre.lbstatus) <- "Name" ## rename to Name for use downstream
  sundre.lbstatus[["shinyLabel"]] <- sundre.lbstatus[["Name"]] ## need shinyLabel downstream

  ml <- mapAdd(sundre, ml, layerName = "SundreFP", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SundreFP", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(sundre.ansr, ml, layerName = "SundreFP ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SundreFP ANSR",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(sundre.lbstatus, ml, layerName = "SundreFP LBstatus", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SundreFP LBstatus",
               columnNameForLabels = "Name", filename2 = NULL)

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  sundre_sr <- postProcess(ml[["LandWeb Study Area"]],
                           studyArea = amc::outerBuffer(sundre, bufferDist),
                           useSAcrs = TRUE,
                           filename2 = file.path(dataDir, "SundreFP_SR.shp"),
                           overwrite = TRUE)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(sundre_sr, ml, isStudyArea = TRUE, layerName = "SundreFP SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  plotFMA(sundre, provs = ab, caribou = NULL, xsr = sundre_sr,
          title = "Sundre Forest Products", png = file.path(dataDir, "SundreFP.png"))
  #plotFMA(sundre, provs = ab, caribou = sundre.caribou, xsr = sundre_sr,
  #        title = "Sundre Forest Products", png = NULL)

  return(ml)
}
