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

  if (!grepl("LandWeb", studyAreaName)) {
    ## NOTE: updated fall 2024 to use gdb which does not contain invalid geometries;

    # sundre.lbstatus <- Cache({
    #   prepInputs(
    #     url = "https://drive.google.com/file/d/1FcIogFQ8veA25T1HEIgw-SyG_Dk21rw4/",
    #     destinationPath = dataDir,
    #     targetFile = "SFP_Landbase.shp", alsoExtract = "similar",
    #     fun = "sf::st_read", studyArea = sundre, useSAcrs = TRUE
    #   )
    # })

    ## TODO: prepInputs can't deal with gdb files
    SFP_gdb <- file.path(dataDir, "SFP_Landbase_2024.gdb")
    SFP_zip <- paste0(SFP_gdb, ".zip")

    if (!file.exists(SFP_zip)) {
      googledrive::as_id("1oh_w9nALKufCQXb1PR3VIlChPHPysHF4") |>
        googledrive::drive_download(path = SFP_zip)
    }

    if (!file.exists(SFP_gdb)) {
      archive::archive_extract(SFP_zip, dataDir) ## TODO: fails to extract; do it manually
    }

    sundre.lbstatus <- sf::st_read(SFP_gdb) |>
      sf::st_transform(crs(sundre))

    ## TODO: cache these to speed up subsequent processing
    sundre.lbstatus <- sundre.lbstatus[st_is_valid(sundre.lbstatus), ] ## remove invalid geometries
    sundre.lbstatus <- sundre.lbstatus[!st_is_empty(sundre.lbstatus), ] ## remove empty polygons

    sundre.lbstatus <- Cache({
      # mutate(sundre.lbstatus, Name = LBC_LBStat, geometry = geometry, .keep = "used") |>
      mutate(sundre.lbstatus, Name = LBC_LBStatus, geometry = Shape, .keep = "used") |>
        group_by(Name) |>
        summarise(geometry = sf::st_union(geometry)) |>
        ungroup() |>
        mutate(shinyLabel = Name) |>
        joinReportingPolygons(sundre)
    })
  }

  ml <- mapAdd(sundre, ml, layerName = "SundreFP", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SundreFP", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(sundre.ansr, ml, layerName = "SundreFP ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "SundreFP ANSR",
               columnNameForLabels = "Name", filename2 = NULL)

  if (!grepl("LandWeb", studyAreaName)) {
    ml <- mapAdd(sundre.lbstatus, ml, layerName = "SundreFP LBstatus", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "SundreFP LBstatus",
                 columnNameForLabels = "Name", filename2 = NULL)
  }

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  sundre_sr <- postProcess(ml[["LandWeb Study Area"]],
                           studyArea = spatialutils::outerBuffer(sundre, bufferDist),
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
