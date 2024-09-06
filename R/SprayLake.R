fmaSprayLake <- function(ml, studyAreaName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  spraylake <- extractFMA(ml, "Spray Lake")
  spraylake.c5 <- extractFMA(ml, "Crowsnest") ## Crowsnest is the C5 unit
  spraylake_sa <- rbind(spraylake, spraylake.c5)
  raster::shapefile(spraylake, filename = file.path(dataDir, "SprayLake.shp"), overwrite = TRUE)
  raster::shapefile(spraylake.c5, filename = file.path(dataDir, "SprayLakeC5.shp"), overwrite = TRUE)

  ## reportingPolygons
  spraylake.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                                 studyArea = spraylake, useSAcrs = TRUE,
                                 filename2 = file.path(dataDir, "SprayLake_ANSR.shp"),
                                 overwrite = TRUE) %>%
    joinReportingPolygons(., spraylake)

  spraylake.c5.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                                   studyArea = spraylake.c5, useSAcrs = TRUE,
                                   filename2 = file.path(dataDir, "SprayLakeC5_ANSR.shp"),
                                   overwrite = TRUE) %>%
    joinReportingPolygons(., spraylake.c5)

  ## NOTE: no caribou ranges intersect with this FMA

  if (!grepl("LandWeb", studyAreaName)) {
    ## TODO: prepInputs doesn't work correctly with .gdb files
    C5_gdb <- file.path(dataDir, "C5_info_for_Landweb.gdb")
    C5_zip <- paste0(C5_gdb, ".zip")

    if (!file.exists(C5_zip)) {
      googledrive::as_id("1FpMg6dJ4eblMjMkEHdcoFAYSDyo1OvTv") |>
        googledrive::drive_download(path = dataDir)
    }

    if (!file.exists(C5_gdb)) {
      archive::archive_extract(C5_zip, dataDir)
    }

    C5.lbstatus <- sf::st_read(C5_gdb, layer = "lb_20230901_tsa") |>
      postProcess(projectTo = spraylake.c5, cropTo = NULL, maskTo = NULL)
    C5.lbstatus <- C5.lbstatus[st_is_valid(C5.lbstatus), ] ## remove invalid geometries
    C5.lbstatus <- C5.lbstatus[!st_is_empty(C5.lbstatus), ] ## remove empty polygons
    C5.lbstatus <- Cache({
      mutate(C5.lbstatus, Name = f_active, geometry = SHAPE, .keep = "used") |>
        group_by(Name) |>
        summarise(geometry = sf::st_union(geometry)) |>
        ungroup() |>
        mutate(shinyLabel = Name, .before = geometry) |>
        joinReportingPolygons(spraylake.c5)
    })
  }

  ml <- mapAdd(spraylake_sa, ml, layerName = "Spray Lake + C5 study area", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Spray Lake + C5 study area", isStudyArea = isTRUE(asStudyArea),
               columnNameForLabels = "Name", filename2 = NULL)

  ml <- mapAdd(spraylake, ml, layerName = "Spray Lake", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Spray Lake",
               columnNameForLabels = "Name", filename2 = NULL)

  ml <- mapAdd(spraylake.ansr, ml, layerName = "Spray Lake ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Spray Lake ANSR",
               columnNameForLabels = "Name", filename2 = NULL)

  ml <- mapAdd(spraylake.c5, ml, layerName = "Spray Lake C5", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Spray Lake C5",
               columnNameForLabels = "Name", filename2 = NULL)
  ml <- mapAdd(spraylake.c5.ansr, ml, layerName = "Spray Lake C5 ANSR", useSAcrs = TRUE, poly = TRUE,
               analysisGroupReportingPolygon = "Spray Lake C5 ANSR",
               columnNameForLabels = "Name", filename2 = NULL)

  if (!grepl("LandWeb", studyAreaName)) {
    ml <- mapAdd(C5.lbstatus, ml, layerName = "Spray Lake C5 LBstatus", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Spray Lake C5 LBstatus",
                 columnNameForLabels = "Name", filename2 = NULL)
  }

  ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
  spraylake_sr <- postProcess(ml[["LandWeb Study Area"]],
                              studyArea = amc::outerBuffer(spraylake_sa, bufferDist),
                              useSAcrs = TRUE,
                              filename2 = file.path(dataDir, "SprayLake_SR.shp"),
                              overwrite = TRUE)

  plotFMA(spraylake_sa, provs = ab, caribou = NULL, xsr = spraylake_sr,
          title = "SprayLake", png = file.path(dataDir, "SprayLake.png"))
  # plotFMA(spraylake_sa, provs = ab, caribou = NULL, xsr = spraylake_sr, title = "SprayLake", png = NULL)

  if (isTRUE(asStudyArea)) {
    ml <- mapAdd(spraylake_sr, ml, isStudyArea = TRUE, layerName = "Spray Lake SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  return(ml)
}
