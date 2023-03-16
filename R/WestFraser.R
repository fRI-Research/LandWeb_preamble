fmaWestFraser <- function(ml, studyAreaName, dataDir, canProvs, bufferDist, asStudyArea = FALSE) {
  ## There are multiple parts to the WestFraser FMAs:
  ## - also includes Tolko_AB_S
  ## - also includes Blue Ridge
  ab <- canProvs[canProvs$NAME_1 == "Alberta", ]
  wf <- extractFMA(ml, "West Fraser|Blue Ridge")
  raster::shapefile(wf, filename = file.path(dataDir, "WestFraser_full.shp"), overwrite = TRUE)

  if (grepl("LandWeb|BlueRidge", studyAreaName)) {
    ## reportingPolygons
    wf_br <- wf[grepl("Blue Ridge", wf$Name), ] ## first subpolygon
    raster::shapefile(wf_br, filename = file.path(dataDir, "WestFraser_BlueRidge.shp"), overwrite = TRUE)

    wf_br.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                              studyArea = wf_br, useSAcrs = TRUE,
                              filename2 = file.path(dataDir, "WestFraser_BlueRidge_ANSR"),
                              overwrite = TRUE) %>%
      joinReportingPolygons(., wf_br)

    ## NOTE: no intersecting caribou areas

    if (!grepl("LandWeb", studyAreaName)) {
      wf_br.lbstatus <- Cache(
        prepInputs,
        url = "https://drive.google.com/file/d/1A7N_EIbO2wMBI_YTmU2Z-bQwqC9sY_EC/",
        destinationPath = dataDir,
        targetFile = "BRL_Landbase.shp", alsoExtract = "similar",
        fun = "sf::st_read", studyArea = wf_br, useSAcrs = TRUE
      )
      wf_br.lbstatus <- wf_br.lbstatus[st_is_valid(wf_br.lbstatus), ] ## remove invalid geometries
      wf_br.lbstatus <- wf_br.lbstatus[!st_is_empty(wf_br.lbstatus), ] ## remove empty polygons
      wf_br.lbstatus <- Cache({
        mutate(wf_br.lbstatus, Name = LBC_LBStat, geometry = geometry, .keep = "used") |>
          group_by(Name) |>
          summarise(geometry = sf::st_union(geometry)) |>
          ungroup() |>
          mutate(shinyLabel = Name, .before = geometry) |>
          joinReportingPolygons(wf_br)
      })
    }

    ml <- mapAdd(wf_br, ml, layerName = "West Fraser Blue Ridge", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser Blue Ridge", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_br.ansr, ml, layerName = "West Fraser Blue Ridge ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser Blue Ridge ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)

    if (!grepl("LandWeb", studyAreaName)) {
      ml <- mapAdd(wf_br.lbstatus, ml, layerName = "West Fraser Blue Ridge LBstatus", useSAcrs = TRUE, poly = TRUE,
                   analysisGroupReportingPolygon = "West Fraser Blue Ridge LBstatus",
                   columnNameForLabels = "Name", filename2 = NULL)
    }

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    wf_br_sr <- postProcess(ml[["LandWeb Study Area"]],
                            studyArea = amc::outerBuffer(wf_br, bufferDist),
                            useSAcrs = TRUE,
                            filename2 = file.path(dataDir, "WestFraser_BlueRidge_SR.shp"),
                            overwrite = TRUE)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(wf_br_sr, ml, isStudyArea = TRUE, layerName = "West Fraser Blue Ridge SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }

    plotFMA(wf_br, provs = ab, caribou = NULL, xsr = wf_br_sr, title = "West Fraser Blue Ridge",
            png = file.path(dataDir, "WestFraser_BR.png"))
    # plotFMA(wf_br, provs = ab, caribou = NULL, xsr = wf_br_sr,
    #         title = "West Fraser Blue Ridge", png = NULL)
  }

  if (grepl("LandWeb|WestFraser_N", studyAreaName)) {
    ## reportingPolygons
    wf_n <- wf[c(2:3, 6), ] ## 3 FMAs: Slave Lake; shared w/ Tolko; shared vith Tolko & Vanderwell.
    raster::shapefile(wf_n, filename = file.path(dataDir, "WestFraser_N.shp"), overwrite = TRUE)

    wf_n.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                             studyArea = wf_n, useSAcrs = TRUE,
                             filename2 = file.path(dataDir, "WestFraser_N_ANSR"),
                             overwrite = TRUE) %>%
      joinReportingPolygons(., wf_n)
    wf_n.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                studyArea = wf_n, useSAcrs = TRUE,
                                filename2 = file.path(dataDir, "WestFraser_N_caribou.shp"),
                                overwrite = TRUE) %>%
      joinReportingPolygons(., wf_n)

    if (!grepl("LandWeb", studyAreaName)) {
      s17 <- Cache({
        prepInputs(
          url = "https://drive.google.com/file/d/1XrF9ygQruC2FsUulWhDxR-nD3Cd4eu7B/", ## S17
          destinationPath = dataDir,
          filename1 = "CLS_Clipped_S17.shp",
          targetFile = "CLS_Clipped_S17.shp", alsoExtract = "similar",
          fun = "sf::st_read", studyArea = wf_n, useSAcrs = TRUE
        )
      })
      s17 <- s17[st_is_valid(s17), ] ## remove invalid geometries
      s17 <- s17[!st_is_empty(s17), ] ## remove empty polygons
      s17 <- Cache({
        mutate(s17, Name = F_CONDITIO, geometry = geometry, .keep = "used") |>
          group_by(Name) |>
          summarise(geometry = sf::st_union(geometry)) |>
          ungroup() |>
          mutate(shinyLabel = Name, .before = geometry) |>
          joinReportingPolygons(wf_n)
      })

      s20 <- Cache({
        prepInputs(
          url = "https://drive.google.com/file/d/17fZw80w3n2jIKRP1-X6gq8tyOjSWh0ky/", ## S20
          destinationPath = dataDir,
          filename1 = "CLS_Clipped_S20.shp",
          targetFile = "CLS_Clipped_S20.shp", alsoExtract = "similar",
          fun = "sf::st_read", studyArea = wf_n, useSAcrs = TRUE
        )
      })
      s20 <- s20[st_is_valid(s20), ] ## remove invalid geometries
      s20 <- s20[!st_is_empty(s20), ] ## remove empty polygons
      s20 <- Cache({
        mutate(s20, Name = F_CONDITIO, geometry = geometry, .keep = "used") |>
          group_by(Name) |>
          summarise(geometry = sf::st_union(geometry)) |>
          ungroup() |>
          mutate(shinyLabel = Name, .before = geometry) |>
          joinReportingPolygons(wf_n)
      })

      s21 <- Cache({
        prepInputs(
          url = "https://drive.google.com/file/d/1akMUL-lRumTfmmWG7WF7-9KDrFxTPN3Z/", ## S21
          destinationPath = dataDir,
          filename1 = "CLS_Clipped_S21.shp",
          targetFile = "CLS_Clipped_S21.shp", alsoExtract = "similar",
          fun = "sf::st_read", studyArea = wf_n, useSAcrs = TRUE
        )
      })
      s21 <- s21[st_is_valid(s21), ] ## remove invalid geometries
      s21 <- s21[!st_is_empty(s21), ] ## remove empty polygons
      s21 <- Cache({
        mutate(s21, Name = F_CONDITIO, geometry = geometry, .keep = "used") |>
          group_by(Name) |>
          summarise(geometry = sf::st_union(geometry)) |>
          ungroup() |>
          mutate(shinyLabel = Name, .before = geometry) |>
          joinReportingPolygons(wf_n)
      })

      ## Active/Passive by caribou ranges (no caribou in s21)
      s17.caribou <- joinReportingPolygons(s17, wf_n.caribou)
      s20.caribou <- joinReportingPolygons(s20, wf_n.caribou)
    }

    ml <- mapAdd(wf_n, ml, layerName = "West Fraser N", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser N", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_n.ansr, ml, layerName = "West Fraser N ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser N ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_n.caribou, ml, layerName = "West Fraser N Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser N Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    if (!grepl("LandWeb", studyAreaName)) {
      ml <- mapAdd(s17, ml, layerName = "West Fraser N LBstatus S17", useSAcrs = TRUE, poly = TRUE,
                   analysisGroupReportingPolygon = "West Fraser N LBstatus S17",
                   columnNameForLabels = "Name", filename2 = NULL)
      ml <- mapAdd(s20, ml, layerName = "West Fraser N LBstatus S20", useSAcrs = TRUE, poly = TRUE,
                   analysisGroupReportingPolygon = "West Fraser N LBstatus S20",
                   columnNameForLabels = "Name", filename2 = NULL)
      ml <- mapAdd(s21, ml, layerName = "West Fraser N LBstatus S21", useSAcrs = TRUE, poly = TRUE,
                   analysisGroupReportingPolygon = "West Fraser N LBstatus S21",
                   columnNameForLabels = "Name", filename2 = NULL)
      ml <- mapAdd(s17.caribou, ml, layerName = "West Fraser N LBstatus S17 Caribou", useSAcrs = TRUE, poly = TRUE,
                   analysisGroupReportingPolygon = "West Fraser N LBstatus S17 Caribou",
                   columnNameForLabels = "Name", filename2 = NULL)
      ml <- mapAdd(s20.caribou, ml, layerName = "West Fraser N LBstatus S20 Caribou", useSAcrs = TRUE, poly = TRUE,
                   analysisGroupReportingPolygon = "West Fraser N LBstatus S20 Caribou",
                   columnNameForLabels = "Name", filename2 = NULL)
    }

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    wf_n_sr <- postProcess(ml[["LandWeb Study Area"]],
                           studyArea = amc::outerBuffer(wf_n, bufferDist),
                           useSAcrs = TRUE,
                           filename2 = file.path(dataDir, "WestFraser_N_SR.shp"),
                           overwrite = TRUE)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(wf_n_sr, ml, isStudyArea = TRUE, layerName = "West Fraser N SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }

    plotFMA(wf_n, provs = ab, caribou = NULL, xsr = wf_n_sr, title = "West Fraser North",
            png = file.path(dataDir, "WestFraser_N.png"))
    # plotFMA(wf_n, provs = ab, caribou = NULL, xsr = wf_s_sr,
    #         title = "West Fraser North", png = NULL)
  }

  if (grepl("LandWeb|WestFraser_S", studyAreaName)) {
    ## reportingPolygons
    wf_s <- wf[4:5, ] ## Hinton and Edson FMAs
    raster::shapefile(wf_s, filename = file.path(dataDir, "WestFraser_S.shp"), overwrite = TRUE)

    wf_s.ansr <- postProcess(ml[["Alberta Natural Subregions"]],
                             studyArea = wf_s, useSAcrs = TRUE,
                             filename2 = file.path(dataDir, "WestFraser_S_ANSR"),
                             overwrite = TRUE) %>%
      joinReportingPolygons(., wf_s)
    wf_s.caribou <- postProcess(ml[["LandWeb Caribou Ranges"]],
                                studyArea = wf_s, useSAcrs = TRUE,
                                filename2 = file.path(dataDir, "WestFraser_S_caribou.shp"),
                                overwrite = TRUE) %>%
      joinReportingPolygons(., wf_s)

    ml <- mapAdd(wf_s, ml, layerName = "West Fraser S", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser S", isStudyArea = isTRUE(asStudyArea),
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_s.ansr, ml, layerName = "West Fraser S ANSR", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser S ANSR",
                 columnNameForLabels = "Name", filename2 = NULL)
    ml <- mapAdd(wf_s.caribou, ml, layerName = "West Fraser S Caribou", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "West Fraser S Caribou",
                 columnNameForLabels = "Name", filename2 = NULL)

    ## studyArea shouldn't use analysisGroup because it's not a reportingPolygon
    wf_s_sr <- postProcess(ml[["LandWeb Study Area"]],
                           studyArea = amc::outerBuffer(wf_s, bufferDist),
                           useSAcrs = TRUE,
                           filename2 = file.path(dataDir, "WestFraser_S_SR.shp"),
                           overwrite = TRUE)

    if (isTRUE(asStudyArea)) {
      ml <- mapAdd(wf_s_sr, ml, isStudyArea = TRUE, layerName = "West Fraser S SR",
                   useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                   columnNameForLabels = "NSN", filename2 = NULL)
    }

    plotFMA(wf_s, provs = ab, caribou = NULL, xsr = wf_s_sr, title = "West Fraser South",
            png = file.path(dataDir, "WestFraser_S.png"))
    # plotFMA(wf_s, provs = ab, caribou = NULL, xsr = wf_s_sr,
    #         title = "West Fraser South", png = NULL)

  } ## TODO: add "All" option

  return(ml)
}
