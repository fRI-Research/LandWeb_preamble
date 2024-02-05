defineModule(sim, list(
  name = "LandWeb_preamble",
  description = "define FMA-specific study areas etc. for LandWeb",
  keywords = c("LandWeb"),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut"))
  ),
  childModules = character(0),
  version = list(LandWeb_preamble = "0.0.5"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandWeb_preamble.Rmd"),
  reqdPkgs = list("achubaty/amc@development",
                  "crayon", "curl", "dplyr", "fasterize", "geodata", "ggplot2", "httr",
                  "PredictiveEcology/LandR@development (>= 1.1.0.9015)",
                  "PredictiveEcology/LandWebUtils@development (>= 0.1.5.9000)",
                  "PredictiveEcology/map@development (>= 0.0.5)",
                  "maptools", "nngeo",
                  "PredictiveEcology/pemisc@development (>= 0.0.3.9007)",
                  "raster", "RColorBrewer", "RCurl",
                  "PredictiveEcology/reproducible@development (>= 1.2.16.9024)",
                  "scales", "sf", "sp", "SpaDES.tools", "XML"),
  parameters = rbind(
    defineParameter("bufferDist", "numeric", 25000, 20000, 100000,
                    "Study area buffer distance (m) used to make `studyArea`."),
    defineParameter("bufferDistLarge", "numeric", 50000, 20000, 100000,
                    "Study area buffer distance (m) used to make `studyAreaLarge`."),
    defineParameter("forceResprout", "logical", FALSE, NA, NA,
                    paste("`TRUE` forces all species to resprout, setting `resproutage_min` to zero,",
                          "`resproutage_max` to 400, and `resproutProb` to 1.0.")),
    defineParameter("friMultiple", "numeric", 1.0, 0.5, 2.0,
                    "Multiplication factor for adjusting fire return intervals."),
    defineParameter("dispersalType", "character", "default", NA, NA,
                    "One of 'aspen', 'high', 'none', or 'default'."),
    defineParameter("mergeSlivers", "logical", FALSE, NA, NA,
                    "Should sliver polygons in LTHFC map be merged into nearest non-zero polygon?"),
    defineParameter("minFRI", "numeric", 40, 0, 200,
                    "The value of fire return interval below which, pixels will be changed to `NA`, i.e., ignored"),
    defineParameter("pixelSize", "numeric", 250, NA, NA,
                    paste("Pixel size in metres. Should be one of 250, 125, 50, 25.")),
    defineParameter("ROStype", "character", "default", NA, NA,
                    "Rate of spread preset to use. One of 'burny', 'equal', 'log', or 'default'."),
    defineParameter("treeClassesLCC", "integer", c(1L:15L, 20L, 32L, 34L:36L), 0L, 39L,
                    paste("AKA `forestedLCCClasses`. The classes in the `LCC2005` layer that are",
                          "considered 'trees' from the perspective of LandR-Biomass.")),
    defineParameter("treeClassesToReplace", "numeric", c(34:36), 0, 39,
                    paste("The transient classes in the `LCC2005` layer that will become 'trees'",
                          "from the perspective of LandR-Biomass (e.g., burned)")),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA,
                    "This describes the simulation time interval between plot events"),
    defineParameter(".plots", "character", "object", NA, NA,
                    paste("Passed to `types` in `Plots` (see `?Plots`).",
                          "There are a few plots that are made within this module, if set.",
                          "Note that plots (or their data) saving will ONLY occur at `end(sim)`.",
                          "If `NA`, plotting is turned off completely (this includes plot saving).")),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events"),
    defineParameter(".sslVerify", "integer", as.integer(unname(curl::curl_options("^ssl_verifypeer$"))), NA , NA,
                    paste("Passed to `httr::config(ssl_verifypeer = P(sim)$sslVerify)` when downloading KNN",
                          "(NFI) datasets. Set to 0L if necessary to bypass checking the SSL certificate (this",
                          "may be necessary when NFI's website SSL certificate is not correctly configured).")),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used. If `NA`, a hash of `studyAreaLarge` will be used."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity and time are not relevant"))
  ),
  inputObjects = bindrows(
    ## TODO: uses CC and fire return interval maps from URL in init
    expectsInput("canProvs", "SpatialPolygonsDataFrame", "Canadian provincial boundaries shapefile", NA)
  ),
  outputObjects = bindrows(
    createsOutput("CC TSF", "RasterLayer",
                  desc = "Time since fire (aka age) map derived from Current Conditions data."),
    createsOutput("fireReturnInterval", "RasterLayer",
                  desc = "fire return interval raster"),
    createsOutput("LandTypeCC", "RasterLayer",
                  desc = "Land Cover Classification map derived from Current Conditions data."),
    createsOutput("ml", "map",
                  desc = "`map` object containing study areas, reporting polygons, etc. for post-processing."),
    createsOutput("LCC", "RasterLayer",
                  desc = "The result of `LandR::overlayLCCs()` on `LCC2005` and `LandTypeCC`."),
    createsOutput("nonTreePixels", "integer",
                  desc = NA),
    createsOutput("rasterToMatch", "RasterLayer",
                  desc = NA),
    createsOutput("rasterToMatchLarge", "RasterLayer",
                  desc = NA),
    createsOutput("rasterToMatchReporting", "RasterLayer",
                  desc = NA),
    createsOutput("ROSTable", "data.table",
                  desc = paste("A `data.table` with 3 columns: `age`, `leading`, and `ros`.",
                               "The values under the `age` column can be `mature`, `immature`,",
                               "`young` and compound versions of these, e.g., `immature_young`",
                               "which can be used when 2 or more age classes share same `ros`.",
                               "`leading` should be vegetation type.",
                               "`ros` gives the rate of spread values for each age and type.")),
    createsOutput("rstFlammable", "RasterLayer",
                  desc = NA),
    createsOutput("speciesParams", "list",
                  desc = paste("list of updated species trait values to be used to updated",
                               "`speciesTable` to create `species`.")),
    createsOutput("speciesTable", "data.table",
                  desc = paste("a table of invariant species traits with the following trait colums:",
                               "'species', 'Area', 'longevity', 'sexualmature', 'shadetolerance',",
                               "'firetolerance', 'seeddistance_eff', 'seeddistance_max', 'resproutprob',",
                               "'resproutage_min', 'resproutage_max', 'postfireregen', 'leaflongevity',",
                               "'wooddecayrate', 'mortalityshape', 'growthcurve', 'leafLignin',",
                               "'hardsoft'. Names can differ, but not the column order.",
                               "Default is from Dominic Cyr and Yan Boulanger's project.")),
    createsOutput("sppColorVect", "character",
                  desc = paste("A named vector of colors to use for plotting.",
                               "The names must be in `sim$sppEquiv[['LandWeb']]`,",
                               "and should also contain a color for 'Mixed'")),
    createsOutput("sppEquiv", "data.table",
                  desc = "table of species equivalencies. See `LandR::sppEquivalencies_CA`."),
    createsOutput("studyArea", "SpatialPolygonsDataFrame",
                  desc = "Polygon to use as the simulation study area."),
    createsOutput("studyAreaLarge", "SpatialPolygonsDataFrame",
                  desc = paste("Polygon to use as the parametrisation study area.",
                               "Note that `studyAreaLarge` is only used for parameter estimation, and",
                               "can be larger than the actual study area used for LandR simulations",
                               "(e.g, larger than `studyArea` in LandR `Biomass_core`).")),
    createsOutput("studyAreaReporting", "SpatialPolygonsDataFrame",
                  desc = paste("multipolygon (typically smaller/unbuffered than `studyAreaLarge` and `studyArea`",
                               "in LandR `Biomass_core`) to use for plotting/reporting."))
  )
))

doEvent.LandWeb_preamble = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- InitMaps(sim)
      sim <- InitSpecies(sim)
      sim <- InitLandMine(sim)

      if (anyPlotting(P(sim)$.plots)) {
        if ("screen" %in% P(sim)$.plots) {
          sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "LandWeb_preamble", "plotMaps")
        }
      }
    },
    plotMaps = {
      PlotMaps(sim)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

InitMaps <- function(sim) {
  allowedStudyAreaNames <- c("ANC", "AlPac", "BlueRidge", "DMI", "Edson", "FMANWT", "FMU",
                             "LandWeb", "LP", "Manning", "MillarWestern", "Mistik", "MPR",
                             "provAB", "provMB", "provNWT", "provSK", "random",
                             "SprayLake", "Sundre", "Tolko", "Vanderwell", "WeyCo", "WestFraser")
  if (!grepl(paste(allowedStudyAreaNames, collapse = "|"), P(sim)$.studyAreaName)) {
    stop(".studyAreaName, ", P(sim)$.studyAreaName, ", does not contain valid study area name.\n",
         "Study area name must be one of:\n", paste(allowedStudyAreaNames, collapse = ", "), ".")
  }

  ## NOTE (2019-11-08): targetCRS needs to be character, not CRS class due to change in data.table
  targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                     "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  ## TODO: use terra
  opts <- options(reproducible.useTerra = FALSE)
  lthfc <- prepInputs(
    # url = "https://drive.google.com/file/d/1JptU0R7qsHOEAEkxybx5MGg650KC98c6", ## landweb_ltfc_v6.shp
    # url = "https://drive.google.com/file/d/1eu5TJS1NhzqbnDenyiBy2hAnVI1E3lsC", ## landweb_ltfc_v8.shp
    # url = "https://drive.google.com/file/d/1wNxOeV1vl05WDp6DsyuyRSbDZOu87N17", ## landweb_ltfc_v8a.shp
    url = "https://drive.google.com/file/d/1R9QLvW_yD482xv_6ZF1yhB32blaDPWjV", ## landweb_ltfc_v8c.shp
    targetCRS = targetCRS,
    overwrite = TRUE,
    filename2 = NULL
  )
  options(opts)

  ## keep only the LTHFC column
  lthfc <- lthfc[, "LTHFC"]
  lthfc$area <- sf::st_area(lthfc)

  ## 2023-09: added additional geoprocessing to LTHFC map to remove polygon fragments
  if (isTRUE(P(sim)$mergeSlivers)) {
    smallerThanOnePixel <- (lthfc$area <= units::as_units((P(sim)$pixelSize)^2, "m^2"))
    # smallerThanOnePixel <- (lthfc$area <= units::as_units(1500, "ha")) ## MB LTHFC 85 fragment size

    slivers <- lthfc[smallerThanOnePixel, ]
    nonSlivers <- lthfc[!smallerThanOnePixel, ] |> subset(LTHFC > 0)

    nearest <- sf::st_nearest_feature(slivers, nonSlivers)
    lthfc_merged <- lapply(unique(nearest), function(i) {
      slivers[nearest == i, ] |>
        sf::st_union() |> ## merge multiple slivers if more than one
        sf::st_union(nonSlivers[i, ]) |> ## merge with non-slivers (i.e., update geametries)
        cbind(sf::st_drop_geometry(nonSlivers[i, ]))
    }) |>
      do.call(rbind, args = _) |>
      sf::st_as_sf() |>
      sf::st_make_valid() |>
      rbind(nonSlivers[!(seq_len(nrow(nonSlivers)) %in% nearest), ]) |> ## merge remaining nonSlivers
      rbind(subset(lthfc, LTHFC == 0)) ## add back the zero LTHFC polygons
    lthfc_merged$area <- sf::st_area(lthfc_merged) ## recalculate areas

    lthfc_clean <- LandWebUtils::polygonClean(as_Spatial(lthfc_merged), type = "LandWeb", minFRI = P(sim)$minFRI)
  } else {
    lthfc_clean <- LandWebUtils::polygonClean(as_Spatial(lthfc), type = "LandWeb", minFRI = P(sim)$minFRI)
  }

  sf::st_as_sf(lthfc_clean) |>
    sf::write_sf(file.path(outputPath(sim), "landweb_lthfc_clean.shp"))

  ## LandWeb study area provides LTHFC (aka "fire return interval") map:
  ## 1. we want the actual LTHFC map;
  ## 2. we want the boundary (outline) of the entire study area.
  ml <- mapAdd(lthfc_clean, layerName = "LTHFC", overwrite = TRUE,
               columnNameForLabels = "fireReturnInterval", isStudyArea = FALSE, filename2 = NULL)

  ## use outer perimeter as LandWeb study area (don't need the internal polygon boundaries)
  landweb_area <- sf::st_as_sf(lthfc_clean) |>
    sf::st_union() |>
    sf::st_make_valid() |>
    nngeo::st_remove_holes() |>
    sf::as_Spatial()
  landweb_area$Name <- "LandWeb Study Area"

  ml <- mapAdd(landweb_area, map = ml, layerName = "LandWeb Study Area",
               targetCRS = targetCRS, overwrite = TRUE,
               columnNameForLabels = "Name", isStudyArea = TRUE, filename2 = NULL)

  ## Updated FMA boundaries
  ml <- mapAdd(map = ml, layerName = "FMA Boundaries Updated",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1BNsUiMqENJa0I8gzhO68K307ySPHbdGk",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## AB FMU boundaries
  ## TODO: only add if studyAreaReporting in AB
  ml <- mapAdd(map = ml, layerName = "AB FMU Boundaries",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/open?id=1OH3b5pwjumm1ToytDBDI6jthVe2pp0tS", # 2020-06
               columnNameForLabels = "FMU_NAME", isStudyArea = FALSE, filename2 = NULL)

  ### Rename some polygons:
  ###   - DMI is now Mercer (MPR)
  ids <- grep("Daishowa-Marubeni International Ltd", ml[["FMA Boundaries Updated"]][["Name"]])
  newNames <- c("Mercer Peace River Pulp Ltd. (East)", "Mercer Peace River Pulp Ltd. (West)")
  ml[["FMA Boundaries Updated"]][["Name"]][ids] <- newNames
  ml[["FMA Boundaries Updated"]][["shinyLabel"]][ids] <- newNames

  ## National ecozones
  ml <- mapAdd(map = ml, layerName = "National Ecozones",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
               columnNameForLabels = "REGION_NAM", isStudyArea = FALSE, filename2 = NULL)
  ml[["National Ecozones"]][["Name"]] <- tools::toTitleCase(tolower(ml[["National Ecozones"]][["ZONE_NAME"]]))

  ## National ecoregions
  ml <- mapAdd(map = ml, layerName = "National Ecoregions",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
               columnNameForLabels = "REGION_NAM", isStudyArea = FALSE, filename2 = NULL)
  ml[["National Ecoregions"]][["Name"]] <- ml[["National Ecoregions"]][["REGION_NAM"]]

  ## Alberta Natural Subregions (ANSRs)
  ## TODO: only add if studyAreaReporting in AB
  ml <- mapAdd(map = ml, layerName = "Alberta Natural Subregions",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1hW6zy0CpUBdk-K2IAjzW4INjVl1J4aLJ",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## BC biogeoclimatic zones
  ## TODO: only add if studyAreaReporting in BC
  ml <- mapAdd(map = ml, layerName = "BC Biogeoclimatic zones",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1NS15Gd7dHEhvPOy-Ol_LBtf-4Ch6mPnS",
               columnNameForLabels = "ZONE_NAME", isStudyArea = FALSE, filename2 = NULL)
  ml[["BC Biogeoclimatic zones"]][["Name"]] <- ml[["BC Biogeoclimatic zones"]][["ZONE_NAME"]]

  ## NWT ecoregions
  ## TODO: only add if studyAreaReporting in NWT
  ml <- mapAdd(map = ml, layerName = "Northwest Territories Ecoregions",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1iRAQfARkmS6-XVHFnTkB-iltzMNPAczC",
               columnNameForLabels = "ECO4_NAM_1", isStudyArea = FALSE, filename2 = NULL)
  ml[["Northwest Territories Ecoregions"]][["Name"]] <- ml[["Northwest Territories Ecoregions"]][["ECO4_NAM_1"]]

  ## Caribou Ranges
  # ml <- mapAdd(map = ml, layerName = "Boreal Caribou Ranges",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://drive.google.com/file/d/1PYLou8J1wcrme7Z2tx1wtA4GvaWnU1Jy",
  #              columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)
  # ml <- mapAdd(map = ml, layerName = "BC Caribou Ranges",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://drive.google.com/file/d/1uqEVID74y4enPMee2w3axBcR1agw_kMT",
  #              columnNameForLabels = "HERD_NAME", isStudyArea = FALSE, filename2 = NULL) ## untested
  # ml <- mapAdd(map = ml, layerName = "AB Caribou Ranges",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://extranet.gov.ab.ca/srd/geodiscover/srd_pub/LAT/FWDSensitivity/CaribouRange.zip",
  #              columnNameForLabels = "SUBUNIT", isStudyArea = FALSE, filename2 = NULL) ## untested
  ml <- mapAdd(map = ml, layerName = "SK Caribou Ranges",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1LiizDyXOfJPQ76FQM8SQ1_kYG9hJUDdK",
               columnNameForLabels = "RGEUNIT", isStudyArea = FALSE, filename2 = NULL)
  ml[["SK Caribou Ranges"]][["Name"]] <- ml[["SK Caribou Ranges"]][["RGEUNIT"]]

  if (grepl("provMB", P(sim)$.studyAreaName)) {
    ## TODO: .zipx file; needs 'manual' extract 1st time
    ml <- mapAdd(map = ml, layerName = "MB Caribou Ranges",
                 useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
                 url = "https://drive.google.com/file/d/1Y_Qi3twoU3fHaNgMzF5QEl1CosGmGyha/",
                 targetFile = "Boreal_caribou_MUs_MB_2015.shp", alsoExtract = "similar",
                 columnNameForLabels = "RANGE_NAME", isStudyArea = FALSE, filename2 = NULL)
    ml[["MB Caribou Ranges"]][["Name"]] <- ml[["MB Caribou Ranges"]][["RANGE_NAME"]]
  }

  ml <- mapAdd(map = ml, layerName = "LandWeb Caribou Ranges",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1mrsxIJfdP-XxEZkO6vs2J6lYbGry67A2",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## Provincial Boundaries
  ml <- mapAdd(sim$canProvs, map = ml, layerName = "Provincial Boundaries",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               columnNameForLabels = "NAME_1", isStudyArea = FALSE, filename2 = NULL)

  ################################################################################
  ## COMPANY-SPECIFIC STUDY AREAS -- be sure to update allowedStudyAreaNames above !!
  dataDir <- checkPath(file.path(inputPath(sim), "studyAreas"), create = TRUE)

  if (grepl("ANC", P(sim)$.studyAreaName)) {
    ml <- fmaANC(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("AlPac", P(sim)$.studyAreaName)) {
    ml <- fmaAlpac(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("DMI|MPR", P(sim)$.studyAreaName)) {
    ml <- fmaDMI(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("Edson", P(sim)$.studyAreaName)) {
    ml <- fmaEdsonFP(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("FMANWT", P(sim)$.studyAreaName)) {
    ml <- fmaNWT(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("FMU", P(sim)$.studyAreaName)) {
    ml <- fmu(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("LandWeb", P(sim)$.studyAreaName)) {
    ml <- allLandWeb(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("LP", P(sim)$.studyAreaName)) {
    ml <- fmaLP(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("Manning", P(sim)$.studyAreaName)) {
    ml <- fmaManning(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("MillarWestern", P(sim)$.studyAreaName)) {
    ml <- fmaMillarWestern(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("Mistik", P(sim)$.studyAreaName)) {
    ml <- fmaMistik(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("SprayLake", P(sim)$.studyAreaName)) {
    ml <- fmaSprayLake(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("Sundre", P(sim)$.studyAreaName)) {
    ml <- fmaSundreFP(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("Tolko|tolko", P(sim)$.studyAreaName)) {
    ml <- fmaTolko(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("Vanderwell", P(sim)$.studyAreaName)) {
    ml <- fmaVanderwell(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("WeyCo", P(sim)$.studyAreaName)) {
    ml <- fmaWeyCo(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("WestFraser|BlueRidge", P(sim)$.studyAreaName)) {
    ml <- fmaWestFraser(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("provAB", P(sim)$.studyAreaName)) {
    ml <- provAB(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("provMB", P(sim)$.studyAreaName)) {
    ml <- provMB(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("provNWT", P(sim)$.studyAreaName)) {
    ml <- provNWT(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("provSK", P(sim)$.studyAreaName)) {
    ml <- provSK(ml, P(sim)$.studyAreaName, dataDir, sim$canProvs, P(sim)$bufferDist, asStudyArea = TRUE)
  } else if (grepl("random", P(sim)$.studyAreaName)) {
    ## use a small random study area
    message(crayon::red("Using random study area."))
    ranSeed <- .Random.seed
    set.seed(867)
    rnd <- SpaDES.tools::randomPolygon(ml[["Alberta Natural Subregions"]], area = 4e5) ## random area in Central-East AB
    set.seed(ranSeed)

    if (FALSE) {
      sp::plot(spTransform(sim$canProvs[sim$canProvs$NAME_1 == "Alberta", ], targetCRS))
      sp::plot(rnd, col = "darkgrey", add = TRUE)
    }

    ml <- mapAdd(rnd, ml, layerName = "Random Study Area", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Random Study Area", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)

    rnd_sr <- postProcess(ml[["LandWeb Study Area"]],
                          studyArea = amc::outerBuffer(rnd, P(sim)$bufferDist),
                          useSAcrs = TRUE, filename2 = NULL)
    ml <- mapAdd(rnd_sr, ml, isStudyArea = TRUE, layerName = "Random Study Area SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  ## study areas ---------------------------------------------------------------------------------
  sim$studyArea <- studyArea(ml, 3)           ## buffered study area
  #sim$studyAreaLarge <- studyArea(ml, 1)     ## entire LandWeb area (too big for fitting etc. for now)
  sim$studyAreaLarge <- amc::outerBuffer(studyArea(ml, 2), P(sim)$bufferDistLarge) ## further buffered study area
  sim$studyAreaReporting <- studyArea(ml, 2)  ## reporting area (e.g., FMA)

  ## LCC 2005 / raster to match ------------------------------------------------------------------
  LCC2005large <- prepInputsLCC(year = 2005, studyArea = sim$studyAreaLarge, destinationPath = Paths$inputPath)
  if (P(sim)$pixelSize != 250) {
    stopifnot(P(sim)$pixelSize %in% c(125, 50, 25))
    LCC2005large <- Cache(raster::disaggregate, x = LCC2005large,
                          fact = as.integer(250 / P(sim)$pixelSize))
  }
  LCC2005large[] <- as.integer(LCC2005large[])

  ml <- mapAdd(LCC2005large, layerName = "LCC2005large", map = ml, filename2 = NULL, leaflet = FALSE,
               isRasterToMatch = TRUE, method = "ngb")
  ## TODO: should be rasterToMatch, but not getting studyAreaLarge
  ml[[ml@metadata[ml@metadata$rasterToMatch == 1, ]$layerName]] <- LCC2005large ## workaround

  sim$rasterToMatch <- postProcess(rasterToMatch(ml), studyArea = sim$studyArea, filename2 = NULL)
  sim$rasterToMatchLarge <- LCC2005large
  sim$rasterToMatchReporting <- postProcess(rasterToMatch(ml), studyArea = sim$studyAreaReporting, filename2 = NULL)

  ## Current Conditions --------------------------------------------------------------------------

  ## Manitoba uses current conditions layers (2016) which cover the province;
  ## otherwise, use the original CC layers
  if (grepl("provMB", P(sim)$.studyAreaName)) {
    ccURL <- "https://drive.google.com/file/d/1KTqNBntNrEsDL6jk-5bchsBOcraDqNHe/"
    fname_age <- "MB_Age2016_NRV.tif"
    LandTypeFileCC <- file.path(Paths$inputPath, "MB_Landtype2016_NRV.tif")
  } else {
    ccURL <- "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1"
    fname_age <- "Age1.tif"
    LandTypeFileCC <- file.path(Paths$inputPath, "LandType1.tif")
  }

  sim$LandTypeCC <- Cache(prepInputs, LandTypeFileCC,
                          studyArea = sim$studyAreaLarge,
                          url = ccURL, method = "ngb",
                          rasterToMatch = rasterToMatch(ml),
                          filename2 = NULL)
  sim$LandTypeCC[] <- as.integer(sim$LandTypeCC[])

  ## Non-Tree pixels -----------------------------------------------------------------------------
  ## Setting NA values
  ## 3 is shrub, wetland, grassland -- no veg dynamics happen -- will burn in fire modules
  ## 4 is water, rock, ice
  ## 5 is no Data ... this is currently cropland -- will be treated as grassland for fires
  treeClassesCC <- c(0, 1, 2)
  nontreeClassesCC <- c(3, 4)
  treePixelsCCTF <- sim$LandTypeCC[] %in% treeClassesCC
  LandTypeCCNA <- is.na(sim$LandTypeCC[])
  noDataPixelsCC <- LandTypeCCNA | (sim$LandTypeCC[] == 5)
  treePixelsCC <- which(treePixelsCCTF)

  uniqueLCCClasses <- na.omit(unique(ml$LCC2005large[]))
  nontreeClassesLCC <- sort(uniqueLCCClasses[!uniqueLCCClasses %in% P(sim)$treeClassesLCC])

  ## for each LCC2005 + CC class combo, define which LCC2005 code should be used
  ## remember, setting a pixel to NA will omit it entirely (i.e., non-vegetated)
  remapDT <- as.data.table(expand.grid(LCC2005 = c(NA_integer_, sort(uniqueLCCClasses)),
                                       CC = c(NA_integer_, 0:5)))
  remapDT[LCC2005 == 0, newLCC := NA_integer_]
  remapDT[is.na(CC) | CC == 5, newLCC := LCC2005]
  remapDT[CC == 4, newLCC := NA_integer_]
  remapDT[CC %in% 0:3, newLCC := LCC2005]
  remapDT[is.na(LCC2005) & CC %in% 0:2, newLCC := 99] ## reclassification needed
  remapDT[LCC2005 %in% P(sim)$treeClassesToReplace, newLCC := 99] ## reclassification needed

  message("Overlaying land cover maps...")
  LCClarge <- Cache(overlayLCCs,
                    LCCs = list(CC = sim$LandTypeCC, LCC2005 = ml$LCC2005large),
                    forestedList = list(CC = 0, LCC2005 = P(sim)$treeClassesLCC),
                    outputLayer = "LCC2005",
                    remapTable = remapDT,
                    classesToReplace = c(P(sim)$treeClassesToReplace, 99),
                    availableERC_by_Sp = NULL)
  message("...done.")

  treePixelsLCC <- which(LCClarge[] %in% P(sim)$treeClassesLCC)
  nonTreePixels <- which(LCClarge[] %in% nontreeClassesLCC)

  sim$nonTreePixels <- nonTreePixels

  ## Update rasterToMatch layer with all trees
  ml[[ml@metadata[ml@metadata$rasterToMatch == 1, ]$layerName]][sim$nonTreePixels] <- NA
  sim$rasterToMatch <- postProcess(rasterToMatch(ml), studyArea = sim$studyArea, filename2 = NULL)

  ## Age from Current Conditions -----------------------------------------------------------------
  TSFLayerName <- "CC TSF"
  ml <- mapAdd(map = ml, url = ccURL, layerName = TSFLayerName, CC = TRUE,
               tsf = file.path(Paths$inputPath, fname_age), analysisGroup1 = "CC",
               targetFile = fname_age, filename2 = NULL,
               useCache = TRUE, isRasterToMatch = FALSE,
               alsoExtract = "similar", leaflet = FALSE)
  ml[[TSFLayerName]][] <- as.integer(ml[[TSFLayerName]][])

  ageCClarge <- Cache(postProcess,
                      x = raster(file.path(Paths$inputPath, fname_age)),
                      filename1 = NULL,
                      filename2 = NULL,
                      studyArea = sim$studyAreaLarge,
                      rasterToMatch = sim$rasterToMatchLarge,
                      maskWithRTM = TRUE,
                      method = "bilinear",
                      datatype = "INT2U",
                      userTags = c("stable", currentModule(sim)))
  ageCClarge[ageCClarge < 0] <- 0
  ml[[TSFLayerName]] <- as.integer(ageCClarge)

  ## Age from kNN --------------------------------------------------------------------------------
  ## see https://open.canada.ca/data/en/dataset/ec9e2659-1c29-4ddb-87a2-6aced147a990
  standAgeMapURL <- paste0(
    "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
    "canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/",
    "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"
  )
  standAgeMapFileName <- basename(standAgeMapURL)

  httr::with_config(config = httr::config(ssl_verifypeer = P(sim)$.sslVerify), {
    standAgeMap <- Cache(prepInputs, #notOlderThan = Sys.time(),
                         targetFile = standAgeMapFileName,
                         destinationPath = Paths$inputPath,
                         url = standAgeMapURL,
                         fun = "raster::raster",
                         studyArea = sim$studyAreaLarge,
                         rasterToMatch = sim$rasterToMatchLarge,
                         maskWithRTM = TRUE,
                         method = "bilinear",
                         datatype = "INT2U",
                         filename2 = NULL, overwrite = TRUE,
                         userTags = c("stable", currentModule(sim)))
  })
  ml[[TSFLayerName]][noDataPixelsCC] <- standAgeMap[noDataPixelsCC]
  ml[[TSFLayerName]][sim$nonTreePixels] <- NA
  attr(ml[[TSFLayerName]], "imputedPixID") <- integer(0) ## TODO: reassess whether overlay counts as imputation

  ## Flammability and Fire Return Interval rasters -----------------------------------------------

  ## flammability map shouldn't be masked (no gaps!);
  #    NAs outside the buffered study & snow/rock/ice area are the only values we want NA
  #    use the LCC flammability map to fill in NA / nodata values

  # No data class is 5 -- these will be filled in by LCC2005 layer
  # NA_ids <- which(is.na(sim$LandTypeCC[]) | sim$LandTypeCC[] == 5)
  # Only class 4 is considered non-flammable
  rstFlammableCC <- defineFlammable(sim$LandTypeCC, nonFlammClasses = 4L,
                                    mask = NULL, filename2 = NULL)
  rstFlammableCC <- deratify(rstFlammableCC, complete = TRUE)

  # Only classes 36, 37, 38, 39 is considered non-flammable
  rstFlammableLCC <- defineFlammable(LCC2005large, nonFlammClasses = 36L:39L,
                                     mask = NULL, filename2 = NULL)
  rstFlammableLCC <- deratify(rstFlammableLCC, complete = TRUE)

  sim$rstFlammable <- rstFlammableCC
  sim$rstFlammable[LandTypeCCNA] <- rstFlammableLCC[LandTypeCCNA]
  sim$rstFlammable[] <- as.integer(sim$rstFlammable[])
  sim$rstFlammable <- crop(sim$rstFlammable, sim$rasterToMatch) ## ensure it matches studyArea

  ## fireReturnInterval needs to be masked by rstFlammable
  rstFireReturnInterval <- fasterize::fasterize(sf::st_as_sf(ml[["LTHFC"]]),
                                                raster = rasterToMatch(ml),
                                                field = "fireReturnInterval")
  rstFireReturnInterval <- crop(rstFireReturnInterval, sim$rasterToMatch) ## ensure it matches studyArea

  if (!is.integer(rstFireReturnInterval[]))
    rstFireReturnInterval[] <- as.integer(rstFireReturnInterval[])

  ml <- mapAdd(rstFireReturnInterval, layerName = "fireReturnInterval", filename2 = NULL,
               map = ml, leaflet = FALSE, maskWithRTM = FALSE)
  ml$fireReturnInterval <- crop(ml$fireReturnInterval, sim$rasterToMatch) ## ensure it matches studyArea

  if (P(sim)$friMultiple != 1) {
    ml$fireReturnInterval <- as.integer(P(sim)$friMultiple * ml$fireReturnInterval)
  }

  sim$fireReturnInterval <- ml$fireReturnInterval
  sim$LCC <- LCClarge
  sim[[TSFLayerName]] <- ml[[TSFLayerName]]

  sim$ml <- ml

  ## some assertions:
  testObjs <- c("studyArea", "studyAreaLarge", "studyAreaReporting",
                "rasterToMatch", "rasterToMatchLarge", "rasterToMatchReporting",
                "fireReturnInterval", TSFLayerName)
  lapply(testObjs, function(x) {
    if (is.null(sim[[x]]))
      stop("LandWeb_preamble: ", paste0("sim$", x, " returned NULL."), call. = FALSE)
  })

  compareRaster(sim$rasterToMatch, rstFireReturnInterval, sim$rstFlammable)
  ## end assertions

  return(invisible(sim))
}

InitSpecies <- function(sim) {
  sppEquiv <- LandR::sppEquivalencies_CA
  sppEquiv[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                    EN_generic_full = "Pine",
                                    Leading = "Pine leading")]

  # Make LandWeb spp equivalencies
  sppEquiv[, LandWeb := c(Pice_mar = "Pice_mar", Pice_gla = "Pice_gla",
                          Pinu_con = "Pinu_sp", Pinu_ban = "Pinu_sp",
                          Popu_tre = "Popu_sp", Betu_pap = "Popu_sp",
                          Abie_bal = "Abie_sp", Abie_las = "Abie_sp", Abie_sp = "Abie_sp")[LandR]]

  sppEquiv[LandWeb == "Abie_sp", `:=`(EN_generic_full = "Fir",
                                      EN_generic_short = "Fir",
                                      Leading = "Fir leading")]

  sppEquiv[LandWeb == "Popu_sp", `:=`(EN_generic_full = "Deciduous",
                                      EN_generic_short = "Decid",
                                      Leading = "Deciduous leading")]

  sim$sppEquiv <- sppEquiv[!is.na(LandWeb), ]
  sim$sppColorVect <- LandR::sppColors(sim$sppEquiv, "LandWeb", newVals = "Mixed", palette = "Accent")

  ## species parameter tables
  sim$speciesTable <- LandR::getSpeciesTable(dPath = mod$dPath) ## uses default URL

  speciesParams <- list(
    growthcurve = list(Abie_sp = 0, Pice_gla = 1, Pice_mar = 1, Pinu_sp = 0, Popu_sp = 0),
    mortalityshape = list(Abie_sp = 15L, Pice_gla = 15L, Pice_mar = 15L, Pinu_sp = 15L, Popu_sp = 25L),
    resproutage_min = list(Popu_sp = 25L), # default 10L
    #resproutprob = list(Popu_sp = 0.1), # default 0.5
    shadetolerance = list(Abie_sp = 3, Pice_gla = 2, Pice_mar = 3, Pinu_sp = 1, Popu_sp = 1) # defaults 4, 3, 4, 1, 1
  )

  ## seed dispersal (see LandWeb#96, LandWeb#112)
  stopifnot(P(sim)$dispersalType %in% c("default", "aspen", "high", "none"))

  if (isTRUE(P(sim)$forceResprout)) {
    speciesParams <- append(speciesParams, list(
      postfireregen = list(Abie_sp = "resprout", Pice_gla = "resprout", Pice_mar = "resprout",
                           Pinu_sp = "resprout", Popu_sp = "resprout"),
      resproutage_max = list(Abie_sp = 400L, Pice_gla = 400L, Pice_mar = 400L, Pinu_sp = 400L, Popu_sp = 400L),
      resproutage_min = list(Abie_sp = 0L, Pice_gla = 0L, Pice_mar = 0L, Pinu_sp = 0L, Popu_sp = 0L),
      resproutprob = list(Abie_sp = 1.0, Pice_gla = 1.0, Pice_mar = 1.0, Pinu_sp = 1.0, Popu_sp = 1.0)
    ))
  }

  speciesParams <- append(speciesParams, switch(
    P(sim)$dispersalType,
    aspen = list(
      seeddistance_eff = list(Abie_sp = 0L, Pice_gla = 0L, Pice_mar = 0L, Pinu_sp = 0L, Popu_sp = 100L),
      seeddistance_max = list(Abie_sp = 125L, Pice_gla = 125L, Pice_mar = 125L, Pinu_sp = 125L, Popu_sp = 235L)
    ),
    high = list(
      seeddistance_eff = list(Abie_sp = 250L, Pice_gla = 100L, Pice_mar = 320L, Pinu_sp = 300L, Popu_sp = 500L),
      seeddistance_max = list(Abie_sp = 1250L, Pice_gla = 1250L, Pice_mar = 1250L, Pinu_sp = 3000L, Popu_sp = 3000L)
    ),
    none = list(
      seeddistance_eff = list(Abie_sp = 25L, Pice_gla = 100L, Pice_mar = 80L, Pinu_sp = 30L, Popu_sp = 200L), ## default but disabled downstream
      seeddistance_max = list(Abie_sp = 160L, Pice_gla = 303L, Pice_mar = 200L, Pinu_sp = 100L, Popu_sp = 2000L) ## default but disabled downstream
    ),
    default = list(
      seeddistance_eff = list(Abie_sp = 25L, Pice_gla = 100L, Pice_mar = 80L, Pinu_sp = 30L, Popu_sp = 200L),
      seeddistance_max = list(Abie_sp = 160L, Pice_gla = 303L, Pice_mar = 200L, Pinu_sp = 100L, Popu_sp = 2000L)
    )
  ))

  if (P(sim)$.studyAreaName == "SprayLake") {
    message(crayon::red("Fir shade tolerance lowered below default (3). Using value 2."))
    message(crayon::red("Spruce shade tolerance raised above default (2, 3). Using values 3, 4."))
    speciesParams <- append(speciesParams, list(
      shadetolerance = list(Abie_sp = 2, Pice_gla = 3, Pice_mar = 4))
    )
  }

  sim$speciesParams <- speciesParams

  return(invisible(sim))
}

InitLandMine <- function(sim) {
  stopifnot(P(sim)$ROStype %in% c("default", "burny", "equal", "log"))

  LandMineROStable <- data.table::rbindlist(list(
    list("mature", "decid", 9L),
    list("immature_young", "decid", 6L),
    list("immature_young", "mixed", 12L),
    list("mature", "mixed", 17L),
    list("immature", "pine", 14L),
    list("mature", "pine", 21L),
    list("young", "pine", 22L),
    list("immature_young", "softwood", 18L),
    list("mature", "softwood", 27L),
    list("immature_young", "spruce", 20L),
    list("mature", "spruce", 30L)
  ))
  data.table::setnames(LandMineROStable, old = 1:3, new = c("age", "leading", "ros"))

  if (P(sim)$ROStype == "equal") {
    LandMineROStable$ros <- 1L
  } else if (P(sim)$ROStype == "log") {
    LandMineROStable$ros <- log(LandMineROStable$ros)
  }

  sim$ROSTable <- LandMineROStable

  return(invisible(sim))
}

PlotMaps <- function(sim) {
  if (isFALSE(quickPlot::isRstudioServer())) {
    lapply(dev.list(), function(x) {
      try(quickPlot::clearPlot(force = TRUE))
      try(dev.off())
    })
    quickPlot::dev(2, width = 18, height = 10)
    grid::grid.rect(0.90, 0.03, width = 0.2, height = 0.06, gp = gpar(fill = "white", col = "white"))
    grid::grid.text(label = P(sim)$.studyAreaName, x = 0.90, y = 0.03)
  }
  Plot(sim$studyAreaReporting, sim$studyArea, sim$studyAreaLarge,
       sim$rasterToMatchReporting, sim$rasterToMatch, sim$rasterToMatchLarge)
}

.inputObjects <- function(sim) {
  #cacheTags <- c(currentModule(sim), "function:.inputObjects")
  mod$dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", mod$dPath, "'.")

  if (!suppliedElsewhere("canProvs", sim)) {
    sim$canProvs <- geodata::gadm(country = "CAN", level = 1, path = mod$dPath) %>%
      sf::st_as_sf() %>%
      sf::as_Spatial()
  }

  return(invisible(sim))
}
