defineModule(sim, list(
  name = "LandWeb_preamble",
  description = "define FMA-specific study areas etc. for LandWeb",
  keywords = c("LandWeb"),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = "aut"),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(LandWeb_preamble = "1.0.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandWeb_preamble.Rmd"),
  reqdPkgs = list(
    "crayon", "curl", "dplyr", "fasterize", "geodata", "ggplot2", "httr",
    "nngeo", "RColorBrewer", "RCurl", "scales", "sf", "sp", "SpaDES.tools", "XML",
    "FOR-CAST/spatialutils",
    "PredictiveEcology/LandR@development (>= 1.1.0.9015)",
    "PredictiveEcology/LandWebUtils@development (>= 0.1.5.9000)",
    "PredictiveEcology/map@development (>= 0.0.5)",
    "PredictiveEcology/pemisc@development (>= 0.0.3.9007)",
    "PredictiveEcology/reproducible@development (>= 1.2.16.9024)"
  ),
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
    defineParameter("pixelSize", "integer", 240L, NA, NA,
                    paste("Pixel size in metres. Should be one of 240, 120, 90, 30.")),
    defineParameter("ROStype", "character", "default", NA, NA,
                    "Rate of spread preset to use. One of 'burny', 'equal', 'log', or 'default'."),
    defineParameter("treeClassesLCC", "integer", c(81, 210, 220, 230, 240), 0L, 240L,
                    paste("AKA `forestedLCCClasses`. The classes in the `LCC` layer that are",
                          "considered 'trees' from the perspective of LandR-Biomass.")),
    defineParameter("treeClassesToReplace", "numeric", 240, NA, NA,
                    paste("The transient classes in the `LCC` layer that will become 'trees'",
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
  ),
  outputObjects = bindrows(
    createsOutput("CC TSF", "RasterLayer",
                  desc = "Time since fire (aka age) map derived from Current Conditions data."),
    createsOutput("fireReturnInterval", "RasterLayer",
                  desc = "fire return interval raster"),
    createsOutput("LandTypeCC", "RasterLayer",
                  desc = "Land Cover Classification map derived from Current Conditions data."),
    createsOutput("LCC", "RasterLayer",
                  desc = "The result of `LandR::overlayLCCs()` on `LCC` and `LandTypeCC`."),
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
      mod$dPath <- asPath(inputPath(sim), 1)

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
  ## NOTE needs to be character, not `CRS` class, for downstream use with `data.table`
  targetCRS <- LandWebUtils::LandWebCRS

  if (grepl("SprayLake", P(sim)$.studyAreaName)) {
    ## 2024-09-23 per Dave, use custom lthfc only for Spray Lake + C5 runs;
    ## LTHFCS are *much* lower (200/150 reduced to 50 in eastern portion of study area)
    # lthfc_url <- "https://drive.google.com/file/d/1vvwqlS0hrD2s7Eq4N7NKrRDKWon4RvUw" ## ltfc_sls_v2.shp
    lthfc_url <- "https://drive.google.com/file/d/1udhnNh_zWap1fORuDMYVUXWQ0bNeeRAT" ## ltfc_sls_v3.shp
  } else {
    # lthfc_url <- "https://drive.google.com/file/d/1JptU0R7qsHOEAEkxybx5MGg650KC98c6" ## landweb_ltfc_v6.shp
    # lthfc_url <- "https://drive.google.com/file/d/1eu5TJS1NhzqbnDenyiBy2hAnVI1E3lsC" ## landweb_ltfc_v8.shp
    # lthfc_url <- "https://drive.google.com/file/d/1wNxOeV1vl05WDp6DsyuyRSbDZOu87N17" ## landweb_ltfc_v8a.shp
    lthfc_url <- "https://drive.google.com/file/d/1R9QLvW_yD482xv_6ZF1yhB32blaDPWjV" ## landweb_ltfc_v8c.shp
  }

  ## keep only the LTHFC column, and recalculate area
  lthfc <- prepInputs(
    url = lthfc_url,
    targetCRS = targetCRS,
    overwrite = TRUE,
    filename2 = NULL
  ) |>
    dplyr::select(LTHFC) |>
    dplyr::mutate(area = sf::st_area(geometry))

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
        sf::st_union(nonSlivers[i, ]) |> ## merge with non-slivers (i.e., update geometries)
        cbind(sf::st_drop_geometry(nonSlivers[i, ]))
    }) |>
      do.call(rbind, args = _) |>
      sf::st_as_sf() |>
      sf::st_make_valid() |>
      rbind(nonSlivers[!(seq_len(nrow(nonSlivers)) %in% nearest), ]) |> ## merge remaining nonSlivers
      rbind(subset(lthfc, LTHFC == 0)) ## add back the zero LTHFC polygons
    lthfc_merged$area <- sf::st_area(lthfc_merged) ## recalculate areas

    lthfc_clean <- LandWebUtils::polygonClean(lthfc_merged, type = "LandWeb", minFRI = P(sim)$minFRI)
  } else {
    lthfc_clean <- LandWebUtils::polygonClean(lthfc, type = "LandWeb", minFRI = P(sim)$minFRI)
  }

  ## LandWeb study area provides LTHFC (aka "fire return interval") map:
  ## 1. we want the actual LTHFC map (lthfc_clean);
  ## 2. we want the outer boundary of the entire study area (landweb_area).

  sf::st_as_sf(lthfc_clean) |>
    sf::write_sf(file.path(outputPath(sim), "landweb_lthfc_clean.shp"))

  landweb_area <- sf::st_as_sf(lthfc_clean) |>
    sf::st_union() |>
    sf::st_make_valid() |>
    nngeo::st_remove_holes()

  ## study areas ---------------------------------------------------------------------------------
  ## studyAreaReporting is the study area used for reporting (e.g., FMA);
  ## studyArea buffered to reduce edge effects in simulation;
  ## studyAreaLarge is further buffered for model parameter calibration.
  sim$studyAreaReporting <- LandWebUtils::prepStudyArea(
    name = P(sim)$.studyAreaName,
    destinationPath = mod$dPath,
    targetCRS = LandWebUtils::LandWebCRS
  )
  sim$studyArea <- spatialutils::outerBuffer(sim$studyAreaReporting, P(sim)$bufferDist)
  sim$studyAreaLarge <- spatialutils::outerBuffer(sim$studyAreaReporting, P(sim)$bufferDistLarge)

  ## save study area maps to file
  studyAreaDir <- file.path(inputPath(sim), "studyAreas") |> fs::dir_create()
  sf::st_write(
    obj = sim$studyAreaReporting,
    dsn = file.path(studyAreaDir, glue::glue("{P(sim)$.studyAreaName}.shp")),
    append = FALSE
  )

  ## LCC / rasterToMatch -------------------------------------------------------------------------
  LCClarge <- LandR::prepInputs_SCANFI_LCC_FAO( ## TODO: prepInputs fails to unzip
    year = 2020,
    destinationPath = mod$dPath,
    cropTo = sim$studyAreaLarge,
    maskTo = sim$studyAreaLarge
  ) |>
    Cache()

  if (P(sim)$pixelSize != 30) {
    stopifnot(P(sim)$pixelSize %in% c(240, 120, 90))
    LCClarge <- terra::aggregate(LCClarge, fact = as.integer(P(sim)$pixelSize / 30), fun = "modal")
  }
  LCClarge <- terra::as.int(LCClarge)

  sim$rasterToMatch <- terra::crop(LCClarge, terra::vect(sim$studyArea), mask = TRUE)
  sim$rasterToMatchLarge <- LCClarge
  sim$rasterToMatchReporting <- terra::crop(LCClarge, terra::vect(sim$studyAreaReporting), mask = TRUE)

  if (FALSE) {
    terra::plot(sim$rasterToMatchReporting)

    terra::plot(sim$rasterToMatch)
    terra::plot(terra::vect(sim$studyAreaReporting), add = TRUE)

    terra::plot(sim$rasterToMatchLarge)
    terra::plot(terra::vect(sim$studyArea), add = TRUE)
    terra::plot(terra::vect(sim$studyAreaReporting), add = TRUE)
  }

  ## Current Conditions --------------------------------------------------------------------------

  browser() ## TODO: need CC maps at 30 m resolution to match SCANFI -- in progress (Heather)

  ## Manitoba uses current conditions layers (2016) which cover the province;
  ## otherwise, use the original CC layers
  if (LandWebUtils::studyAreaIn(P(sim)$.studyAreaName, "MB")) {
    ccURL <- "https://drive.google.com/file/d/1KTqNBntNrEsDL6jk-5bchsBOcraDqNHe/"
    fname_age <- "MB_Age2016_NRV.tif"
    LandTypeFileCC <- file.path(mod$dPath, "MB_Landtype2016_NRV.tif")
  } else {
    ccURL <- "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1"
    fname_age <- "Age1.tif"
    LandTypeFileCC <- file.path(mod$dPath, "LandType1.tif")
  }

  sim$LandTypeCC <- prepInputs(
    LandTypeFileCC,
    url = ccURL,
    method = "near",
    to = sim$rasterToMatchLarge,
    filename2 = NULL
  ) |>
    Cache()
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

  ## LCC map codes:
  ##  20 = water
  ##  30 = snow_ice, rock_rubble, exposed_barren_land
  ##  40 = bryoids
  ##  50 = shrubs
  ##  80 = wetland
  ##  81 = wetland-treed
  ##  100 = herbs
  ##  210 = coniferous
  ##  220 = broadleaf
  ##  230 = mixedwood
  ##  240 = recently disturbed
  uniqueLCCClasses <- na.omit(unique(LCClarge[]))
  nontreeClassesLCC <- sort(uniqueLCCClasses[!uniqueLCCClasses %in% P(sim)$treeClassesLCC])

  ## for each LCC + CC class combo, define which LCC code should be used:
  ## setting a pixel to NA will omit it entirely (i.e., non-vegetated)
  remapDT <- expand.grid(
    LCC = c(NA_integer_, sort(uniqueLCCClasses)),
    CC = c(NA_integer_, 0:5)
  ) |>
    as.data.table() ## TODO: confirm conversions
  remapDT[LCC %in% c(0, 20, 30), newLCC := NA_integer_]
  remapDT[is.na(CC) | CC == 5, newLCC := LCC]
  remapDT[CC == 4, newLCC := NA_integer_]
  remapDT[CC %in% 0:3, newLCC := LCC]
  remapDT[is.na(LCC) & CC %in% 0:2, newLCC := 99] ## reclassification needed
  remapDT[LCC %in% P(sim)$treeClassesToReplace, newLCC := 99] ## reclassification needed

  message("Overlaying land cover maps...")
  LCClarge <- overlayLCCs(
    LCCs = list(CC = sim$LandTypeCC, LCC = LCClarge),
    forestedList = list(CC = 0, LCC = P(sim)$treeClassesLCC),
    outputLayer = "LCC",
    remapTable = remapDT,
    classesToReplace = c(P(sim)$treeClassesToReplace, 99),
    availableERC_by_Sp = NULL
  ) |>
    Cache()
  message("...done.")

  treePixelsLCC <- which(LCClarge[] %in% P(sim)$treeClassesLCC)
  nonTreePixels <- which(LCClarge[] %in% nontreeClassesLCC)

  sim$nonTreePixels <- nonTreePixels

  ## Update rasterToMatch layer with all trees
  sim$rasterToMatchLarge[sim$nonTreePixels] <- NA
  sim$rasterToMatch <- postProcess(sim$rasterToMatchLarge, to = sim$studyArea, filename2 = NULL)

  ## Age from Current Conditions -----------------------------------------------------------------
  browser() ## TODO: need updated age map
  CC_TSF <- prepInputs(
    url = ccURL,
    targetFile = fname_age,
    filename2 = NULL,
    alsoExtract = "similar",
    to = sim$rasterToMatch
  ) |>
    terra::as.int()

  ageCClarge <- postProcess(
    x = terra::rast(file.path(mod$dPath, fname_age)),
    filename1 = NULL,
    filename2 = NULL,
    to = sim$rasterToMatchLarge,
    maskWithRTM = TRUE,
    method = "bilinear",
    datatype = "INT2U"
  ) |>
    Cache(userTags = c("stable", currentModule(sim)))
  ageCClarge[ageCClarge < 0] <- 0
  CC_TSF <- as.int(ageCClarge)

  ## Age map -------------------------------------------------------------------------------------

  standAgeMap <- prepInputsStandAgeMap(
    dataSource = "SCANFI",
    dataYear = 2020, ## TODO: add dataYear param to module?
    ageFun = "terra::rast",
    maskWithRTM = TRUE,
    method = "bilinear",
    datatype = "INT2U",
    destinationPath = mod$dPath,
    writeTo = NULL,
    firePerimeters = NULL,
    fireURL = "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip",
    fireFun = "terra::vect",
    fireField = "YEAR",
    rasterToMatch = sim$rasterToMatchLarge,
    startTime = NULL
  )

  CC_TSF[noDataPixelsCC] <- standAgeMap[noDataPixelsCC]
  CC_TSF[sim$nonTreePixels] <- NA
  attr(CC_TSF, "imputedPixID") <- integer(0) ## TODO: reassess whether overlay counts as imputation

  ## Flammability and Fire Return Interval rasters -----------------------------------------------

  ## flammability map shouldn't be masked (no gaps!);
  #    NAs outside the buffered study & snow/rock/ice area are the only values we want NA
  #    use the LCC flammability map to fill in NA / nodata values

  ## No data class is 5 -- these will be filled in by LCC layer
  # NA_ids <- which(is.na(sim$LandTypeCC[]) | sim$LandTypeCC[] == 5)
  ## Only class 4 is considered non-flammable
  rstFlammableCC <- defineFlammable(sim$LandTypeCC, nonFlammClasses = 4L,
                                    mask = NULL, filename2 = NULL)

  rstFlammableLCC <- defineFlammable(
    LCClarge,
    nonFlammClasses = c(20, 30, 40, 80), ## see LCC classes above
    mask = NULL,
    filename2 = NULL
  )

  sim$rstFlammable <- rstFlammableCC
  sim$rstFlammable[LandTypeCCNA] <- rstFlammableLCC[LandTypeCCNA]
  sim$rstFlammable <- terra::as.int(sim$rstFlammable)

  ## fireReturnInterval needs to be masked by rstFlammable
  rstFireReturnInterval <- terra::rasterize(
    x = terra::vect(lthfc_clean),
    y = sim$rasterToMatch,
    field = "fireReturnInterval",
    wopt = list(datatype = "INT1U")
  ) |>
    terra::as.int() |>
    terra::classify(matrix(c(0, NA_integer_), ncol = 2))

  if (FALSE) {
    terra::plot(rstFireReturnInterval)
  }

  sim$fireReturnInterval <- rstFireReturnInterval
  sim$LCC <- LCClarge
  sim$CC_TSF <- CC_TSF

  ## some assertions:
  testObjs <- c("studyArea", "studyAreaLarge", "studyAreaReporting",
                "rasterToMatch", "rasterToMatchLarge", "rasterToMatchReporting",
                "fireReturnInterval", "CC_TSF")
  lapply(testObjs, function(x) {
    if (is.null(sim[[x]])) {
      stop("LandWeb_preamble: ", paste0("sim$", x, " returned NULL."), call. = FALSE)
    }
  })

  compareGeom(sim$rasterToMatch, rstFireReturnInterval, sim$rstFlammable)
  ## end assertions

  return(invisible(sim))
}

InitSpecies <- function(sim) {
  sppEquiv <- LandR::sppEquivalencies_CA

  sppEquiv[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                    EN_generic_full = "Pine",
                                    Leading = "Pine leading")]

  ## Make LandWeb spp equivalencies
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

  if (grepl("SprayLake", P(sim)$.studyAreaName)) {
    ## 2024-09-23: add Douglas fir for Spray Lakes + C5 runs
    sppEquiv[LandR == "Pseu_men", LandWeb := "Pseu_men"]
    sppEquiv[LandWeb == "Pseu_men",  `:=`(EN_generic_full = "Douglas fir",
                                          EN_generic_short = "Doug fir",
                                          Leading = "Doug fir leading")]
  }

  sim$sppEquiv <- sppEquiv[!is.na(LandWeb), ]
  sim$sppColorVect <- LandR::sppColors(sim$sppEquiv, "LandWeb", newVals = "Mixed", palette = "Accent")

  ## species parameter tables
  sim$speciesTable <- LandR::getSpeciesTable(dPath = mod$dPath) ## uses default URL

  ## TODO: don't change params at all in v3;
  ## maybe restore changes made by LandR::speciesTableUpdate,
  ## so shadetol to 'defaults' listed below -- except perhaps increase Pinu to 1.5
  speciesParams <- list(
    growthcurve = list(Abie_sp = 0, Pice_gla = 1, Pice_mar = 1, Pinu_sp = 0, Popu_sp = 0),
    mortalityshape = list(Abie_sp = 15L, Pice_gla = 15L, Pice_mar = 15L, Pinu_sp = 15L, Popu_sp = 25L),
    resproutage_min = list(Popu_sp = 25L), # default 10L
    # resproutprob = list(Popu_sp = 0.1), # default 0.5
    shadetolerance = list(Abie_sp = 3, Pice_gla = 2, Pice_mar = 3, Pinu_sp = 1, Popu_sp = 1) # defaults 4, 3, 4, 1, 1
  )

  if (grepl("SprayLake", P(sim)$.studyAreaName)) {
    ## 2024-09-23: add Douglas fir for Spray Lakes + C5 runs
    speciesParams <- modifyList(speciesParams, list(
      growthcurve = list(Pseu_men = 1), ## default 1
      mortalityshape = list(Pseu_men = 15L), ## default 15L
      shadetolerance = list(Pseu_men = 3) ## default 3
    ))
  }

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

    if (grepl("SprayLake", P(sim)$.studyAreaName)) {
      ## 2024-09-23: add Douglas fir for Spray Lakes + C5 runs
      speciesParams <- modifyList(speciesParams, list(
        postfireregen = list(Pseu_men = "resprout"),
        resproutage_max = list(Pseu_men = 400L),
        resproutage_min = list(Pseu_men = 0L),
        resproutprob = list(Pseu_men = 1.0)
      ))
    }
  }

  speciesParams <- append(speciesParams, switch(
    P(sim)$dispersalType,
    aspen = list(
      seeddistance_eff = list(Abie_sp = 1L, Pice_gla = 1L, Pice_mar = 1L, Pinu_sp = 1L, Popu_sp = 100L),
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

  if (grepl("SprayLake", P(sim)$.studyAreaName)) {
    ## 2024-09-23: add Douglas fir for Spray Lakes + C5 runs
    speciesParams <- modifyList(speciesParams, switch(
      P(sim)$dispersalType,
      aspen = list(
        seeddistance_eff = list(Pseu_men = 0L),
        seeddistance_max = list(Pseu_men = 125L)
      ),
      high = list(
        seeddistance_eff = list(Pseu_men = 300L),
        seeddistance_max = list(Pseu_men = 1250L)
      ),
      none = list(
        seeddistance_eff = list(Pseu_men = 100L), ## default but disabled downstream
        seeddistance_max = list(Pseu_men = 500L) ## default but disabled downstream
      ),
      default = list(
        seeddistance_eff = list(Pseu_men = 100L),
        seeddistance_max = list(Pseu_men = 500L)
      )
    ))
  }

  # if (grepl("SprayLake", P(sim)$.studyAreaName)) {
  #   message(crayon::red("Fir shade tolerance lowered below default (3). Using value 2."))
  #   message(crayon::red("Spruce shade tolerance raised above default (2, 3). Using values 3, 4."))
  #   speciesParams <- append(speciesParams, list(
  #     shadetolerance = list(
  #       Abie_sp = 2,
  #       Pice_gla = 3,
  #       Pice_mar = 4
  #     )
  #   ))
  # }

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
