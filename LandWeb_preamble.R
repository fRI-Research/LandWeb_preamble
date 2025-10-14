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
    "curl", "dplyr", "fasterize", "geodata", "ggplot2", "httr",
    "nngeo", "RColorBrewer", "RCurl", "scales", "sf", "SpaDES.tools", "XML",
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
    defineParameter("treeClassesLCC", "integer", c(81L, 210L, 220L, 230L, 240L), 0L, 240L,
                    paste("AKA `forestedLCCClasses`. The classes in the `LCC` layer that are",
                          "considered 'trees' from the perspective of LandR-Biomass.")),
    defineParameter("treeClassesToReplace", "integer", c(240L), NA, NA,
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
    createsOutput("studyArea", "sf",
                  desc = "Polygon to use as the simulation study area."),
    createsOutput("StudyAreaLandWeb", "sf",
                  desc = "Polygon boundary of the full LandWeb study area"),
    createsOutput("studyAreaLarge", "sf",
                  desc = paste("Polygon to use as the parametrisation study area.",
                               "Note that `studyAreaLarge` is only used for parameter estimation, and",
                               "can be larger than the actual study area used for LandR simulations",
                               "(e.g, larger than `studyArea` in LandR `Biomass_core`).")),
    createsOutput("studyAreaReporting", "sf",
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

  sim$StudyAreaLandWeb <- landweb_area

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
  rstFlammableCC <- defineFlammable(
    sim$LandTypeCC,
    nonFlammClasses = 4L,
    mask = NULL,
    filename2 = NULL
  )

  rstFlammableLCC <- defineFlammable(
    LCClarge,
    nonFlammClasses = c(20, 30, 40, 80), ## see LCC classes above
    mask = NULL,
    filename2 = NULL
  )

  sim$rstFlammable <- rstFlammableCC
  sim$rstFlammable[LandTypeCCNA] <- rstFlammableLCC[LandTypeCCNA]
  sim$rstFlammable <- terra::as.int(sim$rstFlammable) |>
    terra::crop(sim$rasterToMatch) ## ensure it matches studyArea

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

  if (FALSE) {
    LandR::speciesInStudyArea(sim$studyArea, dataSource = "SCANFI")

    LandR::speciesInStudyArea(sim$StudyAreaLandWeb, dataSource = "SCANFI")
    ## $speciesList
    ## [1] "PICE_MAR"     "PINU_CON_LAT" "PICE_GLA"     "BETU_PAP"     "POPU_TRE"
    ## [6] "PINU_BAN"     "LARI_LAR"     "POPU_BAL"     "ABIE_LAS"     "PICE_ENG_GLA"
    ## [11] "TSUG_HET"     "ABIE_BAL"     "PICE_ENG"     "PSEU_MEN_GLA" "PSEU_MEN"
    ## [16] "THUJ_PLI"     "POPU_GRA"     "LARI_OCC"
  }
browser()
  ## Make LandWeb spp equivalencies
  sppEquiv[, LandWeb := c(
    ABIE_BAL = "Abie_spp", ABIE_LAS = "Abie_spp",
    BETU_PAP = "Popu_spp",
    LARI_LAR = "Lari_spp", LARI_OCC = "Lari_spp",
    PICE_ENG = "Pice_gla", PICE_ENG_GLA = "Pice_gla", ## TODO: confirm merge with Pice_gla
    PICE_GLA = "Pice_gla",
    PICE_MAR = "Pice_mar",
    PINU_BAN = "Pinu_spp",
    PINU_CON = "Pinu_spp", PINU_CON_CON = "Pinu_spp", PINU_CON_LAT = "Pinu_spp",
    POPU_BAL = "Popu_spp", POPU_TRE = "Popu_spp",
    PSEU_MEN = "Pseu_men", PSEU_MEN_GLA = "Pseu_men",
    THUJ_PLI = "Thuj_pli",
    TSUG_HET = "Tsug_het"
  )[SCANFI]]

  sppEquiv[LandWeb == "Lari_spp", `:=`(EN_generic_full = "Western Larch & Tamarack",
                                       EN_generic_short = "Larch & Tamarack",
                                       Leading = "Larch & Tamarack leading")]

  sppEquiv[LandWeb == "Pice_gla", `:=`(EN_generic_full = "White & Engelmann's Spruce",
                                       EN_generic_short = "Whi & Eng Spr",
                                       Leading = "White & Engelmann's Spruce leading")]

  sppEquiv[grep("Pin", LandWeb), `:=`(EN_generic_short = "Pine",
                                      EN_generic_full = "Pine",
                                      Leading = "Pine leading")]

  sppEquiv[LandWeb == "Popu_spp", `:=`(EN_generic_full = "Deciduous",
                                       EN_generic_short = "Decid",
                                       Leading = "Deciduous leading")]

  sppEquiv[LandWeb == "Pseu_men",  `:=`(EN_generic_full = "Douglas fir",
                                        EN_generic_short = "Doug fir",
                                        Leading = "Douglas fir leading")]

  sim$sppEquiv <- sppEquiv[!is.na(LandWeb), ]
  sim$sppColorVect <- LandR::sppColors(sim$sppEquiv, "LandWeb", newVals = "Mixed", palette = "Accent")

  ## species parameter tables
  sim$speciesTable <- LandR::getSpeciesTable(dPath = mod$dPath) ## uses default URL

  ## TODO: restore changes made by LandR::speciesTableUpdate,
  ## so shadetol to 'defaults' listed below -- except perhaps increase Pinu to 1.5 or 2
  speciesParams <- list(
    # resproutage_min = list(Popu_spp = 25L), # default 10L
    shadetolerance = list(
      ## defaults: 4, 3, 4, 1, 1, 3
      Abie_spp = 3, Pice_gla = 2, Pice_mar = 3, Pinu_spp = 1, Popu_spp = 1, Pseu_men = 3
    )
  )

  sim$speciesParams <- speciesParams

  return(invisible(sim))
}

InitLandMine <- function(sim) {
  stopifnot(P(sim)$ROStype %in% c("default", "burny"))

  ## ROS classes and values from Table 3.2 of Andison 1996
  ## - omitting 'water', 'non-productive brush', and 'non-productive black spruce' classes;
  ## - typo in Andison 1996: 'young mixed wood = 6' is really 'young hardwood = 6'.
  LandMineROStable <- data.table::rbindlist(list(
    list("immature_young", "decid", 6L), ## aka hardwood
    list("mature", "decid", 9L), ## aka hardwood
    list("immature_young", "mixed", 12L),
    list("immature", "pine", 14L),
    list("mature", "mixed", 17L),
    list("immature_young", "softwood", 18L),
    list("immature_young", "spruce", 20L),
    list("mature", "pine", 21L),
    list("young", "pine", 22L),
    list("mature", "softwood", 27L),
    list("mature", "spruce", 30L)
  )) |>
    data.table::setnames(old = 1:3, new = c("age", "leading", "ros"))

  if (P(sim)$ROStype == "equal") {
    LandMineROStable$ros <- 1L
  } else if (P(sim)$ROStype == "log") {
    LandMineROStable$ros <- log(LandMineROStable$ros)
  }

  sim$ROSTable <- LandMineROStable

  return(invisible(sim))
}
