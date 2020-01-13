defineModule(sim, list(
  name = "LandWeb_preamble",
  description = "define FMA-specific study areas etc. for LandWeb",
  keywords = c("LandWeb"),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut"))
  ),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.3.9009", LandWeb_preamble = "0.0.2", LandR = "0.0.2.9011"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandWeb_preamble.Rmd"),
  reqdPkgs = list("achubaty/amc@development",
                  "crayon", "dplyr", "fasterize", "ggplot2",
                  "PredictiveEcology/LandR@development",
                  "magrittr", "PredictiveEcology/map@development", "maptools",
                  "PredictiveEcology/pemisc@development",
                  "raster", "RColorBrewer", "reproducible", "rgeos",
                  "scales", "sf", "sp", "SpaDES.tools"),
  parameters = rbind(
    defineParameter("mapResFact", "numeric", 1, 1, 10,
                    paste("The map resolution factor to use with raster::disaggregate to reduce pixel size below 250 m.",
                          "Should be one of 1, 2, 5, 10, which correspends to pixel size of 250m, 125m, 50m, 25m, repsectively.")),
    defineParameter("minFRI", "numeric", 40, 0, 200, "The value of fire return interval below which, pixels will be changed to NA, i.e., ignored"),
    defineParameter("runName", "character", NA, NA, NA, "A description for run; this will form the basis of cache path and output path"),
    defineParameter("treeClassesLCC", "numeric", c(1:15, 20, 32, 34:36), 0, 39,
                    "The classes in the LCC2005 layer that are considered 'trees' from the perspective of LandR-Biomass"),
    defineParameter("treeClassesToReplace", "numeric", c(34:36), 0, 39,
                    "The transient classes in the LCC2005 layer that will become 'trees' from the perspective of LandR-Biomass (e.g., burned)"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    ## TODO: uses CC and fire return interval maps from URL in init
    expectsInput("canProvs", "SpatialPolygonsDataFrame", "Canadian provincial boundaries shapefile", NA)
  ),
  outputObjects = bind_rows(
    createsOutput("CC TSF", "RasterLayer", desc = NA), ## TODO: need descriptions for all outputs
    createsOutput("fireReturnInterval", "RasterLayer", desc = NA),
    createsOutput("LandTypeCC", "RasterLayer", desc = NA),
    createsOutput("LCC2005", "RasterLayer", desc = NA),
    createsOutput("ml", "map", desc = NA),
    createsOutput("LCC", "RasterLayer", desc = "A key output fromt this module: it is the result of LandR::overlayLCCs on LCC2005 and LandTypeCC"),
    createsOutput("nonTreePixels", "integer", desc = NA),
    createsOutput("rasterToMatch", "RasterLayer", desc = NA),
    createsOutput("rasterToMatchReporting", "RasterLayer", desc = NA),
    createsOutput("rstFlammable", "RasterLayer", desc = NA),
    createsOutput("studyArea", "SpatialPolygonsDataFrame", desc = NA),
    createsOutput("studyAreaLarge", "SpatialPolygonsDataFrame", desc = NA),
    createsOutput("studyAreaReporting", "SpatialPolygonsDataFrame", desc = NA)
  )
))

doEvent.LandWeb_preamble = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  ## NOTE (2019-11-08): targetCRS needs to be character, not CRS class due to change in data.table
  targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                     "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  ## LandWeb study area -- LTHFC (aka "fire return interval") map
  ml <- mapAdd(layerName = "LandWeb Study Area",
               targetCRS = targetCRS, overwrite = TRUE,
               url = "https://drive.google.com/open?id=1JptU0R7qsHOEAEkxybx5MGg650KC98c6", ## landweb_ltfc_v6.shp
               columnNameForLabels = "NSN", isStudyArea = TRUE, filename2 = NULL)

  ## Updated FMA boundaries
  ml <- mapAdd(map = ml, layerName = "FMA Boundaries Updated",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1nTFOcrdMf1hIsxd_yNCSTr8RrYNHHwuc/view?usp=sharing",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## AB FMU boundaries
  ml <- mapAdd(map = ml, layerName = "AB FMU Boundaries",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/open?id=1OH3b5pwjumm1ToytDBDI6jthVe2pp0tS",
               columnNameForLabels = "FMU_NAME", isStudyArea = FALSE, filename2 = NULL)

  ### Rename some polygons:
  ###   - DMI is now Mercer (MPR)
  ids <- grep("Daishowa-Marubeni International Ltd", ml[["FMA Boundaries Updated"]][["Name"]])
  newNames <- c("Mercer Peace River Pulp Ltd. (East)", "Mercer Peace River Pulp Ltd. (West)")
  ml[["FMA Boundaries Updated"]][["Name"]][ids] <- newNames
  ml[["FMA Boundaries Updated"]][["shinyLabel"]][ids] <- newNames

  ## Alberta Natural Subregions (ANSRs)
  ml <- mapAdd(map = ml, layerName = "Alberta Natural Subregions",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1mCEynahKnFkStJUJC8ho5ndRD41olz9F/view?usp=sharing",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## Caribou Ranges
  # ml <- mapAdd(map = ml, layerName = "Boreal Caribou Ranges",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://drive.google.com/file/d/1PYLou8J1wcrme7Z2tx1wtA4GvaWnU1Jy/view?usp=sharing",
  #              columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)
  # ml <- mapAdd(map = ml, layerName = "AB Caribou Ranges",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://extranet.gov.ab.ca/srd/geodiscover/srd_pub/LAT/FWDSensitivity/CaribouRange.zip",
  #              columnNameForLabels = "SUBUNIT", isStudyArea = FALSE, filename2 = NULL) ## untested
  # ml <- mapAdd(map = ml, layerName = "BC Caribou Ranges",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://drive.google.com/file/d/1uqEVID74y4enPMee2w3axBcR1agw_kMT/view?usp=sharing",
  #              columnNameForLabels = "HERD_NAME", isStudyArea = FALSE, filename2 = NULL) ## untested
  ml <- mapAdd(map = ml, layerName = "LandWeb Caribou Ranges",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1mrsxIJfdP-XxEZkO6vs2J6lYbGry67A2/view?usp=sharing",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## Provincial Boundaries
  ml <- mapAdd(sim$canProvs, map = ml, layerName = "Provincial Boundaries",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               columnNameForLabels = "NAME_1", isStudyArea = FALSE, filename2 = NULL)

  ################################################################################
  ## COMPANY-SPECIFIC STUDY AREAS
  dataDir <- file.path("inputs", "FMA_Boundaries")

  if (grepl("ANC", P(sim)$runName)) {
    ml <- fmaANC(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("DMI|MPR", P(sim)$runName)) {
    ml <- fmaDMI(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("Edson", P(sim)$runName)) {
    ml <- fmaEdsonFP(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("FMANWT", P(sim)$runName)) {
    ml <- fmaNWT(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("FMU", P(sim)$runName)) {
    ml <- fmu(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("LandWeb", P(sim)$runName)) {
    ml <- allLandWeb(ml, P(sim)$runName, file.path("inputs", "LandWeb"), sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("LP", P(sim)$runName)) {
    ml <- fmaLP(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("Manning", P(sim)$runName)) {
    ml <- fmaManning(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("MillarWestern", P(sim)$runName)) {
    ml <- fmaMillarWestern(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("Mistik", P(sim)$runName)) {
    ml <- fmaMistik(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("Sundre", P(sim)$runName)) {
    ml <- fmaSundreFP(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("Tolko|tolko", P(sim)$runName)) {
    ml <- fmaTolko(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("Vanderwell", P(sim)$runName)) {
    ml <- fmaVanderwell(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("WeyCo", P(sim)$runName)) {
    ml <- fmaWeyCo(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("WestFraser|BlueRidge", P(sim)$runName)) {
    ml <- fmaWestFraser(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("provAB", P(sim)$runName)) {
    ml <- provAB(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("provNWT", P(sim)$runName)) {
    ml <- provNWT(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else if (grepl("provSK", P(sim)$runName)) {
    ml <- provSK(ml, P(sim)$runName, dataDir, sim$canProvs, asStudyArea = TRUE)
  } else {
    ## use a small random study area
    message(crayon::red("Using random study area for runName", runName))
    seed <- 863
    ranSeed <- .Random.seed
    set.seed(seed)
    rnd <- Cache(SpaDES.tools::randomPolygon, ml[[studyAreaName(ml)]], 4e5)
    ml <- mapAdd(rnd, ml, layerName = "Random Study Area", useSAcrs = TRUE, poly = TRUE,
                 analysisGroupReportingPolygon = "Random Study Area", isStudyArea = TRUE,
                 columnNameForLabels = "Name", filename2 = NULL)

    rnd_sr <- postProcess(ml[["LandWeb Study Area"]],
                          studyArea = amc::outerBuffer(rnd, 50000), # 50 km buffer
                          useSAcrs = TRUE, filename2 = NULL)
    ml <- mapAdd(rnd_sr, ml, isStudyArea = TRUE, layerName = "Random Study Area SR",
                 useSAcrs = TRUE, poly = TRUE, studyArea = NULL, # don't crop/mask to studyArea(ml, 2)
                 columnNameForLabels = "NSN", filename2 = NULL)
  }

  ##########################################################
  # LCC2005
  ##########################################################
  LCC2005large <- prepInputsLCC(studyArea = studyArea(ml, 1), destinationPath = Paths$inputPath)
  if (P(sim)$mapResFact != 1) {
    stopifnot(P(sim)$mapResFact %in% c(2, 5, 10)) ## 125m, 50m, 25m resolutions respectively
    LCC2005large <- Cache(raster::disaggregate, x = LCC2005large, fact = P(sim)$mapResFact)
  }

  LCC2005 <- prepInputsLCC(studyArea = studyArea(ml), destinationPath = Paths$inputPath)
  if (P(sim)$mapResFact != 1) {
    stopifnot(P(sim)$mapResFact %in% c(2, 5, 10)) ## 125m, 50m, 25m resolutions respectively
    LCC2005 <- Cache(raster::disaggregate, x = LCC2005, fact = P(sim)$mapResFact)
  }

  ml <- mapAdd(LCC2005, layerName = "LCC2005", map = ml, filename2 = NULL, leaflet = FALSE,
               isRasterToMatch = TRUE, method = "ngb")

  ##########################################################
  # Current Conditions
  ##########################################################
  ccURL <- "https://drive.google.com/file/d/1JnKeXrw0U9LmrZpixCDooIm62qiv4_G1/view?usp=sharing"
  LandTypeFileCC <- file.path(Paths$inputPath, "LandType1.tif")
  sim$LandTypeCC <- Cache(prepInputs, LandTypeFileCC, studyArea = studyArea(ml),
                          url = ccURL, method = "ngb",
                          rasterToMatch = rasterToMatch(ml), filename2 = NULL)

  ##########################################################
  # Non Tree pixels
  ##########################################################
  # Setting NA values
  # 3 is shrub, wetland, grassland -- no veg dynamics happen -- will burn in fire modules
  # 4 is water, rock, ice
  # 5 is no Data ... this is currently cropland -- will be treated as grassland for fires
  treeClassesCC <- c(0, 1, 2)
  nontreeClassesCC <- c(3, 4)
  treePixelsCCTF <- sim$LandTypeCC[] %in% treeClassesCC
  LandTypeCCNA <- is.na(sim$LandTypeCC[])
  noDataPixelsCC <- LandTypeCCNA | (sim$LandTypeCC[] == 5)
  treePixelsCC <- which(treePixelsCCTF)

  uniqueLCCClasses <- na.omit(unique(ml$LCC2005[]))
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

  sim$LCC <- overlayLCCs(list(CC = sim$LandTypeCC, LCC2005 = ml$LCC2005),
                         forestedList = list(CC = 0, LCC2005 = P(sim)$treeClassesLCC),
                         outputLayer = "LCC2005",
                         #NAcondition = "LCC2005 == 0",
                         #NNcondition = "CC == 1 & LCC2005 == 0",
                         remapTable = remapDT,
                         classesToReplace = c(P(sim)$treeClassesToReplace, 99),
                         availableERC_by_Sp = NULL)

  treePixelsLCC <- which(sim$LCC[] %in% P(sim)$treeClassesLCC)
  nonTreePixels <- which(sim$LCC[] %in% nontreeClassesLCC)

  sim$nonTreePixels <- nonTreePixels

  # Update rasterToMatch layer with all trees
  ml[[ml@metadata[ml@metadata$rasterToMatch == 1, ]$layerName]][sim$nonTreePixels] <- NA

  fname_age <- "Age1.tif"
  TSFLayerName <- "CC TSF"
  ml <- mapAdd(map = ml, url = ccURL, layerName = TSFLayerName, CC = TRUE,
               tsf = file.path(Paths$inputPath, fname_age), analysisGroup1 = "CC",
               targetFile = fname_age, filename2 = NULL,
               useCache = TRUE, isRasterToMatch = FALSE,
               alsoExtract = "similar", leaflet = FALSE)
  ml[[TSFLayerName]][] <- as.integer(ml[[TSFLayerName]][])

  ########################################################################
  # Age from KNN
  ########################################################################

  standAgeMapFilename <- "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif"
  standAgeMap <- Cache(prepInputs, #notOlderThan = Sys.time(),
                       targetFile = standAgeMapFilename,
                       archive = asPath(c("kNN-StructureStandVolume.tar",
                                          "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip")),
                       destinationPath = Paths$inputPath,
                       url = "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar",
                       fun = "raster::raster",
                       studyArea = studyArea(ml),
                       rasterToMatch = rasterToMatch(ml),
                       maskWithRTM = TRUE,
                       method = "bilinear",
                       datatype = "INT2U",
                       filename2 = NULL, overwrite = TRUE,
                       userTags = c("stable", currentModule(sim)))
  ml[[TSFLayerName]][noDataPixelsCC] <- standAgeMap[noDataPixelsCC]
  ml[[TSFLayerName]][sim$nonTreePixels] <- NA

  ##########################################################
  # Clean up the study area
  ##########################################################
  #studyArea(ml) <- polygonClean(studyArea(ml), type = P(sim)$runName, minFRI = P(sim)$minFRI)
  studyArea(ml) <- polygonClean(studyArea(ml), type = "LandWeb", minFRI = P(sim)$minFRI)

  ##########################################################
  # Flammability and Fire Return Interval maps
  ##########################################################

  ## flammability map shouldn't be masked (no gaps!);
  #    NAs outside the buffered study & snow/rock/ice area are the only values we want NA
  #    use the LCC flammability map to fill in NA / nodata values

  # No data class is 5 -- these will be filled in by LCC2005 layer
  # NA_ids <- which(is.na(sim$LandTypeCC[]) | sim$LandTypeCC[] == 5)
  # Only class 4 is considered non-flammable
  rstFlammableCC <- defineFlammable(sim$LandTypeCC, nonFlammClasses = 4,
                                    mask = NULL, filename2 = NULL)
  rstFlammableCC <- deratify(rstFlammableCC, complete = TRUE)

  #LandTypeFileLCC <- file.path(Paths$inputPath, "LCC2005_V1_4a.tif")
  # Only classes 36, 37, 38, 39 is considered non-flammable
  rstFlammableLCC <- defineFlammable(LCC2005, nonFlammClasses = 36:39, mask = NULL, filename2 = NULL)
  rstFlammableLCC <- deratify(rstFlammableLCC, complete = TRUE)

  #rstFlammableLCC <- Cache(prepInputs, LandTypeFileLCC, studyArea = studyArea(ml),
  #                         url = ccURL, method = "ngb",
  #                         rasterToMatch = rasterToMatch(ml), filename2 = NULL) %>%
  #  defineFlammable(., nonFlammClasses = 36:39, mask = NULL, filename2 = NULL)

  sim$rstFlammable <- rstFlammableCC
  sim$rstFlammable[LandTypeCCNA] <- rstFlammableLCC[LandTypeCCNA]
  sim$rstFlammable[] <- as.integer(sim$rstFlammable[])

  ## fireReturnInterval needs to be masked by rstFlammable
  rstFireReturnInterval <- fasterize::fasterize(sf::st_as_sf(studyArea(ml)),
                                                raster = rasterToMatch(ml),
                                                field = "fireReturnInterval")

  if (!is.integer(rstFireReturnInterval[]))
    rstFireReturnInterval[] <- as.integer(rstFireReturnInterval[])

  ml <- mapAdd(rstFireReturnInterval, layerName = "fireReturnInterval", filename2 = NULL,
               map = ml, leaflet = FALSE, maskWithRTM = FALSE)

  if (grepl("doubleFRI", P(sim)$runName))
    ml$fireReturnInterval <- 2L * ml$fireReturnInterval

  sim$studyArea <- studyArea(ml, 3)
  sim$studyAreaLarge <- studyArea(ml, 1)
  sim$studyAreaReporting <- studyArea(ml, 2)
  sim$rasterToMatch <- sim$LCC #rasterToMatch(ml)
  sim$fireReturnInterval <- ml$fireReturnInterval # no NAing here because this needs only
  sim$LCC2005 <- sim$LCC #ml$LCC2005
  sim[[TSFLayerName]] <- ml[[TSFLayerName]]
  sim$rasterToMatchReporting <- postProcess(rasterToMatch(ml),
                                            studyArea = studyArea(ml, 2),
                                            filename2 = NULL) # this is the small one
  sim$rasterToMatchLarge <- LCC2005large

  sim$ml <- ml

  ## some assertions:
  testObjs <- c("studyArea", "studyAreaLarge", "studyAreaReporting",
                "rasterToMatch", "rasterToMatchReporting",
                "fireReturnInterval", TSFLayerName)
  lapply(testObjs, function(x) {
    if (is.null(sim[[x]]))
      stop("LandWeb_preamble: ", paste0("sim$", x, " returned NULL."), call. = FALSE)
  })
  ## end assertions

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("canProvs", sim))
    sim$canProvs <- getData("GADM", country = "CAN", level = 1, path = dPath)

  return(sim)
}
