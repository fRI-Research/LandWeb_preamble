defineModule(
  sim,
  list(
    name = "LandWeb_preamble",
    description = "define FMA-specific study areas etc. for LandWeb",
    keywords = c("LandWeb"),
    authors = c(
      person(
        c("Eliot", "J", "B"),
        "McIntire",
        email = "eliot.mcintire@nrcan-rncan.gc.ca",
        role = "aut"
      ),
      person(
        c("Alex", "M."),
        "Chubaty",
        email = "achubaty@for-cast.ca",
        role = c("aut", "cre")
      )
    ),
    childModules = character(0),
    version = list(LandWeb_preamble = "1.0.1"),
    spatialExtent = raster::extent(rep(NA_real_, 4)),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = "year",
    citation = list("citation.bib"),
    documentation = list("README.md", "LandWeb_preamble.Rmd"),
    reqdPkgs = list(
      "curl",
      "dplyr",
      "fasterize",
      "geodata",
      "ggplot2",
      "googledrive",
      "httr",
      "nngeo",
      "RColorBrewer",
      "RCurl",
      "scales",
      "sf",
      "SpaDES.tools",
      "terra",
      "tidyterra",
      "XML",
      "FOR-CAST/spatialutils",
      "FOR-CAST/workflowtools@development",
      "PredictiveEcology/LandR@development (>= 1.1.0.9015)",
      "PredictiveEcology/LandWebUtils@development (>= 0.1.5.9000)",
      "PredictiveEcology/map@development (>= 0.0.5)",
      "PredictiveEcology/pemisc@development (>= 0.0.3.9007)",
      "PredictiveEcology/reproducible@development (>= 1.2.16.9024)"
    ),
    parameters = rbind(
      defineParameter(
        "bufferDist",
        "numeric",
        25000,
        20000,
        100000,
        "Study area buffer distance (m) used to make `studyArea`."
      ),
      defineParameter(
        "bufferDistLarge",
        "numeric",
        50000,
        20000,
        100000,
        "Study area buffer distance (m) used to make `studyArea_biomassParam`."
      ),
      defineParameter(
        "forceResprout",
        "logical",
        FALSE,
        NA,
        NA,
        paste(
          "`TRUE` forces all species to resprout, setting `resproutage_min` to zero,",
          "`resproutage_max` to 400, and `resproutProb` to 1.0."
        )
      ),
      defineParameter(
        "friMultiple",
        "numeric",
        1.0,
        0.5,
        2.0,
        "Multiplication factor for adjusting fire return intervals."
      ),
      defineParameter(
        "dispersalType",
        "character",
        "default",
        NA,
        NA,
        "One of 'aspen', 'high', 'none', or 'default'."
      ),
      defineParameter(
        "mergeSlivers",
        "logical",
        FALSE,
        NA,
        NA,
        "Should sliver polygons in LTHFC map be merged into nearest non-zero polygon?"
      ),
      defineParameter(
        "minFRI",
        "numeric",
        40,
        0,
        200,
        "The value of fire return interval below which, pixels will be changed to `NA`, i.e., ignored"
      ),
      defineParameter(
        "pixelSize",
        "integer",
        240L,
        NA,
        NA,
        paste("Pixel size in metres. Should be one of 240, 120, 90, 30.")
      ),
      defineParameter(
        "ROStype",
        "character",
        "default",
        NA,
        NA,
        "Rate of spread preset to use. One of 'burny', 'equal', 'log', or 'default'."
      ),
      defineParameter(
        "treeClassesLCC",
        "integer",
        c(81L, 210L, 220L, 230L, 240L),
        0L,
        240L,
        paste(
          "AKA `forestedLCCClasses`. The classes in the `LCC` layer that are",
          "considered 'trees' from the perspective of LandR-Biomass."
        )
      ),
      defineParameter(
        "treeClassesToReplace",
        "integer",
        c(240L),
        NA,
        NA,
        paste(
          "The transient classes in the `LCC` layer that will become 'trees'",
          "from the perspective of LandR-Biomass (e.g., burned)"
        )
      ),
      defineParameter(
        ".plotInitialTime",
        "numeric",
        start(sim),
        NA,
        NA,
        "This describes the simulation time at which the first plot event should occur"
      ),
      defineParameter(
        ".plotInterval",
        "numeric",
        1,
        NA,
        NA,
        "This describes the simulation time interval between plot events"
      ),
      defineParameter(
        ".plots",
        "character",
        "object",
        NA,
        NA,
        paste(
          "Passed to `types` in `Plots` (see `?Plots`).",
          "There are a few plots that are made within this module, if set.",
          "Note that plots (or their data) saving will ONLY occur at `end(sim)`.",
          "If `NA`, plotting is turned off completely (this includes plot saving)."
        )
      ),
      defineParameter(
        ".saveInitialTime",
        "numeric",
        NA,
        NA,
        NA,
        "This describes the simulation time at which the first save event should occur"
      ),
      defineParameter(
        ".saveInterval",
        "numeric",
        NA,
        NA,
        NA,
        "This describes the simulation time interval between save events"
      ),
      defineParameter(
        ".sslVerify",
        "integer",
        as.integer(unname(curl::curl_options("^ssl_verifypeer$"))),
        NA,
        NA,
        paste(
          "Passed to `httr::config(ssl_verifypeer = P(sim)$sslVerify)` when downloading KNN",
          "(NFI) datasets. Set to 0L if necessary to bypass checking the SSL certificate (this",
          "may be necessary when NFI's website SSL certificate is not correctly configured)."
        )
      ),
      defineParameter(
        ".studyAreaName",
        "character",
        NA,
        NA,
        NA,
        "Human-readable name for the study area used. If `NA`, a hash of `studyArea_biomassParam` will be used."
      ),
      defineParameter(
        ".useCache",
        "logical",
        FALSE,
        NA,
        NA,
        paste(
          "Should this entire module be run with caching activated?",
          "This is generally intended for data-type modules, where stochasticity and time are not relevant"
        )
      )
    ),
    inputObjects = bindrows(
      ## TODO: uses CC and fire return interval maps from URL in init
    ),
    outputObjects = bindrows(
      createsOutput(
        "CC TSF",
        "RasterLayer",
        desc = "Time since fire (aka age) map derived from Current Conditions data."
      ),
      createsOutput(
        "fireReturnInterval",
        "RasterLayer",
        desc = "fire return interval raster"
      ),
      createsOutput(
        "LandTypeCC",
        "RasterLayer",
        desc = paste(
          "Current-conditions land cover (Canada LCC 2020, NALCMS level-II codes), aligned to",
          "`rasterToMatch_biomassParam`. SIMULATION-side copy: urban is sent to 99 by `remapDT`",
          "and reclassified to the nearest type, approximating the pre-industrial state."
        )
      ),
      createsOutput(
        "LandTypeCC_reporting",
        "RasterLayer",
        desc = paste(
          "Current-conditions land cover (Canada LCC 2020) with urban RETAINED -- the reporting",
          "reference, so current condition reflects the actual landscape rather than the",
          "pre-industrial approximation the simulation runs on. Do NOT feed this to the overlay."
        )
      ),
      createsOutput(
        "LCC",
        "RasterLayer",
        desc = "The result of `LandR::overlayLCCs()` on `LCC` and `LandTypeCC`."
      ),
      createsOutput(
        "rstLCC",
        "SpatRaster",
        desc = "Land cover raster (identical to `LCC`); standard LandR name used downstream."
      ),
      createsOutput(
        "standAgeMap",
        "SpatRaster",
        desc = "Current-condition stand age (SCANFI 2020 median), aligned to the RTM."
      ),
      createsOutput("nonTreePixels", "integer", desc = NA),
      createsOutput("rasterToMatch", "RasterLayer", desc = NA),
      createsOutput("rasterToMatch_biomassParam", "RasterLayer", desc = NA),
      createsOutput("rasterToMatchReporting", "RasterLayer", desc = NA),
      createsOutput(
        "ROSTable",
        "data.table",
        desc = paste(
          "A `data.table` with 3 columns: `age`, `leading`, and `ros`.",
          "The values under the `age` column can be `mature`, `immature`,",
          "`young` and compound versions of these, e.g., `immature_young`",
          "which can be used when 2 or more age classes share same `ros`.",
          "`leading` should be vegetation type.",
          "`ros` gives the rate of spread values for each age and type."
        )
      ),
      createsOutput("flammableMap", "RasterLayer", desc = NA),
      createsOutput(
        "speciesParams",
        "list",
        desc = paste(
          "list of updated species trait values to be used to updated",
          "`speciesTable` to create `species`."
        )
      ),
      createsOutput(
        "speciesTable",
        "data.table",
        desc = paste(
          "a table of invariant species traits with the following trait colums:",
          "'species', 'Area', 'longevity', 'sexualmature', 'shadetolerance',",
          "'firetolerance', 'seeddistance_eff', 'seeddistance_max', 'resproutprob',",
          "'resproutage_min', 'resproutage_max', 'postfireregen', 'leaflongevity',",
          "'wooddecayrate', 'mortalityshape', 'growthcurve', 'leafLignin',",
          "'hardsoft'. Names can differ, but not the column order.",
          "Default is from Dominic Cyr and Yan Boulanger's project."
        )
      ),
      createsOutput(
        "sppColorVect",
        "character",
        desc = paste(
          "A named vector of colors to use for plotting.",
          "The names must be in `sim$sppEquiv[['LandWeb']]`,",
          "and should also contain a color for 'Mixed'"
        )
      ),
      createsOutput(
        "sppEquiv",
        "data.table",
        desc = "table of species equivalencies. See `LandR::sppEquivalencies_CA`."
      ),
      createsOutput(
        "studyArea",
        "sf",
        desc = "Polygon to use as the simulation study area."
      ),
      createsOutput(
        "studyAreaANPP",
        "sf",
        desc = paste(
          "study area to use for parameterization with PSP data in",
          "`Biomass_speciesParameters`."
        )
      ),
      createsOutput(
        "studyAreaLandWeb",
        "sf",
        desc = "Polygon boundary of the full LandWeb study area"
      ),
      createsOutput(
        "studyArea_biomassParam",
        "sf",
        desc = paste(
          "Polygon to use as the parametrisation study area.",
          "Note that `studyArea_biomassParam` is used for species parameter estimation,",
          "and should be larger than the actual study area used for LandR simulations",
          "(e.g, larger than `studyArea` in LandR `Biomass_core`)."
        )
      ),
      createsOutput(
        "studyAreaReporting",
        "sf",
        desc = paste(
          "multipolygon (typically smaller than `studyArea_biomassParam` and `studyArea`",
          "in LandR `Biomass_core`) to use for plotting/reporting."
        )
      )
    )
  )
)

doEvent.LandWeb_preamble <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      mod$dPath <- asPath(inputPath(sim), 1)

      sim <- InitMaps(sim)
      sim <- InitSpecies(sim)
      sim <- InitLandMine(sim)
    },
    warning(paste(
      "Undefined event type: '",
      current(sim)[1, "eventType", with = FALSE],
      "' in module '",
      current(sim)[1, "moduleName", with = FALSE],
      "'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

InitMaps <- function(sim) {
  ## NOTE needs to be character, not `CRS` class, for downstream use with `data.table`
  targetCRS <- LandWebUtils::LandWebCRS

  ## v10 LTHFC map: download with workflowtools (googledrive direct), bypassing
  ## reproducible's Drive path which lost service-account auth (reproducible #447).
  ## The session/download controller authenticates first via
  ## googledrive::drive_auth(path = <service-account JSON>).
  lthfc_id <- "176yAq5NCfZZ5ZQX36zHcu0w3uh-V9qvf" ## landweb_ltfc_v10 (Google Drive)
  lthfc_dir <- file.path(inputPath(sim), "lthfc") |> fs::dir_create()
  lthfc_zip <- file.path(lthfc_dir, "landweb_ltfc_v10.zip")
  workflowtools::drive_download_once(googledrive::as_id(lthfc_id), lthfc_zip)
  workflowtools::archive_extract_once(lthfc_zip, dir = lthfc_dir)

  ## keep only the LTHFC column (v10 renamed it LTFC10), and recalculate area.
  ## TODO (lakes): v10 is the wLakes layer -- LAKE_TYPE == 1 are lakes (LAKE_NAME).
  ## Decide whether to drop lakes here or carry them as a water mask downstream.
  lthfc <- terra::vect(list.files(lthfc_dir, "\\.shp$", full.names = TRUE)[[
    1
  ]]) |>
    terra::project(targetCRS) |>
    dplyr::select(LTHFC = LTFC10) ## tidyterra dispatches dplyr verbs on the SpatVector
  lthfc$area <- terra::expanse(lthfc, unit = "m") ## terra::expanse, not sf::st_area(geometry)

  ## 2023-09: added additional geoprocessing to LTHFC map to remove polygon fragments
  ## TODO (mergeSlivers + terra migration): replace the nearest-feature merge below
  ## with the FOR-CAST/spatialutils function that merges a sliver into the polygon
  ## with the LONGEST SHARED BORDER (arcpy "Eliminate" equivalent), not the nearest
  ## feature. This whole block is still sf-based (units, st_nearest_feature, st_union,
  ## st_drop_geometry) -- migrate to terra/tidyterra; and `area` is now plain numeric
  ## m^2 from terra::expanse(), so fix the units comparison on the next line.
  ## (mergeSlivers defaults FALSE, so this is not exercised by the current spike.)
  if (isTRUE(P(sim)$mergeSlivers)) {
    smallerThanOnePixel <- (lthfc$area <=
      units::as_units((P(sim)$pixelSize)^2, "m^2"))
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

    lthfc_clean <- LandWebUtils::polygonClean(
      lthfc_merged,
      type = "LandWeb",
      minFRI = P(sim)$minFRI
    )
  } else {
    lthfc_clean <- LandWebUtils::polygonClean(
      lthfc,
      type = "LandWeb",
      minFRI = P(sim)$minFRI
    )
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

  sim$studyAreaLandWeb <- landweb_area

  ## study areas ---------------------------------------------------------------------------------
  ## studyAreaReporting is the study area used for reporting (e.g., FMA);
  ## studyArea is buffered version of studyAreaReporting to reduce edge effects in simulation;
  ## studyArea_biomassParam is used for parameter estimation in Biomass_borealDataPrep;
  ## studyAreaANPP uses ecological boundaries for getting PSP data in Biomass_speciesParameters.
  sim$studyAreaReporting <- LandWebUtils::prepStudyArea(
    name = P(sim)$.studyAreaName,
    destinationPath = mod$dPath,
    targetCRS = LandWebUtils::LandWebCRS
  )

  sim$studyArea <- spatialutils::outerBuffer(
    sim$studyAreaReporting,
    P(sim)$bufferDist
  )
  sim$studyArea_biomassParam <- spatialutils::outerBuffer(
    sim$studyAreaReporting,
    P(sim)$bufferDistLarge
  )
  ## TODO: is ecoprovince a good size? ecoregion not big enough
  ## use ecological boundaries to create studyAreaANPP
  studyAreaANPP <- prepInputs(
    # url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
    # url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
    url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip",
    destinationPath = mod$dPath,
    projectTo = sim$studyArea,
    fun = "sf::st_read",
    overwrite = TRUE
  )
  ## ensure matching CRS before the intersect (prepInputs projectTo not honoured here)
  studyAreaANPP <- sf::st_transform(studyAreaANPP, sf::st_crs(sim$studyArea))
  studyAreaANPP <- studyAreaANPP[
    which(sapply(sf::st_intersects(studyAreaANPP, sim$studyArea), length) > 0),
  ]
  sim$studyAreaANPP <- studyAreaANPP

  ## save study area maps to file
  studyAreaDir <- file.path(inputPath(sim), "studyAreas") |> fs::dir_create()
  sf::st_write(
    obj = sim$studyAreaReporting,
    dsn = file.path(studyAreaDir, glue::glue("{P(sim)$.studyAreaName}.shp")),
    append = FALSE
  )

  f_gg_studyAreas <- file.path(figurePath(sim), "studyAreas.png")
  gg_studyAreas <- ggplot() +
    geom_sf(data = sim$studyAreaANPP, fill = "gray") +
    geom_sf(
      data = sim$studyArea_biomassParam,
      fill = "lightblue",
      alpha = 0.3
    ) +
    geom_sf(data = sim$studyArea, fill = "violet", alpha = 0.3) +
    geom_sf(data = sim$studyAreaReporting, fill = "darkblue", alpha = 0.3)

  ggsave(f_gg_studyAreas, gg_studyAreas)
  sim <- registerOutputs(f_gg_studyAreas)

  ## LCC / rasterToMatch -------------------------------------------------------------------------
  ## SCANFI is a restricted Drive dataset the landweb service account CAN read
  ## (reproducible uses the SA when GOOGLEDRIVE_AUTH -> SA JSON, set in LandWeb.Renviron).
  ##
  ## BYPASS LandR::prepInputs_SCANFI_LCC_FAO: its FAO step -- prepInputs(fao, to = lcc) --
  ## makes reproducible reproject the WHOLE 840 MB Canada-wide FAO (>50 min) instead of
  ## cropping first. Keep its (fast) SCANFI LCC load, align the FAO windowed (~2 s), then
  ## apply the identical DisturbedAdjust (FAO == 2 & non-forest LCC -> disturbedCode 240).
  ## TODO: move into a LandWebUtils helper (e.g. prepInputs_SCANFI_LCC_FAO_fast).
  ## download only (SCANFI via SA/GOOGLEDRIVE_AUTH, FAO via http), then crop both layers
  ## windowed ourselves. The FAO MUST be windowed before reprojecting or reproducible warps
  ## the whole ~840 MB Canada-wide raster (>50 min); see LandR::prepInputs_SCANFI_LCC_FAO and
  ## _tmp_upstream_issues.md #1. SCANFI LCC cropped the same way for consistency/speed.
  reproducible::preProcess(
    url = "https://drive.google.com/file/d/1EGp7LUA7cXMR6KpXDmu617xsjwGM6aIx",
    targetFile = "SCANFI_att_nfiLandcover_CanadaLCCclassCodes_2020_v2_20260119.tif",
    destinationPath = mod$dPath
  )
  reproducible::preProcess(
    url = "https://opendata.nfis.org/downloads/forest_change/CA_FAO_forest_2019.zip",
    targetFile = "CA_FAO_forest_2019.tif",
    alsoExtract = "similar",
    destinationPath = mod$dPath
  )
  ## study area as a SpatVector for the windowed crops below
  sa <- terra::vect(sf::st_as_sf(sim$studyArea_biomassParam))
  lcc <- terra::rast(file.path(
    mod$dPath,
    "SCANFI_att_nfiLandcover_CanadaLCCclassCodes_2020_v2_20260119.tif"
  ))
  lcc <- terra::crop(lcc, terra::project(sa, terra::crs(lcc)), mask = TRUE) |>
    terra::as.int()
  fao <- terra::rast(file.path(
    mod$dPath,
    "CA_FAO_forest_2019",
    "CA_FAO_forest_2019.tif"
  ))
  fao <- terra::crop(fao, terra::ext(terra::project(sa, terra::crs(fao)))) |>
    terra::project(lcc, method = "near")
  LCClarge <- terra::lapp(
    c(lcc, fao),
    usenames = FALSE,
    fun = function(LCC, FAO) {
      LCC[FAO == 2 & !LCC %in% c(210, 220, 230)] <- 240L
      LCC
    }
  )

  ## Canada LCC 2020 (NALCMS/CCRS) -- the CURRENT-CONDITIONS land cover, independent of SCANFI.
  ## Public datacube COG (no auth), EPSG:3979, tiled with overviews, so windowed reads are cheap.
  ## Reprojected onto the SCANFI grid: same LCC projection family but lat_0 = 0 vs 49, a constant
  ## 6,585,077 m northing shift that is NOT a whole pixel (/30 = 219502.57), so this always
  ## resamples -- `method = "near"` because the values are categorical.
  ## Guard the download: for a plain http source `preProcess()` ERRORS when the target file is
  ## already present ("already exists ... Use overwrite = TRUE?") rather than skipping the way the
  ## Drive path does. Re-downloading 2 GB every run to dodge that is not an option, so fetch once.
  ccFile <- file.path(mod$dPath, "landcover-2020-classification.tif")
  if (!file.exists(ccFile)) {
    reproducible::preProcess(
      url = paste0(
        "https://datacube-prod-data-public.s3.ca-central-1.amazonaws.com/",
        "store/land/landcover/landcover-2020-classification.tif"
      ),
      targetFile = basename(ccFile),
      destinationPath = mod$dPath
    )
  }
  cc2020 <- terra::rast(ccFile)
  cc2020 <- terra::crop(cc2020, terra::ext(terra::project(sa, terra::crs(cc2020)))) |>
    terra::project(lcc, method = "near") |>
    terra::as.int()

  if (P(sim)$pixelSize != 30) {
    stopifnot(P(sim)$pixelSize %in% c(240, 120, 90))
    aggFact <- as.integer(P(sim)$pixelSize / 30)
    LCClarge <- terra::aggregate(LCClarge, fact = aggFact, fun = "modal")
    ## MUST use the same fact/fun as LCClarge: the NRV envelope (from the simulated landscape)
    ## and the current-condition marker (from this layer) have to share one grid.
    cc2020 <- terra::aggregate(cc2020, fact = aggFact, fun = "modal")
  }
  LCClarge <- terra::as.int(LCClarge)
  cc2020 <- terra::as.int(cc2020)

  sim$rasterToMatch_biomassParam <- LCClarge
  sim$rasterToMatch <- terra::crop(
    LCClarge,
    terra::vect(sim$studyArea),
    mask = TRUE
  )
  sim$rasterToMatchReporting <- terra::crop(
    LCClarge,
    terra::vect(sim$studyAreaReporting),
    mask = TRUE
  )

  if (FALSE) {
    terra::plot(sim$rasterToMatchReporting)

    terra::plot(sim$rasterToMatch)
    terra::plot(terra::vect(sim$studyAreaReporting), add = TRUE)

    terra::plot(sim$rasterToMatch_biomassParam)
    terra::plot(terra::vect(sim$studyArea), add = TRUE)
    terra::plot(terra::vect(sim$studyAreaReporting), add = TRUE)
  }

  ## Current Conditions --------------------------------------------------------------------------

  ## Current-conditions land cover = Canada LCC 2020, NOT SCANFI. Using SCANFI here would be
  ## circular: it is already the LCC that drives the simulation. The two are methodologically
  ## independent (NFI photo-plot training vs unsupervised clustering + expert interpretation;
  ## kNN imputation vs per-tile random forest; CFS vs CCRS) -- see report 06.
  ##
  ## TWO layers are kept, deliberately:
  ##   sim$LandTypeCC           -- feeds the overlay; urban is sent to 99 and reclassified to the
  ##                               nearest type, approximating the PRE-INDUSTRIAL state we simulate.
  ##   sim$LandTypeCC_reporting -- urban RETAINED; the current-condition reference for reporting,
  ##                               so "where the landscape sits now" reflects the actual landscape.
  ## Feeding one layer to both would count imputed forest as real current forest.
  ##
  ## NB LandTypeCC keeps NATIVE LCC 2020 codes (1-19); it is not remapped to v2's 0-5 scheme.
  ## The semantics are carried by remapDT below instead.
  sim$LandTypeCC <- cc2020
  sim$LandTypeCC_reporting <- cc2020

  ## TODO (CC age -- follow up with Julie): LCC 2020 carries no age, so the current-condition
  ## age basis is still the SCANFI stand-age map below. Unchanged by this switch.

  ## Non-Tree pixels -----------------------------------------------------------------------------
  ## Canada LCC 2020 (NALCMS level-II) classes, and the v2 CC class each stands in for:
  ##   1, 2, 5, 6   forest (needleleaf / taiga / broadleaf / mixed)  <- v2 CC 0-2 (tree)
  ##   8, 10-13     shrubland / grassland / lichen-moss              <- v2 CC 3 (no veg dynamics,
  ##   14           wetland                                             but burns)
  ##   15           cropland                                         <- v2 CC 5 (grassland for fire)
  ##   16, 18, 19   barren / water / snow-ice                        <- v2 CC 4 (dropped)
  ##   17           urban                                            <- NO v2 equivalent (see below)
  treeClassesCC <- c(1L, 2L, 5L, 6L)
  nonFlammClassesCC <- c(16L, 18L, 19L) ## barren, water, snow/ice
  ## Urban has no v2 counterpart: v2's CC layer had no urban class at all. It is sent to 99 so
  ## convertUnwantedLCC() imputes the nearest type -- the correct PRE-INDUSTRIAL treatment, since
  ## that land was forest. Measured footprint is small: 0.15% of the FMA reporting area at 240 m
  ## (0.50% at 30 m; modal aggregation suppresses it 3.4x), max 1.44% in any one FMA.
  urbanClassCC <- 17L
  treePixelsCCTF <- sim$LandTypeCC[] %in% treeClassesCC
  LandTypeCCNA <- is.na(sim$LandTypeCC[])
  noDataPixelsCC <- LandTypeCCNA | (sim$LandTypeCC[] == 15L) ## cropland == v2's "no data" class 5
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
  nontreeClassesLCC <- sort(uniqueLCCClasses[
    !uniqueLCCClasses %in% P(sim)$treeClassesLCC
  ])

  ## for each LCC + CC class combo, define which LCC code should be used:
  ## setting a pixel to NA will omit it entirely (i.e., non-vegetated)
  ## Rule ORDER matters -- later assignments overwrite earlier ones. This mirrors v2's ordering
  ## exactly, with LCC 2020 codes substituted for v2's CC 0-5 (and urban added).
  ## NB CC does NOT override LCC's forest determination: every CC class except
  ## barren/water/snow-ice defers to the LCC code, so LCC 2020 calling treed wetland "wetland"
  ## will not strip forest out of the simulation -- SCANFI still drives forest extent.
  remapDT <- expand.grid(
    LCC = c(NA_integer_, sort(uniqueLCCClasses)),
    CC = c(NA_integer_, sort(unique(na.omit(sim$LandTypeCC[]))))
  ) |>
    as.data.table()
  remapDT[LCC %in% c(0, 20, 30), newLCC := NA_integer_]
  remapDT[is.na(CC) | CC == 15L, newLCC := LCC] ## cropland: defer to LCC (v2 CC 5)
  remapDT[CC %in% nonFlammClassesCC, newLCC := NA_integer_] ## drop water/barren/ice (v2 CC 4)
  remapDT[CC %in% c(treeClassesCC, 8L, 10L, 11L, 12L, 13L, 14L), newLCC := LCC] ## v2 CC 0-3
  remapDT[is.na(LCC) & CC %in% treeClassesCC, newLCC := 99] ## CC says forest, LCC has none
  remapDT[CC == urbanClassCC, newLCC := 99] ## urban -> reclassify to nearest type
  remapDT[LCC %in% P(sim)$treeClassesToReplace, newLCC := 99] ## reclassification needed

  ## LandTypeCC now carries real data (Canada LCC 2020), so the all-NA workaround that forced an
  ## all-"5" filler is no longer needed (it existed because overlayLCCs cannot digest an all-NA
  ## CC layer -- see _tmp_upstream_issues.md #5).
  LandTypeCCfiller <- sim$LandTypeCC
  message("Overlaying land cover maps...")
  LCClarge <- overlayLCCs(
    LCCs = list(CC = LandTypeCCfiller, LCC = LCClarge),
    forestedList = list(CC = treeClassesCC, LCC = P(sim)$treeClassesLCC),
    outputLayer = "LCC",
    remapTable = remapDT,
    ## `convertUnwantedLCC()` draws each replaced pixel's new class from the surrounding pixels
    ## that are NOT in `classesToReplace` -- so membership here is also what excludes a class from
    ## the DONOR pool. `30` is included deliberately: SCANFI conflates urban into its `30`
    ## (rock/barren) code, so leaving it available let urban pixels be "reclassified" straight back
    ## to another urban pixel's code, perpetuating the industrial footprint this step exists to
    ## remove (37% of urban landed on `30` before this change).
    ##
    ## This is surgical rather than broad: genuine barren -- where SCANFI AND LCC 2020 agree
    ## (`CC %in% c(16, 18, 19)`) -- is already sent to NA by `remapDT` above and is untouched here.
    ## The only `30`s that survive to this point are pixels SCANFI reads as barren while LCC 2020
    ## calls them vegetated or cropland, i.e. roads/clearings/industrial disturbance. Those are
    ## exactly what should be reclassified, not donated from.
    classesToReplace = c(P(sim)$treeClassesToReplace, 99L, 30L),
    availableERC_by_Sp = NULL
  ) |>
    Cache()
  message("...done.")

  treePixelsLCC <- which(LCClarge[] %in% P(sim)$treeClassesLCC)
  nonTreePixels <- which(LCClarge[] %in% nontreeClassesLCC)

  sim$nonTreePixels <- nonTreePixels

  ## Update rasterToMatch layer with all trees
  sim$rasterToMatch_biomassParam[sim$nonTreePixels] <- NA
  sim$rasterToMatch <- postProcess(
    sim$rasterToMatch_biomassParam,
    to = sim$studyArea,
    filename2 = NULL
  )

  ## Age from Current Conditions -----------------------------------------------------------------
  ## No CC age raster available yet (TODO: Julie to add one) -- use the SCANFI
  ## stand-age map below as the current-condition age (CC_TSF).

  ## Age map -------------------------------------------------------------------------------------

  ## SCANFI 2020 stand age (Drive id 1nXPS3bp..., ~5.5 GB). Fetch via the authenticated
  ## googledrive API (workflowtools::drive_download_once) -- a fresh reproducible Drive download
  ## of this large restricted file returns an unauthenticated sign-in HTML page (the SA token
  ## works for the googledrive API but not reproducible's content download; see
  ## _tmp_upstream_issues.md #6). Then crop windowed + align to the RTM (bilinear), masked by it.
  ## TODO: this SKIPS prepInputsStandAgeMap's NTEMS fire/harvest + kNN age adjustment -- raw
  ## SCANFI median age for now; revisit once the upstream Drive-auth issue is resolved.
  ageFile <- file.path(mod$dPath, "SCANFI_age_median_2020_v2_20260119.tif")
  workflowtools::drive_download_once(googledrive::as_id("1nXPS3bpFUESYieNfXO25OKlZJEgqtRnD"), ageFile)
  ageRast <- terra::rast(ageFile)
  saAge <- terra::project(terra::vect(sf::st_as_sf(sim$studyArea_biomassParam)), terra::crs(ageRast))
  standAgeMap <- terra::crop(ageRast, saAge, mask = TRUE) |>
    terra::project(sim$rasterToMatch_biomassParam, method = "bilinear") |>
    terra::mask(sim$rasterToMatch_biomassParam)

  ## current-condition age = SCANFI stand age (no CC age raster yet -- TODO: Julie)
  CC_TSF <- standAgeMap
  CC_TSF[sim$nonTreePixels] <- NA
  attr(CC_TSF, "imputedPixID") <- integer(0) ## TODO: reassess whether overlay counts as imputation

  ## Flammability and Fire Return Interval rasters -----------------------------------------------

  ## flammability map shouldn't be masked (no gaps!);
  #    NAs outside the buffered study & snow/rock/ice area are the only values we want NA
  #    use the LCC flammability map to fill in NA / nodata values

  ## No data class is 5 -- these will be filled in by LCC layer
  # NA_ids <- which(is.na(sim$LandTypeCC[]) | sim$LandTypeCC[] == 5)
  ## Only class 4 is considered non-flammable
  ## With no CC layer (LandTypeCC all NA), flammability comes entirely from the LCC map:
  ## defineFlammable() errors on an all-NA layer, and the CC values would be fully overwritten
  ## by LCC below anyway (LandTypeCCNA all TRUE). Only blend a CC-based layer when CC has data.
  flammableMapLCC <- defineFlammable(
    LCClarge,
    nonFlammClasses = c(20, 30, 40, 80), ## see LCC classes above
    mask = NULL,
    filename2 = NULL
  )

  if (all(LandTypeCCNA)) {
    sim$flammableMap <- flammableMapLCC
  } else {
    flammableMapCC <- defineFlammable(
      sim$LandTypeCC,
      nonFlammClasses = nonFlammClassesCC, ## LCC 2020: barren, water, snow/ice
      mask = NULL,
      filename2 = NULL
    )
    sim$flammableMap <- flammableMapCC
    sim$flammableMap[LandTypeCCNA] <- flammableMapLCC[LandTypeCCNA]
  }
  sim$flammableMap <- terra::as.int(sim$flammableMap) |>
    terra::crop(sim$rasterToMatch) ## ensure it matches studyArea

  ## fireReturnInterval needs to be masked by flammableMap
  rstFireReturnInterval <- terra::rasterize(
    x = lthfc_clean, ## already a SpatVector (tidyterra); terra::vect() has no SpatVector method
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
  sim$rstLCC <- LCClarge ## standard LandR name expected by the pipeline + Biomass modules
  sim$CC_TSF <- CC_TSF
  sim$standAgeMap <- standAgeMap ## standard LandR name (current-condition stand age)

  ## some assertions:
  testObjs <- c(
    "studyArea",
    "studyArea_biomassParam",
    "studyAreaReporting",
    "rasterToMatch",
    "rasterToMatch_biomassParam",
    "rasterToMatchReporting",
    "fireReturnInterval",
    "CC_TSF"
  )
  lapply(testObjs, function(x) {
    if (is.null(sim[[x]])) {
      stop(
        "LandWeb_preamble: ",
        paste0("sim$", x, " returned NULL."),
        call. = FALSE
      )
    }
  })

  compareGeom(sim$rasterToMatch, rstFireReturnInterval, sim$flammableMap)
  ## end assertions

  return(invisible(sim))
}

InitSpecies <- function(sim) {
  sppEquiv <- LandR::sppEquivalencies_CA

  if (FALSE) {
    LandR::speciesInStudyArea(sim$studyArea, dataSource = "SCANFI")

    LandR::speciesInStudyArea(
      sim$studyAreaLandWeb,
      dataSource = "SCANFI"
    )$speciesList |>
      sort()
    ##>  [1] "ABIE_BAL"     "ABIE_LAS"     "BETU_PAP"     "LARI_LAR"     "LARI_OCC"
    ##>  [6] "PICE_ENG"     "PICE_ENG_GLA" "PICE_GLA"     "PICE_MAR"     "PINU_BAN"
    ##> [11] "PINU_CON_LAT" "POPU_BAL"     "POPU_GRA"     "POPU_TRE"     "PSEU_MEN"
    ##> [16] "PSEU_MEN_GLA" "THUJ_PLI"     "TSUG_HET"

    ## NOTE: POPU_GRA is unreliable, do not use!
  }

  ## Make LandWeb spp equivalencies
  sppEquiv[,
    LandWeb := c(
      ABIE_BAL = "Abie_spp",
      ABIE_LAS = "Abie_spp",
      BETU_PAP = "Popu_spp",
      LARI_LAR = "Lari_spp",
      LARI_OCC = "Lari_spp",
      PICE_ENG = "Pice_gla",
      PICE_ENG_GLA = "Pice_gla", ## TODO: confirm merge with Pice_gla
      PICE_GLA = "Pice_gla",
      PICE_MAR = "Pice_mar",
      PINU_BAN = "Pinu_spp",
      PINU_CON_CON = "Pinu_spp", ## shore pine (Pinus contorta var. contorta; coastal)
      PINU_CON_LAT = "Pinu_spp", ## lodgepole pine (Pinus contorta var. latifolia; interior)
      POPU_BAL = "Popu_spp",
      POPU_TRE = "Popu_spp",
      PSEU_MEN = "Pseu_men",
      PSEU_MEN_GLA = "Pseu_men",
      ## Western redcedar & western hemlock are absent from the original Silvacom
      ## CurrentConditions species groups (White/Black Spruce, Pine, Fir, Deciduous) and
      ## look like SCANFI over-attribution in AB (e.g. Tsug_het is ~25% of the Spray Lake
      ## FMA, well outside its real range). Merge both into Abie_spp -- the closest
      ## shade-tolerant softwood analog -- rather than simulating them as distinct species.
      ## TODO: revisit -- confirm Abie_spp is the right target (vs. dropping them, or a
      ## per-study-area rule for FMAs nearer the BC coast where they may genuinely occur).
      THUJ_PLI = "Abie_spp",
      TSUG_HET = "Abie_spp"
    )[SCANFI]
  ]

  sppEquiv[
    LandWeb == "Lari_spp",
    `:=`(
      EN_generic_full = "Western Larch & Tamarack",
      EN_generic_short = "Larch & Tamarack",
      Leading = "Larch & Tamarack leading"
    )
  ]

  sppEquiv[
    LandWeb == "Pice_gla",
    `:=`(
      EN_generic_full = "White & Engelmann's Spruce",
      EN_generic_short = "Whi & Eng Spr",
      Leading = "White & Engelmann's Spruce leading"
    )
  ]

  sppEquiv[
    grep("Pin", LandWeb),
    `:=`(
      EN_generic_short = "Pine",
      EN_generic_full = "Pine",
      Leading = "Pine leading"
    )
  ]

  sppEquiv[
    LandWeb == "Popu_spp",
    `:=`(
      EN_generic_full = "Deciduous",
      EN_generic_short = "Decid",
      Leading = "Deciduous leading"
    )
  ]

  sppEquiv[
    LandWeb == "Pseu_men",
    `:=`(
      EN_generic_full = "Douglas fir",
      EN_generic_short = "Doug fir",
      Leading = "Douglas fir leading"
    )
  ]

  sim$sppEquiv <- sppEquiv[!is.na(LandWeb), ]
  sim$sppColorVect <- LandR::sppColors(
    sim$sppEquiv,
    "LandWeb",
    newVals = "Mixed",
    palette = "Accent"
  )

  ## species parameter tables
  sim$speciesTable <- LandR::getSpeciesTable(dPath = mod$dPath) ## uses default URL

  ## TODO: restore changes made by LandR::speciesTableUpdate,
  ## so shadetol to 'defaults' listed below -- except perhaps increase Pinu to 1.5 or 2
  speciesParams <- list(
    # resproutage_min = list(Popu_spp = 25L), # default 10L
    shadetolerance = list(
      ## defaults: 4, 3, 4, 1, 1, 3
      Abie_spp = 3,
      Pice_gla = 2,
      Pice_mar = 3,
      Pinu_spp = 1,
      Popu_spp = 1,
      Pseu_men = 3
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
