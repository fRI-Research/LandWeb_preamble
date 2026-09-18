## The module's metadata is its public contract: a project using this module binds to these
## object names and classes, and `reqdPkgs` states what it needs to run at all. These are
## CHARACTERIZATION tests -- they pin today's contract so a change to it has to be deliberate,
## rather than describing behaviour that did not exist before.
##
## GENERATED from the module's live metadata, then reviewed. When a change is intended, update
## this file in the same commit and bump the module version to match: removed, renamed or
## retyped is a MAJOR bump.

test_that("module metadata parses", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_type(md, "list")
  expect_identical(md$name, moduleName)
})

test_that("the module declares no input objects", {
  ## It is a data-preparation module: it reads from disk and the network, not from the simList.
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_equal(sum(!is.na(md$inputObjects$objectName)), 0L)
})

test_that("outputs are the expected names and classes", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  outputs <- stats::setNames(md$outputObjects$objectClass, md$outputObjects$objectName)
  outputs <- outputs[!is.na(names(outputs))]
  expect_identical(
    outputs[order(tolower(names(outputs)))],
    c(
      "CC TSF"                     = "RasterLayer",
      "fireReturnInterval"         = "RasterLayer",
      "flammableMap"               = "RasterLayer",
      "LandTypeCC"                 = "RasterLayer",
      "LandTypeCC_reporting"       = "RasterLayer",
      "LCC"                        = "RasterLayer",
      "nonTreePixels"              = "integer",
      "rasterToMatch"              = "RasterLayer",
      "rasterToMatch_biomassParam" = "RasterLayer",
      "rasterToMatchReporting"     = "RasterLayer",
      "ROSTable"                   = "data.table",
      "rstLCC"                     = "SpatRaster",
      "speciesParams"              = "list",
      "speciesTable"               = "data.table",
      "sppColorVect"               = "character",
      "sppEquiv"                   = "data.table",
      "standAgeMap"                = "SpatRaster",
      "studyArea"                  = "sf",
      "studyArea_biomassParam"     = "sf",
      "studyAreaANPP"              = "sf",
      "studyAreaLandWeb"           = "sf",
      "studyAreaReporting"         = "sf"
)
  )
})

test_that("parameters are the expected names", {
  md <- SpaDES.core::moduleMetadata(module = moduleName, path = modulePath)
  expect_identical(
    sort(md$parameters$paramName),
    c(
      ".plotInitialTime", ".plotInterval", ".plots", ".saveInitialTime",
      ".saveInterval", ".sslVerify", ".studyAreaName", ".useCache", "bufferDist",
      "bufferDistLarge", "ccAgeDriveId", "ccAgeMaxMissing", "dispersalType",
      "forceResprout", "friMultiple", "mergeSlivers", "minFRI", "ntemsAgeFile",
      "ntemsAgeYear", "pixelSize", "ROStype", "treeClassesLCC", "treeClassesToReplace"
)
  )
})

