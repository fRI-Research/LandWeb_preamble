---
title: "LandWeb_preamble"
author: ""
date: "15 August 2023"
output:
  pdf_document:
    keep_md: yes
editor_options:
  chunk_output_type: console
---



#### Authors:

Eliot J B McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut, cre], Alex M. Chubaty <achubaty@for-cast.ca> [aut], Ceres Barros <cbarros@mail.ubc.ca> [aut]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Set up study areas and parameters for LandWeb simulations.

## Parameters

Provide a summary of user-visible parameters.


\resizebox{\linewidth}{!}{
\begin{tabu} to \linewidth {>{\raggedright}X>{\raggedright}X>{\raggedright}X>{\raggedright}X>{\raggedright}X>{\raggedright}X}
\hline
paramName & paramClass & default & min & max & paramDesc\\
\hline
bufferDist & numeric & 25000 & 20000 & 1e+05 & Study area buffer distance (m) used to make `studyArea`.\\
\hline
bufferDistLarge & numeric & 50000 & 20000 & 1e+05 & Study area buffer distance (m) used to make `studyAreaLarge`.\\
\hline
forceResprout & logical & FALSE & NA & NA & `TRUE` forces all species to resprout, setting `resproutage\_min` to zero, `resproutage\_max` to 400, and `resproutProb` to 1.0.\\
\hline
friMultiple & numeric & 1 & 0.5 & 2 & Multiplication factor for adjusting fire return intervals.\\
\hline
dispersalType & character & default & NA & NA & One of 'aspen', 'high', 'none', or 'default'.\\
\hline
minFRI & numeric & 40 & 0 & 200 & The value of fire return interval below which, pixels will be changed to `NA`, i.e., ignored\\
\hline
pixelSize & numeric & 250 & NA & NA & Pixel size in metres. Should be one of 250, 125, 50, 25.\\
\hline
ROStype & character & default & NA & NA & Rate of spread preset to use. One of 'burny', 'equal', 'log', or 'default'.\\
\hline
treeClassesLCC & integer & 1, 2, 3,.... & 0 & 39 & AKA `forestedLCCClasses`. The classes in the `LCC2005` layer that are considered 'trees' from the perspective of LandR-Biomass.\\
\hline
treeClassesToReplace & numeric & 34, 35, 36 & 0 & 39 & The transient classes in the `LCC2005` layer that will become 'trees' from the perspective of LandR-Biomass (e.g., burned)\\
\hline
.plotInitialTime & numeric & 0 & NA & NA & This describes the simulation time at which the first plot event should occur\\
\hline
.plotInterval & numeric & 1 & NA & NA & This describes the simulation time interval between plot events\\
\hline
.plots & character & object & NA & NA & Passed to `types` in `Plots` (see `?Plots`). There are a few plots that are made within this module, if set. Note that plots (or their data) saving will ONLY occur at `end(sim)`. If `NA`, plotting is turned off completely (this includes plot saving).\\
\hline
.saveInitialTime & numeric & NA & NA & NA & This describes the simulation time at which the first save event should occur\\
\hline
.saveInterval & numeric & NA & NA & NA & This describes the simulation time interval between save events\\
\hline
.sslVerify & integer & 64 & NA & NA & Passed to `httr::config(ssl\_verifypeer = P(sim)\$sslVerify)` when downloading KNN (NFI) datasets. Set to 0L if necessary to bypass checking the SSL certificate (this may be necessary when NFI's website SSL certificate is not correctly configured).\\
\hline
.studyAreaName & character & NA & NA & NA & Human-readable name for the study area used. If `NA`, a hash of `studyAreaLarge` will be used.\\
\hline
.useCache & logical & FALSE & NA & NA & Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant\\
\hline
\end{tabu}}

## Data dependencies

### Input data

Description of the module inputs.


\resizebox{\linewidth}{!}{
\begin{tabu} to \linewidth {>{\raggedright}X>{\raggedright}X>{\raggedright}X>{\raggedright}X}
\hline
objectName & objectClass & desc & sourceURL\\
\hline
canProvs & SpatialPolygonsDataFrame & Canadian provincial boundaries shapefile & NA\\
\hline
\end{tabu}}

### Output data

Description of the module outputs.


\resizebox{\linewidth}{!}{
\begin{tabu} to \linewidth {>{\raggedright}X>{\raggedright}X>{\raggedright}X}
\hline
objectName & objectClass & desc\\
\hline
CC TSF & RasterLayer & Time since fire (aka age) map derived from Current Conditions data.\\
\hline
fireReturnInterval & RasterLayer & fire return interval raster\\
\hline
LandTypeCC & RasterLayer & Land Cover Classification map derived from Current Conditions data.\\
\hline
ml & map & `map` object containing study areas, reporting polygons, etc. for post-processing.\\
\hline
LCC & RasterLayer & The result of `LandR::overlayLCCs()` on `LCC2005` and `LandTypeCC`.\\
\hline
nonTreePixels & integer & NA\\
\hline
rasterToMatch & RasterLayer & NA\\
\hline
rasterToMatchLarge & RasterLayer & NA\\
\hline
rasterToMatchReporting & RasterLayer & NA\\
\hline
ROSTable & data.table & A `data.table` with 3 columns: `age`, `leading`, and `ros`. The values under the `age` column can be `mature`, `immature`, `young` and compound versions of these, e.g., `immature\_young` which can be used when 2 or more age classes share same `ros`. `leading` should be vegetation type. `ros` gives the rate of spread values for each age and type.\\
\hline
rstFlammable & RasterLayer & NA\\
\hline
speciesParams & list & list of updated species trait values to be used to updated `speciesTable` to create `species`.\\
\hline
speciesTable & data.table & a table of invariant species traits with the following trait colums: 'species', 'Area', 'longevity', 'sexualmature', 'shadetolerance', 'firetolerance', 'seeddistance\_eff', 'seeddistance\_max', 'resproutprob', 'resproutage\_min', 'resproutage\_max', 'postfireregen', 'leaflongevity', 'wooddecayrate', 'mortalityshape', 'growthcurve', 'leafLignin', 'hardsoft'. Names can differ, but not the column order. Default is from Dominic Cyr and Yan Boulanger's project.\\
\hline
sppColorVect & character & A named vector of colors to use for plotting. The names must be in `sim\$sppEquiv[['LandWeb']]`, and should also contain a color for 'Mixed'\\
\hline
sppEquiv & data.table & table of species equivalencies. See `LandR::sppEquivalencies\_CA`.\\
\hline
studyArea & SpatialPolygonsDataFrame & Polygon to use as the simulation study area.\\
\hline
studyAreaLarge & SpatialPolygonsDataFrame & Polygon to use as the parametrisation study area. Note that `studyAreaLarge` is only used for parameter estimation, and can be larger than the actual study area used for LandR simulations (e.g, larger than `studyArea` in LandR `Biomass\_core`).\\
\hline
studyAreaReporting & SpatialPolygonsDataFrame & multipolygon (typically smaller/unbuffered than `studyAreaLarge` and `studyArea` in LandR `Biomass\_core`) to use for plotting/reporting.\\
\hline
\end{tabu}}

## Links to other modules

Originally developed for use with the LandR Biomass suite of modules, with LandMine fire model.
