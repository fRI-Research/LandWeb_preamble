---
title: "LandWeb_preamble"
author: ""
date: "09 December 2018"
output:
  pdf_document:
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Set up study areas and parameters for LandWeb simulations.

# Parameters

Provide a summary of user-visible parameters.


\resizebox{\linewidth}{!}{
\begin{tabu} to \linewidth {>{\raggedright}X>{\raggedright}X>{\raggedright}X>{\raggedright}X>{\raggedright}X>{\raggedright}X}
\hline
paramName & paramClass & default & min & max & paramDesc\\
\hline
bufferDist & numeric & 25000 & 20000 & 1e+05 & Study area buffer distance (m) used to make studyArea.\\
\hline
bufferDistLarge & numeric & 50000 & 20000 & 1e+05 & Study area buffer distance (m) used to make studyAreaLarge.\\
\hline
friMultiple & numeric & 1 & 0.5 & 2 & Multiplication factor for adjusting fire return intervals.\\
\hline
mapResFact & numeric & 1 & 1 & 10 & The map resolution factor to use with raster::disaggregate to reduce pixel size below 250 m. Should be one of 1, 2, 5, 10, which correspends to pixel size of 250m, 125m, 50m, 25m, repsectively.\\
\hline
minFRI & numeric & 40 & 0 & 200 & The value of fire return interval below which, pixels will be changed to NA, i.e., ignored\\
\hline
runName & character & NA & NA & NA & A description for run; this will form the basis of cache path and output path\\
\hline
treeClassesLCC & numeric & 1, 2, 3,.... & 0 & 39 & The classes in the LCC2005 layer that are considered 'trees' from the perspective of LandR-Biomass\\
\hline
treeClassesToReplace & numeric & 34, 35, 36 & 0 & 39 & The transient classes in the LCC2005 layer that will become 'trees' from the perspective of LandR-Biomass (e.g., burned)\\
\hline
.plotInitialTime & numeric & NA & NA & NA & This describes the simulation time at which the first plot event should occur\\
\hline
.plotInterval & numeric & NA & NA & NA & This describes the simulation time interval between plot events\\
\hline
.plots & character & object & NA & NA & Passed to `types` in `Plots` (see `?Plots`). There are a few plots that are made within this module, if set. Note that plots (or their data) saving will ONLY occur at `end(sim)`. If `NA`, plotting is turned off completely (this includes plot saving).\\
\hline
.saveInitialTime & numeric & NA & NA & NA & This describes the simulation time at which the first save event should occur\\
\hline
.saveInterval & numeric & NA & NA & NA & This describes the simulation time interval between save events\\
\hline
.useCache & logical & FALSE & NA & NA & Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant\\
\hline
\end{tabu}}

# Data dependencies

## Input data

Description of the module inputs.


\resizebox{\linewidth}{!}{
\begin{tabu} to \linewidth {>{\raggedright}X>{\raggedright}X>{\raggedright}X>{\raggedright}X}
\hline
objectName & objectClass & desc & sourceURL\\
\hline
canProvs & SpatialPolygonsDataFrame & Canadian provincial boundaries shapefile & NA\\
\hline
\end{tabu}}

## Output data

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
LCC2005 & RasterLayer & Land Cover Classification map derived from national data.\\
\hline
ml & map & `map` object containing study areas, reporting polygons, etc. for post-processing.\\
\hline
LCC & RasterLayer & A key output from this module: it is the result of LandR::overlayLCCs on LCC2005 and LandTypeCC\\
\hline
nonTreePixels & integer & NA\\
\hline
rasterToMatch & RasterLayer & NA\\
\hline
rasterToMatchReporting & RasterLayer & NA\\
\hline
rstFlammable & RasterLayer & NA\\
\hline
sppColorVect & character & A named vector of colors to use for plotting. The names must be in `sim\$sppEquiv[[sim\$sppEquivCol]]`, and should also contain a color for 'Mixed'\\
\hline
sppEquiv & data.table & table of species equivalencies. See `LandR::sppEquivalencies\_CA`.\\
\hline
studyArea & SpatialPolygonsDataFrame & NA\\
\hline
studyAreaLarge & SpatialPolygonsDataFrame & Polygon to use as the parametrisation study area. Note that `studyAreaLarge` is only used for parameter estimation, and can be larger than the actual study area used for LandR simulations (e.g, larger than `studyArea` in LandR Biomass\_core).\\
\hline
studyAreaReporting & SpatialPolygonsDataFrame & multipolygon (typically smaller/unbuffered than `studyAreaLarge` and `studyArea` in LandR Biomass\_core) to use for plotting/reporting.\\
\hline
\end{tabu}}

# Links to other modules

Originally developed for use with the LandR Biomass suite of modules, with LandMine fire model.
