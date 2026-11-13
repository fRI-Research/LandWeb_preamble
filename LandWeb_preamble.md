---
title: "LandWeb_preamble Manual"
subtitle: "v.1.0.15"
date: "Last updated: 2026-11-13"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
  bibliography: citations/references_LandWeb_preamble.bib
citation-style: citations/ecology-letters.csl
link-citations: true
always_allow_html: true
---

# *LandWeb_preamble* Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:LandWeb_preamble) *LandWeb_preamble*



#### Authors:

Eliot J B McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut], Alex M. Chubaty <achubaty@for-cast.ca> [aut, cre]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

Set up study areas and parameters for LandWeb simulations.

## Parameters

Provide a summary of user-visible parameters.

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
   <th style="text-align:left;"> cacheKey </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> dataYear </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 2020 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Year of the SCANFI v2 land cover that `rstLCC` and `rasterToMatch` are built from; forest land without trees in that year becomes 240. Set it to Biomass_borealDataPrep's `dataYear`, so the land cover is from the year of the SCANFI biomass and species cover that dataPrep reads. One of 1985, 1995, 2005, 2015, 2020 or 2025. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ccAgeDriveId </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> 1fRd8UdC.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Google Drive id of fRI Research's `age_in2025.zip` current-condition stand-age composite (AB Crown AVI + AB AVIE + BC VEG_COMP 2025 + CanLAD, combined by `CellStatistics(MINIMUM, DATA)`). Update if the file is re-delivered. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ntemsAgeFile </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> CA_fores.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> File name, resolved under `inputPath(sim)`, of the NTEMS per-pixel forest age used to fill `age_in2025`'s gaps outside AB/BC (where CanLAD is its only source and cannot date an undisturbed stand). Values are 0-150 years, 151 meaning '&gt;150', and 255 non-treed. Deliberately NOT SCANFI, which would be circular and is 14-100x too low on old forest outside AB -- see the fill block in `.inputObjects`. `NA` disables the fill and is allowed only where `age_in2025` alone meets `ccAgeMaxMissing`. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ntemsAgeYear </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 2022 </td>
   <td style="text-align:left;"> 1985 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Vintage of `ntemsAgeFile`. The fill is aged forward to the `age_in2025` epoch by `2025 - ntemsAgeYear`; omitting that would put a step discontinuity along the AB/BC boundary. Must match the file actually supplied -- NTEMS publishes several vintages of one product line (2019 and 2022 differ only in the disturbance window). </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ccAgeMaxMissing </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Maximum percent of FOREST (LCC 2020 classes 1/2/5/6) inside the study area allowed to have no current-condition stand age before `LandWeb_preamble` stops. Measured: groups whose only age source is CanLAD sit near 74%; with the NTEMS fill, 0.8-12.8%. Raising this does not fix the data -- it initialises a landscape with almost no old forest. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> anppEcoLevel </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> ecoprovince </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Ecological level whose units touching `studyArea` make `studyAreaANPP`, the area Biomass_speciesParameters takes permanent sample plots from to fit growth curves: 'ecoprovince' or 'ecozone'. Ecozones hold more plots, so more species get growth curves of their own, fitted over a wider area. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bufferDist </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 25000 </td>
   <td style="text-align:left;"> 20000 </td>
   <td style="text-align:left;"> 1e+05 </td>
   <td style="text-align:left;"> Study area buffer distance (m) used to make `studyArea`. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bufferDistLarge </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 50000 </td>
   <td style="text-align:left;"> 20000 </td>
   <td style="text-align:left;"> 1e+05 </td>
   <td style="text-align:left;"> Study area buffer distance (m) used to make `studyArea_biomassParam`. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forceResprout </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> `TRUE` forces all species to resprout, setting `resproutage_min` to zero, `resproutage_max` to 400, and `resproutProb` to 1.0. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> friMultiple </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0.5 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Multiplication factor for adjusting fire return intervals. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersalType </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> default </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> One of 'aspen', 'high', 'none', or 'default'. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mergeSlivers </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should sliver polygons in LTHFC map be merged into nearest non-zero polygon? </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minFRI </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 200 </td>
   <td style="text-align:left;"> The value of fire return interval below which, pixels will be changed to `NA`, i.e., ignored </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelSize </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 240 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Pixel size in metres. Should be one of 240, 120, 90, 30. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ROStype </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> default </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Rate of spread preset to use. One of 'default' or 'burny'. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> treeClassesLCC </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 81, 210,.... </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 240 </td>
   <td style="text-align:left;"> AKA `forestedLCCClasses`. The classes in the `LCC` layer that are considered 'trees' from the perspective of LandR-Biomass. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> treeClassesToReplace </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 240 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> The transient classes in the `LCC` layer that will become 'trees' from the perspective of LandR-Biomass (e.g., burned) </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time at which the first plot event should occur </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between plot events </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plots </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> object </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Passed to `types` in `Plots` (see `?Plots`). There are a few plots that are made within this module, if set. Note that plots (or their data) saving will ONLY occur at `end(sim)`. If `NA`, plotting is turned off completely (this includes plot saving). </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time at which the first save event should occur </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between save events </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .sslVerify </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 64 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Passed to `httr::config(ssl_verifypeer = P(sim)$sslVerify)` when downloading KNN (NFI) datasets. Set to 0L if necessary to bypass checking the SSL certificate (this may be necessary when NFI's website SSL certificate is not correctly configured). </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Human-readable name for the study area used. If `NA`, a hash of `studyArea_biomassParam` will be used. </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
</tbody>
</table>

## Data dependencies

### Input data

Description of the module inputs.

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> character(0) </td>
   <td style="text-align:left;"> c(":----------", ":-----------", ":----", ":---------") </td>
   <td style="text-align:left;"> character(0) </td>
   <td style="text-align:left;"> c(":----------", ":-----------", ":----", ":---------") </td>
  </tr>
</tbody>
</table>

### Output data

Description of the module outputs.

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> CC TSF </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Time since fire (aka age) map derived from Current Conditions data. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireReturnInterval </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> fire return interval raster </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LandTypeCC </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Current-conditions land cover (Canada LCC 2020, NALCMS level-II codes), aligned to `rasterToMatch_biomassParam`. SIMULATION-side copy: urban is sent to 99 by `remapDT` and reclassified to the nearest type, approximating the pre-industrial state. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LandTypeCC_reporting </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Current-conditions land cover (Canada LCC 2020) with urban RETAINED -- the reporting reference, so current condition reflects the actual landscape rather than the pre-industrial approximation the simulation runs on. Do NOT feed this to the overlay. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LCC </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> The result of `LandR::overlayLCCs()` on `LCC` and `LandTypeCC`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rstLCC </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Land cover raster (identical to `LCC`); standard LandR name used downstream. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> standAgeMap </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Current-condition stand age, aligned to the RTM: fRI Research's `age_in2025` (AB/BC inventories + CanLAD), gap-filled from SBFI where configured. NOT SCANFI-derived -- `Biomass_borealDataPrep` already defaults its stand age to a SCANFI product, so a SCANFI basis here would make the current condition a restatement of the model's own assumption rather than an observation against it. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nonTreePixels </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch_biomassParam </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatchReporting </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ROSTable </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> A `data.table` with 3 columns: `age`, `leading`, and `ros`. The values under the `age` column can be `mature`, `immature`, `young` and compound versions of these, e.g., `immature_young` which can be used when 2 or more age classes share same `ros`. `leading` should be vegetation type. `ros` gives the rate of spread values for each age and type. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> flammableMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesTable </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> a table of invariant species traits with the following trait colums: 'species', 'Area', 'longevity', 'sexualmature', 'shadetolerance', 'firetolerance', 'seeddistance_eff', 'seeddistance_max', 'resproutprob', 'resproutage_min', 'resproutage_max', 'postfireregen', 'leaflongevity', 'wooddecayrate', 'mortalityshape', 'growthcurve', 'leafLignin', 'hardsoft'. Names can differ, but not the column order. Default is from Dominic Cyr and Yan Boulanger's project. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppColorVect </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> A named vector of colors to use for plotting: one per species in `sim$sppEquiv[['LandWeb']]`, and one for 'Mixed'. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquiv </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table of species equivalencies. See `LandR::sppEquivalencies_CA`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> Polygon to use as the simulation study area. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaANPP </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> study area to use for parameterization with PSP data in `Biomass_speciesParameters`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaLandWeb </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> Polygon boundary of the full LandWeb study area </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea_biomassParam </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> Polygon to use as the parametrisation study area. Note that `studyArea_biomassParam` is used for species parameter estimation, and should be larger than the actual study area used for LandR simulations (e.g, larger than `studyArea` in LandR `Biomass_core`). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyAreaReporting </td>
   <td style="text-align:left;"> sf </td>
   <td style="text-align:left;"> multipolygon (typically smaller than `studyArea_biomassParam` and `studyArea` in LandR `Biomass_core`) to use for plotting/reporting. </td>
  </tr>
</tbody>
</table>

## Links to other modules

Initially developed for use with the [LandWeb](https://github.com/PredictiveEcology/LandWeb) model.

## Getting help

-   <https://github.com/PredictiveEcology/LandWeb_preamble/issues>

## References {#lwpre-refs}
