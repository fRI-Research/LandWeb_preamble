Known issues: <https://github.com/fRI-Research/LandWeb_preamble/issues>

# LandWeb_preamble (development version)

* Ported the preamble to terra/targets for the Phase-0 pipeline; Phase-0 now runs end-to-end for the Spray Lake study area. Moved `amc::outerBuffer` to `spatialutils::outerBuffer` across all study-area (`R/*`) files; added a SCANFI LCC + FAO windowed self-crop (skips LandR's whole-Canada FAO reproject); fetch SCANFI age via `workflowtools::drive_download_once` (Google Drive service-account auth) plus a windowed crop; derive flammability from LCC; fix `fireReturn` via `vect()`; and emit `rstLCC` + `standAgeMap` under the names the pipeline/Biomass modules expect.
* Removed all `browser()` debugging calls.
* Species relabelling: merged western redcedar (`THUJ_PLI`) and western hemlock (`TSUG_HET`) into `Abie_spp` (both are absent from the Silvacom CurrentConditions species groups and look like SCANFI over-attribution in AB, e.g. `Tsug_het` covering ~25% of the Spray Lake FMA, well outside its real range).
* `InitSpecies`: dropped the vestigial `PINU_CON` handling (SCANFI v2 splits Pinus contorta into `PINU_CON_CON` and `PINU_CON_LAT`, both already mapped to `Pinu_spp`) and labelled the contorta varieties.
* Renamed the flammable-map object `rstFlammable` to `flammableMap` to align with scfm/fireSense and downstream burnSummaries/NRV_summary.

# LandWeb_preamble 1.0.1 (2025-10-14)

* Renamed `rasterToMatchLarge` to `rasterToMatch_biomassParam`.

# LandWeb_preamble 1.0.0 (2025-09-18)

* Reworked the preamble for SCANFI and other input-data updates.
* Simulate more tree species with updated species parameters; switched tree classes to integers.
* Removed the unreliable SCANFI species layer `Popu_gra` (and updated related comments).
* Study-area object restructuring and renaming: `studyAReaLarge` to `studyArea_biomassParam`; used national ecoregions to define `studyArea_biomassParam`; created `studyAReaANPP` from ecoregions and added it to plots; `StudyAreaLandWeb` to `studyAreaLandWeb`.

# LandWeb_preamble 0.0.9 (2025-01-14)

* Enforce a minimum `seeddistance_eff` (cannot be zero).
* Switched to the native R pipe and moved plotting functions out to the LandWebUtils package.
* Removed the `map` object and dropped the `map` package dependency; Rmd fixes.

# LandWeb_preamble 0.0.8 (2024-10-07)

* Fixed the C5 study-area boundaries.
* Use `ltfc_sls_v3` for Spray Lakes.

# LandWeb_preamble 0.0.7 (2024-10-01)

* Additional C5 study-area fix; version bump.

# LandWeb_preamble 0.0.6 (2024-09-26)

* Spray Lakes: include Douglas-fir, using custom LTHFCs; added generic names and leading species for Douglas-fir.
* Fixed `studyAreaName` checks for Spray Lake to allow v3 runs.

# LandWeb_preamble 0.0.5 (2023-09-15)

* Added new LTHFC polygons in MB and the latest LTHFC polygons (v8c); improved LTHFC polygon geoprocessing and sliver merging (`mergeSlivers`); switched to `write_sf()`; stopped removing slivers by default (pending discussion); updated the LTHFC field name in the map object.
* Added national ecozone and ecoregion reporting polygons for AB.
* Added the Spray Lake + C5 landbase; use `maskTo` instead of `mask` in `postProcess()`; stopped modifying Spray Lakes shade-tolerance values; stopped using the full study area (Spray Lake + C5) as an analysisGroup.

# LandWeb_preamble 0.0.4 (2022-10-27)

* (Development continued through 2023.) Added additional MB reporting polygons plus updates for reproducible/terra; ensured MB caribou and FMLs are joined and added correctly to the map object.
* Active/passive handling for WestFraser and Manning areas and for caribou, with conditional use of MB reporting polygons and improved plotting.
* Allow ROStype "burny" for areas with discontinuous fuels.
* Updated to the latest `map` package; updated Rmd for use with the manual.
