Known issues: <https://github.com/fRI-Research/LandWeb_preamble/issues>

# LandWeb_preamble (development version)

## 1.0.3

* **Dropped the SCANFI fallback for current-condition stand age**, for two independent reasons.
    - *Circularity.* `Biomass_borealDataPrep` already derives its default `standAgeMap` from a "SCANFI-derived data product for 2020" via `LandR::prepInputsStandAgeMap()` with NTEMS fire/harvest adjustments. A SCANFI-based current condition would restate the model's own age assumption rather than observe against it — and current-condition-vs-simulated-envelope is precisely the comparison NRV exists to make.
    - *It is wrong outside Alberta.* Tested against the fire regime itself (equilibrium Poisson, `P(age >= 120) = exp(-120 / FRI)`, from the LTHFC v10 layer and so independent of any age product): SBFI tracks the expectation (Lake of the Woods 19.7 vs 18.0; Big Trout Lake 22.9 vs 20.2; Slave River 25.6 vs 20.2) while SCANFI is 14-100x too low (Lac Seul 1.3 vs 18.0; Slave River 0.2 vs 20.2). Both SCANFI variants (`_age_median_v2`, `_att_age_S_v1_1`) agree with each other, so this is a product-level bias, not a median-vs-mean artefact. In WesternAlbertaUpland both sit *below* expectation (14.0, 17.7 vs 22.9), which is what harvest in a managed landscape should do.
* New `ccAgeMaxMissing` parameter (default 25%). The preamble now **stops** rather than initialising a landscape whose age is mostly unknown. AB/BC groups sit at 1.5-7.5% missing and proceed; the seven groups whose only age source is CanLAD sit at 62-96% and fail with a message naming the cause and the fix. Raising the threshold does not fix the data — it initialises a landscape with almost no old forest.

## 1.0.2

* **Current-condition stand age now comes from fRI Research's `age_in2025` composite** rather than the SCANFI median age. New parameters `ccAgeDriveId` (the delivered raster) and `sbfiAgeDriveId` (the SBFI fill, `NA` until fRI deliver it).
* The composite is built with `terra::cover()` — a **fill**, not a minimum. `age_in2025` stays authoritative wherever it has a value and the fills supply only what it left empty. Using `min()` as the upstream ArcGIS composite does would let a *modelled* age drag the AB/BC *inventory* ages younger, and since `min()` is one-directional that is a systematic young bias into exactly the areas where the data is best. Verified: every pixel that already had a value is preserved (66,252,321/66,252,321 in WesternAlbertaUpland; 13,894,595/13,894,595 in Lac Seul Upland).
* **The wiring target is `standAgeMap`, not `CC_TSF`.** In the v3 `targets` pipeline dataPrep consumes `standAgeMap`; `CC_TSF` is exported but read only by the legacy v2 `00-main.R`, which passes it *as* `standAgeMap`. Wiring `CC_TSF` alone would have had no effect on the simulation. Both now derive from the same composite.
* `NAflag()` is set to 65535 explicitly and asserted: the GeoTIFF declares that NoData value but `terra` does not pick the tag up, so left unset 65535 reads as a stand age of 65,535 years and silently poisons every downstream aggregate.
* Fills carry `+ 5` because SBFI and SCANFI are age-at-2020 against a 2025 composite; omitting it would put a five-year step discontinuity along the AB/BC boundary.
* Reprojection to the biomass-parameter grid uses `method = "average"` rather than `"bilinear"`: going 30 m to 240 m, `average` aggregates the ~8x8 block each coarse cell covers, whereas `bilinear` samples four neighbours and discards the rest.
* **Known limitation.** `age_in2025` has inventory input only in AB and BC; elsewhere CanLAD is its sole source and cannot date an undisturbed stand, so outside AB/BC no pixel exceeds 40 years and 78-100% carries no age. The SCANFI fallback closes the coverage gap but not the old-forest one — in Lac Seul Upland SCANFI's age has median 80, p99 120, max 165 and **zero** pixels over 150, where the zone's fire-return interval implies roughly 20%. Seven of the eighteen study-area groups are affected. This must be re-checked against the SBFI raster when it arrives; do not assume SBFI resolves it.

* Ported the preamble to terra/targets for the Phase-0 pipeline; Phase-0 now runs end-to-end for the Spray Lake study area. Moved `amc::outerBuffer` to `spatialutils::outerBuffer` across all study-area (`R/*`) files; added a SCANFI LCC + FAO windowed self-crop (skips LandR's whole-Canada FAO reproject); fetch SCANFI age via `workflowtools::drive_download_once` (Google Drive service-account auth) plus a windowed crop; derive flammability from LCC; fix `fireReturn` via `vect()`; and emit `rstLCC` + `standAgeMap` under the names the pipeline/Biomass modules expect.
* Removed all `browser()` debugging calls.
* Species relabelling: merged western redcedar (`THUJ_PLI`) and western hemlock (`TSUG_HET`) into `Abie_spp` (both are absent from the Silvacom CurrentConditions species groups and look like SCANFI over-attribution in AB, e.g. `Tsug_het` covering ~25% of the Spray Lake FMA, well outside its real range).
* `InitSpecies`: dropped the vestigial `PINU_CON` handling (SCANFI v2 splits Pinus contorta into `PINU_CON_CON` and `PINU_CON_LAT`, both already mapped to `Pinu_spp`) and labelled the contorta varieties.
* Renamed the flammable-map object `rstFlammable` to `flammableMap` to align with scfm/fireSense and downstream burnSummaries/NRV_summary.
* **Current-conditions land cover now comes from the 2020 Land Cover of Canada (NALCMS/CCRS, 30 m), not SCANFI.** Using SCANFI would be circular --- it is already the LCC that drives the simulation. Two layers are derived: `LandTypeCC` feeds `overlayLCCs()` with urban sent to `99` and reclassified to the nearest type (approximating the pre-industrial landscape the model simulates), while the new `LandTypeCC_reporting` retains urban as the reference for reporting where the landscape sits today. `remapDT` keeps native LCC 2020 codes and preserves v2's rule ordering, with urban added --- v2's current-conditions layer had no urban class at all. As in v2, CC does not override LCC's forest determination.
* `30` added to `classesToReplace`: that list also determines which classes may *donate* a replacement value, and SCANFI conflates urban into its `30` (rock/barren) code, so leaving it available let urban be reclassified straight back to another urban pixel's code. Genuine barren (where both products agree) is already sent to `NA` by `remapDT` and is unaffected. On WesternAlbertaUpland this moved urban outcomes from 37% rock/barren to 0%, with forested outcomes rising from 31.1% to 47.7% and no pixels left `NA`.
* Guarded the LCC 2020 download: `reproducible::preProcess()` errors when an http-sourced target file already exists rather than skipping the way the Drive path does, which would otherwise break every run after the first.

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
