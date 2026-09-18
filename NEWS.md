Known issues: <https://github.com/fRI-Research/LandWeb_preamble/issues>

# LandWeb_preamble (development version)

* **Replaced the never-run test stub with metadata tests, and added testthat CI.** `tests/testthat/test-template.R` was the SpaDES boilerplate, unedited: paths from another machine, calls to `Event1`/`Event2` functions this module does not define, and assertions against placeholder strings. It had never been run and would have failed instantly, while making the module look tested. In its place, characterization tests over the module's public contract -- the input and output object names and classes, and the parameter names -- which is what a project binds to and what nothing checked until now. The expectations are GENERATED from the module's live metadata rather than transcribed, and were verified to fail when the contract changes.
* The contract test records that this module declares **no input objects** -- it is the data-preparation stage and reads from disk and the network rather than the simList -- and pins all 22 outputs. Several are still `RasterLayer` and `sf` rather than `SpatRaster`/`SpatVector`, so the terra migration is incomplete here; the test makes finishing it a deliberate, visible change.
* **Added the `render-module-rmd` workflow**, so `LandWeb_preamble.Rmd` is built on every change to it or to `LandWeb_preamble.R`. This module had NO CI of any kind. Its three evaluated chunks are `moduleParams`/`moduleInputs`/`moduleOutputs` tables, so the render needs no data and no credentials. The commit step stages the rendered `.html` explicitly, because `git commit <path>` fails on an untracked file and this module has never had one committed.

## 1.0.8

* **The species equivalency table and the rate-of-spread table now come from `LandWebUtils`** (>= 1.0.3.9036): `InitSpecies()` calls `landweb_sppEquiv()` and `InitLandMine()` calls `landmine_ros_table()`. On the real `LandR::sppEquivalencies_CA` the species table is identical to 1.0.7's in all 31 columns apart from the `Abie_spp` label below, and the rate-of-spread table is identical to the one written out here and in `LandMine`. The SCANFI-to-LandWeb species merges now have one definition, `landweb_species_map()`.
* **`Abie_spp` is now labelled "Fir"** in `sppEquiv`'s `EN_generic_short`, `EN_generic_full` and `Leading` columns. It had no group label, so `Biomass_core`'s leading-vegetation maps, which take a group's label from its first row, called the whole group -- subalpine fir, western redcedar and western hemlock included -- "Balsam fir". Species codes, the species merged and all simulation inputs are unchanged.
* `InitSpecies()` no longer modifies the lazy-loaded `LandR::sppEquivalencies_CA` by reference; the package function works on a copy.
* Removed `InitLandMine()`'s `ROStype == "equal"` and `"log"` branches, which were unreachable after its own `stopifnot(ROStype %in% c("default", "burny"))`, and corrected `ROStype`'s description to list only those two values.
* Removed the exploratory `if (FALSE)` block in `InitSpecies()` that listed SCANFI species by study area; its one finding, that `POPU_GRA` is unreliable, is documented on `landweb_species_map()`.

## 1.0.7

* **The current-condition age composite, the missing-age check and the LCC 2020 remap now call `LandWebUtils`** (>= 1.0.3.9035) -- `cc_age_composite()`, `cc_age_pct_missing()`, `lcc2020_remap_table()` and `lcc2020_classes()` -- instead of running inline, so each is covered by package tests and CI rather than exercised only by a full preamble run. Proven behaviour-neutral on real data: on LacSeulUpland (130,360,104 cells) the composite is cell-for-cell identical to 1.0.6 (0 NA-pattern differences, max difference 0) and the missing-age figure matches exactly (0.8220%).
* Two latent fragilities of the inline code do not carry over. Its `stopifnot(identical(NAflag(x), 65535))` fails for an in-memory raster even when the sentinel was converted (terra reports `NaN`); the package clears sentinels with `classify()` instead. And its forest mask, `ifel(lcc2020 %in% treeClassesCC, ...)`, only worked because SpaDES attaches `terra`: `%in%` is a plain base closure, so terra's method is found only on the search path.
* The LCC 2020 class groups (`treeClassesCC`, `nonFlammClassesCC`) now come from `LandWebUtils::lcc2020_classes()`, so the remap, flammability map and age check share one definition.
* **Removed the `R/` directory**: 21 per-FMA/province study-area helpers plus `utils.R`, about 1,900 lines of LandWeb v2 code. Nothing called them -- they were reachable only through `allLandWeb()`, which nothing called -- and `utils.R` redefined `extractFMA()`, `extractFMU()` and `joinReportingPolygons()`, which `LandWebUtils` exports in diverged form. Study areas come from `LandWebUtils::prepStudyArea()`, and landbase processing from `LandWebUtils::buildLandbasePolygons()`.

## 1.0.6

* **`ccAgeMaxMissing` now measures the share of FOREST with no age, not the share of the study area.** Stand age is only defined for forest, and NTEMS maps only treed pixels, so after the fill the polygon-wide figure mostly measured lakes: ChurchillRiverUpland still stopped at 39.4% missing, and 76% of what remained was water/barren/ice (47%) or wetland (29%); LacSeulUpland's remainder was 89% water. Over forest (percent of forest with no age):

    | group | forest share | unfilled | filled |
    |---|---:|---:|---:|
    | WesternAlbertaUpland | 74.9 | 3.0 | 0.8 |
    | LacSeulUpland | 69.2 | 74.1 | 0.8 |
    | ChurchillRiverUpland | 52.9 | 74.6 | 12.8 |

    CanLAD-only groups still read ~74% and stop, so the check still catches the landscape it exists for, and every filled group passes the unchanged 25% limit.
* Forest is **LCC 2020** classes 1/2/5/6 (`treeClassesCC`), independent of both age sources and deliberately *not* SCANFI, which would bring it back into the current-condition path. This adds one 30 m reprojection of LCC 2020 per study area.
* **ChurchillRiverUpland's 12.8% is a real gap, not a denominator artefact**: roughly 8M 30 m cells that LCC 2020 calls forest, NTEMS calls non-treed, and CanLAD never saw disturbed, against 0.8% in the other groups. The same group's NTEMS Old share is half its fire-return-interval expectation (1.0.4); a shared cause is plausible but untested. Flag the group when reporting it.
* The `ccAgeMaxMissing` description and its calibration figures are updated to the forest denominator; the old "1.5-7.5% / 62-96%" figures were polygon-wide.

## 1.0.5

* **Fixed `ccAgeMaxMissing` measuring the bounding box instead of the data.** The check computed `mean(is.na(.))` over the whole cropped raster, but `crop(..., mask = TRUE)` sets every *outside*-polygon cell to `NA`, indistinguishable from an inside cell that genuinely lacks an age. Study-area groups are irregular and fill only 30-44% of their bounding boxes, so the figure was inflated roughly tenfold: WesternAlbertaUpland measured **71.4% missing against a true 3.9%**, and would have aborted its own validated test area on the default 25% limit. It never fired only because the check landed in 1.0.3 and nothing had been re-run since (the last preamble output anywhere predates it by almost four weeks).
* The check now rasterizes the study-area polygon and counts with `terra::global()` over in-polygon cells, so terra works in its own chunks and no 30 m vector is materialised in R. `rasterToMatch_biomassParam` could not be reused as the denominator: it is the *unmasked* `LCClarge`; only `sim$rasterToMatch` is masked to the study area. A zero-cell study area now stops explicitly instead of yielding `NaN` and silently skipping the check.
* **The check deliberately stays at 30 m, before the projection.** Moving it onto the 240 m grid was tried and rejected: `average` fills a coarse cell from any one of its ~64 children, so a cell reads as missing only when *all* of them are, which loosened the threshold 3-20x (WAU 0.2% vs 3.9%; LacSeulUpland 7.1% vs 19.5%). The 25% limit is calibrated on 30 m completeness, and the coarse version risks passing exactly the CanLAD-only landscape the check exists to stop.
* The "AB/BC groups sit at 1.5-7.5% missing" figure quoted in 1.0.3 was measured correctly all along; it was the module's own computation that disagreed with it.

## 1.0.4

* **The current-condition age fill outside AB/BC is now per-pixel NTEMS forest age**, replacing the never-delivered SBFI age raster. `sbfiAgeDriveId` is retired; new parameters are `ntemsAgeFile` (resolved under `inputPath()`, default `CA_forest_age_2022/CA_forest_age_2022.tif`) and `ntemsAgeYear` (default 2022), the latter setting the forward ageing to the 2025 epoch.
* **Why NTEMS rather than a rasterised SBFI.** SBFI's own stand age *is* NTEMS age, summarised to polygons by the same published method (Maltman et al. 2023), so going to the source both avoids asking fRI Research for a derived product and avoids the polygon-collapse loss that rasterising SBFI's `AGE_MEDIAN` would have caused. Pairing it with fRI's SBFI-derived species percent outside AB/BC is therefore internally consistent, not a mix of methodologies: the 2019 and 2022 NTEMS vintages differ only in the disturbance window (1985-2019 vs 1985-2022), documented identically.
* **The source switch at the AB/BC boundary is a real discontinuity, and is accepted deliberately.** Where both sources exist they disagree — Spearman rho 0.20-0.35 per pixel, NTEMS median 65-75 against `age_in2025`'s 95, and NTEMS reports roughly half the Old-seral area. Judged against the fire regime itself (equilibrium Poisson `P(age >= 120) = exp(-120 / FRI)`, area-weighted over LTHFC v10 and independent of every age product), it is **NTEMS that is better calibrated**: across the three AB/BC groups it sits at 0.74-0.91 of expectation over FRIs from 49 to 82 — consistently just below, as harvest in a managed landscape should put it — while `age_in2025` sits 1.44-2.20x *above* expectation, which harvest cannot explain. `age_in2025` is nonetheless kept authoritative wherever it has a value, because switching source *within* a study area would be worse than a bias at its edge.
* A single bias correction fitted in the overlap was considered and rejected: the three adjacent AB/BC groups imply corrections of -4.5, -10.3 and -19.6 years, a 4x spread that is direct evidence such a correction would not transfer to Saskatchewan.
* **Known limitation: outside AB/BC, old-seral area is modelled rather than observed.** NTEMS derives age three ways and *all* of its Old signal comes from the third: broken down by the companion `_approach` raster, the disturbance and recovery approaches contribute 0.0% Old in every group tested, the allometric approach 6.8-33.0%. That is structural — disturbance detection reaches only 1985 (~40 yr) and recovery only 1965 (~60 yr), so neither can ever produce a 120-year stand — and allometric covers 77-93% of treed pixels (78.3% nationally). No independent alternative exists (SBFI's old forest is this same model summarised; SCANFI's is 14-100x too low), so this is a limitation to state, not a reason to prefer another source. It matters most where current condition is set against the simulated NRV envelope.
* Corroboration that SBFI's age *is* this product: in LacSeulUpland the SBFI figure is 28.1% Old and NTEMS gives 28.0%.
* ChurchillRiverUpland does not fit the pattern (9.6% Old vs 19.4% expected, 0.49x; its allometric pixels give 12.4% against 20-33% elsewhere). Whether its detected-disturbance rate accounts for this was checked and does not generalise — disturbance-share/expectation runs 0.13-0.55 across groups with no correspondence to the old-share ratios. Left unexplained rather than rationalised; flag the group when reporting it.
* NTEMS `255` (non-treed) is flagged `NA` and asserted, for the same reason `age_in2025`'s `65535` is: left unset it enters the composite as a 255-year stand and registers as Old across every non-treed pixel the fill touches. `151` (meaning ">150") is kept as-is — it is already past the 120-year Old cutoff, so the cap cannot change a seral-stage assignment.
* Unlike fRI's rasters, NTEMS is **not** co-registered with `age_in2025` — it is on its own Lambert variant (NAD83 LCC, standard parallels 49/77, central meridian -95) — so the fill is a true reprojection rather than a crop. The existing crop-before-project ordering already bounds the cost.

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
