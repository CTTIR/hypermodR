# Interoperability specification

**Status: proposed; awaiting approval.** This document describes intended interfaces and acceptance criteria. The repository does not yet implement them. Existing companion APIs below were inspected at the stated source revisions; a source inspection is not an installed-package integration qualification.

`hypermodR` will own modelling, model comparison and prediction provenance. Companion packages retain acquisition decoding, calibration, annotation and visualisation responsibilities. Conversions preserve their original objects and identity fields; they do not rewrite source recordings or annotation projects.

## Companion contracts

| Package and inspected revision | Existing role and public contract | Planned use |
|---|---|---|
| [`hyperspectR`](https://github.com/CTTIR/hyperspectR/tree/642fc056cd43bee6e41aa4cd3b849a279a698b53), 0.2.0.9000 | S3 `hsi_cube`; `hsi_cube(data, wavelengths, fwhm = NULL, metadata = list(), mask = NULL)`. Data are `[row, column, band]`; wavelengths and FWHM are nm; `TRUE` in the spatial mask means valid. | Explicit conversion to a modelling dataset, and bounded single-frame conversion back for existing preprocessing and visualisation. |
| [`tivis.r`](https://github.com/CTTIR/tivis.r/tree/7f4d443385398629642353a8a1a89b2dda522aa4), 0.1.0 | `tivis_read_cube()` returns a three-dimensional array with a wavelength attribute. `tivis_measurement()` returns an **unclassed list**, not a class named `tivis_measurement`. | An explicitly named adapter validates a declared device/layout profile and records wavelength, geometry, metadata and asset provenance. |
| [`cuvis.r`](https://github.com/CTTIR/cuvis.r/tree/ea199c4866c5ca78a98c743a3faf633a8835f6b1), 0.1.0 | S3 `cuvis_session`, `cuvis_measurement` and `cuvis_processing_context` wrap CUVIS SDK handles. `cuvis_get_measurement()`, `cuvis_get_cube()` and `cuvis_get_metadata()` expose individual measurements. | Recorded Cubert sequences and single measurements through an optional SDK-backed adapter. A serialised dataset retains a reopening recipe, never an external pointer as portable data. |
| [`hyperannotatR`](https://github.com/CTTIR/hyperannotatR/tree/bbfd0724ac09d4eae6d5f27dc8904b5e0a7a8284), 0.2.0 | S3 `annot_image`, `annot_project`, `annot_session`, `annot_roi`, `annot_layer` and `annot_mask`; public tile, metadata, manifest and training handoff APIs. | Lazy image reads, explicit annotation revision binding and reviewed prediction handoff through existing public functions. |
| [`hyperspectaculR`](https://github.com/CTTIR/hyperspectaculR/tree/610632a9c8ae302ffedcf95d60f20e236a46364e), 0.2.0 | Descriptive and artistic visualisation of hyperspectral cubes or numeric arrays. It does not supply acquisition calibration or physiological reference labels. | Bounded compatible frame views for presentation; rendered imagery never substitutes for the original modelling values. |

The inspected packages primarily expose S3 objects. There is no existing common CTTIR S4 hyperspectral class that this package can assume. Optional third-party formal classes require their own documented public adapter and compatibility tests.

## Proposed object and dispatch contract

### Dataset and sequence objects

`HMDataset` is a proposed formal S4 class with validated spatial and spectral metadata, observation tables, annotation bindings, provenance and a lazy owned reader reference. The reader reference identifies an adapter and a reopening configuration; it is not an opaque live SDK handle in saved files. Acquisition, subject, procedure, recording, sequence, frame and annotation identifiers use character columns. A missing identifier remains missing; it is not replaced by a row number and presented as independent biological identity.

`HMSequence` extends `HMDataset` with an explicit frame/timestamp index. It retains frame identifiers, source enumeration indices, capture timestamps, timestamp units/timezone evidence, validity and gap/reference/preview status. Capture time and nominal FPS are different fields. Irregular intervals, missing frames and absent timing remain observable rather than being converted silently to a constant-rate sequence.

Band metadata retain native band index/order, selected position, wavelength in nm, source wavelength/unit, FWHM when known, and conversion provenance. Unit conversion is explicit. Missing or duplicated wavelength metadata can be represented for inspection, but a model requiring a unique calibrated wavelength grid must reject such input until it is resolved.

Spatial metadata identify dimensions, axis order, origin, pixel-edge/pixel-centre convention, y direction, pixel size/unit, pyramid level and the transform to the original acquisition grid. Value semantics distinguish raw counts, reflectance, radiance, absorbance, intensity and unknown. Scaling, offsets, calibration and any resampling are recorded independently.

The class validity functions are the single source of structural validation. S3 adapters and S4 accessors use those same checks. Conversion must not install methods on unrelated foreign classes or depend on another package's private functions.

The companion-free constructors `hm_dataset(values, observations, bands, geometry = NULL, provenance = list())` and `hm_sequence(frames, frame_index, observations, bands, geometry, provenance = list())` are defined in the [architecture](architecture.md). A native observation-by-band spectra matrix has no spatial geometry. Its `hm_spectra(x, selection = list())` accessor returns a bounded matrix and associated identity/band/provenance tables. Spatial block access below applies only to image/sequence data; it must not invent pixel locations for spectra or silently reduce an image to a spectrum.

### Import and conversion interfaces

The proposed S3 generic is:

```r
hm_as_dataset(x, profile = NULL, ...)
```

Methods will be registered for actually classed inputs, including `hsi_cube`, `annot_image`, `annot_project`, `cuvis_session` and `cuvis_measurement`, with further optional adapters admitted separately. The profile declares facts the input cannot establish; it does not turn an assertion into verified device calibration. Unsupported input classes return an actionable error rather than falling through to arbitrary list interpretation.

The explicit adapter for a `tivis_measurement()` result is:

```r
hm_from_tivis(x, profile, ...)
```

It validates the unclassed list's required components and binds them to the declared profile. It must not be implemented as an unrestricted `hm_as_dataset.list()` method that guesses whether any list is a TIVITA recording.

The reverse conversion is an explicit single-frame materialisation:

```r
hm_as_hsi_cube(x, acquisition_id, frame_id, ...)
```

It calls the real `hyperspectR::hsi_cube()` constructor with three-dimensional data, wavelengths, FWHM, metadata and mask. It checks a memory bound before reading. Any band sorting performed by that constructor is recorded in the returned provenance. A multi-frame object cannot be flattened into spatial rows implicitly.

Model specifications, workflows, fitted models and predictions will use S3 wrappers with conventional registered methods such as `print()`, `summary()` and `predict()` where their semantics apply. The formal dataset classes and modelling wrappers remain separate so optional model runtimes do not determine how acquisitions are represented.

### Bounded numerical access

The proposed S4 generic is:

```r
hm_read_block(x, selection = list())
```

`selection` accepts `acquisition_id`, `frame_id`, `rows`, `cols` and `bands`. Acquisition/frame identifiers are character IDs. Spatial and band indices are positive 1-based indices into the declared grid and band table. Omitted selections expand only within the configured read-memory limit; exceeding it gives an error before materialisation. No dimension is dropped.

One call reads **one acquisition**. Multiple selected frames must have compatible spatial grids and selected band definitions. Different acquisitions or incompatible frame geometries require separate block reads; they are never silently stacked, resized or spectrally interpolated. A single-frame dataset retains a singleton time axis.

The result is a named list with these required components:

| Component | Contract |
|---|---|
| `values` | Numeric array with axes named `y`, `x`, `wavelength`, `time`, including singleton axes. The wavelength axis is associated with `band_map`; an unknown wavelength is not invented to label the axis. Values preserve the declared domain and scaling. |
| `valid` | Logical array `[y, x, time]` describing spatial eligibility. It is not an annotation class mask or proof that every selected spectral value is finite. Missing spectral samples remain explicit and model admission handles them under its declared policy. |
| `observations` | Data frame with one row per returned frame, in the returned time-axis order, carrying acquisition/frame/group/time identities and their provenance. |
| `band_map` | Data frame with one row per returned band, mapping selected position to native band index/order, wavelength/unit conversion, FWHM and metadata status. |
| `geometry` | Original and selected grids, transform, window bounds, pyramid level and any explicit padding. No eligible ROI pixel may be lost by a square-window convenience rule. |
| `provenance` | Source content identity, adapter/package revision, profile, calibration/scaling, annotation revision when relevant and read selection. Cache identities bind the complete consumed inputs and model preprocessing contract. |

Model tensor conversions use the appropriate validated spatial-block or native-spectra contract. They record axis permutations, dtype, device, scaling and any lossy channel adaptation. A display RGB conversion is a separate input track and cannot be described as a native hyperspectral input.

## Annotation exchange

The integration uses the existing public functions rather than reproducing annotation internals:

- [`at_dims()` and `at_bands()`](https://github.com/CTTIR/hyperannotatR/blob/bbfd0724ac09d4eae6d5f27dc8904b5e0a7a8284/R/class-image.R) supply width/height and band metadata. Width/height is explicitly reordered when creating `[y,x,...]` numerical arrays.
- [`at_tile()`](https://github.com/CTTIR/hyperannotatR/blob/bbfd0724ac09d4eae6d5f27dc8904b5e0a7a8284/R/backend.R) supplies `[y,x,band]` tiles using 1-based inclusive ranges and a 0-based pyramid level.
- [`at_hsi_meta()`](https://github.com/CTTIR/hyperannotatR/blob/bbfd0724ac09d4eae6d5f27dc8904b5e0a7a8284/R/hsi-meta.R) supplies source/value/layout/calibration/plane semantics; unknown fields remain unknown.
- [`at_interop_manifest()`](https://github.com/CTTIR/hyperannotatR/blob/bbfd0724ac09d4eae6d5f27dc8904b5e0a7a8284/R/interop-manifest.R) and `at_annotation_revision()` bind image content and annotation revisions to the derived modelling dataset.
- [`at_training_export()`, `at_training_check()` and `at_training_import()`](https://github.com/CTTIR/hyperannotatR/blob/bbfd0724ac09d4eae6d5f27dc8904b5e0a7a8284/R/training.R) provide existing dataset checks and staged predictions. The modelling workflow passes biological-unit grouping explicitly; the export default of image grouping is insufficient for repeated images or video from one subject.

Predictions carry their model/checkpoint identity, input grid, label map and annotation revision. Probability arrays, integer class masks, unknown/abstained pixels and background remain distinct. Imports use `stage = TRUE`; model output starts as unreviewed and never replaces reviewed/locked annotations automatically. Stale image content, changed revision or ambiguous geometry rejects handoff. The Shiny workflow exposes these same checks and staged review status.

## Recorded Cubert sequences and TIVITA assets

“Cubert video HSI” means a sequence of hyperspectral measurements with recoverable spectral and temporal identity, not a colour video rendering. Initial support is recorded CUVIS sessions and explicitly indexed portable exports. Live camera acquisition requires its own capability and lifecycle qualification; it must not be inferred from the ability to read a session file.

The existing CUVIS wrappers expose individual measurements. Gap-preserving enumeration and nominal frame rate need an additive companion API and tests before complete sequence support is advertised. The adapter must distinguish science frames, reference captures, missing positions and preview-only records. Reopening a saved sequence must verify source identity and reproduce the same ordered frame index.

TIVITA ingestion is profile-based. Assets include native spectral cubes, acquisition metadata, RGB renderings, colour parameter displays, segmentation products and any independently qualified quantitative exports. These roles are recorded separately. A readable PNG is not sufficient evidence of a quantitative physiological map; a spectral model cannot accept it as a raw cube. Unknown device/software/export combinations remain visible in the capability report and do not inherit the native 100-band Tissue profile automatically.

## Additive companion work required before qualification

These observations concern the inspected revisions. They define planned compatibility work; no companion has been modified by this proposal.

1. **Resolve Cubert reflectance scaling across readers.** [`hyperspectR` divides SDK reflectance by 10,000](https://github.com/CTTIR/hyperspectR/blob/642fc056cd43bee6e41aa4cd3b849a279a698b53/R/io-cubert.R#L84-L87). [`hyperannotatR` copies the SDK cube values and records a reflectance mode](https://github.com/CTTIR/hyperannotatR/blob/bbfd0724ac09d4eae6d5f27dc8904b5e0a7a8284/R/backend-cuvis.R#L62-L104), without an equivalent explicit scaling field in that branch. A common qualified SDK fixture must establish the correct value convention and conversion. Magnitudes alone do not justify automatic inference.
2. **Expose typed session enumeration and FPS in `cuvis.r`.** [`cuvis_session()` stores the no-gaps count](https://github.com/CTTIR/cuvis.r/blob/ea199c4866c5ca78a98c743a3faf633a8835f6b1/R/cuvis-session.R#L25-L31), while [`cuvis_get_measurement()` uses that count for bounds across item types](https://github.com/CTTIR/cuvis.r/blob/ea199c4866c5ca78a98c743a3faf633a8835f6b1/R/cuvis-measurement.R#L13-L26). Native routines already expose item-type sizes and FPS, but the inspected R interface lacks an exported FPS accessor. Add public accessors and gap/reference fixtures in a separately reviewed companion change; do not call private native symbols from hypermodR.
3. **Qualify TIVITA profiles and reconcile layout documentation.** [`tivis_read_cube()` documentation and implementation](https://github.com/CTTIR/tivis.r/blob/7f4d443385398629642353a8a1a89b2dda522aa4/R/tivis-read.R#L7-L79) disagree on which spatial axis varies fastest in storage. Independently coded asymmetric fixtures must establish cross-reader parity before documenting a shared transform. Its wavelength helper currently generates `500 + 5 * (band - 1)` for any band count; this is not universal device calibration.
4. **Preserve timestamp assumptions.** [`tivis_parse_timestamp()`](https://github.com/CTTIR/tivis.r/blob/7f4d443385398629642353a8a1a89b2dda522aa4/R/tivis-measurement.R#L79-L84) assigns UTC to filename timestamps. Record that assumption until the acquisition timezone is established; do not infer clinical timing from a filename alone.
5. **Keep evaluation estimands explicit.** [`hs_grouped_evaluate()`](https://github.com/CTTIR/hyperspectR/blob/642fc056cd43bee6e41aa4cd3b849a279a698b53/R/evaluate.R) provides fixed-parameter SVM/RF evaluation and fold-local preprocessing. Its pooled class recalls and separate group-mean accuracy are not interchangeable with a declared equal-class, equal-animal, equal-acquisition estimand or nested tuning.

## Planned implementation units and acceptance tests

| Proposed files | Required acceptance evidence |
|---|---|
| `R/classes-dataset.R`, `R/classes-sequence.R`; class tests | Formal validity, character IDs, unique native-to-selected band mapping, explicit missing metadata, serialisation/reopen without saved SDK pointers. |
| `R/io-blocks.R`, `R/geometry.R`; block/geometry tests | Non-square asymmetric coded cubes; edge/corner and singleton axes; full-frame versus tiled equality; memory limit before materialisation; every eligible ROI pixel retained; stale content and transforms rejected. |
| `R/interop-hyperspectR.R`; bridge tests | Round-trip data, mask, wavelength, FWHM and metadata; unsorted native bands mapped explicitly; no implicit multi-frame flattening; optional dependency absent. |
| `R/interop-tivis.R`; profile tests | Unclassed-list schema rejection, declared profiles, header/content consistency, independent orientation fixture, unknown device wavelengths, timestamp assumption and asset-role separation. |
| `R/interop-cuvis.R`; recorded-sequence tests | Raw/reflectance/radiance scale parity, gap/reference/preview enumeration, irregular/duplicate/reversed timestamps, source drift, read-after-reopen, supported and unavailable SDK paths. |
| `R/interop-hyperannotatR.R`, `R/prediction-handoff.R`; handoff tests | Width/height versus y/x; 0-based levels versus 1-based indices; mask→ROI→mask parity; label/probability separation; changed revision rejected; staged predictions leave reviewed/locked ROIs unchanged. |
| `R/interop-s4.R`; optional adapter tests | Registered public dispatch, formal validity/coercion, no private namespace calls, no loading unrelated optional runtimes at package attachment. |
| Grouped evaluation and sequence tests | Subject/procedure/recording-connected splits; overlapping temporal windows never cross partitions; all fitted preprocessing/calibration/tuning remains inside training units; missing frames do not become additional independent observations. |
| Installed-package integration and vignettes | Small synthetic workflows through each available companion; model input equality under documented tolerances; offline core use; explicit skips for absent SDK/device fixtures; no support badge inferred from a skip. |

Cross-package tests will record the exact package, source and SDK versions. Syntax checks, mock fixtures, successful installation, device fixtures and scientific evaluation are separate evidence levels. A capability becomes qualified only for the tested profile/backend/version combination.
