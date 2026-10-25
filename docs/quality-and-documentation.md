# Quality and documentation specification

**Status: implementation in development.** This document remains the architectural and acceptance contract. Core R APIs, data bridges, runtime lifecycle code, model contracts, executable articles and a Shiny workbench now exist. Implemented interfaces do not establish full acceptance: instrument/vendor parity, named upstream model qualification, live platform/model support, complete browser/accessibility checks and scientific validation require their own evidence. See the executable articles and current API reference for callable behavior.

## Source of the requirements

The organization contribution policy is the authority for shared conventions. Reference packages illustrate implementation patterns; their current code does not override that policy.

| Source | Revision reviewed | Relevant requirements |
|---|---|---|
| [CTTIR contribution policy](https://github.com/CTTIR/.github/blob/bd4664ca7168f9c5f86c521bf81293e2db006eec/CONTRIBUTING.md) | `bd4664ca7168f9c5f86c521bf81293e2db006eec` | Naming, statistics vocabulary, identifiers, units, conditions, measurable Shiny code and reusable workflows |
| [CTTIR security policy](https://github.com/CTTIR/.github/blob/bd4664ca7168f9c5f86c521bf81293e2db006eec/SECURITY.md) | `bd4664ca7168f9c5f86c521bf81293e2db006eec` | Identifiable data, raw media and deployment boundaries |
| [qviewparsR](https://github.com/CTTIR/qviewparsR/tree/a5386a26db17fb1efaa4fde9f4ec422007625376) | `a5386a26db17fb1efaa4fde9f4ec422007625376` | Exact-value fixtures, vendor-output comparisons, API documentation and Shiny tests |
| [themakR](https://github.com/CTTIR/themakR/tree/af490b90d483bcde8b22b348cd5a539621ade8f7) | `af490b90d483bcde8b22b348cd5a539621ade8f7` | Shared pkgdown presentation and adoption contract |

These revisions identify the source reviewed for this proposal. They are not a claim that remote branches cannot change or that a future build should silently accept changed dependencies.

## CTTIR conventions

- Exported package functions use the `hm_` prefix. Standard S3/S4 methods retain the appropriate generic names and registration conventions.
- Statistical tables use `estimate`, `std.error`, `conf.low`, `conf.high`, `p.value` and, when applicable, `p.adj`. A method that does not produce a quantity leaves it unavailable instead of inventing a value.
- Identifiers such as `subject_id`, `sample_id`, `image_id` and `frame_id` are character columns in the appropriate linked tables. Row names are not identifiers. Array indices and identifiers are separate concepts.
- The spectral coordinate is `wavelength`, expressed in nm under the documented vocabulary. Coordinate units, time units, radiometric units and reference frames are explicit. The spectral grid is validated rather than inferred from a camera brand or channel count.
- User-facing failures use classed `cli::cli_abort()` conditions. Missing optional runtimes, invalid scientific inputs, corrupt artifacts and unsupported model/data combinations have distinct condition classes.
- All executable Shiny R logic lives in `R/`. `inst/app/www/` contains static assets only.
- Optional backends and the application must not prevent installation or use of the supported core R functionality.
- Public examples use synthetic or explicitly redistributable fixtures. Raw research media, credentials and identifiable metadata do not enter the repository or source package.

The exact S4 data classes proposed are `HMDataset` and `HMSequence`. `hm_as_dataset()` is the source bridge; modelling uses `hm_model_spec()` and `hm_workflow()`. Fitted objects use the S3 class `hm_fit`, and predictions use `hm_prediction`. The [architecture specification](architecture.md) owns their detailed contracts. Documentation and app modules must use those same names and semantics.

## Acceptance matrix

The following table distinguishes existing CTTIR requirements from stronger requirements proposed specifically for hypermodR.

| Area | Basis | Proposed acceptance evidence |
|---|---|---|
| Package checks | CTTIR reusable workflow | Source archive passes `R CMD check --as-cran` with zero errors and warnings. Every NOTE is assessed and recorded; no unexplained NOTE is accepted. |
| Supported R/platform matrix | CTTIR workflow plus explicit compatibility claim | Linux release, devel and oldrel-1; Windows release; macOS release. Test the actual declared minimum R version separately. An informational oldrel-3 job does not prove that minimum works. |
| Test coverage | CTTIR workflow supplies an absolute gate | Proposed floor: 95% assertion-bearing R coverage using tests only. Report per-file tails. All named integrity invariants must have direct assertions even when the numerical coverage threshold passes. |
| Lint | CTTIR house rules, strengthened for a new package | Zero findings with blocking house and default style checks from the first implemented release. |
| Numerical correctness | CTTIR reference pattern | Independent expected values, vendor/export comparisons where legally available, explicit tolerances and asymmetric fixtures that reveal axis or mask mistakes. |
| Object interchange | Package-specific requirement | S3/S4 dispatch and round trips preserve identity, units, wavelength grid, geometry, calibration status, temporal order and provenance. Lossy conversions are explicit. |
| Statistical validity | Package-specific requirement | Grouped and temporal leakage tests; train-only preprocessing and selection; independently reconstructed metrics and paired unit-level uncertainty. |
| Optional backend qualification | Package-specific requirement | Exact environment/model versions, actual model smoke tests, resource checks and numerical parity for each claimed platform. A skipped test does not establish support. |
| Shiny behavior | CTTIR measurable-code requirement plus reference tests | UI construction and server tests in ordinary package coverage; separate browser checks for complete user workflows, downloads, recovery and session isolation. |
| Accessibility | Proposed strengthening | WCAG 2.2 AA as the design target, with automated checks and manual keyboard, focus, contrast and screen-reader spot checks on actual rendered pages. |
| Documentation | CTTIR reference and theme | Offline core examples execute; reference entries cover every export; rendered navigation, assets and links work in both colour modes and narrow layouts. |
| Distribution | Package-specific requirement | Git and source-tarball inspections include runtime installer/lifecycle code, model registries, platform profiles/locks and package-owned workers. Only development orchestration and downloaded/private artifacts are excluded. Clean-library installed tests verify namespace functions and `system.file()` resource hashes without the checkout. |

Coverage is a defect-detection aid, not a scientific qualification score. Core tests, backend tests, app tests and scientific benchmarks may overlap; their counts are not added to imply independent evidence.

## Verification layers

### Core and interchange

The core suite must exercise empty and malformed inputs, duplicate IDs, missing labels, nonfinite values, unsupported units, irregular wavelength grids, reversed axes, complete ROI retention, incorrect calibration declarations and conflicting provenance. Geometry fixtures include non-square images, edge-touching masks and explicit transformations. Video fixtures include unequal frame intervals, missing or duplicated timestamps, interrupted recordings and repeated acquisitions of the same biological unit.

Conformance fixtures for each sibling-package bridge check actual public classes and APIs. Tests must reject stale annotation revisions and mismatched image identities. Compatibility documentation identifies tested sibling revisions; a common class name alone does not establish interoperability.

### Models and evaluation

Every model adapter needs a task/input contract, native versus adapted input designation, preprocessing definition, output schema, fit/predict availability and reference implementation or independent numerical oracle. Optional weight files require source, version, license and content identity. Unsupported tasks fail before expensive work starts.

Evaluation tests use known synthetic predictions to check classification, regression, segmentation, spectral reconstruction and temporal metrics. Hyperparameter selection, preprocessing, feature selection, early stopping and calibration operate on training/validation units only. Temporal windows from one recording and repeated views from one subject cannot leak across folds. Paired comparisons retain identical support and disclose conditional uncertainty and invalid resampling draws.

A model's presence in a registry, a successfully installed runtime and a technically valid prediction are separate from useful scientific performance. Documentation must keep these states separate.

### Backends and platform support

Core checks run without a GPU, vendor SDK, network connection or model download. Separate opt-in suites verify the supported Python, neural-network, SDK and serving backends. Their receipts identify actual system capabilities, versions, model hashes, input semantics, outputs, skips and failures.

Backend installation is an explicit package operation with preflight and a reviewable installation specification. Loading the package, opening the app, rendering documentation and running ordinary checks must not install runtimes or download weights. Existing services and environments are not overwritten implicitly. The backend specification determines which architectures can use vLLM; vLLM is not assumed to execute every neural architecture.

### Shiny and browser checks

The [workbench specification](shiny-workbench.md) defines the application state and acceptance scenarios. Tests include empty states, guarded input transitions, asynchronous completion, cancellation, failure recovery, resource errors, stale results, two simultaneous sessions and exported artifacts read back through the package API.

Browser tests are a separate qualification lane. They may be excluded from routine package checks where browser dependencies are unavailable, but an advertised app workflow cannot be marked browser-tested when its lane was skipped.

## Documentation design

The [vignette catalogue](../vignettes/README.md) defines the proposed articles. README and vignettes distinguish implemented, tested and planned features. Executable synthetic examples now accompany the development implementation; no installation command claims a released package exists.

The README acceptance criteria are:

1. Intended audience, task scope and maturity.
2. A compact capability matrix with exact platform/backend qualification status.
3. Core installation and explicit optional-backend installation instructions.
4. A complete synthetic CPU example with saved object, grouped model fit, prediction and independent evaluation.
5. Sibling-package interchange and camera/sequence pathways.
6. The app launcher and a reproducible app-to-script workflow.
7. Scientific limitations, data/model licensing boundaries, contribution and citation links.

A source README is rendered into the published README only after its executable core example passes. Heavy examples must provide a bounded opt-in reproduction path and provenance-bound expected outputs. An entire vignette must not be globally unevaluated merely to conceal broken examples.

## Website and app presentation

The development website uses `Config/Needs/website: CTTIR/themakR` and `template.package: themakR` in `_pkgdown.yml`, following the [theme adoption guide](https://github.com/CTTIR/themakR/blob/af490b90d483bcde8b22b348cd5a539621ade8f7/vignettes/themakR.Rmd). Keep inherited navigation and the light/dark/auto switch. Verify the Reference, Articles, Changelog, search, citation and license views after rendering.

The theme requires real source metadata for sidebar sections. A logo, DOI, CRAN badge or successful workflow badge is added only when the corresponding artifact or service exists. Bibliographic metadata must identify the correct package or third-party work.

themakR is a pkgdown template. The Shiny app will use `bslib` with a compatible palette and local/system font fallbacks; no unverified themakR Shiny API is assumed. Figures and maps must remain understandable in light and dark modes, with axes, units, legends and alternatives to colour-only encoding.

## Proposed repository files

Public package files will include `DESCRIPTION`, `NAMESPACE`, `R/`, `man/`, `tests/testthat/`, the vignette catalogue, `_pkgdown.yml`, `README.Rmd`, `README.md`, `NEWS.md`, `inst/CITATION`, `LICENSE`, `LICENSE.md` and thin workflow callers. The declared license is MIT for package-owned code; external model weights, SDKs and redistributed data keep their own terms.

The three organization workflows must be called as reusable workflows rather than copied. Browser and backend qualification lanes supplement them. Exact workflow and dependency revisions are recorded for reproducible release checks.

Public product specifications describe behavior and acceptance criteria. Only package-development plans, task ledgers, reviews and local build/check evidence are maintained in the ignored orchestration directory and excluded from R source builds. End-user vLLM/DNN/transformer installation and execution automation is public, tracked, packaged product code. Its R functions and installed registry/profile/worker resources must operate without `orchestration/`, `docs/` or a development checkout. Runtime receipts and downloaded artifacts belong in an external user cache. Release evidence must verify both inclusion of required runtime assets and exclusion of development/private material; `.gitignore` alone does not control a source archive.

This specification does not authorize CRAN submission, model publication or application deployment. Those actions remain separate from implementation and technical verification.
