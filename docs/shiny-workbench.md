# Shiny workbench specification

**Status: proposed for approval.** `hm_app()` and the application modules described below are planned interfaces. No application, backend installation or modelling workflow is implemented by this document.

The workbench will expose the same validated data and modelling operations as the R API. It must not maintain a second numerical implementation. A completed app workflow must export enough configuration and provenance to reproduce its package-API counterpart.

## Intended workflow

The primary sequence is **Import → Quality and geometry → Regions and task → Model and runtime → Run → Compare → Export**. Each page explains the information needed for its next action. Advanced provenance and resource details remain available without dominating the ordinary modelling flow.

The user can work with a validated `HMDataset` or `HMSequence`, including sources admitted through `hm_as_dataset()`. A model is represented by `hm_model_spec()` and combined with the selected task and processing rules through `hm_workflow()`. Completed fits are S3 `hm_fit` objects; predictions are S3 `hm_prediction` objects. The [architecture](architecture.md) defines their fields and validation rules.

## Proposed module boundaries

All executable R modules live in `R/`; static assets belong in `inst/app/www/`. Module names and files below describe ownership, not currently callable APIs.

| File | Responsibility | Inputs and outputs | Required acceptance behavior |
|---|---|---|---|
| `R/app.R` | `hm_app()` launcher | Validated launch options → running local app | Missing optional dependencies reported; loopback default; temporary option changes restored on exit |
| `R/app-ui.R` | Page composition and navigation | Current state and capability summary → accessible UI | Light/dark modes, narrow layout, keyboard navigation and meaningful empty states |
| `R/app-server.R` | Connect modules to the public API | Session actions → versioned state transitions | No duplicate estimator logic; no automatic fitting on import or page navigation |
| `R/app-state.R` | State and result identity | Dataset/task/model/runtime revisions → current or stale outputs | Incompatible revisions cannot reuse results; retained completed artifacts remain available |
| `R/app-mod-import.R` | Source selection and bounded import | Supported reader source → `HMDataset` or `HMSequence` | Reader/version support displayed; corrupt, oversized and unsupported inputs fail with useful conditions |
| `R/app-mod-qc.R` | Calibration, spectral and geometry review | Dataset plus QC declaration → validated data view | Units, axes, invalid bands, saturation and transformations visible; no silent calibration or resampling |
| `R/app-mod-roi.R` | Region and annotation interchange | Image-bound annotations → selected validated regions | Image identity/revision checks; complete ROI support; clear annotation ownership and coordinate transforms |
| `R/app-mod-task.R` | Endpoint and independent-unit definition | Labels/grouping/time rules → validated task configuration | Subject/recording grouping required where applicable; missing references remain unresolved |
| `R/app-mod-model.R` | Model selection and explanation | Task/data capabilities → `hm_model_spec()` | Only compatible modes offered; scientific assumptions, input adaptations and qualification status shown |
| `R/app-mod-runtime.R` | Runtime discovery and explicit installation | Capability preflight and approved installation specification → runtime status | No downloads on launch; supported architectures/platforms shown; credentials never echoed |
| `R/app-mod-jobs.R` | Queue, progress, cancellation and recovery | `hm_workflow()` plus resource limits → run state | Background work remains cancellable; incomplete work cannot appear completed; repeated submission cannot overwrite |
| `R/app-mod-results.R` | Predictions, uncertainty and comparisons | `hm_fit`, `hm_prediction`, evaluation tables → interpretable displays | Correct units/support; development and confirmation roles explicit; failed/absent results distinguished from zero |
| `R/app-mod-export.R` | Reproducible exports | Completed selected artifacts → verified download bundle | Exact output identity, configuration, code and provenance; no hidden credentials or temporary paths |

The annotation page integrates the existing HSI annotation owner through its verified public interchange contract. It does not create a second incompatible annotation format or repurpose microscopy annotation workflows.

## State contract

The session separates dataset, annotation, task, preprocessing, model, runtime and run revisions. A result is current only when the complete recorded input/configuration identity matches the selected workflow.

| State | Entry condition | Permitted next actions | Required UI behavior |
|---|---|---|---|
| Empty | No admitted source | Select a source or open a synthetic example | Explain supported inputs; disable fitting and result exports |
| Source selected | Reader/source chosen, not imported | Inspect preflight, import or cancel | Show expected data kind and bounded size/resource estimate |
| Importing | Explicit import requested | Cancel; view progress | No model work or second import into the same mutable state |
| Imported | Source parsed and identity retained | Review QC, geometry and annotations | Display raw/calibrated/derived-image status without inferring equivalence |
| Configuring | Data is available; task/model/runtime choices incomplete | Resolve missing requirements; inspect capabilities | Explain each unavailable action with its concrete missing prerequisite |
| Ready | All technical requirements for the chosen action pass | Queue an explicit operation | Show task, grouping, model/input track, runtime, outputs and limits before execution |
| Queued | Immutable run specification reserved | Cancel before execution | Prevent duplicate submission and display queue status |
| Running | Resource admission and input recheck passed | Cancel; inspect bounded progress | Maintain responsive UI and session isolation |
| Completed | Output coverage, integrity and status finalized | Compare or export; create a new run | Show exact scope, independent units, failures and qualified input/runtime versions |
| Failed | Operation failed validation or execution | Inspect error; create an explicitly corrected retry | Preserve failure evidence; never display partial work as a completed result |
| Cancelled | Cancellation acknowledged | Inspect retained partial status; create a new run | Distinguish requested cancellation from confirmed termination |

Changing data, geometry, labels, grouping, preprocessing, model weights or runtime identity marks incompatible selected results stale. It does not silently recompute them, delete their retained artifacts or relabel them with the new settings. Changing presentation-only choices must not refit a model.

Scientific admission is separate from technical readiness. A structurally complete label table cannot prove that a clinical reference is credible. The app must retain declared reference limitations and avoid presenting an unavailable clinical endpoint as a validated task.

## Import, visualization and sequences

Large images and sequences use bounded previews and chunked access. A displayed RGB rendering or parameter map is labelled as that representation; it is not presented as the complete spectral cube. Wavelength selection and spectral resampling are explicit operations with visible units and recorded transformations.

Sequence views display frame identity, timestamps, elapsed time, missing frames and the source acquisition. Frame rate is not assumed constant. Spatial registration, radiometric calibration and temporal aggregation remain separate operations. Train/validation/test grouping must keep related frames and recordings within the declared independent unit.

Geometry displays preserve image origin and orientation. Overlay and cursor readback tests use asymmetric fixtures to reveal axis swaps, flips and off-by-one errors. Padded regions are not treated as valid tissue, and masks cannot silently lose eligible pixels.

## Models, installation and resources

The model catalogue distinguishes interpretable statistical/physical models, conventional machine learning, neural models and serving-compatible language/vision models. Each entry shows supported tasks, expected dimensions, wavelength requirements, calibration assumptions, native or adapted inputs, weights/license, and its actual package qualification status.

Runtime status distinguishes unavailable, installation proposed, installing, installed but unqualified, smoke-tested and failed. A user-facing install operation may automate supported steps after an explicit choice, but package load, app launch and data upload never initiate installation. The UI must not imply that every transformer or DNN can run through vLLM.

Preflight reports estimated storage, memory and accelerator requirements before work is queued. Work is constrained by the configured resource budget, and failure to meet it leaves the operation unstarted. The app does not stop another service or replace an existing environment to free resources. Credentials are handled as secrets and omitted from logs, exports and ordinary error text.

Local operation is the default. A remote backend must be explicitly selected and identify what representation would leave the machine. Model availability, technical execution, comparative performance and clinical usefulness are displayed as distinct concepts.

## Results and exports

Displays include task-appropriate predictions and residuals, class/region/sequence support, calibration and uncertainty where defined, paired comparisons on common cases, and explicit missing/failed observations. Probability-like outputs are not called calibrated risks unless that calibration has been established for the intended setting.

Native sensor, documented spectral adaptation and lossy/RGB auxiliary tracks remain distinct in tables and figures. Comparisons record masks/context, source cases, grouping, preprocessing and selection rules. A promising development result is not labelled external confirmation solely because its images came from another source.

A completed run exports the selected fit/prediction objects, tabular results, configuration, model/input/runtime identity, and a package-API reproduction script. The export operation reads the selected immutable artifacts. Download validation reads them back and checks coverage, schema and content identity. Sensitive source metadata is excluded from public-friendly exports unless the user deliberately requests an authorized private export.

## Proposed tests

| File | Required assertions |
|---|---|
| `tests/testthat/test-app-state.R` | Every state transition; invalid transitions rejected; stale results after relevant edits; presentation changes do not refit; failed/cancelled runs cannot finalize as complete |
| `tests/testthat/test-app-modules.R` | UI builders construct labelled controls; `shiny::testServer()` exercises validation, grouping, model compatibility and service failures without a browser |
| `tests/testthat/test-app-downloads.R` | Every supported download produces the expected artifact; readback checks schema/identity and excludes credentials; incomplete runs cannot export as complete |
| `tests/testthat/test-app-session-isolation.R` | Two sessions cannot read or mutate each other's data, credentials, queue state, temporary files or result selection |
| `tests/testthat/test-app-jobs.R` | Explicit submission, resource denial, cancellation acknowledgement, interruption, retry and exact run-identity behavior |
| `tests/testthat/test-app-browser.R` | Synthetic import-to-export workflow, annotation round trip, sequence preview, model/runtime choice, progress, failed run, cancellation and successful retry |
| `tests/testthat/apps/hypermodR/app.R` | Minimal browser harness invoking the package-owned app builder; no duplicated app logic |

Browser acceptance checks both light and dark modes, narrow and desktop widths, keyboard-only navigation, focus after errors, text alternatives to spectral maps and colour-independent status cues. The proposed accessibility target and documentation gates are defined in the [quality specification](quality-and-documentation.md).

Server coverage, browser acceptance and actual backend/model execution are separate evidence. A mocked installer or mocked model is appropriate for UI state tests but does not qualify the real installer or model. The ordinary core test suite remains offline and lightweight; actual optional backend lanes are explicit.

## Completion boundary

The app is ready for a claimed workflow only after its API counterpart, server behavior, browser journey and exported reproduction have passed. Public documentation must identify any remaining unqualified platform, camera, model or endpoint. This specification alone makes no support or deployment claim.
