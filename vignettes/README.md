# Vignette catalogue

**Status: executable development articles implemented.** Core synthetic examples execute offline; optional network/runtime examples are explicit opt-ins. The evidence column retains acceptance requirements, including work still pending. Implemented articles do not establish instrument, platform, browser or scientific qualification. Their APIs and acceptance criteria agree with the [architecture](../docs/architecture.md), [quality specification](../docs/quality-and-documentation.md) and [Shiny specification](../docs/shiny-workbench.md).

| Article | Intended reader outcome | Required executable evidence |
|---|---|---|
| `hypermodR.Rmd` | Complete a synthetic CPU workflow from data validation to a grouped model, prediction, uncertainty and exported result | Offline example with known source values, fixed independent units and verified output readback |
| `data-and-interoperability.Rmd` | Exchange `HMDataset`/`HMSequence` objects and annotations with the supported HSI packages through `hm_as_dataset()` and documented S3/S4 methods | Sibling-package round trips preserving identity, axes, units, wavelength grid, calibration, annotation revision and temporal order |
| `tivita-workflows.Rmd` | Choose the correct verified TIVITA source pathway and distinguish raw spectra, calibrated spectra, RGB and vendor parameter images | Legally redistributable or synthetic format fixtures; orientation, mask, metadata and reference-output checks for each advertised pathway |
| `cubert-video.Rmd` | Import admitted Cubert/cuvis sequence sources, inspect time and calibration, and model sequences without frame leakage | Unequal frame intervals, missing frames, calibration state, bounded access and recording/subject-grouped evaluation fixtures |
| `interpretable-models.Rmd` | Fit and interpret statistical, chemometric and physiological models with explicit assumptions | Independent numerical values, train-only preprocessing, coefficient/uncertainty interpretation and failure cases |
| `machine-learning.Rmd` | Select conventional model families with leakage-resistant tuning and comparison | Identical grouped support across methods; independently verified metrics and paired uncertainty |
| `deep-and-foundation-models.Rmd` | Use compatible frozen/fine-tuned neural adapters and understand input adaptations, model provenance and limits | Small qualified model examples in explicit backend lanes; exact weights/input/preprocessing records and reference-output comparisons |
| `backends-and-installation.Rmd` | Inspect capabilities and explicitly install a supported isolated runtime/model setup | Platform-specific preflight, repeat-install behavior, interrupted install, integrity failure and actual model smoke receipts |
| `validation-and-reliability.Rmd` | Define independent units, estimands and intended use; distinguish development, transfer and confirmation | Synthetic leakage traps, known calibration/probability scores, conditional-uncertainty examples and invalid-draw accounting |
| `shiny-workbench.Rmd` | Reproduce the API workflow through planned `hm_app()` and export the same configuration/results | Browser-tested synthetic walkthrough, cancellation/recovery and app-to-script equivalence |
| `extending-hypermodR.Rmd` | Add a data, model or backend adapter against the documented extension contract | A minimal test adapter passing the same conformance checks as built-in adapters |

Core articles execute without internet access, model downloads, vendor SDKs or GPUs. Examples use synthetic or explicitly redistributable fixtures with source and license metadata. Heavy examples identify their optional dependency, tested platform/model versions, bounded runtime and opt-in reproduction procedure.

Every article states the scientific task, input representation, independent unit, assumptions and limitations before presenting a score. Models selected with target outcomes remain development models. Technical execution is not clinical validation, and a plotted RGB image is not the full spectral measurement.

The notation is shared with the package: `wavelength` is in nm; identifiers are character columns; statistics use broom-compatible names. Executable examples use `hm_model_spec()`, `hm_workflow()`, S3 `hm_fit` and S3 `hm_prediction` consistently with the formal data classes.

The vignette build must not hide all examples behind a global unevaluated setting. Small core examples run and check their values. Optional backend sections show their support status and distinguish retained example output from a run performed during the build. Links, figures, captions, units and code wrapping are checked in the rendered package website and standalone vignette output.

Installation and reference statements are added only when the corresponding software exists. This catalogue does not claim CRAN availability, a qualified upstream model backend or complete browser qualification.
