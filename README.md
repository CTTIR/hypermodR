# hypermodR

**Hyperspectral Modelling and Validation in R**

**Status: planning-stage repository; implementation awaits approval.** No
modelling functions, model installers or Shiny application are available yet.
The interfaces and capabilities below describe the proposed package, not
completed support. The package's own code is licensed under [MIT](LICENSE.md).
External models, weights, datasets and instrument SDKs retain their own terms.

hypermodR is intended to connect interpretable and black-box modelling with
reliable hyperspectral data handling, biological-unit validation and a shared
R/Shiny workflow. It will complement the existing CTTIR packages instead of
replacing their readers, annotation tools or visualisation interfaces.

## Proposed scope

| Area | Intended capability |
|---|---|
| Data | Spectra, hyperspectral images, registered companion images, ROI/label maps, longitudinal acquisitions and hyperspectral video |
| Instruments | Profile-qualified TIVITA exports and Cubert/CUVIS sessions, including video chronology; unknown formats remain identifiable rather than guessed |
| Open-box methods | Spectral indices with explicit definitions, chemometrics, linear/generalised/additive/mixed models, physiological forward/inverse models and constrained unmixing |
| Conventional learning | Regularised models, discriminant methods, kernel methods, trees/forests, boosting, Gaussian processes, ensembles and task-appropriate survival/ordinal methods |
| Deep learning | Spectral, spatial, spectral-spatial and temporal networks; transformers, representation learning, segmentation, reconstruction and domain adaptation |
| Named research candidates | HTC, HyperVision, HyperFree, HyperSIGMA, Dual-Stream-MHSI, SpectralEarth and SpectralGPT, each with its own input, licensing and qualification record |
| Language/vision-language models | Optional compatible vLLM profiles for explicitly defined tasks; rendered RGB/video/plots are labelled as auxiliary inputs |
| Evaluation | Grouped/nested selection, temporal leakage prevention, paired uncertainty, calibration, abstention, robustness and independent confirmation |
| Interface | Standard S3 modelling methods, formal S4 dataset/sequence contracts and a Shiny workbench using the same public API |

Coverage is organised by task and model family, with an extension contract for
additional implementations. An available architecture is not automatically an
installed, tested or scientifically qualified model.

## Interoperability

The proposed bridges target [hyperspectR](https://github.com/CTTIR/hyperspectR),
[hyperspectaculR](https://github.com/CTTIR/hyperspectaculR),
[hyperannotatR](https://github.com/CTTIR/hyperannotatR),
[tivis.r](https://github.com/CTTIR/tivis.r) and
[cuvis.r](https://github.com/CTTIR/cuvis.r).

Conversions must preserve image identity, values and scaling, wavelength units
and original band mapping, masks, coordinates, annotation revision, acquisition
time and independent-unit identifiers. Video access must be bounded and retain
gaps and irregular timing. Prediction imports must remain unreviewed until
accepted in the annotation package.

See the [data and interoperability specification](docs/interoperability.md).
The current packages do not already share a common S4 HSI class; introducing
one in hypermodR requires explicit bridges and cross-package tests.

## Model runtimes and installation

The R core will remain usable without Python, a GPU or a vendor SDK. Explicit
package functions and corresponding Shiny actions will plan, install and check
optional runtimes in versioned user-space environments. Installation must not
occur during package installation, loading, ordinary checks or vignette builds.

Native HSI networks require their own compatible workers. vLLM is planned for
supported language and vision-language architectures; it is not a universal
runtime for arbitrary HSI neural networks. In particular, SpectralGPT is a
masked autoencoder, not an autoregressive language model. Training and
fine-tuning use separate native workers.

The [backend specification and platform matrix](docs/model-backends.md) and
[proposed model registry](docs/model-registry.json) retain unavailable models
and unresolved terms explicitly. Automatic setup is planned only for a
qualified platform/profile; it will not replace drivers or silently alter
existing environments or services.

## Shiny workbench and documentation

The proposed workbench follows **Import → QC/geometry → ROI/task/grouping →
Model/backend → Run → Compare → Export**. Numerical and validation logic will
live in the R API, with application logic in `R/` and static assets only in
`inst/`. See the [app specification](docs/shiny-workbench.md).

The [vignette catalogue](vignettes/README.md) covers the complete intended
workflow, including executable offline synthetic examples and separately
qualified optional-backend examples. The site will use CTTIR's `themakR`
pkgdown template. A runnable quick start, app launch command and installation
instructions will be added when the corresponding capability passes its gates.

## Quality and scientific scope

The package will follow the [CTTIR contribution policy](https://github.com/CTTIR/.github/blob/main/CONTRIBUTING.md),
with `qviewparsR` as the reference and shared check, coverage and lint workflows.
Proposed additional gates include a 95% test-coverage floor, blocking lint,
independent numerical fixtures, platform-specific backend qualification,
browser/accessibility checks and verified source-tarball contents.

The [architecture](docs/architecture.md) and
[quality/documentation requirements](docs/quality-and-documentation.md) define
the proposed contracts. Technical reproducibility, task performance and
clinical validity remain separate. Expert ROIs, adapted spectra and lossy
multimodal renderings will not be presented as equivalent inputs.

[Spectralverse Heidelberg](https://spectralverse-heidelberg.org/) is the central
external methods reference: HeiPorSPECTRAL for anatomy, xeno-spectral for
domain/species/perfusion transfer, and ManUSPECTRAL for a separately admitted
human branch. These datasets do not provide burn or spinal-injury reference
labels by implication.

Execution plans, task state and audit evidence are maintained in the local
`orchestration/` directory, excluded from Git and R package builds. Public
product specifications remain in this repository. No clinical data, raw
instrument recordings, model weights or machine-specific credentials belong
in the repository or package archive.
