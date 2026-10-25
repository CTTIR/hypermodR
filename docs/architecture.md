# Architecture and acceptance contract

Status: development implementation, 2026-10-25. Core modelling APIs, data bridges,
runtime lifecycle code and application interfaces exist. This document retains
the acceptance contract; interface availability does not establish instrument,
platform, upstream model or scientific qualification. Consult the executable
articles for current callable routes and limitations.

## Separation of responsibilities

The design has a portable R core, explicit companion-package bridges, isolated
optional model workers, and a Shiny interface that calls the same public API.
Readers retain ownership of their vendor/SDK formats; hyperannotatR retains
annotation review and revision ownership. hypermodR owns modelling tasks,
preprocessing, split plans, model adapters, evaluation and run provenance.

An MIT core does not change upstream code, weight, data or SDK licensing.
External workers keep incompatible Python/Torch/compiled dependencies apart.
No source copying or model redistribution follows merely from registration.

End-user setup and execution automation is part of the product. The local
vLLM installer/connector, DNN and transformer setup, model managers, runtime
checks and worker/server lifecycle are public Git-tracked package code. R
implementation lives in `R/`, with runtime registries, platform profiles,
dependency locks and package-owned workers in `inst/`. Installed code resolves
resources through `system.file()` and the package namespace, not checkout paths.
Only development plans, task ledgers, reviews and local build/check evidence
are excluded under `orchestration/`. Downloaded environments, models and runtime
receipts use an external user cache rather than this development directory.

## Data and object contract

Formal S4 `HMDataset` represents validated data sources, observation/band/asset
tables and geometry; `HMSequence` extends it with a frame index. Large cubes
and videos remain lazy. Persisted objects contain reproducible source locators
and hashes, not serialised SDK external pointers or live server handles.

Identifiers are character columns: `subject_id`, `sample_id`, `acquisition_id`,
`image_id`, `sequence_id`, `frame_id`, `roi_id`, `annotation_revision` and
`observation_id` where applicable. Required fields depend on the declared task;
missing subject grouping is never replaced silently by pixel/image identity.
Metadata tables distinguish measured facts, supplied declarations, inferred
values and unavailable fields.

Companion-free construction is part of the core:
`hm_dataset(values, observations, bands, geometry = NULL, provenance = list())`
accepts a numeric observation-by-band matrix or a single `[y,x,wavelength]`
array. A matrix has `data_kind = "spectra"` and no invented spatial geometry.
An image array requires explicit geometry and image/acquisition identity.
`hm_sequence(frames, frame_index, observations, bands, geometry, provenance = list())`
constructs `HMSequence` from compatible frame arrays or admitted lazy frame
sources; frame times and gaps come from the supplied index, not a guessed FPS.
These constructors support offline synthetic examples without vendor packages.

`wavelength` uses nm in the vocabulary. Source values, original band indices,
band centres/FWHM, validity, radiometric units/scale, calibration, array layout,
orientation, coordinate origins, transforms and timestamp timezone assumptions
are explicit. Unknown calibration, physiology or timing stays unknown.

The proposed S3 conversion generic is
`hm_as_dataset(x, profile = NULL, ...)`. Unclassed TIVITA reader lists use the
explicit `hm_from_tivis(x, profile, ...)`. Coercion never registers invented
foreign classes or changes a companion object's semantics.

The S4 generic `hm_read_block(x, selection = list())` requires one acquisition
per call. Selection names are `acquisition_id`, `frame_id`, `rows`, `cols` and
`bands`; numeric indices are 1-based and validated. It returns a named list:

| Element | Contract |
|---|---|
| `values` | Numeric array with named `[y,x,wavelength,time]` dimensions; singleton axes retained |
| `valid` | Logical `[y,x,time]` mask, plus band validity in `band_map` |
| `observations` | Character identity columns and source/frame linkage |
| `band_map` | Native, canonical and selected indices; wavelength/FWHM and conversion provenance |
| `geometry` | Exact source window, padding and invertible coordinate mapping |
| `provenance` | Content, reader, profile, calibration and annotation identities |

Frames may only be stacked when grid/band semantics agree. Variable-geometry
sequences are iterated rather than silently resized. RGB, parameter-display
rasters, quantitative maps, masks and raw/calibrated spectral cubes have
distinct asset roles and model eligibility.

`hm_read_block()` is for spatial image/sequence data. Native spectra use the S4
accessor `hm_spectra(x, selection = list())`, returning `values[observation,wavelength]`,
`observations`, `band_map` and `provenance`, without spatial axes. It rejects
unrequested image-to-spectrum reduction. Image/ROI summaries are explicit
recipe steps with recorded masks, pooling weights and aggregation rules.
Its selection accepts character `observation_id` and positive 1-based `bands`;
both matrix dimensions remain present, including singleton selections.

S3 `hm_model_spec`, `hm_workflow`, `hm_fit`, `hm_prediction` and `hm_evaluation`
objects use validated schemas and standard `print`, `summary`, `plot` and
appropriate `predict`, `coef`, `tidy`, `augment`, `glance` methods. Unsupported
methods give typed errors rather than fabricated coefficients or importance.
S3/S4 dispatch and serialisation are tested from the installed package.

## Proposed modelling interface

```r
hm_task(outcome, type, unit, groups, reference = NULL)
hm_recipe(steps = list())
hm_split(data, task, design = "nested_grouped", seed, ...)
hm_model_spec(model_id, task, parameters = list())
hm_workflow(task, recipe, model)
hm_fit(workflow, data, split, control)
hm_predict(object, newdata, ...)
hm_evaluate(predictions, reference, estimand, resampling, ...)
hm_compare(evaluations, paired = TRUE, ...)
hm_save_run(object, path)
hm_app()
```

These signatures are a proposed contract, not executable examples. A fitted
object owns only training-derived transformations. All learned preprocessing,
feature/band selection, imputation, tuning, calibration, thresholds and stopping
rules remain inside training/inner-validation groups. Prediction never fits
or changes transformations implicitly.

## Task and model-family coverage

| Family | Intended methods and tasks | Required interpretation |
|---|---|---|
| Descriptive/chemometric | Defined indices, derivatives, continuum removal, PCA/ICA/NMF, PLS/PCR, MCR-ALS, endmembers and constrained unmixing | Preserve units, coefficients/loadings, constraints and fitted transformation identity |
| Statistical/open-box | LM/GLM, regularisation, GAM, mixed/hierarchical effects, Bayesian regression, ordinal models, survival/time-to-event and longitudinal models | Expose formulas, estimands, assumptions, identifiability and uncertainty; optional posterior engines require separate runtime checks |
| Physiological/physics-informed | Explicit forward/inverse absorption/scattering models, chromophore estimation, constrained optimisation and hybrid residual models | Validate against physical standards/reference spectra; do not reconstruct quantitative physiology from a colour legend |
| Conventional predictive | LDA/QDA, kNN, naive Bayes, SVM/kernels, trees/random forests, boosting, Gaussian processes and ensembles | Task-appropriate grouped tuning, probabilities/calibration and transparent diagnostics |
| Spectral/spatial deep learning | 1-D/2-D/3-D CNNs, U-Net/segmentation families, autoencoders, pretrained frozen features and fine-tuning | Exact channels, receptive field, ROI pooling, label map, task head and checkpoint |
| Transformer/representation | Spectral/spatial/spatiotemporal transformers, masked/contrastive learning, state-space sequences and multimodal fusion | Preserve measured support and distinguish pretraining, transfer, adaptation and auxiliary rendering |
| Temporal/video | Framewise baselines, sequence aggregation, temporal CNNs, RNN/LSTM/GRU, transformers/state-space models, online detection and forecasting | Real timestamps/gaps, causal look-back, bounded state, whole-sequence/patient splits and temporal tolerance |
| Generative/unsupervised | Clustering, anomaly detection, VAE/GAN/diffusion, denoising, reconstruction and synthetic augmentation | Separate reconstruction quality, detection and downstream validity; fit augmentation on training only |
| Learning strategies | Supervised, semi/self-supervised, active learning, domain adaptation, multitask, ensembles and distillation | Explicit data-access ledger; no use of held-out target labels or unlabelled targets without declaring transductive status |
| LLM/VLM auxiliary | Structured interpretation, retrieval-grounded explanations, candidate ROI assistance and compatible image/video prompting | Exact rendering/prompt/model provenance; no invented endpoints, ground truth or quantitative spectral information |

The registry is extensible across these families. Each entry must declare its
task granularity (spectrum, pixel, ROI, image, sequence or subject), inputs,
outputs, backend, modes, terms and qualification evidence. A generic family
interface does not claim support for every published architecture. Advanced
families require one tested reference implementation before being advertised.

## Geometry, temporal integrity and scientific evaluation

All eligible ROI pixels must survive preprocessing, cropping and pooling.
Padding never creates valid pixels. Native sensor, documented spectral
adaptation and lossy auxiliary pathways are distinct throughout fit, plots,
app views and exports. No missing wavelength is silently invented.

Training units are connected components of subject/animal, procedure, wound,
acquisition/sequence, overlapping windows and annotation/source lineage as
appropriate. Random pixel/frame splits cannot bypass these constraints. Time
ordering, look-back and embargo rules are explicit for forecasting. Invalid
or missing classes/folds/draws remain visible.

Evaluation separates anatomy, segmentation, physiology, burn grade, healing,
injury, survival and workflow utility. Exact biological-unit hierarchies and
training weights are fixed before outcomes. Comparisons use matched cases and
paired unit resampling. Intervals state whether fitting/selection uncertainty
is included. Calibration, abstention and OOD scores require task-specific
validation; a functioning model server does not establish safety.

Spectralverse supplies the central external reference program. Protocols
retain pretraining/source/target overlap, prior outcome access and cohort roles.
Known targets remain development data. Improved annotation releases require
reviewed import, and human cohorts require verified person grouping/use terms.

## Runtime, storage and reproducibility

Native workers and vLLM connectors use versioned request/response schemas.
Process isolation avoids shared Python ABI conflicts; typed array containers
replace arbitrary pickle exchange. Full input/model/preprocessing/source hashes
identify caches. Callers receive independent mutable working storage; masks
pool independently from cached encoder features.

Runs reserve exclusive output IDs. Partial, cancelled and failed runs cannot
replace completed results or publish false success. Atomic receipts bind
inputs, labels, splits, transforms, checkpoint/head, source, environment, RNG,
resources and output hashes. Package installation and loading are offline and
side-effect-free. On Linux, bounded children receive inherited CPU-affinity
subsets, numerical thread limits, per-process virtual address-space limits and
an external elapsed timeout. CUDA selection is explicit but does not limit GPU
memory. Network use is a policy declaration without network isolation; disk
quotas and aggregate descendant-memory limits are not enforced. No background
work starts from data upload.

Core tests use small synthetic data. Public benchmark fixtures are separately
licensed/versioned; human raw data and model weights stay outside Git and the
package tarball. Hosted CI, installed-package checks, backend parity, hardware
qualification and scientific evaluation have separate states.

## Architectural decisions

1. Formal datasets/sequences with explicit S3 bridges; S3 modelling objects.
2. Lazy block IO and separate asset roles; no universal vendor-format claim.
3. Isolated native workers plus compatible vLLM connector; no universal vLLM conversion.
4. Explicit automated optional installation; no load-time downloads or privileged setup.
5. Shared API for scripts and Shiny; annotation review remains with hyperannotatR.
6. Mandatory biological-unit/temporal split integrity and immutable run provenance.
7. MIT package core with independently tracked external artifact terms.
8. Public and packaged runtime automation; ignored development orchestration only.
