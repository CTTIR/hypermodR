hypermodR
================

hypermodR brings hyperspectral data, train-only preprocessing, grouped
modelling and explicit evaluation into one R workflow. It is development
software for research. Executable interfaces, technical qualification
and scientific validity are separate states. No clinical performance or
general instrument compatibility is claimed.

| Route | Current scope | Qualification boundary |
|----|----|----|
| R models | Grouped statistical, chemometric and conventional learning adapters | Synthetic tests; optional R engines require their packages |
| Data bridges | S4 datasets/sequences, S3 source conversion and bounded blocks | Synthetic and companion fixtures; camera profiles remain unqualified |
| Neural models | Package-owned trainable references and named upstream admission contracts | No named pretrained model scientifically qualified |
| Runtime setup | Explicit resolver, reviewed manifest, isolated installation and owned workers | Automated resolver: Linux x86_64 only; exact live smoke/parity required |
| Workbench | Local Shiny workflow using the same package API | Browser/accessibility qualification tracked separately |

## Install the development source

From a checked-out source directory:

``` r
install.packages(".", repos = NULL, type = "source")
```

Install optional R engines only when needed. Loading hypermodR does not
install Python, download weights or start a server. The package is not
presented as a CRAN release. Read the installed runtime setup article
with `vignette("backends-and-installation", package = "hypermodR")`.

## A complete synthetic workflow

The task is continuous response prediction from eight synthetic spectral
bands (500–850 nm, arbitrary radiometric units). Subjects are the
independent split units; the 96 spectra are repeated measurements from
12 subjects. This is one outer fold for illustration, not a selected
model or confirmation study. Scaling is learned on training rows only.
The held-out RMSE is reconstructed independently before an immutable run
is saved and read back.

``` r
data <- hm_example(seed = 42L)
if (!isTRUE(all(
  methods::validObject(data), nrow(data@observations) == 96L,
  identical(data@bands$wavelength, seq(500, 850, length.out = 8L))
))) {
  cli::cli_abort("Example assertion failed.", class = "hm_example_error")
}
task <- hm_task("response", "regression", "spectrum", "subject_id")
splits <- hm_split(data, task, folds = 3L, inner_folds = 2L, seed = 7L)
fold <- splits$folds[[1L]]
if (!isTRUE(all(!length(intersect(
  data@observations$subject_id[fold$train],
  data@observations$subject_id[fold$test]
))))) {
  cli::cli_abort("Example assertion failed.", class = "hm_example_error")
}
workflow <- hm_workflow(
  task, hm_recipe(list(list(type = "scale"))),
  hm_model_spec("lm", task)
)
fit <- hm_fit(workflow, data, fold)
heldout <- hm_spectra(data, list(
  observation_id = data@observations$observation_id[fold$test]
))
prediction <- hm_predict(fit, heldout)
evaluation <- hm_evaluate(prediction, heldout$observations)
errors <- prediction$values - heldout$observations$response
expected_rmse <- sqrt(mean(errors^2))
if (!isTRUE(all(isTRUE(all.equal(unname(evaluation$metrics["rmse"]),
  expected_rmse))))) {
  cli::cli_abort("Example assertion failed.", class = "hm_example_error")
}
evaluation
#>       rmse        mae       bias
#> 0.10558376 0.08430486 0.03453047
uncertainty <- hm_bootstrap(evaluation, unit = "subject_id", draws = 20L,
  seed = 9L)
if (!isTRUE(all(uncertainty$invalid_draws == 0L))) {
  cli::cli_abort("Example assertion failed.", class = "hm_example_error")
}
uncertainty$conditioning
#> [1] "fixed predictions; fitting and selection uncertainty excluded"
run <- tempfile("hypermodR-run-")
hm_save_run(list(
  fit = fit, prediction = prediction, evaluation = evaluation,
  uncertainty = uncertainty
), run)
restored <- hm_read_run(run)
if (!isTRUE(all(identical(restored$prediction, prediction)))) {
  cli::cli_abort("Example assertion failed.", class = "hm_example_error")
}
unlink(run, recursive = TRUE)
```

## Data, models and the workbench

`HMDataset` and `HMSequence` are formal S4 containers. `hm_spectra()`
and `hm_read_block()` are S4 accessors; `hm_as_dataset()` bridges
supported S3 sources. `hm_fit` and `hm_prediction` are S3 results with
standard print/summary and prediction methods. Camera brands do not
establish wavelength calibration. TIVITA profiles distinguish spectral
cubes, RGB and vendor displays; Cubert sequences retain frame identity,
explicit time and calibration declarations. See the [article
catalogue](https://github.com/CTTIR/hypermodR/blob/main/vignettes/README.md).

`hm_models()` exposes model identities and support status.
`hm_model_spec()` is the common constructor. Named upstream constructors
are `hm_htc()`, `hm_hyperfree()`, `hm_hypersigma()`, `hm_hypervision()`,
`hm_dual_stream_mhsi()`, `hm_spectralearth()` and `hm_spectralgpt()`. A
registry entry is not an executable or qualified model.
`hm_temporal_model()` declares causal package reference families. Stock
vLLM serves compatible language/vision models; it does not execute these
native HSI architectures.

``` r
hm_app()
```

The local app offers a synthetic example and reproducible exports
through the same task, workflow, split and run APIs. Read the installed
workbench guide with
`vignette("shiny-workbench", package = "hypermodR")`.

## Scientific use and the Spectralverse programme

[Spectralverse](https://spectralverse-heidelberg.org) is the central
external reference programme:
[HeiPor](https://spectralverse-heidelberg.org/HeiPorSPECTRAL/) anchors
primary anatomy,
[xeno](https://spectralverse-heidelberg.org/xeno-spectral/)
species/perfusion development, and
[ManU](https://spectralverse-heidelberg.org/manuspectral/) separate
human admission. `hm_benchmark_protocol()` validates protocol metadata;
it does not supply data, grant rights or certify external validation.
Target outcome access, case/ROI support and native versus adapted
spectral tracks remain explicit.

Bootstrap intervals condition on fixed predictions and omit
training/selection uncertainty. Development, transfer and confirmation
claims require distinct independent units and admission evidence.
Missing-class draws remain invalid; model confidence is not an admitted
out-of-distribution detector.

Package-owned code uses the [MIT license](LICENSE.md). Data, external
source, SDKs and weights retain their own terms. Review the
[contribution guide](CONTRIBUTING.md),
[architecture](https://github.com/CTTIR/hypermodR/blob/main/docs/architecture.md)
and [quality
requirements](https://github.com/CTTIR/hypermodR/blob/main/docs/quality-and-documentation.md).
Use `citation("hypermodR")` for the software citation; no publication
DOI is claimed.
