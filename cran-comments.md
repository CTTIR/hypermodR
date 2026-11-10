## Submission

This is a new submission.

## Test environments

* local: macOS (aarch64), R 4.6.1 — `R CMD check --as-cran --no-manual`
  (no LaTeX installation available locally, hence `--no-manual`; the PDF
  manual still needs to be checked, e.g. on win-builder, before submission).
  The suggested package 'brms' was not installed locally, so this run used
  `_R_CHECK_FORCE_SUGGESTS_=false`; all code using it is conditional.
* GitHub Actions (r-lib/actions v2, `--as-cran`):
  * macOS-latest (release)
  * windows-latest (release)
  * ubuntu-latest (devel, release, oldrel-1)
* GitHub Actions: R 4.3.0 (the declared minimum)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'R. Heller <raban.heller@uni-ulm.de>'

  New submission

  Suggests or Enhances not in mainstream repositories:
    hyperspectR, hyperannotatR, tivis.r, cuvis.r
  Availability using Additional_repositories specification:
    all yes (https://cttir.r-universe.dev)

  This is the first submission of this package. The four packages are
  optional companion readers and data models of the same hyperspectral
  imaging suite, published at <https://cttir.r-universe.dev> and declared in
  `Additional_repositories`. They are used only conditionally
  (an internal `requireNamespace()` guard in code, guarded examples and
  `skip_if_not_installed()` in tests); the package checks cleanly without
  them.

## Notes for the reviewer

* Optional neural-network runtimes (an isolated Python environment, worker
  processes, 'vLLM' servers, model checkpoints downloaded from Hugging Face)
  are never installed, started or downloaded implicitly. Their examples are
  in `\dontrun{}` with a comment stating the external requirement, and their
  tests call `skip_on_cran()`.
* Examples that start bounded child processes run only in interactive Linux
  sessions; all other examples use synthetic data and run in a few seconds.
* Default cache locations use `tools::R_user_dir("hypermodR", "cache")`;
  examples and tests always pass temporary directories instead.

## Downstream dependencies

There are currently no downstream dependencies on CRAN.
