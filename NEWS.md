# hypermodR 0.1.0

## CRAN preparation

* Every exported function now has a documented return value and an example.
  Examples that need optional R packages are guarded with
  `requireNamespace()`; examples that need an isolated Python runtime, a
  network checkpoint or a running model server are not run.
* Tests that start Python, R or HTTP worker processes, need `openssl` or are
  Linux-specific are skipped on CRAN.
* The random-number state is restored through an explicit `.Random.seed`
  binding, which removes the "assignments to the global environment" NOTE.
* Reworded the Description in complete sentences, declared the language,
  added `cran-comments.md` and an `inst/WORDLIST`.
* Updated the maintainer e-mail address to raban.heller@uni-ulm.de.
* Includes the unreleased 0.0.0.9001 changes listed below.

# hypermodR 0.0.0.9001 (development version, not released)

* Reject stale temporal partitions, incompatible radiometric prediction units,
  invalid survival inputs and misaligned named matrix outcomes. Reconstruction
  evaluation requires an explicit reference in the fitted output space.
  Refit legacy models lacking the new input-unit contract.
* Preserve selected-frame observations, tri-state masks and complete band
  metadata through hyperspectR conversion; reject stale content identities and
  hidden live state in persistent metadata.
* Align backend availability checks with startup integrity validation and
  stop owned workers after malformed responses. Ignore external pip configuration
  during explicitly requested environment setup.
* Update the CPU reference profile to Torch 2.13.0, NumPy 2.2.6 and setuptools
  84.1.0. Retire affected generated profiles and block automatic vLLM setup
  until a replacement is reviewed; existing runtime files remain preserved.
* Honor the workbench's 50 MiB upload limit, bind setup consent to the reviewed
  resolution, and show unsupported-platform restrictions before job submission.
* Make development installation and first use executable from a fresh session,
  repair packaged-document links, pin shared lint configuration and exclude
  Python bytecode caches from source distributions.

# hypermodR 0.0.0.9000

* Standardize the named v5 package logo across README, documentation pages
  and available application headers, with matching light and dark artwork.


* Added formal spectral/sequence data contracts, bounded access and explicit
  companion-package source bridges with synthetic conformance fixtures.
* Added grouped workflows, training-only recipes, statistical/chemometric and
  conventional model engines, nested tuning, explicit evaluation and conditional
  whole-unit uncertainty.
* Added immutable run readback, runtime metadata resolution, reviewed isolated
  installation, owned workers and explicit named upstream admission contracts.
* Added separate trainable neural/temporal reference implementations; named
  pretrained models, instruments and scientific transfer remain unqualified.
* Added a local Shiny workbench, eleven executable development articles and the
  shared themakR website template. Browser/platform acceptance remains separate.
