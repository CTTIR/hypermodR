# Contributing

Follow the [CTTIR contribution policy](https://github.com/CTTIR/.github/blob/main/CONTRIBUTING.md)
and the proposed [quality requirements](https://github.com/CTTIR/hypermodR/blob/main/docs/quality-and-documentation.md).
The repository currently contains a specification for approval; implementation
has not started. Proposed interfaces are not a compatibility promise until
qualified and released.

Use `hm_` for package functions, character identifier columns, documented units,
broom-compatible statistical column names and classed `cli::cli_abort()` errors.
Place Shiny logic in `R/`, maintain independent numerical fixtures, and keep
optional runtimes out of the core installation path. Do not advertise a skipped
backend test as a passing qualification.

Keep product specifications, source/tests and all end-user runtime automation in
Git. The vLLM/DNN/transformer installer and lifecycle code belongs in `R/`, with
package-owned registries, platform profiles, locks and workers in `inst/`; these
must be included in the source archive and usable after installation. Do not
place runtime implementation in ignored `orchestration/` or build-excluded
`docs/`. Only development execution ledgers, local build/check evidence and
reviews belong in `orchestration/`. Raw data, credentials, downloaded
environments and weights remain outside tracked package content. New external
dependencies and model artifacts require separate source, license, version and
checksum records.
