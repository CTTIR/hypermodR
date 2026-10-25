# Contributing

Follow the [CTTIR contribution policy](https://github.com/CTTIR/.github/blob/main/CONTRIBUTING.md)
and the proposed [quality requirements](docs/quality-and-documentation.md).
The repository currently contains a specification for approval; implementation
has not started. Proposed interfaces are not a compatibility promise until
qualified and released.

Use `hm_` for package functions, character identifier columns, documented units,
broom-compatible statistical column names and classed `cli::cli_abort()` errors.
Place Shiny logic in `R/`, maintain independent numerical fixtures, and keep
optional runtimes out of the core installation path. Do not advertise a skipped
backend test as a passing qualification.

Keep product specifications and source/tests in Git. Execution ledgers, local
audit evidence, raw data, credentials, installed environments and model weights
remain outside tracked package content. New external dependencies and model
artifacts require separate source, license, version and checksum records.
