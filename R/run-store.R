#' Save an immutable run with an atomic completion receipt
#' @param object R object to serialize and verify by readback.
#' @param path New run-directory path whose parent already exists.
#' @return Normalized run-directory path invisibly.
#' @details Reserves a new directory, writes object.rds and a hash-bound
#' receipt, and atomically finalizes COMPLETE. Existing paths are rejected. A
#' failed write leaves a failed receipt for inspection; completion markers are
#' removed.
#' @export
hm_save_run <- function(object, path) {
  .hm_scalar_character(path, "path")
  if (!dir.exists(dirname(path))) {
    .hm_abort("Run parent directory must exist.", "hm_io_error")
  }
  if (!dir.create(path, showWarnings = FALSE)) {
    .hm_abort(
      "Run path is already reserved or cannot be created.",
      "hm_run_conflict"
    )
  }
  success <- FALSE
  on.exit(
    if (!success) {
      unlink(file.path(path, "COMPLETE"))
      try(
        saveRDS(
          list(state = "failed"),
          file.path(path, "receipt.rds")
        ),
        silent = TRUE
      )
    }
  )
  .hm_run_receipt(path, "reserved")
  .hm_run_receipt(path, "running")
  partial <- file.path(path, "object.partial")
  saveRDS(object, partial)
  restored <- readRDS(partial)
  if (!isTRUE(all.equal(object, restored, check.environment = FALSE))) {
    .hm_abort("Run readback mismatch.", "hm_integrity_error")
  }
  if (!file.rename(partial, file.path(path, "object.rds"))) {
    .hm_abort("Cannot finalize run object.", "hm_io_error")
  }
  hash <- digest::digest(
    file = file.path(path, "object.rds"),
    algo = "sha256"
  )
  .hm_run_receipt(path, "complete", hash)
  marker <- file.path(path, "COMPLETE.partial")
  writeLines(hash, marker)
  if (!file.rename(marker, file.path(path, "COMPLETE"))) {
    .hm_abort("Cannot finalize completion marker.", "hm_io_error")
  }
  success <- TRUE
  invisible(normalizePath(path))
}
.hm_run_receipt <- function(path, state, hash = NULL) {
  states <- c(
    "reserved", "running", "partial", "failed", "cancelled",
    "complete"
  )
  if (!state %in% states) {
    .hm_abort("Unknown run state.")
  }
  tmp <- file.path(path, "receipt.partial")
  saveRDS(
    list(schema = "1.0", state = state, hash = hash),
    tmp
  )
  if (!file.rename(tmp, file.path(path, "receipt.rds"))) {
    .hm_abort("Cannot finalize receipt.", "hm_io_error")
  }
}
#' Read and verify a completed run
#' @param path Run directory created by `hm_save_run()`.
#' @return The deserialized original R object.
#' @details Requires matching complete receipt, object SHA-256 and COMPLETE
#' marker before reading the object. Read only trusted RDS sources.
#' @export
hm_read_run <- function(path) {
  if (!file.exists(file.path(path, "COMPLETE"))) {
    .hm_abort("Run is not complete.", "hm_integrity_error")
  }
  r <- readRDS(file.path(path, "receipt.rds"))
  h <- digest::digest(
    file = file.path(path, "object.rds"),
    algo = "sha256"
  )
  if (!identical(r$state, "complete") ||
    !identical(r$hash, h) ||
    !identical(
      readLines(
        file.path(path, "COMPLETE"),
        warn = FALSE
      ),
      h
    )) {
    .hm_abort("Run integrity verification failed.", "hm_integrity_error")
  }
  readRDS(file.path(path, "object.rds"))
}
