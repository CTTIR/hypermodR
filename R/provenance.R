#' Capture explicit reproducibility identities
#' @param objects Named list of R objects whose serialized identities are
#' hashed.
#' @param files Character vector of existing file paths whose bytes are SHA-256
#' hashed.
#' @param resources Optional resource declaration retained in the receipt.
#' @return An `hm_provenance` receipt with object/file hashes, R version,
#' platform and resources.
#' @export
hm_provenance <- function(
  objects = list(), files = character(),
  resources = NULL
) {
  if (!is.list(objects) ||
        !is.character(files) ||
        anyNA(files) ||
        any(!file.exists(files))) {
    .hm_abort("Invalid provenance inputs.")
  }
  if (length(objects) &&
        (is.null(names(objects)) ||
           any(!nzchar(names(objects))))) {
    .hm_abort("Provenance objects require names.")
  }
  structure(
    list(
      schema = "1.0", objects = vapply(objects, .hm_hash, character(1)),
      files = setNames(
        vapply(
          files, function(f) digest::digest(file = f, algo = "sha256"),
          character(1)
        ),
        files
      ),
      R = as.character(getRversion()),
      platform = R.version$platform, resources = resources
    ),
    class = "hm_provenance"
  )
}
#' Verify source identities against a provenance receipt
#' @param receipt An `hm_provenance` receipt.
#' @param objects Named objects corresponding exactly to those originally
#' recorded.
#' @return TRUE invisibly on matching hashes; otherwise raises an integrity
#' error.
#' @details Rehashes the recorded file paths and supplied objects. Does not
#' compare current R/platform metadata or establish source truth.
#' @export
hm_verify_provenance <- function(receipt, objects = list()) {
  if (!inherits(receipt, "hm_provenance")) {
    .hm_abort("Invalid provenance receipt.")
  }
  current <- hm_provenance(
    objects, names(receipt$files),
    receipt$resources
  )
  if (!identical(receipt$objects, current$objects) ||
        !identical(receipt$files, current$files)) {
    .hm_abort("Provenance identity changed.", "hm_integrity_error")
  }
  invisible(TRUE)
}
