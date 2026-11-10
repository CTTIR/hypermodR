#' Inspect installation and separately supplied parity evidence
#' @param model_id Model identity.
#' @param tests Requested checks.
#' @param backend Installed backend.
#' @param parity Named evidence including plan_digest and passed.
#' @return List with model identity, availability status, reported parity
#' status and scientific_qualification=FALSE.
#' @details Availability verifies the declared installation path, containment,
#' artifact sizes/hashes and any Python environment inventory. Parity is
#' caller-supplied evidence bound to the plan digest,
#' reported as reported_pass rather than independently executed here. No
#' scientific qualification follows.
#' @examples
#' hm_check_backend("absent-model")$availability
#'
#' dir <- tempfile("hm-backend-")
#' dir.create(dir)
#' worker <- file.path(dir, "worker")
#' writeLines("#!/bin/sh\nexit 0", worker)
#' terms <- "Local example artifact only; unrestricted example use."
#' profile <- list(
#'   profile_id = "example-cpu-v1", model_id = "example-identity",
#'   source_revision = "example-source-1",
#'   runtime_version = "example-runtime-1",
#'   custom_code = TRUE, custom_code_reviewed = TRUE,
#'   os = unname(Sys.info()["sysname"]), arch = unname(Sys.info()["machine"]),
#'   device = "cpu", qualification = "test_only",
#'   artifacts = list(list(
#'     path = "worker", url = paste0("file://", worker),
#'     bytes = unname(file.info(worker)$size),
#'     sha256 = digest::digest(file = worker, algo = "sha256")
#'   )),
#'   terms = list(list(
#'     reviewed = TRUE, text = terms,
#'     sha256 = digest::digest(terms, algo = "sha256", serialize = FALSE)
#'   )),
#'   command = "worker", args = character()
#' )
#' # Keep the runtime cache in the temporary directory
#' plan <- hm_backend_plan("example-identity", profile = profile,
#'                         cache = file.path(dir, "cache"))
#' backend <- hm_install_backend(plan, accept_terms = TRUE, offline = TRUE,
#'                               acknowledge_custom_code = TRUE)
#' report <- hm_check_backend(
#'   "example-identity", backend = backend,
#'   parity = list(plan_digest = plan$digest, passed = TRUE)
#' )
#' report$availability
#' report$parity
#' unlink(dir, recursive = TRUE)
#' @export
hm_check_backend <- function(
  model_id, tests = c("availability", "parity"),
  backend = NULL, parity = NULL
) {
  if (any(!tests %in% c("availability", "parity"))) {
    .hm_abort("Unknown qualification check.")
  }
  available <- inherits(backend, "hm_backend") &&
    identical(model_id, backend$plan$model_id)
  if (available) {
    .hm_validate_plan(backend$plan)
    available <- isTRUE(tryCatch(
      .hm_validate_installation(backend), error = function(e) FALSE
    ))
  }
  list(
    model_id = model_id, availability = if (
                                            available) {
      "pass"
    } else {
      "unavailable"
    },
    parity = if (!"parity" %in% tests) {
      "not_requested"
    } else if (available && identical(
      parity$plan_digest,
      backend$plan$digest
    ) &&
      isTRUE(parity$passed)) {
      "reported_pass"
    } else {
      "not_established"
    }, scientific_qualification = FALSE
  )
}
