#' Inspect installation and separately supplied parity evidence
#' @param model_id Model identity.
#' @param tests Requested checks.
#' @param backend Installed backend.
#' @param parity Named evidence including plan_digest and passed.
#' @return List with model identity, availability status, reported parity
#' status and scientific_qualification=FALSE.
#' @details Availability verifies artifact sizes/hashes for the supplied
#' installation. Parity is caller-supplied evidence bound to the plan digest,
#' reported as reported_pass rather than independently executed here. No
#' scientific qualification follows.
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
    available <- all(
      vapply(
        backend$plan$profile$artifacts, function(a) {
          .hm_artifact_ok(
            file.path(backend$path, a$path),
            a
          )
        }, logical(1)
      )
    )
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
