.hm_safe_relative <- function(path) {
  is.character(path) &&
    length(path) ==
      1L && !is.na(path) &&
    nzchar(path) &&
    !grepl("(^[/\\\\]|^[A-Za-z]:|\\\\|(^|/)\\.\\.?(/|$))", path)
}
.hm_validate_profile <- function(p) {
  required <- c(
    "profile_id", "model_id", "source_revision", "runtime_version", "os",
    "arch", "device", "artifacts", "terms", "command", "args",
    "qualification",
    "custom_code"
  )
  if (!is.list(p) ||
        !all(required %in% names(p))) {
    .hm_abort("Profile lacks required exact manifest fields.")
  }
  for (field in c(
    "profile_id", "model_id", "source_revision",
    "runtime_version"
  )) {
    .hm_scalar_character(p[[field]], field)
    if (tolower(p[[field]]) %in%
          c("latest", "main", "master")) {
      .hm_abort("Moving revisions are forbidden.")
    }
  }
  if (!length(p$artifacts) ||
        !length(p$terms)) {
    .hm_abort("Artifacts and reviewed terms are required.")
  }
  paths <- character()
  for (a in p$artifacts) {
    if (!.hm_safe_relative(a$path) ||
          !grepl("^[a-f0-9]{64}$", a$sha256) ||
          !is.numeric(a$bytes) ||
          length(a$bytes) !=
            1L || !is.finite(a$bytes) ||
          a$bytes < 0 || !is.character(a$url) ||
          length(a$url) !=
            1L || !grepl("^(https://|file://)", a$url)) {
      .hm_abort("Invalid artifact pin, location or byte count.")
    }
    paths <- c(paths, a$path)
  }
  if (anyDuplicated(paths) ||
    any(
      paths == "receipt.json" | paths == "environment" | startsWith(
        paths,
        "environment/"
      )
    ) ||
    any(
      vapply(
        paths, function(x) {
          any(startsWith(paths[paths != x], paste0(x, "/")))
        }, logical(1)
      )
    )) {
    .hm_abort("Artifact paths overlap or use reserved runtime paths.")
  }
  for (t in p$terms) {
    if (isTRUE(t$unresolved) ||
          !isTRUE(t$reviewed) ||
          !is.character(t$text) ||
          length(t$text) !=
            1L || !nzchar(t$text) ||
          !identical(t$sha256, digest::digest(t$text,
            algo = "sha256",
            serialize = FALSE
          ))) {
      .hm_abort(
        "Terms are unresolved or their text hash differs.",
        "hm_terms_error"
      )
    }
  }
  if (!.hm_safe_relative(p$command) ||
        !p$command %in% paths || !is.character(p$args) ||
        anyNA(p$args)) {
    .hm_abort(paste0(
      "Command must be a declared artifact and arguments a ",
      "character vector."
    ))
  }
  if (!is.logical(p$custom_code) ||
        length(p$custom_code) !=
          1L || is.na(p$custom_code)) {
    .hm_abort("Custom-code declaration must be explicit.")
  }
  if (isTRUE(p$custom_code) &&
        !isTRUE(p$custom_code_reviewed)) {
    .hm_abort("Custom code requires hash-bound review.")
  }
  invisible(p)
}
#' Plan an isolated runtime installation
#' @param model_id Candidate or custom model identity.
#' @param device Requested device.
#' @param profile Exact custom artifact manifest; no upstream profile is
#' inferred.
#' @param cache External user cache.
#' @param capabilities Local capability record.
#' @return An `hm_backend_plan` containing the validated profile, capabilities,
#' cache, artifact-byte budget, actions and immutable digest.
#' @export
hm_backend_plan <- function(
  model_id, device = "auto", profile = NULL, cache = tools::R_user_dir(
    "hypermodR", "cache"
  ),
  capabilities = hm_backend_capabilities()
) {
  .hm_scalar_character(model_id, "model_id")
  if (is.null(profile)) {
    ids <- vapply(
      hm_models(), function(x) x$model_id,
      ""
    )
    if (!model_id %in% ids) {
      .hm_abort("Unknown model identity.")
    }
    .hm_abort(
      paste0(
        "No exact runtime profile supplied. Use ",
        "hm_resolve_backend explicitly for available setup ",
        "routes; artifact terms and platform admission precede ",
        "installation."
      ),
      "hm_backend_unavailable"
    )
  }
  .hm_validate_profile(profile)
  if (!identical(model_id, profile$model_id)) {
    .hm_abort("Model/profile identity mismatch.")
  }
  if (!capabilities$os %in% profile$os ||
        !capabilities$arch %in% profile$arch ||
        (!identical(device, "auto") &&
           !identical(device, profile$device))) {
    .hm_abort("Unsupported OS, architecture or device.", "hm_platform_error")
  }
  if (!identical(profile$device, "cpu") &&
        !isTRUE(profile$hardware_verified)) {
    .hm_abort("GPU driver/ABI qualification is absent.", "hm_platform_error")
  }
  plan <- list(
    schema_version = 1L, model_id = model_id, profile = profile,
    capabilities = capabilities,
    cache = normalizePath(cache, mustWork = FALSE),
    download_bytes = sum(
      vapply(
        profile$artifacts, function(x) x$bytes,
        0
      )
    ),
    actions = c(
      "lock", "stage_declared_files", "verify_sha256_and_bytes",
      "activate_immutable_directory"
    ),
    registry_status = "custom_explicit_profile",
    scientific_qualification = FALSE
  )
  plan$digest <- .hm_hash(plan)
  structure(plan, class = "hm_backend_plan")
}
.hm_validate_plan <- function(plan) {
  if (!inherits(plan, "hm_backend_plan")) {
    .hm_abort("A backend plan is required.")
  }
  d <- plan$digest
  plan$digest <- NULL
  class(plan) <- "list"
  if (!identical(d, .hm_hash(plan))) {
    .hm_abort("Plan changed after resolution.", "hm_integrity_error")
  }
  .hm_validate_profile(plan$profile)
  invisible(TRUE)
}
