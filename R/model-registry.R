.hm_resource <- function(...) {
  path <- system.file(..., package = "hypermodR")
  if (!nzchar(path)) {
    .hm_abort("Installed package resource is missing.", "hm_resource_error")
  }
  path
}

#' List candidate model families
#' @param task Optional task filter.
#' @param backend Optional backend filter.
#' @param include_unavailable Include unqualified candidates.
#' @return List of model records with identity, supported task declarations,
#' candidate backend, availability/admission status and scientific
#' qualification.
#' @details Registry visibility denotes candidate declaration or setup
#' availability, not installed execution or independent parity. Reference
#' families have separate spectral/tensor routes; not every listed identity is
#' accepted by hm_fit(). All returned scientific qualification flags remain
#' FALSE.
#' @export
hm_models <- function(
  task = NULL, backend = NULL,
  include_unavailable = TRUE
) {
  registry <- jsonlite::read_json(
    .hm_resource("models", "registry.json"),
    simplifyVector = FALSE
  )
  models <- c(
    registry$models, lapply(
      registry$optional_families, function(x) {
        x$model_id <- x$candidate_id
        x$package_enabled <- FALSE
        x
      }
    )
  )
  catalog <- if (exists(".hm_engine_catalog", mode = "function")) {
    .hm_engine_catalog()
  } else {
    list()
  }
  core <- lapply(
    names(catalog),
    function(id) {
      list(
        model_id = id, display_name = id, tasks = catalog[[id]],
        candidate_backend = "r_core",
        package_enabled = TRUE, package_status = "implemented",
        scientific_qualification = FALSE
      )
    }
  )
  reference_tasks <- list(
    cnn1d = "classification", unet2d = "segmentation",
    cnn3d = "classification",
    spectral_vit = "classification", masked_autoencoder = "reconstruction",
    contrastive_encoder = "embedding", vae = "reconstruction",
    conditional_gan = "reconstruction",
    ddpm = "reconstruction", multitask = "classification",
    domain_adversarial = "classification",
    distillation = "classification"
  )
  refs <- lapply(
    names(reference_tasks),
    function(id) {
      list(
        model_id = paste0("reference:", id),
        display_name = paste("CPU reference", id),
        tasks = reference_tasks[[id]],
        candidate_backend = "python_reference_cpu",
        package_enabled = TRUE,
        package_status = "setup_available_unqualified",
        scientific_qualification = FALSE
      )
    }
  )
  for (i in seq_along(models)) {
    models[[i]]$tasks <- if (identical(
      models[[i]]$category,
      "research_hsi_candidate"
    )) {
      c("classification", "segmentation", "reconstruction")
    } else {
      "auxiliary_explanation"
    }
    models[[i]]$scientific_qualification <- FALSE
  }
  models <- c(core, refs, models)
  if (!is.null(backend)) {
    models <- Filter(
      function(x) identical(x$candidate_backend, backend),
      models
    )
  }
  if (!is.null(task)) {
    models <- Filter(
      function(x) {
        task %in% x$tasks
      }, models
    )
  }
  if (!isTRUE(include_unavailable)) {
    models <- Filter(
      function(x) isTRUE(x$package_enabled),
      models
    )
  }
  models
}

#' Verify a signed registry release
#' @param path Registry JSON file.
#' @param signature Detached SHA256 RSA signature file.
#' @param key PEM public key file provisioned by the maintainer.
#' @param trusted_keys Allowlisted SHA256 hashes of public key files.
#' @param revoked_keys Revoked key hashes.
#' @param minimum_version Minimum allowed integer release sequence.
#' @param now Verification time.
#' @return An `hm_registry_verification` list with verified flag, file digest,
#' trusted key fingerprint and release sequence.
#' @export
hm_verify_registry <- function(
  path, signature, key, trusted_keys, revoked_keys = character(),
  minimum_version = 1L,
  now = Sys.time()
) {
  .hm_require("processx")
  fingerprint <- digest::digest(file = key, algo = "sha256")
  if (!fingerprint %in% trusted_keys || fingerprint %in% revoked_keys) {
    .hm_abort("Registry key is untrusted or revoked.", "hm_signature_error")
  }
  result <- processx::run(
    "openssl", c(
      "dgst", "-sha256", "-verify", normalizePath(key),
      "-signature", normalizePath(signature),
      normalizePath(path)
    ),
    error_on_status = FALSE
  )
  if (result$status != 0L) {
    .hm_abort("Registry signature verification failed.", "hm_signature_error")
  }
  reg <- jsonlite::read_json(path)
  if (!is.numeric(reg$release_sequence) ||
        reg$release_sequence < minimum_version || is.null(reg$expires) ||
        is.na(as.POSIXct(reg$expires, tz = "UTC")) ||
        as.POSIXct(reg$expires, tz = "UTC") <=
          now) {
    .hm_abort(
      "Registry release is expired or rolled back.",
      "hm_signature_error"
    )
  }
  structure(
    list(
      verified = TRUE, digest = digest::digest(file = path, algo = "sha256"),
      key = fingerprint, release_sequence = reg$release_sequence
    ),
    class = "hm_registry_verification"
  )
}
