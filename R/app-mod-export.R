#' Export a reproducible workbench result
#' @param result Completed fit, prediction, evaluation and configuration bundle.
#' @param path New export directory, never an existing run.
#' @param include_private Include fitted objects and original prediction
#' identities.
#'   The default exports configuration, aggregate metrics and a reproduction
#' script.
#' @return Verified export directory, invisibly.
#' @details Reference reproduction requires an explicitly supplied admitted
#' backend and artifact_root; it retrains in an owned bounded job. Private
#' reference exports include checked learned files. The generated
#' read_private_fit() helper resolves their location after moving the export;
#' use hm_predict with an owned matching backend_handle to verify and predict.
#' Installed runtime receipts and cache paths are not portable exports.
#' @export
hm_export_result <- function(result, path, include_private = FALSE) {
  if (!is.list(result) ||
        !inherits(result$fit, "hm_fit") ||
        !inherits(result$prediction, "hm_prediction") ||
        !inherits(result$evaluation, "hm_evaluation")) {
    .hm_abort("Only a completed result can be exported.")
  }
  if (!is.logical(include_private) ||
        length(include_private) !=
          1L || is.na(include_private)) {
    .hm_abort("include_private must be explicit TRUE or FALSE.")
  }
  deep <- startsWith(result$workflow$model$model_id, "reference:")
  control <- result$control
  if (deep) {
    if (!inherits(control$resources, "hm_resources")) {
      .hm_abort(
        "Reference exports require an explicit saved resource declaration."
      )
    }
    runtime_fields <- c("backend", "backend_handle", "artifact_path",
                        "artifact_root")
    control[runtime_fields] <- NULL
  }
  out <- list(
    workflow = result$workflow, split = result$split[c("train", "test")],
    control = control, input_hash = result$input_hash,
    metrics = result$evaluation$metrics,
    private = include_private
  )
  .hm_app_export_safe(out)
  artifact_source <- NULL
  if (include_private) {
    out$private_artifacts <- .hm_app_strip_secrets(result[c(
      "fit",
      "prediction", "evaluation"
    )])
  }
  if (include_private && deep) {
    artifact_source <- result$fit$engine$artifact_path
    if (!identical(
      .hm_deep_artifact_hash(artifact_source),
      result$fit$engine$artifact_sha256
    )) {
      .hm_abort(
        "Reference artifact integrity changed before export.",
        "hm_integrity_error"
      )
    }
    out$private_artifacts$fit$engine$artifact_path <-
      file.path("private", "reference-model")
  }
  hm_save_run(out, path)
  complete <- FALSE
  on.exit(if (!complete) {
    unlink(file.path(path, "COMPLETE"))
    .hm_run_receipt(path, "failed")
  }, add = TRUE)
  if (!is.null(artifact_source)) {
    destination <- file.path(path, "private", "reference-model")
    dir.create(destination, recursive = TRUE)
    files <- c("model.json", "state.npz", "input.json")
    if (!all(file.copy(file.path(artifact_source, files), destination)) ||
      !identical(
        .hm_deep_artifact_hash(destination),
        result$fit$engine$artifact_sha256
      )) {
      .hm_abort(
        "Reference artifact export verification failed.",
        "hm_integrity_error"
      )
    }
  }
  writeLines(
    c(
      "# Supply the original admitted dataset as data; load hypermodR first.",
      "reproduce <- function(data, export_directory, backend = NULL,",
      "                      artifact_root = NULL) {",
      "  saved <- hypermodR::hm_read_run(export_directory)",
      "  if (!identical(digest::digest(data, algo = 'sha256'),",
      "      saved$input_hash)) {",
      "    cli::cli_abort('Input hash mismatch.', class = 'hm_input_error')",
      "  }",
      "  if (startsWith(saved$workflow$model$model_id, 'reference:')) {",
      "    if (is.null(backend) || is.null(artifact_root)) {",
      paste0(
        "      cli::cli_abort('Supply an admitted backend and ",
        "explicit artifact root.', class = 'hm_export_error')"
      ),
      "    }",
      "    control <- saved$control",
      "    control$backend <- backend",
      "    control$artifact_root <- artifact_root",
      paste0(
        "    job <- hypermodR::hm_submit_job(saved$workflow, ",
        "data, saved$split,"
      ),
      "      control, resources = control$resources)",
      "    on.exit(hypermodR::hm_cancel_job(job), add = TRUE)",
      "    repeat {",
      "      status <- hypermodR::hm_job_status(job)",
      "      if (status$state != 'running') break",
      "      Sys.sleep(0.05)",
      "    }",
      paste0(
        "    if (status$state != 'completed') ",
        "cli::cli_abort('{status$error}', class = ",
        "'hm_export_error')"
      ),
      "    return(status$result$prediction)",
      "  }",
      paste0(
        "  fit <- hypermodR::hm_fit(saved$workflow, data, ",
        "saved$split, saved$control)"
      ),
      "  spectra <- hypermodR::hm_spectra(data)", "  idx <- saved$split$test",
      paste0(
        "  heldout <- hypermodR::hm_dataset(spectra$values[idx, ",
        ", drop = FALSE],"
      ),
      "    spectra$observations[idx, , drop = FALSE], spectra$band_map,",
      "    provenance = spectra$provenance)",
      "  hypermodR::hm_predict(fit, heldout)",
      "}",
      "",
      "read_private_fit <- function(export_directory) {",
      "  saved <- hypermodR::hm_read_run(export_directory)",
      "  fit <- saved$private_artifacts$fit",
      paste0(
        "  if (is.null(fit)) cli::cli_abort('This export does ",
        "not include a private fit.', class = 'hm_export_error')"
      ),
      "  if (startsWith(fit$workflow$model$model_id, 'reference:')) {",
      "    fit$engine$artifact_path <- file.path(",
      "      normalizePath(export_directory),",
      "      'private', 'reference-model')",
      paste0(
        "    # hm_predict verifies the bundled artifact hashes ",
        "before inference."
      ),
      "  }",
      "  fit",
      "}"
    ),
    file.path(path, "reproduce.R")
  )
  restored <- hm_read_run(path)
  if (!isTRUE(all.equal(out, restored, check.environment = FALSE))) {
    .hm_abort("Export readback failed.", "hm_integrity_error")
  }
  complete <- TRUE
  invisible(path)
}

.hm_app_export_safe <- function(x) {
  if (is.list(x)) {
    if (any(
      grepl(
        "token|password|secret|credential|api.key|authorization", names(x),
        ignore.case = TRUE
      )
    )) {
      .hm_abort(
        "Remove credentials from workflow configuration before export."
      )
    }
    invisible(lapply(x, .hm_app_export_safe))
  } else if (is.character(x) &&
    any(
      grepl(
        "Bearer |://[^/ ]+:[^/ ]+@|/tmp/|/home/.*/Downloads/[.]llmtmp/",
        x
      )
    )) {
    .hm_abort(paste0(
      "Remove private credential URLs or temporary source ",
      "paths before export."
    ))
  }
  invisible(TRUE)
}

.hm_app_strip_secrets <- function(x) {
  if (is.list(x)) {
    for (i in seq_along(x)) {
      name <- if (is.null(names(x))) {
        ""
      } else {
        names(x)[i]
      }
      x[i] <- list(if (grepl(
        "token|password|secret|credential|api.key|authorization", name,
        ignore.case = TRUE
      )) {
        "[redacted]"
      } else {
        .hm_app_strip_secrets(x[[i]])
      })
    }
  } else if (is.character(x)) {
    x <- gsub("(https?://)[^/ :]+:[^/@ ]+@", "\\1[redacted]@", x)
    x[grepl("Bearer |/tmp/|/home/.*/Downloads/[.]llmtmp/", x)] <- "[redacted]"
  }
  x
}
