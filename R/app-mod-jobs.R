.hm_app_parameters <- function(text) {
  if (!is.character(text) ||
        length(text) !=
          1L || !grepl("^\\s*\\{", text)) {
    .hm_abort("Parameters must be a JSON object.")
  }
  value <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(e) .hm_abort("Parameters must be valid JSON.")
  )
  if (!is.list(value) ||
        (length(value) &&
           (is.null(names(value)) ||
              any(!nzchar(names(value))) ||
              anyDuplicated(names(value))))) {
    .hm_abort(
      "Parameters must have unique named JSON object fields."
    )
  }
  if (!length(value))
    value <- list()
  value
}

.hm_app_validate_backend <- function(backend, model, memory_mb) {
  if (!inherits(backend, "hm_backend") ||
        !identical(backend$plan$model_id, model)) {
    .hm_abort(
      "An installed backend receipt matching the reference model is required."
    )
  }
  if (!is.numeric(memory_mb) ||
        length(memory_mb) !=
          1L || !is.finite(memory_mb) ||
        memory_mb < 4096) {
    .hm_abort(
      paste0(
        "CPU references require an explicitly declared memory ",
        "limit of at least 4096 MiB."
      ),
      "hm_resource_error"
    )
  }
  .hm_validate_plan(backend$plan)
  invisible(TRUE)
}

#' Submit an isolated, cancellable modelling job
#' @param workflow Validated modelling workflow.
#' @param data Spectral dataset.
#' @param split One explicit grouped train/test fold.
#' @param control Fitting controls.
#' @param resources Execution resource declaration.
#' @return An owned job handle; poll with [hm_job_status()].
#' @details Input object size is checked before launching a bounded Linux CPU
#' child. The launcher applies an inherited-affinity subset, per-process
#' virtual address-space limit, numerical thread settings and external elapsed
#' timeout; see hm_resources for limits and network policy. Poll
#' hm_job_status() to collect completion or failure. Handles belong to the
#' creating R session.
#' @export
hm_submit_job <- function(
  workflow, data, split, control = list(), resources = hm_resources()
) {
  .hm_require("processx")
  if (!methods::is(data, "HMDataset")) {
    .hm_abort("Jobs require an admitted HMDataset.")
  }
  if (!inherits(resources, "hm_resources") ||
        !identical(resources$device, "cpu")) {
    .hm_abort(
      "This R job route requires a declared CPU resource budget."
    )
  }
  .hm_validate_fit_split(
    .hm_model_data(data),
    workflow$task, split
  )
  if (as.numeric(utils::object.size(data)) >
        resources$memory_mb * 1024^2) {
    .hm_abort(
      "Input exceeds the declared memory admission bound.",
      "hm_resource_error"
    )
  }
  if (!is.null(control$backend_handle)) {
    .hm_abort(
      paste0(
        "Job controls cannot contain a live backend handle; ",
        "supply an installed backend receipt."
      )
    )
  }
  if (startsWith(workflow$model$model_id, "reference:")) {
    .hm_app_validate_backend(
      control$backend, workflow$model$model_id,
      resources$memory_mb
    )
    .hm_scalar_character(control$artifact_root, "artifact_root")
    dir.create(
      control$artifact_root, recursive = TRUE,
      showWarnings = FALSE
    )
    .hm_no_symlink(control$artifact_root)
    control$artifact_path <- tempfile(
      "learned-",
      tmpdir = normalizePath(control$artifact_root, mustWork = TRUE)
    )
  }
  control$resources <- resources
  job <- new.env(parent = emptyenv())
  job$id <- .hm_hash(list(Sys.time(), Sys.getpid(), tempfile()))
  job$started <- proc.time()[[3L]]
  job$resources <- resources
  job$input_hash <- digest::digest(data, algo = "sha256")
  job$prediction_ids <- data@observations$observation_id[split$test]
  job$owner_pid <- Sys.getpid()
  job$state <- "running"
  job$result <- NULL
  job$error <- NULL
  job$process <- .hm_resource_r_bg(
    function(workflow, data, split, control) {
      handle <- NULL
      if (startsWith(workflow$model$model_id, "reference:")) {
        handle <- hypermodR::hm_start_backend(
          control$backend, model = workflow$model$model_id,
          resources = control$resources
        )
        on.exit(
          hypermodR::hm_stop_backend(handle),
          add = TRUE
        )
        control$backend_handle <- handle
      }
      fit <- hypermodR::hm_fit(workflow, data, split, control)
      spectra <- hypermodR::hm_spectra(data)
      test <- hypermodR::hm_dataset(
        spectra$values[split$test, , drop = FALSE],
        spectra$observations[split$test,
                             , drop = FALSE], spectra$band_map,
        provenance = spectra$provenance
      )
      prediction <- hypermodR::hm_predict(
        fit, test, control = if (is.null(handle)) {
          list()
        } else {
          list(
            backend_handle = handle, timeout = control$resources$timeout
          )
        }
      )
      evaluation <- hypermodR::hm_evaluate(prediction, test@observations)
      control$backend_handle <- NULL
      list(
        fit = fit, prediction = prediction,
        evaluation = evaluation, workflow = workflow,
        split = split, control = control,
        input_hash = digest::digest(data, algo = "sha256")
      )
    }, args = list(
      workflow = workflow, data = data,
      split = split, control = control
    ),
    resources = resources
  )
  class(job) <- "hm_job"
  job
}

#' Poll a modelling job
#' @param job Owned job handle.
#' @return State, elapsed time, result only after successful completion, and
#' error.
#' @export
hm_job_status <- function(job) {
  if (!inherits(job, "hm_job")) {
    .hm_abort("Expected an owned job handle.")
  }
  .hm_resource_owned(job)
  elapsed <- proc.time()[[3L]] - job$started
  if (job$state == "running") {
    if (job$process$is_alive() && elapsed >
          job$resources$timeout) {
      hm_cancel_job(job)
      job$state <- "failed"
      job$error <- "The modelling job exceeded its time budget."
    } else if (!job$process$is_alive()) {
      result <- tryCatch(
        job$process$get_result(), error = function(e) e
      )
      if (inherits(result, "error")) {
        job$state <- "failed"
        job$error <- .hm_app_safe_error(result)
      } else if (!is.list(result) ||
        !inherits(result$fit, "hm_fit") ||
        !inherits(result$prediction, "hm_prediction") ||
        !inherits(result$evaluation, "hm_evaluation") ||
        !identical(result$input_hash, job$input_hash) ||
        !identical(
          result$prediction$observation_id,
          job$prediction_ids
        )) {
        job$state <- "failed"
        job$error <- "Worker returned incomplete output."
      } else {
        job$result <- result
        job$state <- "completed"
      }
    }
  }
  list(
    state = job$state, elapsed = elapsed,
    result = if (job$state == "completed") {
      job$result
    } else {
      NULL
    }, error = job$error
  )
}

#' Cancel only an owned modelling job
#' @param job Owned job handle.
#' @return Confirmed job state, invisibly.
#' @export
hm_cancel_job <- function(job) {
  if (!inherits(job, "hm_job")) {
    .hm_abort("Expected an owned job handle.")
  }
  .hm_resource_owned(job)
  if (job$state != "running") {
    return(invisible(job$state))
  }
  if (job$process$is_alive()) {
    job$process$kill_tree()
    job$process$wait(timeout = 5000L)
  }
  if (job$process$is_alive()) {
    .hm_abort(
      "Cancellation is not yet acknowledged.",
      "hm_process_error"
    )
  }
  job$state <- "cancelled"
  job$result <- NULL
  invisible(job$state)
}

.hm_app_engine_dependency <- function(id) {
  dependencies <- c(
    glmnet = "glmnet", gam = "mgcv", mixed = "lme4",
    pls = "pls", brms = "brms", lda = "MASS",
    qda = "MASS", naive_bayes = "e1071", svm = "e1071",
    rf = "ranger", xgboost = "xgboost", clm = "ordinal",
    ica = "fastICA", fcls = "quadprog", coxph = "survival",
    pam = "cluster", dbscan = "dbscan", isolation_forest = "isotree",
    one_class_svm = "kernlab"
  )
  dependency <- unname(dependencies[id])
  if (is.na(dependency))
    NULL else dependency
}

.hm_app_engine_status <- function(id) {
  if (startsWith(id, "reference:")) {
    return(
      "Spectral CPU route; admitted installed runtime required"
    )
  }
  dependency <- .hm_app_engine_dependency(id)
  if (!is.null(dependency) &&
        !requireNamespace(dependency, quietly = TRUE)) {
    return(
      paste(
        "Unavailable: missing R dependency",
        dependency
      )
    )
  }
  "R engine available; model-specific parameters validated by hm_fit"
}
