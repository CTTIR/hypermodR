#' Create an isolated workbench state
#' @return An independent session state environment.
#' @export
hm_app_state <- function() {
  state <- new.env(parent = emptyenv())
  state$status <- "empty"
  state$revision <- 0L
  state$dataset <- NULL
  state$workflow <- NULL
  state$result <- NULL
  state$result_revision <- NULL
  state$job <- NULL
  state$error <- NULL
  state
}

.hm_app_transition <- function(state, next_state) {
  transitions <- list(
    empty = c("imported", "failed"),
    imported = c("configuring", "ready", "failed"),
    configuring = c("imported", "ready", "failed"),
    ready = c("queued", "configuring", "imported", "failed"),
    queued = c("running", "cancelled", "failed"),
    running = c("completed", "failed", "cancelled"),
    completed = c("ready", "queued", "configuring", "imported", "failed"),
    failed = c("imported", "configuring", "ready", "queued"),
    cancelled = c("ready", "queued", "configuring", "imported", "failed")
  )
  if (identical(state$status, next_state) &&
        next_state %in% c("imported", "ready", "configuring")) {
    return(invisible(state))
  }
  if (!next_state %in% transitions[[state$status]]) {
    .hm_abort("Invalid workbench state transition.", "hm_state_error")
  }
  state$status <- next_state
  invisible(state)
}

.hm_app_import <- function(state, dataset) {
  if (!methods::is(dataset, "HMDataset")) {
    .hm_abort("Import an HMDataset or HMSequence.")
  }
  if (state$status %in% c("queued", "running")) {
    .hm_abort("Cancel the current run before importing.")
  }
  methods::validObject(dataset)
  if (identical(state$dataset, dataset)) {
    return(invisible(state))
  }
  state$dataset <- dataset
  state$revision <- state$revision + 1L
  state$workflow <- NULL
  state$error <- NULL
  .hm_app_transition(state, "imported")
  invisible(state)
}

.hm_app_configure <- function(state, workflow) {
  if (is.null(state$dataset) ||
        !inherits(workflow, "hm_workflow")) {
    .hm_abort("Import data and configure a workflow.")
  }
  if (state$status %in% c("queued", "running")) {
    .hm_abort("Cancel the current run before reconfiguring.")
  }
  if (!identical(state$workflow, workflow)) {
    state$revision <- state$revision + 1L
  }
  state$workflow <- workflow
  .hm_app_transition(state, "ready")
  invisible(state)
}

.hm_app_current <- function(state) {
  !is.null(state$result) &&
    identical(state$result_revision, state$revision) &&
    identical(state$status, "completed")
}

#' Generate an explicitly synthetic grouped spectral example
#' @param seed Nonnegative integer seed.
#' @return An HMDataset with synthetic labels, subject groups and eight bands.
#' @export
hm_example <- function(seed = 42L) {
  .hm_seed(
    seed,
    {
      n <- 96L
      latent <- rep(
        seq(-1, 1, length.out = 8L),
        12L
      )
      wave <- seq(500, 850, length.out = 8L)
      values <- outer(latent, seq(0.2, 1, length.out = 8L)) +
        matrix(
          stats::rnorm(n * 8L, sd = 0.05),
          n, 8L
        )
      obs <- data.frame(
        observation_id = paste0("spectrum_", seq_len(n)),
        subject_id = rep(
          paste0("synthetic_subject_", seq_len(12L)),
          each = 8L
        ),
        acquisition_id = paste0("synthetic_acquisition_", seq_len(n)),
        response = 1 + 2 * latent + stats::rnorm(n, sd = 0.08),
        class = ifelse(latent > 0, "high", "low")
      )
      hm_dataset(
        values, obs, data.frame(wavelength = wave),
        provenance = list(
          synthetic = TRUE, source = "hm_example", seed = seed,
          radiometric_unit = "arbitrary",
          calibration = "synthetic"
        )
      )
    }
  )
}
