#' Declare a modelling task
#' @param outcome Single outcome-column name, or NULL for clustering, anomaly,
#' reconstruction or unmixing.
#' @param type One of classification, regression, ordinal, multilabel,
#' segmentation, unmixing, reconstruction, clustering, anomaly, survival or
#' temporal.
#' @param unit Observation unit: spectrum, pixel, roi, image, sequence or
#' subject.
#' @param groups Nonempty character vector of biological/source grouping column
#' names.
#' @param reference Optional reference declaration; survival uses time/status
#' column names, unmixing may declare abundance_constraint, and temporal tasks
#' may declare horizon.
#' @return An `hm_task` list recording outcome, type, unit, unique grouping
#' columns and reference.
#' @details Segmentation requires pixel/image units; temporal tasks require
#' sequence/subject units. Declaring a task does not establish an executable
#' backend for every task/model combination.
#' @export
hm_task <- function(outcome, type, unit, groups, reference = NULL) {
  types <- c(
    "classification", "regression", "ordinal", "multilabel", "segmentation",
    "unmixing", "reconstruction", "clustering", "anomaly", "survival",
    "temporal"
  )
  if (!is.character(type) ||
        length(type) !=
          1L || is.na(type) ||
        !type %in% types) {
    .hm_abort("Unknown task type.")
  }
  if (!is.character(unit) ||
    length(unit) !=
      1L || is.na(unit) ||
    !unit %in% c(
      "spectrum", "pixel", "roi", "image", "sequence",
      "subject"
    )) {
    .hm_abort("Unknown task unit.")
  }
  if (type == "segmentation" && !unit %in% c("pixel", "image")) {
    .hm_abort("Segmentation requires spatial units.")
  }
  if (type == "temporal" && !unit %in% c("sequence", "subject")) {
    .hm_abort("Temporal tasks require sequence or subject units.")
  }
  if (!is.null(outcome)) {
    .hm_scalar_character(outcome, "outcome")
  }
  if (is.null(outcome) &&
        !type %in% c("clustering", "anomaly", "reconstruction", "unmixing")) {
    .hm_abort("This task requires an outcome.")
  }
  if (!is.character(groups) ||
        !length(groups) ||
        anyNA(groups) ||
        any(!nzchar(groups))) {
    .hm_abort("Explicit grouping columns are required.")
  }
  structure(
    list(
      outcome = outcome, type = type, unit = unit, groups = unique(groups),
      reference = reference
    ),
    class = "hm_task"
  )
}
.hm_validate_target <- function(y, task, n) {
  type <- task$type
  if (type %in% c("clustering", "anomaly", "reconstruction", "unmixing") &&
        is.null(y)) {
    return(invisible(TRUE))
  }
  if (NROW(y) !=
        n || anyNA(y)) {
    .hm_abort("Outcome shape or missingness is incompatible with task.")
  }
  if (type %in% c("classification", "segmentation") &&
        (!is.factor(y) &&
           !is.character(y))) {
    .hm_abort("Classification outcomes require explicit labels.")
  }
  if (type == "ordinal" && !is.ordered(y)) {
    .hm_abort("Ordinal outcomes require an ordered factor.")
  }
  if (type %in% c("regression", "temporal") &&
        (!is.numeric(y) ||
           !is.null(dim(y)) ||
           any(!is.finite(y)))) {
    .hm_abort("Regression/temporal outcome must be a finite numeric vector.")
  }
  if (type %in% c("multilabel", "unmixing") &&
        (!is.matrix(y) ||
           !is.numeric(y) ||
           any(!is.finite(y)))) {
    .hm_abort("This target requires a finite numeric matrix.")
  }
  if (type == "multilabel" && any(!y %in% c(0, 1))) {
    .hm_abort("Multilabel targets must be binary.")
  }
  if (type == "unmixing" && (any(y < 0) ||
    (!identical(task$reference$abundance_constraint, "nonnegative") &&
      any(
        abs(
          rowSums(y) -
            1
        ) >
          1e-08
      )))) {
    .hm_abort("Abundance targets must sum to one.")
  }
  if (type == "survival" && (!is.data.frame(y) ||
    !identical(
      names(y),
      c("time", "status")
    ) ||
    !is.numeric(y$time) ||
    any(
      !is.finite(y$time) |
        y$time < 0
    ) ||
    any(!y$status %in% c(0, 1)))) {
    .hm_abort(paste0(
      "Survival requires finite nonnegative time and binary ",
      "event status."
    ))
  }
  invisible(TRUE)
}
