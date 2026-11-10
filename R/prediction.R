#' Validate model predictions
#' @param values Prediction vector, or observation-by-output matrix for
#' multilabel, unmixing and reconstruction.
#' @param task An `hm_task`.
#' @param observation_id Unique nonmissing character identifiers in prediction
#' row order.
#' @param probabilities Optional numeric observation-by-class matrix in `[0,1]`,
#' with columns exactly matching class_map; rows sum to one except for
#' multilabel tasks.
#' @param class_map Exact character class/label order, required for categorical
#' and multilabel predictions.
#' @param provenance List of source, fit and transformation provenance.
#' @return An `hm_prediction` list retaining values, task, identities,
#' probabilities, class map and provenance.
#' @details Categorical labels must belong to the declared map. Unmixing
#' outputs are nonnegative and sum to one unless task reference explicitly
#' declares nonnegative-only abundances. This validation does not establish
#' predictive accuracy or calibrated probabilities. Numeric scalar tasks
#' (including survival risk) require finite numeric vectors; matrix tasks
#' require finite numeric matrices. Named matrix outputs retain column identity.
#' @examples
#' task <- hm_task("y", "classification", "spectrum", "subject_id")
#' reference <- data.frame(
#'   observation_id = paste0("o", 1:6),
#'   y = c("a", "a", "b", "b", "a", "b"),
#'   subject_id = c("s1", "s1", "s1", "s2", "s2", "s2"),
#'   acquisition_id = paste0("a", 1:6)
#' )
#' probabilities <- cbind(
#'   a = c(0.8, 0.3, 0.7, 0.1, 0.6, 0.4),
#'   b = c(0.2, 0.7, 0.3, 0.9, 0.4, 0.6)
#' )
#' prediction <- hm_prediction(
#'   c("a", "b", "a", "b", "a", "b"), task, reference$observation_id,
#'   probabilities, class_map = c("a", "b")
#' )
#' prediction
#' summary(prediction)
#'
#' regression <- hm_task("response", "regression", "spectrum", "subject_id")
#' numeric_prediction <- hm_prediction(c(1.2, 0.8, 1.9), regression,
#'                                     c("o1", "o2", "o3"))
#' plot(numeric_prediction)
#' @export
hm_prediction <- function(
  values, task, observation_id, probabilities = NULL, class_map = NULL,
  provenance = list()
) {
  if (!inherits(task, "hm_task") ||
        !is.character(observation_id) ||
        anyNA(observation_id) ||
        anyDuplicated(observation_id)) {
    .hm_abort("Invalid prediction identities or task.")
  }
  n <- length(observation_id)
  if (NROW(values) !=
        n) {
    .hm_abort("Prediction row count differs from identities.")
  }
  .hm_numeric_prediction(values, task)
  if (is.numeric(values) &&
        any(!is.finite(values))) {
    .hm_abort(paste0(
      "Predictions must be finite; unavailable outputs must ",
      "be explicit."
    ))
  }
  if (task$type %in% c("classification", "ordinal", "segmentation")) {
    if (is.null(class_map) ||
      !is.character(class_map) ||
      anyNA(class_map) ||
      anyDuplicated(class_map) ||
      any(
        !as.character(values) %in%
          class_map
      )) {
      .hm_abort("Exact class map is required.")
    }
  }
  if (!is.null(probabilities)) {
    if (!is.matrix(probabilities) ||
      !is.numeric(probabilities) ||
      nrow(probabilities) !=
        n || !identical(
      colnames(probabilities),
      class_map
    ) ||
      any(!is.finite(probabilities)) ||
      any(probabilities < 0 | probabilities > 1) ||
      (task$type != "multilabel" && any(
        abs(
          rowSums(probabilities) -
            1
        ) >
          1e-08
      ))) {
      .hm_abort("Invalid class probabilities.")
    }
  }
  if (task$type %in% c("multilabel", "unmixing", "reconstruction") &&
        (!is.matrix(values) || !is.numeric(values))) {
    .hm_abort("This task requires a numeric matrix output.",
              "hm_prediction_error")
  }
  if (task$type == "multilabel" && (any(!values %in% c(0, 1)) ||
    is.null(class_map) ||
    !identical(
      colnames(values),
      class_map
    ))) {
    .hm_abort(paste0(
      "Multilabel outputs require binary values and an exact ",
      "named label map."
    ))
  }
  if (task$type == "unmixing" && (any(values < 0) ||
    (!identical(task$reference$abundance_constraint, "nonnegative") &&
      any(
        abs(
          rowSums(values) -
            1
        ) >
          1e-08
      )))) {
    .hm_abort("Unmixing abundances must be nonnegative and sum to one.")
  }
  structure(
    list(
      values = values, task = task, observation_id = observation_id,
      probabilities = probabilities,
      class_map = class_map, provenance = provenance
    ),
    class = "hm_prediction"
  )
}
#' @rdname hm_prediction
#' @param x An object returned by `hm_prediction()`.
#' @param ... Additional arguments; forwarded to graphics for plotting,
#' otherwise unused.
#' @return The input object invisibly.
#' @export
print.hm_prediction <- function(x, ...) {
  cat(
    "<hm_prediction>", x$task$type, length(x$observation_id),
    "observations\n"
  )
  invisible(x)
}
#' @rdname hm_prediction
#' @param object An object returned by `hm_prediction()`.
#' @param ... Additional arguments; forwarded to graphics for plotting,
#' otherwise unused.
#' @return A list of descriptive fields.
#' @export
summary.hm_prediction <- function(object, ...) {
  list(
    type = object$task$type, n = length(object$observation_id),
    probabilities = !is.null(object$probabilities)
  )
}
#' @rdname hm_prediction
#' @param x An object returned by `hm_prediction()`.
#' @param ... Additional arguments; forwarded to graphics for plotting,
#' otherwise unused.
#' @return The graphics plotting return value, invisibly.
#' @export
plot.hm_prediction <- function(x, ...) {
  if (!is.numeric(x$values)) {
    .hm_abort(
      "Numeric predictions required for this plot.",
      "hm_unsupported_error"
    )
  }
  graphics::plot(x$values, ...)
}

.hm_numeric_prediction <- function(values, task) {
  if (is.matrix(values) && !is.null(colnames(values)) &&
        (anyNA(colnames(values)) || any(!nzchar(colnames(values))) ||
           anyDuplicated(colnames(values)))) {
    .hm_abort("Matrix output column identities must be unique and complete.",
              "hm_prediction_error")
  }
  if (task$type %in% c("multilabel", "unmixing", "reconstruction") &&
        (!is.matrix(values) || !is.numeric(values) ||
           any(!is.finite(values)))) {
    .hm_abort("This task requires a finite numeric matrix output.",
              "hm_prediction_error")
  }
  if (task$type %in% c("regression", "temporal", "survival", "anomaly") &&
        (!is.numeric(values) || !is.null(dim(values)) ||
           any(!is.finite(values)))) {
    .hm_abort("This task requires a finite numeric prediction vector.",
              "hm_prediction_error")
  }
}
