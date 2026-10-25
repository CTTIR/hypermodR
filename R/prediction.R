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
#' predictive accuracy or calibrated probabilities.
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
        !is.matrix(values)) {
    .hm_abort("This task requires a matrix output.")
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
