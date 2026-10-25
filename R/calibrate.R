#' Freeze temperature scaling and abstention using an inner held-out partition
#' @param predictions Probabilistic predictions from fit on the inner held-out
#' calibration partition.
#' @param reference Exact source observation rows for the calibration
#' partition, in prediction order.
#' @param split Validated inner train/test split partitioning outer training.
#' @param data Original source data used for fitting and split validation.
#' @param temperature_grid Positive finite candidate temperatures.
#' @param abstention Confidence threshold in `[0,1]` frozen into the calibrator.
#' @param fit Training-only `hm_fit` that generated the supplied predictions.
#' @param outer_split Validated outer split whose training rows are partitioned
#' by split.
#' @return An `hm_calibration` containing selected temperature, abstention
#' threshold, class map, fit/calibration identities, reference hash and grid
#' scores.
#' @details Selects the first temperature minimizing calibration log loss.
#' Requires full class support and verifies the supplied predictions against
#' the fit; outer test outcomes cannot enter calibration.
#' @export
hm_calibrate <- function(
  predictions, reference, split, data, temperature_grid = c(0.5, 1, 2),
  abstention = 0, fit, outer_split
) {
  p <- predictions
  if (!inherits(p, "hm_prediction") ||
        is.null(p$probabilities) ||
        !identical(p$observation_id, reference$observation_id)) {
    .hm_abort("Aligned probabilistic predictions required.")
  }
  d <- .hm_model_data(data)
  split <- .hm_validate_fit_split(d, p$task, split)
  outer_split <- .hm_validate_fit_split(d, p$task, outer_split)
  if (!inherits(fit, "hm_fit") ||
    !identical(fit$workflow$task, p$task) ||
    !setequal(fit$training, split$train) ||
    !setequal(
      c(split$train, split$test),
      outer_split$train
    ) ||
    length(intersect(split$test, outer_split$test))) {
    .hm_abort(
      paste0(
        "Calibration fit and calibration units must partition ",
        "outer training only."
      )
    )
  }
  subset <- list(
    values = d$values[split$test, , drop = FALSE],
    observations = d$observations[split$test, ,
      drop = FALSE
    ], band_map = d$band_map, provenance = d$provenance
  )
  if (!identical(
    unname(fit$provenance$objects["data"]),
    .hm_hash(data)
  )) {
    .hm_abort(paste0(
      "Calibration source or reference revision differs from ",
      "the fitted data."
    ))
  }
  expected <- hm_predict(fit, subset)
  if (!identical(expected$probabilities, p$probabilities)) {
    .hm_abort(paste0(
      "Calibration predictions must come from the supplied ",
      "training-only fit."
    ))
  }
  if (!length(split$test) ||
        !identical(p$observation_id,
                   d$observations$observation_id[split$test]) ||
        !identical(reference, d$observations[split$test, , drop = FALSE])) {
    .hm_abort(paste0(
      "Calibration must use the declared held-out training ",
      "partition."
    ))
  }
  if (!is.numeric(temperature_grid) ||
    !length(temperature_grid) ||
    any(
      !is.finite(temperature_grid) |
        temperature_grid <= 0
    ) ||
    length(abstention) !=
      1 || !is.finite(abstention) ||
    abstention < 0 || abstention > 1) {
    .hm_abort("Invalid temperature or abstention controls.")
  }
  y <- reference[[p$task$outcome]]
  if (anyNA(y) ||
        any(!y %in% p$class_map) ||
        !setequal(y, p$class_map)) {
    .hm_abort("Calibration requires full class support.")
  }
  scores <- vapply(
    temperature_grid, function(t) {
      q <- .hm_temperature(p$probabilities, t)
      -mean(
        log(
          pmax(
            q[cbind(
              seq_along(y),
              match(y, p$class_map)
            )],
            .Machine$double.xmin
          )
        )
      )
    }, numeric(1)
  )
  structure(
    list(
      temperature = temperature_grid[which.min(scores)],
      abstention = abstention, class_map = p$class_map,
      fit_ids = d$observations$observation_id[split$train],
      calibration_ids = p$observation_id, reference_hash = .hm_hash(
        reference
      ),
      scores = scores, method = "held-out temperature scaling"
    ),
    class = "hm_calibration"
  )
}
.hm_temperature <- function(p, t) {
  z <- log(pmax(p, .Machine$double.xmin)) / t
  z <- exp(z - apply(z, 1, max))
  z / rowSums(z)
}
#' Apply a frozen calibrator without learning from new outcomes
#' @param calibration Frozen `hm_calibration`.
#' @param predictions Probabilistic `hm_prediction` with the identical class
#' map.
#' @return An updated `hm_prediction` with scaled probabilities,
#' maximum-probability labels, logical abstain vector and calibration
#' provenance.
#' @details Does not learn from new outcomes. Abstention marks low confidence
#' without deleting rows.
#' @export
hm_apply_calibration <- function(calibration, predictions) {
  if (!inherits(calibration, "hm_calibration") ||
        !inherits(predictions, "hm_prediction") ||
        is.null(predictions$probabilities) ||
        !identical(calibration$class_map, predictions$class_map)) {
    .hm_abort("Incompatible calibration class map.")
  }
  p <- predictions
  p$probabilities <- .hm_temperature(p$probabilities, calibration$temperature)
  p$values <- p$class_map[max.col(p$probabilities, ties.method = "first")]
  p$abstain <- apply(p$probabilities, 1, max) <
    calibration$abstention
  p$provenance$calibration <- calibration
  p
}
