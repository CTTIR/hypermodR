#' Summarize fitted coefficients in a standard table
#' @param x An `hm_fit`.
#' @param conf.int Include conditional confidence or posterior intervals.
#' @param conf.level Interval level strictly between zero and one.
#' @param ... Additional arguments; currently unused.
#' @return A data frame with `term`, `estimate`, `std.error`, `statistic`,
#'   `p.value`, and optional `conf.low` and `conf.high`. Estimates use the
#'   fitted
#'   recipe coordinates. Bayesian rows are explicitly posterior summaries.
#' @details Frequentist uncertainty is implemented for LM and GLM. Other
#'   coefficient engines retain unavailable uncertainty as NA. Intervals do not
#'   include model selection, preprocessing selection or biological transport
#'   uncertainty. Use `coef(x, scale = "original")` for an admitted affine map.
#' @importFrom generics tidy augment glance
#' @export
# Preserve the standard broom-compatible generic argument names.
tidy.hm_fit <- function(
  x,
  conf.int = FALSE, # nolint: object_name_linter.
  conf.level = 0.95, # nolint: object_name_linter.
  ...
) {
  if (!is.logical(conf.int) || length(conf.int) != 1L || is.na(conf.int) ||
        !is.numeric(conf.level) || length(conf.level) != 1L ||
        !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) {
    .hm_abort("Invalid interval declaration.")
  }
  beta <- stats::coef(x)
  if (!is.numeric(beta) || !is.null(dim(beta))) {
    .hm_abort(
      "This engine needs a model-specific coefficient table.",
      "hm_unsupported_error"
    )
  }
  term <- names(beta)
  if (is.null(term)) term <- paste0("coefficient_", seq_along(beta))
  out <- data.frame(
    term = term, estimate = as.numeric(beta),
    std.error = NA_real_, statistic = NA_real_, p.value = NA_real_
  )
  id <- x$engine$model_id
  critical <- stats::qnorm((1 + conf.level) / 2)
  if (id %in% c("lm", "glm")) {
    tab <- summary(x$engine$object)$coefficients
    index <- match(out$term, rownames(tab))
    out$std.error <- tab[index, 2L]
    out$statistic <- tab[index, 3L]
    out$p.value <- tab[index, 4L]
    if (id == "lm") {
      critical <- stats::qt((1 + conf.level) / 2,
        df = x$engine$object$df.residual
      )
    }
    out$uncertainty <- "conditional frequentist"
  } else if (id == "bayes_conjugate") {
    out$std.error <- sqrt(diag(x$engine$object$covariance))
    out$uncertainty <- "conditional Gaussian posterior"
  } else {
    out$uncertainty <- "unavailable for this engine"
  }
  if (conf.int) {
    out$conf.low <- out$estimate - critical * out$std.error
    out$conf.high <- out$estimate + critical * out$std.error
  }
  out
}

#' Attach predictions to observation identities
#' @param x An `hm_fit`.
#' @param newdata Explicit dataset accepted by `hm_predict()`.
#' @param ... Arguments passed to prediction, including optional managed
#'   workers.
#' @return Observation metadata with `.fitted`, and `.resid` for scalar numeric
#'   outcomes. Matrix outputs remain one explicit list column.
#' @export
augment.hm_fit <- function(x, newdata, ...) {
  data <- .hm_model_data(newdata)
  predictions <- hm_predict(x, newdata, ...)
  out <- data$observations
  if (any(c(".fitted", ".resid") %in% names(out))) {
    .hm_abort("Observation metadata uses reserved augmentation columns.")
  }
  out$.fitted <- if (is.matrix(predictions$values)) {
    I(lapply(seq_len(nrow(predictions$values)), function(i) {
      predictions$values[i, , drop = FALSE]
    }))
  } else {
    predictions$values
  }
  outcome <- x$workflow$task$outcome
  if (!is.null(outcome) && is.numeric(out[[outcome]]) &&
        is.numeric(predictions$values) && is.null(dim(predictions$values))) {
    out$.resid <- out[[outcome]] - predictions$values
  }
  out
}

#' Inspect model and training scope
#' @param x An `hm_fit`.
#' @param ... Currently unused.
#' @return One row recording model, task, training count, input track and the
#'   absence of inherited scientific qualification.
#' @export
glance.hm_fit <- function(x, ...) {
  data.frame(
    model = x$engine$model_id, task = x$workflow$task$type,
    nobs = length(x$training), input_track = x$recipe$track,
    scientific_qualification = FALSE
  )
}
