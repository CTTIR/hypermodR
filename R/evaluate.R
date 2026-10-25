#' Evaluate predictions against an explicitly aligned reference
#' @param predictions An `hm_prediction`.
#' @param reference Data frame with observation_id in exactly prediction order
#' and task outcome/reference columns. Reconstruction targets are supplied as
#' the numeric matrix attribute target.
#' @param estimand Either pooled or equal_class_subject_acquisition.
#' @param resampling Optional named argument list passed to `hm_bootstrap()`.
#' @param hierarchy Two column names giving biological unit then acquisition
#' for hierarchical categorical accuracy.
#' @return An `hm_evaluation` list containing named numeric metrics,
#' task-specific per-class support/confusion, predictions, reference, estimand,
#' hierarchy and optional uncertainty.
#' @details The hierarchical estimand averages observations within
#' class/acquisition, acquisitions within class/subject, subjects within class,
#' then classes equally. Missing class support produces unavailable macro
#' metrics. Anomaly scores are not calibrated risks; reconstruction spectral
#' angles are radians. Survival uses comparable event-time pairs. Evaluation
#' does not fit models or repair reference alignment.
#' @export
hm_evaluate <- function(
  predictions, reference, estimand = "pooled", resampling = NULL,
  hierarchy = c("subject_id", "acquisition_id")
) {
  p <- predictions
  if (!inherits(p, "hm_prediction") ||
        !is.data.frame(reference) ||
        !identical(p$observation_id, reference$observation_id)) {
    .hm_abort("Reference identities must match prediction order exactly.")
  }
  task <- p$task
  y <- if (is.null(task$outcome)) {
    NULL
  } else {
    reference[[task$outcome]]
  }
  if (is.null(y) &&
    !task$type %in% c(
      "survival", "reconstruction", "clustering",
      "anomaly"
    )) {
    .hm_abort("Reference outcome is missing.")
  }
  if (!task$type %in% c("survival", "reconstruction")) {
    .hm_validate_target(y, task, length(p$observation_id))
  }
  categorical <- task$type %in% c("classification", "ordinal", "segmentation")
  if (categorical) {
    classes <- p$class_map
    if (anyNA(y) ||
      any(
        !as.character(y) %in%
          classes
      )) {
      .hm_abort("Reference labels fall outside the full class map.")
    }
    cm <- table(
      factor(y, levels = classes),
      factor(p$values, levels = classes)
    )
    tp <- diag(cm)
    support <- rowSums(cm)
    denominator <- support + colSums(cm) -
      tp
    per <- data.frame(
      class = classes, support = as.integer(support),
      recall = ifelse(support > 0, tp / support, NA_real_),
      iou = ifelse(denominator > 0, tp / denominator, NA_real_)
    )
    loss <- as.numeric(
      as.character(y) !=
        as.character(p$values)
    )
    metrics <- c(
      accuracy = mean(1 - loss),
      balanced_accuracy = if (any(support == 0)) {
        NA_real_
      } else {
        mean(
          per$recall
        )
      },
      mean_iou = if (any(support == 0)) NA_real_ else mean(per$iou)
    )
    if (!is.null(p$probabilities)) {
      truth <- outer(
        as.character(y),
        classes, "=="
      ) *
        1
      metrics <- c(
        metrics,
        brier = mean(rowSums((p$probabilities - truth)^2)),
        log_loss = -mean(
          log(
            pmax(
              p$probabilities[cbind(
                seq_along(y),
                match(y, classes)
              )],
              .Machine$double.xmin
            )
          )
        )
      )
    }
  } else if (task$type == "multilabel") {
    v <- p$values
    if (!is.matrix(y) ||
      !is.matrix(v) ||
      !identical(
        dim(y),
        dim(v)
      ) ||
      !identical(
        colnames(y),
        colnames(v)
      ) ||
      is.null(colnames(y)) ||
      anyNA(v) ||
      any(!v %in% c(0, 1))) {
      .hm_abort("Named, shape-matched binary multilabel matrices required.")
    }
    tp <- colSums(y == 1 & v == 1)
    fp <- colSums(y == 0 & v == 1)
    fn <- colSums(y == 1 & v == 0)
    support <- colSums(y)
    denom <- 2 * tp + fp + fn
    f1 <- ifelse(support > 0 & denom > 0, 2 * tp / denom, NA_real_)
    metrics <- c(
      hamming_loss = mean(v != y),
      subset_accuracy = mean(
        rowSums(v != y) ==
          0
      ),
      micro_f1 = if (sum(denom) >
                       0) {
        2 * sum(tp) / sum(denom)
      } else {
        NA_real_
      }, macro_f1 = if (anyNA(f1)) NA_real_ else mean(f1)
    )
    per <- data.frame(
      class = colnames(y),
      support = support, f1 = f1
    )
    cm <- NULL
    loss <- rowMeans(v != y)
    if (!is.null(p$probabilities)) {
      q <- p$probabilities
      if (!is.matrix(q) ||
        !identical(
          dim(q),
          dim(y)
        ) ||
        !identical(
          colnames(q),
          colnames(y)
        ) ||
        any(!is.finite(q)) ||
        any(q < 0 | q > 1)) {
        .hm_abort(
          paste0(
            "Independent per-label probabilities must be finite in ",
            "[0,1]; no sum-one constraint applies."
          )
        )
      }
      metrics <- c(
        metrics,
        brier = mean((q - y)^2),
        log_loss = -mean(
          y * log(pmax(q, .Machine$double.xmin)) +
            (1 - y) * log(pmax(1 - q, .Machine$double.xmin))
        )
      )
    }
  } else if (task$type == "clustering") {
    if (is.null(y) ||
          anyNA(y) ||
          anyNA(p$values)) {
      .hm_abort(
        paste0(
          "External clustering reference required; labels do not ",
          "imply biological equivalence."
        )
      )
    }
    tab <- table(y, p$values)
    choose2 <- function(x) {
      x * (x - 1) / 2
    }
    n <- sum(tab)
    total <- choose2(n)
    if (n < 2) {
      .hm_abort("ARI requires at least two cases.")
    }
    a <- sum(choose2(rowSums(tab)))
    b <- sum(choose2(colSums(tab)))
    observed <- sum(choose2(tab))
    expected <- a * b / total
    denominator <- (a + b) / 2 - expected
    ari <- if (abs(denominator) <
                 .Machine$double.eps) {
      as.numeric(a == b && observed == a)
    } else {
      (observed - expected) / denominator
    }
    metrics <- c(adjusted_rand_index = ari)
    per <- cm <- loss <- NULL
  } else if (task$type == "anomaly") {
    if (is.null(y) ||
          !is.numeric(y) ||
          anyNA(y) ||
          any(!y %in% c(0, 1)) ||
          !is.numeric(p$values) ||
          any(!is.finite(p$values))) {
      .hm_abort(
        paste0(
          "Binary anomaly reference and finite anomaly scores ",
          "required; scores are not calibrated risks."
        )
      )
    }
    positive <- sum(y == 1)
    negative <- sum(y == 0)
    auc <- ap <- NA_real_
    if (positive > 0 && negative > 0) {
      auc <- (sum(
        rank(p$values, ties.method = "average")[y ==
                                                  1]
      ) -
        positive * (positive + 1) / 2) / (positive * negative)
      thresholds <- sort(
        unique(p$values),
        decreasing = TRUE
      )
      recall <- precision <- numeric(length(thresholds))
      for (i in seq_along(thresholds)) {
        keep <- p$values >= thresholds[i]
        recall[i] <- sum(y[keep] == 1) / positive
        precision[i] <- mean(y[keep] == 1)
      }
      ap <- sum(
        diff(c(0, recall)) *
          precision
      )
    }
    metrics <- c(roc_auc = auc, average_precision = ap)
    per <- data.frame(
      class = c("normal", "anomaly"),
      support = c(negative, positive)
    )
    cm <- loss <- NULL
  } else if (task$type == "survival") {
    ref <- task$reference
    time <- reference[[ref$time]]
    status <- reference[[ref$status]]
    if (anyNA(time) ||
          any(!is.finite(time)) ||
          any(!status %in% c(0, 1))) {
      .hm_abort("Invalid survival reference.")
    }
    pairs <- which(
      outer(time, time, "<") &
        outer(
          status, rep(1, length(status)),
          "*"
        ) ==
          1,
      arr.ind = TRUE
    )
    concordance <- if (nrow(pairs)) {
      mean(
        (p$values[pairs[, 1]] > p$values[pairs[, 2]]) + 0.5 * (p$values[pairs[
          ,
          1
        ]] == p$values[pairs[, 2]])
      )
    } else {
      NA_real_
    }
    metrics <- c(concordance = concordance, comparable_pairs = nrow(pairs))
    loss <- NULL
    per <- cm <- NULL
  } else if (task$type %in% c(
    "regression", "temporal", "unmixing",
    "reconstruction"
  )) {
    if (task$type == "reconstruction") {
      y <- attr(reference, "target")
    }
    if (!is.numeric(y) ||
      !identical(
        dim(y),
        dim(p$values)
      ) ||
      length(y) !=
        length(p$values) ||
      any(!is.finite(y))) {
      .hm_abort("Finite shape-matched numeric reference required.")
    }
    error <- p$values - y
    loss <- if (is.matrix(error)) {
      rowMeans(error^2)
    } else {
      error^2
    }
    metrics <- c(
      rmse = sqrt(mean(error^2)),
      mae = mean(abs(error)),
      bias = mean(error)
    )
    per <- cm <- NULL
    if (task$type == "reconstruction") {
      if (!is.matrix(y)) {
        .hm_abort("Reconstruction spectral angles require matrix spectra.")
      }
      norms <- sqrt(
        rowSums(y^2) *
          rowSums(p$values^2)
      )
      valid <- norms > 0
      angles <- rep(NA_real_, nrow(y))
      angles[valid] <- acos(
        pmax(
          -1, pmin(
            1, rowSums(y[valid, , drop = FALSE] * p$values[valid, ,
                         drop = FALSE
                       ]) / norms[valid]
          )
        )
      )
      metrics <- c(
        metrics,
        spectral_angle = if (all(valid)) mean(angles) else NA_real_,
        zero_norm_spectra = sum(!valid)
      )
    }
  } else {
    .hm_abort("No qualified metric for this task.", "hm_unsupported_error")
  }
  if (!estimand %in% c("pooled", "equal_class_subject_acquisition")) {
    .hm_abort("Unknown estimand.")
  }
  if (estimand == "equal_class_subject_acquisition") {
    if (!categorical || length(hierarchy) !=
          2 || !all(hierarchy %in% names(reference))) {
      .hm_abort("Class, biological unit and acquisition hierarchy required.")
    }
    if (anyNA(reference[, hierarchy, drop = FALSE])) {
      .hm_abort("Hierarchy identities cannot be missing.")
    }
    # First average observations within class/acquisition, then
    # acquisitions within class/subject, then subjects within class,
    # finally equally across classes.
    z <- data.frame(
      value = 1 - loss, class = as.character(y),
      subject = reference[[hierarchy[1]]],
      acquisition = reference[[hierarchy[2]]]
    )
    a <- stats::aggregate(value ~ class + subject + acquisition, z, mean)
    b <- stats::aggregate(value ~ class + subject, a, mean)
    cc <- stats::aggregate(value ~ class, b, mean)
    metrics <- c(
      hierarchical_accuracy = if (setequal(cc$class, p$class_map)) {
        mean(
          cc$value
        )
      } else {
        NA_real_
      }
    )
  }
  interpretation <- switch(task$type,
    clustering = paste0(
      "Permutation-invariant external agreement; cluster ",
      "labels do not establish biological equivalence."
    ),
    anomaly = paste0(
      "Higher scores mean more anomalous; scores are not ",
      "calibrated risks; ROC/AP unavailable when either ",
      "reference class is absent."
    ),
    reconstruction = paste0(
      "Spectral angle in radians; zero-norm spectra remain ",
      "unavailable."
    ),
    temporal = paste(
      "Numeric temporal error; declared horizon:", if (is.null(
        task$reference$horizon
      )) {
        "unavailable"
      } else {
        task$reference$horizon
      },
      "; no event metric inferred."
    ),
    NULL
  )
  out <- structure(
    list(
      interpretation = interpretation, metrics = metrics, per_class = per,
      confusion = cm, predictions = p, reference = reference,
      estimand = estimand,
      hierarchy = hierarchy, uncertainty = NULL
    ),
    class = "hm_evaluation"
  )
  if (!is.null(resampling)) {
    out$uncertainty <- do.call(
      hm_bootstrap, c(
        list(evaluation = out),
        resampling
      )
    )
  }
  out
}
#' @rdname hm_evaluate
#' @param x An object returned by `hm_evaluate()`.
#' @param ... Additional arguments; forwarded to graphics for plotting,
#' otherwise unused.
#' @return The input object invisibly.
#' @export
print.hm_evaluation <- function(x, ...) {
  print(x$metrics)
  invisible(x)
}
#' @rdname hm_evaluate
#' @param object An object returned by `hm_evaluate()`.
#' @param ... Additional arguments; forwarded to graphics for plotting,
#' otherwise unused.
#' @return A list of descriptive fields.
#' @export
summary.hm_evaluation <- function(object, ...) {
  list(
    metrics = object$metrics, support = object$per_class,
    uncertainty = object$uncertainty,
    interpretation = object$interpretation
  )
}
