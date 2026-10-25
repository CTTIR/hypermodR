#' Select parameters using only grouped inner training folds
#' @param workflow An `hm_workflow`.
#' @param data Source data accepted by `hm_fit()`.
#' @param split Validated outer split containing inner grouped folds; their
#' test rows must cover outer training exactly once.
#' @param candidates Nonempty list of complete model-parameter lists.
#' @param control Fit-control list; resources must declare one CPU/thread.
#' Optional max_trials must cover all candidates and seed controls
#' reproducibility.
#' @param metric Metric name returned by `hm_evaluate()`, or NULL for
#' categorical log_loss and otherwise rmse.
#' @param direction Whether smaller or larger mean inner-fold scores are
#' preferred.
#' @return An outer-training `hm_fit` refitted with the winning parameters and
#' a tuning ledger containing fold scores, identities, seeds and protocol
#' hashes.
#' @details Search is sequential inside one bounded child, so the declared
#' elapsed timeout covers the entire search and final refit. The first tied
#' candidate wins. Outer test outcomes are excluded from selection.
#' @export
hm_tune <- function(
  workflow, data, split, candidates, control = list(), metric = NULL,
  direction = c("minimize", "maximize")
) {
  direction <- match.arg(direction)
  d <- .hm_model_data(data)
  split <- .hm_validate_fit_split(d, workflow$task, split)
  if (!is.list(candidates) ||
        !length(candidates) ||
        !all(vapply(candidates, is.list, logical(1))) ||
        !length(split$inner)) {
    .hm_abort("Explicit candidate lists and grouped inner folds required.")
  }
  if (!is.null(control$tuning)) {
    control$tuning <- NULL
  }
  if (is.null(control$resources)) {
    control$resources <- hm_resources()
  }
  if (!inherits(control$resources, "hm_resources") ||
        control$resources$cpus != 1L || control$resources$threads != 1L) {
    .hm_abort(paste0(
      "Portable tuning currently requires a single CPU/thread ",
      "budget."
    ))
  }
  if (!isTRUE(getOption("hypermodR.resource.child"))) {
    return(
      hm_with_resources(
        control$resources, hm_tune(
          workflow, data, split, candidates,
          control, metric, direction
        )
      )
    )
  }
  budget <- if (is.null(control$max_trials)) {
    length(candidates)
  } else {
    control$max_trials
  }
  if (length(budget) !=
        1 || !is.finite(budget) ||
        budget < length(candidates)) {
    .hm_abort("Candidate count exceeds the declared trial budget.")
  }
  seed <- if (is.null(control$seed)) {
    1L
  } else {
    control$seed
  }
  for (f in split$inner) {
    .hm_validate_fit_split(d, workflow$task, f)
    if (!length(f$test) ||
      !setequal(
        c(f$train, f$test),
        split$train
      ) ||
      length(
        intersect(
          c(f$train, f$test),
          split$test
        )
      )) {
      .hm_abort(
        "Inner folds must partition outer training only.",
        "hm_split_error"
      )
    }
  }
  validations <- unlist(
    lapply(split$inner, `[[`, "test"),
    use.names = FALSE
  )
  if (anyDuplicated(validations) ||
        !setequal(validations, split$train)) {
    .hm_abort(paste0(
      "Inner validation folds must cover outer training ",
      "exactly once."
    ))
  }
  ledger <- list()
  scores <- rep(NA_real_, length(candidates))
  start <- proc.time()[["elapsed"]]
  for (i in seq_along(candidates)) {
    fold_scores <- numeric(length(split$inner))
    for (j in seq_along(split$inner)) {
      if (proc.time()[["elapsed"]] - start > control$resources$timeout) {
        .hm_abort("Tuning timeout reached.", "hm_resource_error")
      }
      f <- split$inner[[j]]
      w <- workflow
      w$model$parameters <- candidates[[i]]
      cc <- control
      cc$seed <- seed + (i - 1L) * length(split$inner) +
        j
      fit <- hm_with_resources(cc$resources, hm_fit(w, data, f, cc))
      nd <- list(
        values = d$values[f$test, , drop = FALSE],
        observations = d$observations[f$test, ,
          drop = FALSE
        ], band_map = d$band_map, provenance = d$provenance
      )
      pred <- hm_predict(fit, nd)
      ev <- hm_evaluate(pred, nd$observations)
      key <- if (is.null(metric)) {
        if (workflow$task$type %in% c(
          "classification", "ordinal",
          "segmentation"
        )) {
          "log_loss"
        } else {
          "rmse"
        }
      } else {
        metric
      }
      if (!key %in% names(ev$metrics) ||
            !is.finite(ev$metrics[[key]])) {
        .hm_abort("Selection metric unavailable or invalid in an inner fold.")
      }
      fold_scores[j] <- ev$metrics[[key]]
      ledger[[length(ledger) +
                1L]] <- list(
        trial = i, fold = j, seed = cc$seed, train = f$train,
        validation = f$test,
        parameters = candidates[[i]], metric = key, score = fold_scores[j]
      )
    }
    scores[i] <- mean(fold_scores)
  }
  selected <- if (direction == "minimize") {
    which.min(scores)
  } else {
    which.max(scores)
  }
  w <- workflow
  w$model$parameters <- candidates[[selected]]
  fit <- hm_with_resources(control$resources, hm_fit(w, data, split, control))
  fit$tuning <- list(
    selected = selected, candidates = candidates, scores = scores,
    ledger = ledger,
    seed = seed, max_trials = budget, resources = control$resources,
    reference_hash = .hm_hash(d$observations),
    protocol_hash = .hm_hash(list(split, candidates, metric, direction))
  )
  fit
}
