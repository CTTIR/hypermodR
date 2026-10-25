.hm_resample_evaluation <- function(e, index) {
  p <- e$predictions
  p$values <- if (is.matrix(p$values)) {
    p$values[index, , drop = FALSE]
  } else {
    p$values[index]
  }
  if (!is.null(p$probabilities)) {
    p$probabilities <- p$probabilities[index, , drop = FALSE]
  }
  p$observation_id <- paste0("resample-", seq_along(index))
  r <- e$reference[index, , drop = FALSE]
  r$observation_id <- p$observation_id
  # Duplicated sampled units retain their whole cluster; equal-unit
  # averaging requires distinct identities for repeated copies.
  attr(r, "target") <- if (is.matrix(attr(e$reference, "target"))) {
    attr(e$reference, "target")[index, ,
      drop = FALSE
    ]
  } else {
    attr(e$reference, "target")[index]
  }
  hm_evaluate(p, r, e$estimand, hierarchy = e$hierarchy)
}
#' Paired whole-unit bootstrap of fixed predictions
#' @param evaluation An `hm_evaluation` of fixed predictions.
#' @param unit Reference column naming whole independent resampling units;
#' connected biological/source components must not be split.
#' @param draws Integer number of bootstrap replicates, at least two.
#' @param seed Seed for reproducible unit sampling.
#' @param level Interval coverage strictly between zero and one.
#' @param comparison Optional exactly matched second evaluation for paired
#' metric differences.
#' @return List with percentile interval, draw-by-metric resamples, row
#' indices, sampled units, validity flags, invalid draw count, seed, level and
#' status.
#' @details Resamples complete units with replacement. Fitting and
#' model-selection uncertainty are excluded. If any draw lacks valid metrics,
#' intervals remain unavailable and status reports invalid draws.
#' @export
hm_bootstrap <- function(
  evaluation, unit, draws = 200L, seed = 1L, level = 0.95, comparison = NULL
) {
  e <- evaluation
  r <- e$reference
  if (!inherits(e, "hm_evaluation") ||
        !unit %in% names(r) ||
        anyNA(r[[unit]]) ||
        !is.character(r[[unit]]) ||
        any(!nzchar(r[[unit]]))) {
    .hm_abort("Complete whole-unit identities required.")
  }
  if (length(draws) !=
        1 || draws < 2 || draws != as.integer(draws) ||
        length(level) !=
          1 || !is.finite(level) ||
        level <= 0 || level >= 1) {
    .hm_abort("Invalid bootstrap controls.")
  }
  grouping <- unique(
    c(
      e$predictions$task$groups, intersect(
        c(
          "subject_id", "procedure_id", "wound_id", "sequence_id",
          "acquisition_id",
          "source_lineage_id"
        ),
        names(r)
      )
    )
  )
  component <- .hm_components(r, grouping)
  if (any(
    vapply(
      split(r[[unit]], component),
      function(x) {
        length(unique(x)) >
          1L
      }, logical(1)
    )
  )) {
    .hm_abort(paste0(
      "Bootstrap unit splits a connected biological/source ",
      "component."
    ))
  }
  if (!is.null(comparison)) {
    .hm_check_pair(e, comparison)
  }
  units <- unique(r[[unit]])
  if (length(units) <
        2) {
    .hm_abort("At least two independent units required.")
  }
  sampled <- .hm_seed(
    seed, lapply(
      seq_len(draws),
      function(i) {
        sample(
          units, length(units),
          replace = TRUE
        )
      }
    )
  )
  values <- matrix(
    NA_real_, draws, length(e$metrics),
    dimnames = list(NULL, names(e$metrics))
  )
  indices <- vector("list", draws)
  for (i in seq_len(draws)) {
    ix <- unlist(
      lapply(sampled[[i]], function(u) which(r[[unit]] == u)),
      use.names = FALSE
    )
    indices[[i]] <- ix
    evaluate <- function(obj) {
      # Give each bootstrap copy a distinct biological/acquisition
      # identity.
      offset <- 0L
      pp <- .hm_resample_evaluation(obj, ix)
      for (j in seq_along(sampled[[i]])) {
        n <- sum(r[[unit]] == sampled[[i]][j])
        rows <- offset + seq_len(n)
        for (g in unique(c(unit, obj$hierarchy))) {
          if (g %in% names(pp$reference)) {
            pp$reference[[g]][rows] <- paste(j, pp$reference[[g]][rows],
              sep = ":"
            )
          }
        }
        offset <- offset + n
      }
      hm_evaluate(pp$predictions, pp$reference, obj$estimand,
        hierarchy = obj$hierarchy
      )$metrics
    }
    values[i, ] <- evaluate(e)
    if (!is.null(comparison)) {
      values[i, ] <- values[i, ] - evaluate(comparison)
    }
  }
  valid <- apply(
    is.finite(values),
    1, all
  )
  ci <- matrix(
    NA_real_, 2, ncol(values),
    dimnames = list(
      c("lower", "upper"),
      colnames(values)
    )
  )
  # Missing-class draws invalidate the requested bootstrap distribution.
  if (all(valid)) {
    ci <- apply(
      values, 2, stats::quantile,
      probs = c((1 - level) / 2, 1 - (1 - level) / 2),
      names = FALSE
    )
  }
  list(
    interval = ci, resamples = values, indices = indices,
    sampled_units = sampled,
    valid = valid, invalid_draws = sum(!valid),
    seed = seed, level = level,
    conditioning = paste0(
      "fixed predictions; fitting and selection ",
      "uncertainty excluded"
    ),
    status = if (all(valid)) "complete" else "invalid_draws_no_interval"
  )
}
