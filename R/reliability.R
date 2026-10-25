#' Describe confidence bins and coverage-risk without claiming OOD validation
#' @param predictions Categorical `hm_prediction` with probabilities.
#' @param reference Exactly aligned reference data frame accepted by
#' `hm_evaluate()`.
#' @param breaks Strictly increasing numeric confidence-bin boundaries from
#' zero to one.
#' @return List of bin counts/mean confidence/accuracy, coverage-risk table,
#' evaluation scores and an OOD interpretation.
#' @details Confidence is the maximum class probability. Empty bins or zero
#' retained coverage have unavailable accuracy/risk. Confidence does not
#' constitute a validated OOD detector.
#' @export
hm_reliability <- function(predictions, reference, breaks = seq(0, 1,
  length.out = 11
)) {
  e <- hm_evaluate(predictions, reference)
  p <- predictions
  if (is.null(p$probabilities) ||
        !p$task$type %in% c("classification", "ordinal", "segmentation")) {
    .hm_abort("Categorical probabilities required.")
  }
  if (!is.numeric(breaks) ||
    anyNA(breaks) ||
    any(
      diff(breaks) <=
        0
    ) ||
    breaks[1] != 0 || tail(breaks, 1) !=
    1) {
    .hm_abort("Bins must span zero to one in increasing order.")
  }
  confidence <- apply(p$probabilities, 1, max)
  correct <- p$values == reference[[p$task$outcome]]
  bins <- cut(confidence, breaks, include.lowest = TRUE)
  table <- lapply(
    levels(bins),
    function(b) {
      ix <- which(bins == b)
      data.frame(
        bin = b, n = length(ix),
        confidence = if (length(ix)) {
          mean(confidence[ix])
        } else {
          NA_real_
        }, accuracy = if (length(ix)) {
          mean(correct[ix])
        } else {
          NA_real_
        }
      )
    }
  )
  thresholds <- sort(unique(c(0, confidence, 1)))
  coverage <- lapply(
    thresholds, function(t) {
      keep <- confidence >= t
      data.frame(
        threshold = t, coverage = mean(keep),
        risk = if (any(keep)) {
          mean(!correct[keep])
        } else {
          NA_real_
        }
      )
    }
  )
  list(
    bins = do.call(rbind, table),
    coverage_risk = do.call(rbind, coverage),
    scores = e$metrics, ood = paste0(
      "Confidence is not a validated OOD detector; no ",
      "OOD/safety claim."
    )
  )
}
