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
#' reliability <- hm_reliability(
#'   prediction, reference,
#'   breaks = c(0, 0.5, 0.75, 1)
#' )
#' reliability$bins
#' reliability$coverage_risk
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
  # Compare labels as text, as hm_evaluate() does, so factors with different
  # level sets remain comparable.
  correct <- as.character(p$values) ==
    as.character(reference[[p$task$outcome]])
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
  # Retained cases at threshold t are those with confidence >= t. Sorting the
  # confidences once gives the retained counts and errors for every threshold
  # from cumulative sums instead of rescanning all cases per threshold.
  n <- length(confidence)
  ordering <- order(confidence)
  errors <- c(0, cumsum(!correct[ordering]))
  below <- findInterval(thresholds, confidence[ordering], left.open = TRUE)
  kept <- n - below
  kept_errors <- errors[n + 1L] - errors[below + 1L]
  coverage <- data.frame(
    threshold = thresholds, coverage = kept / n,
    risk = ifelse(kept > 0, kept_errors / pmax(kept, 1), NA_real_)
  )
  list(
    bins = do.call(rbind, table),
    coverage_risk = coverage,
    scores = e$metrics, ood = paste0(
      "Confidence is not a validated OOD detector; no ",
      "OOD/safety claim."
    )
  )
}
