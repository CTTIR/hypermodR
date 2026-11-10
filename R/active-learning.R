#' Queue uncertain training-pool cases for annotation review
#' @param predictions Probabilistic `hm_prediction` aligned exactly to pool
#' rows.
#' @param pool Unlabelled data frame with observation_id and all task grouping
#' columns.
#' @param training_ids Character identities exactly matching the validated
#' training partition.
#' @param heldout_ids Character identities exactly matching the validated test
#' partition.
#' @param budget Positive integer number of pool cases to queue.
#' @param seed Seed used only to break entropy ties.
#' @param reference_revision Explicit immutable reference revision identifier.
#' @param data Source data used to verify pool identities and grouping.
#' @param split Validated grouped train/test split.
#' @return List with an entropy-ranked pending-review queue, seed, budget,
#' reference revision, run hash, annotation owner and labels_acquired=FALSE.
#' @details Only training-access unlabelled rows may enter the pool. This
#' creates a review queue; it does not acquire labels or modify annotations.
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
#' pool <- reference
#' pool$y <- NA_character_
#' heldout <- data.frame(observation_id = "o7", y = "a", subject_id = "s3",
#'                       acquisition_id = "a7")
#' data <- hm_dataset(matrix(seq_len(14), 7, 2), rbind(reference, heldout),
#'                    data.frame(wavelength = c(500, 600)))
#' queue <- hm_active_learning(
#'   prediction, pool, training_ids = reference$observation_id,
#'   heldout_ids = "o7", budget = 2, seed = 1, reference_revision = "rev-1",
#'   data = data, split = list(train = 1:6, test = 7L)
#' )
#' queue$queue
#' @export
hm_active_learning <- function(
  predictions, pool, training_ids, heldout_ids, budget = 1L, seed = 1L,
  reference_revision,
  data, split
) {
  p <- predictions
  if (!inherits(p, "hm_prediction") ||
        is.null(p$probabilities) ||
        !is.data.frame(pool) ||
        !identical(p$observation_id, pool$observation_id)) {
    .hm_abort("Aligned unlabeled pool and probabilities required.")
  }
  .hm_scalar_character(reference_revision, "reference_revision")
  d <- .hm_model_data(data)
  split <- .hm_validate_fit_split(d, p$task, split)
  if (!setequal(training_ids, d$observations$observation_id[split$train]) ||
        !setequal(heldout_ids, d$observations$observation_id[split$test])) {
    .hm_abort(paste0(
      "Declared training/held-out access must match a ",
      "validated grouped split."
    ))
  }
  ix <- match(pool$observation_id, d$observations$observation_id)
  for (g in p$task$groups) {
    if (!g %in% names(pool) ||
          anyNA(ix) ||
          !identical(pool[[g]], d$observations[[g]][ix])) {
      .hm_abort("Pool biological identities must match the declared source.")
    }
  }
  if (length(intersect(training_ids, heldout_ids)) ||
        any(!p$observation_id %in% training_ids) ||
        length(intersect(p$observation_id, heldout_ids))) {
    .hm_abort("Pool must belong exclusively to training access.")
  }
  if (!is.null(pool[[p$task$outcome]]) &&
        any(!is.na(pool[[p$task$outcome]]))) {
    .hm_abort("Acquisition queue accepts only unlabeled pool rows.")
  }
  if (length(budget) !=
        1 || budget < 1 || budget != as.integer(budget) ||
        budget > nrow(pool)) {
    .hm_abort("Invalid acquisition budget.")
  }
  entropy <- -rowSums(p$probabilities * log(pmax(
    p$probabilities,
    .Machine$double.xmin
  )))
  tie <- .hm_seed(seed, stats::runif(length(entropy)))
  ix <- order(-entropy, tie)[seq_len(budget)]
  list(
    queue = data.frame(
      observation_id = p$observation_id[ix], entropy = entropy[ix],
      status = "pending_annotation_review"
    ),
    seed = seed, budget = budget, reference_revision = reference_revision,
    run_hash = .hm_hash(list(
      p, pool, training_ids, heldout_ids,
      reference_revision
    )),
    annotation_owner = "hyperannotatR", labels_acquired = FALSE
  )
}
