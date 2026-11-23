naive_coverage_risk <- function(confidence, correct) {
  thresholds <- sort(unique(c(0, confidence, 1)))
  do.call(rbind, lapply(thresholds, function(t) {
    keep <- confidence >= t
    data.frame(
      threshold = t, coverage = mean(keep),
      risk = if (any(keep)) mean(!correct[keep]) else NA_real_
    )
  }))
}

reliability_fixture <- function(n, seed) {
  set.seed(seed)
  task <- hm_task("y", "classification", "spectrum", "subject_id")
  reference <- data.frame(
    observation_id = paste0("o", seq_len(n)),
    y = sample(c("a", "b", "c"), n, replace = TRUE),
    subject_id = rep(c("s1", "s2", "s3"), length.out = n),
    acquisition_id = paste0("a", seq_len(n))
  )
  raw <- matrix(round(stats::runif(3 * n), 1) + 0.05, n, 3,
                dimnames = list(NULL, c("a", "b", "c")))
  probabilities <- raw / rowSums(raw)
  labels <- c("a", "b", "c")[max.col(probabilities, ties.method = "first")]
  list(
    prediction = hm_prediction(labels, task, reference$observation_id,
                               probabilities, class_map = c("a", "b", "c")),
    reference = reference
  )
}

test_that("coverage-risk matches the per-threshold definition", {
  fixture <- reliability_fixture(200, 3)
  result <- hm_reliability(fixture$prediction, fixture$reference)
  confidence <- apply(fixture$prediction$probabilities, 1, max)
  correct <- fixture$prediction$values == fixture$reference$y
  expected <- naive_coverage_risk(confidence, correct)
  expect_equal(result$coverage_risk, expected, tolerance = 1e-12)
  expect_true(is.na(utils::tail(result$coverage_risk$risk, 1)) ||
                utils::tail(result$coverage_risk$coverage, 1) > 0)
})

test_that("coverage-risk scales to many held-out observations", {
  fixture <- reliability_fixture(50000, 4)
  elapsed <- system.time(
    result <- hm_reliability(fixture$prediction, fixture$reference)
  )[["elapsed"]]
  expect_identical(result$coverage_risk$coverage[1], 1)
  expect_lt(elapsed, 20)
})

test_that("reliability compares factor labels with different level sets", {
  task <- hm_task("y", "classification", "spectrum", "subject_id")
  reference <- data.frame(
    observation_id = paste0("o", 1:4),
    y = factor(c("a", "b", "a", "b")),
    subject_id = c("s1", "s1", "s2", "s2"),
    acquisition_id = paste0("a", 1:4)
  )
  probabilities <- cbind(a = c(0.8, 0.3, 0.6, 0.4), b = c(0.2, 0.7, 0.4, 0.6))
  prediction <- hm_prediction(
    factor(c("a", "b", "b", "b"), levels = c("b", "a", "c")), task,
    reference$observation_id, probabilities, class_map = c("a", "b")
  )
  result <- hm_reliability(prediction, reference, breaks = c(0, 0.5, 1))
  expect_identical(result$coverage_risk$risk[1], 0.25)
  expect_identical(sum(result$bins$n), 4L)
})
