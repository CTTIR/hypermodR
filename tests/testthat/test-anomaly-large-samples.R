test_that("anomaly AUC remains defined for large tied samples", {
  task <- hm_task("y", "anomaly", "spectrum", "subject_id")
  n <- 100000L
  ids <- paste0("o", seq_len(n))
  reference <- data.frame(observation_id = ids, y = rep(c(0, 1), each = n / 2))
  prediction <- hm_prediction(rep(.5, n), task, ids)
  expect_warning(result <- hm_evaluate(prediction, reference), NA)
  expect_equal(result$metrics[["roc_auc"]], .5)
  expect_equal(result$metrics[["average_precision"]], .5)
})

test_that("anomaly AUC matches direct positive-negative pair comparisons", {
  task <- hm_task("y", "anomaly", "spectrum", "subject_id")
  for (n in 2:12) {
    y <- rep(c(0, 1), length.out = n)
    ids <- paste0("o", seq_len(n))
    for (scores in list(rep(.5, n), seq_len(n), seq_len(n) %% 3)) {
      expected <- mean(outer(scores[y == 1], scores[y == 0],
                             function(a, b) (a > b) + .5 * (a == b)))
      out <- hm_evaluate(hm_prediction(scores, task, ids),
                         data.frame(observation_id = ids, y = y))
      expect_equal(out$metrics[["roc_auc"]], expected)
    }
  }
})
