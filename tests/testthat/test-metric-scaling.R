naive_average_precision <- function(scores, y) {
  thresholds <- sort(unique(scores), decreasing = TRUE)
  recall <- precision <- numeric(length(thresholds))
  for (i in seq_along(thresholds)) {
    keep <- scores >= thresholds[i]
    recall[i] <- sum(y[keep] == 1) / sum(y == 1)
    precision[i] <- mean(y[keep] == 1)
  }
  sum(diff(c(0, recall)) * precision)
}

naive_concordance <- function(time, status, risk) {
  pairs <- which(outer(time, time, "<") & status == 1, arr.ind = TRUE)
  if (!nrow(pairs)) {
    return(c(NA_real_, 0))
  }
  c(mean((risk[pairs[, 1]] > risk[pairs[, 2]]) +
           0.5 * (risk[pairs[, 1]] == risk[pairs[, 2]])), nrow(pairs))
}

test_that("anomaly average precision matches the per-threshold definition", {
  task <- hm_task("y", "anomaly", "spectrum", "subject_id")
  set.seed(5)
  for (replicate in 1:25) {
    n <- sample(2:60, 1)
    y <- sample(c(0, 1), n, replace = TRUE)
    y[sample(n, 2)] <- c(0, 1)
    scores <- round(stats::runif(n), sample(0:2, 1))
    ids <- paste0("o", seq_len(n))
    result <- hm_evaluate(hm_prediction(scores, task, ids),
                          data.frame(observation_id = ids, y = y))
    expect_equal(result$metrics[["average_precision"]],
                 naive_average_precision(scores, y), tolerance = 1e-12)
  }
})

test_that("survival concordance matches all comparable pairs", {
  task <- hm_task("event", "survival", "subject", "subject_id",
                  list(time = "time", status = "status"))
  set.seed(6)
  for (replicate in 1:25) {
    n <- sample(2:40, 1)
    time <- sample(1:15, n, replace = TRUE)
    status <- sample(c(0, 1), n, replace = TRUE)
    risk <- round(stats::runif(n), 1)
    ids <- paste0("o", seq_len(n))
    result <- hm_evaluate(
      hm_prediction(risk, task, ids),
      data.frame(observation_id = ids, time = time, status = status)
    )
    expected <- naive_concordance(time, status, risk)
    expect_equal(result$metrics[["concordance"]], expected[1],
                 tolerance = 1e-12)
    expect_identical(result$metrics[["comparable_pairs"]], expected[2])
  }
})

test_that("survival concordance accepts logical event status", {
  task <- hm_task("event", "survival", "subject", "subject_id",
                  list(time = "time", status = "status"))
  reference <- data.frame(observation_id = c("a", "b", "c"),
                          time = c(1, 2, 4), status = c(TRUE, FALSE, TRUE))
  result <- hm_evaluate(hm_prediction(c(3, 1, 2), task, reference$observation_id),
                        reference)
  expect_identical(result$metrics[["concordance"]], 1)
  expect_identical(result$metrics[["comparable_pairs"]], 2)
})

test_that("anomaly and survival metrics scale to many observations", {
  skip_on_cran()
  set.seed(7)
  n <- 40000
  ids <- paste0("o", seq_len(n))
  anomaly <- hm_task("y", "anomaly", "spectrum", "subject_id")
  elapsed <- system.time(result <- hm_evaluate(
    hm_prediction(stats::runif(n), anomaly, ids),
    data.frame(observation_id = ids, y = rep(c(0, 1), n / 2))
  ))[["elapsed"]]
  expect_true(is.finite(result$metrics[["average_precision"]]))
  expect_lt(elapsed, 20)
  survival <- hm_task("event", "survival", "subject", "subject_id",
                      list(time = "time", status = "status"))
  m <- 15000
  reference <- data.frame(observation_id = ids[seq_len(m)],
                          time = stats::runif(m), status = rep(c(0, 1), m / 2))
  result <- hm_evaluate(
    hm_prediction(stats::runif(m), survival, reference$observation_id),
    reference
  )
  expect_true(is.finite(result$metrics[["concordance"]]))
})
