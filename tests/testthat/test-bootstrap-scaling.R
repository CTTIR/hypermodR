test_that("whole-unit bootstrap scales to many units", {
  skip_on_cran()
  set.seed(8)
  n <- 6000
  units <- sprintf("s%04d", rep(seq_len(1500), length.out = n))
  task <- hm_task("y", "regression", "spectrum", "subject_id")
  reference <- data.frame(
    observation_id = paste0("o", seq_len(n)), y = stats::rnorm(n),
    subject_id = units
  )
  evaluation <- hm_evaluate(
    hm_prediction(reference$y + stats::rnorm(n, sd = 0.1), task,
                  reference$observation_id),
    reference
  )
  elapsed <- system.time(
    result <- hm_bootstrap(evaluation, "subject_id", draws = 20L, seed = 2L)
  )[["elapsed"]]
  expect_identical(result$status, "complete")
  first <- result$sampled_units[[1]]
  expect_identical(
    result$indices[[1]],
    unlist(lapply(first, function(u) which(units == u)), use.names = FALSE)
  )
  expect_lt(elapsed, 30)
})
