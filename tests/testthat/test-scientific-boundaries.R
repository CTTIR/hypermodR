test_that("survival uses numeric risks and complete event references", {
  task <- hm_task("event", "survival", "subject", "subject_id",
                  list(time = "time", status = "status"))
  reference <- data.frame(observation_id = c("a", "b"),
                          subject_id = c("s1", "s2"),
                          time = c(1, 2), status = c(1, 1))
  prediction <- hm_prediction(c(10, 2), task, reference$observation_id)
  expect_equal(hm_evaluate(prediction, reference)$metrics[["concordance"]], 1)
  for (bad in list(c("10", "2"), matrix(c(10, 2), 2), c(10, Inf))) {
    expect_error(hm_prediction(bad, task, reference$observation_id),
                 class = "hm_prediction_error")
    changed <- prediction
    changed$values <- bad
    expect_error(hm_evaluate(changed, reference),
                 class = "hm_prediction_error")
  }
  for (column in c("time", "status")) {
    changed <- reference
    changed[[column]] <- NULL
    expect_error(hm_evaluate(prediction, changed),
                 class = "hm_reference_error")
  }
  for (bad in list(c(-1, 2), c(1, Inf), c("1", "2"))) {
    changed <- reference
    changed$time <- bad
    expect_error(hm_evaluate(prediction, changed),
                 class = "hm_reference_error")
  }
  reference$status <- c(TRUE, FALSE)
  expect_equal(hm_evaluate(prediction, reference)$metrics[["concordance"]], 1)
})

test_that("numeric matrix metrics require exact named output coordinates", {
  task <- hm_task("abundance", "unmixing", "spectrum", "subject_id")
  target <- rbind(c(A = .1, B = .9), c(A = .8, B = .2))
  reference <- data.frame(observation_id = c("a", "b"))
  reference$abundance <- target
  prediction <- hm_prediction(target, task, reference$observation_id)
  expect_equal(hm_evaluate(prediction, reference)$metrics[["rmse"]], 0)
  prediction$values <- target[, 2:1]
  expect_error(hm_evaluate(prediction, reference), class = "hm_reference_error")
  colnames(prediction$values) <- NULL
  expect_error(hm_evaluate(prediction, reference), class = "hm_reference_error")
})

test_that("preprocessed reconstruction cannot be scored against raw spectra", {
  data <- hm_example()
  task <- hm_task(NULL, "reconstruction", "spectrum", "subject_id")
  split <- hm_split(data, task, folds = 3, inner_folds = 2)$folds[[1]]
  workflow <- hm_workflow(task, hm_recipe(list(list(type = "scale"))),
                          hm_model_spec("pca", task, list(rank = 8L)))
  fit <- hm_fit(workflow, data, split)
  heldout <- hm_spectra(data, list(
    observation_id = data@observations$observation_id[split$test]
  ))
  prediction <- hm_predict(fit, heldout)
  reference <- heldout$observations
  attr(reference, "target") <- heldout$values
  expect_error(hm_evaluate(prediction, reference), class = "hm_reference_error")
  target <- hm_bake(fit$recipe, heldout$values)
  attr(reference, "target") <- target
  attr(reference, "target_space") <- prediction$provenance$output_space
  result <- hm_evaluate(prediction, reference)
  expect_lt(result$metrics[["rmse"]], 1e-12)
  expect_equal(result$metrics[["rmse"]],
               sqrt(mean((prediction$values - target)^2)))
  boot <- hm_bootstrap(result, "subject_id", draws = 4)
  expect_true(all(boot$valid))
  expect_true(all(boot$resamples[, "rmse"] < 1e-12))
})

test_that("prediction preserves declared radiometric input units", {
  data <- hm_example()
  task <- hm_task("response", "regression", "spectrum", "subject_id")
  split <- hm_split(data, task, folds = 3, inner_folds = 2)$folds[[1]]
  fit <- hm_fit(hm_workflow(task, hm_recipe(), hm_model_spec("lm", task)),
                data, split)
  heldout <- hm_spectra(data, list(
    observation_id = data@observations$observation_id[split$test]
  ))
  original <- hm_predict(fit, heldout)
  heldout$provenance$calibration <- "independently measured acquisition"
  expect_equal(hm_predict(fit, heldout)$values, original$values)
  heldout$provenance$radiometric_unit <- "radiance"
  expect_error(hm_predict(fit, heldout), class = "hm_radiometric_mismatch")
  heldout$provenance$radiometric_unit <- NULL
  expect_error(hm_predict(fit, heldout), class = "hm_radiometric_mismatch")
  fit$input_radiometric_unit <- NULL
  expect_error(hm_predict(fit, heldout), "refit this workflow")
})

test_that("temporal fitting rejects stale clocks and altered partitions", {
  data <- hm_example()
  data@observations$measured_time <- seq_len(nrow(data@observations))
  task <- hm_task("response", "regression", "spectrum", "subject_id")
  split <- hm_split(data, task, design = "temporal", time = "measured_time",
                    cutoff = 65, look_back = 1, embargo = 1)
  workflow <- hm_workflow(task, hm_recipe(), hm_model_spec("lm", task))
  fit <- hm_fit(workflow, data, split)
  expect_true(all(data@observations$measured_time[fit$training] < 63))
  changed <- data
  changed@observations$measured_time[split$train] <- 1000
  expect_error(hm_fit(workflow, changed, split), class = "hm_split_error")
  changed_split <- split
  changed_split$cutoff <- 80
  expect_error(hm_fit(workflow, data, changed_split), class = "hm_split_error")
  changed_split <- split
  changed_split$excluded <- integer()
  expect_error(hm_fit(workflow, data, changed_split), class = "hm_split_error")
})

test_that("temporal construction retains biological links beyond task groups", {
  observations <- data.frame(
    observation_id = letters[1:4], subject_id = c("s1", "s2", "s1", "s3"),
    session = letters[1:4], timestamp = c(1, 2, 10, 11)
  )
  task <- hm_task("y", "regression", "spectrum", "session")
  split <- hm_split(observations, task, design = "temporal", cutoff = 10)
  expect_identical(split$train, 2L)
  expect_identical(split$test, 3:4)
  expect_identical(split$excluded, 1L)
})
