test_that("categorical matrices cannot manufacture observations", {
  task <- hm_task("y", "classification", "spectrum", "subject_id")
  ids <- paste0("o", 1:4)
  labels <- c("A", "B", "A", "B")
  values <- cbind(first = labels, second = rev(labels))
  expect_error(hm_prediction(values, task, ids, class_map = c("A", "B")),
               "dimensionless vectors")
  valid <- hm_prediction(labels, task, ids, class_map = c("A", "B"))
  reference <- data.frame(observation_id = ids, y = labels)
  out <- hm_evaluate(valid, reference)
  expect_equal(sum(out$confusion), length(ids))
  expect_equal(out$metrics[["accuracy"]], 1)
  reference$y <- cbind(labels, labels)
  expect_error(hm_evaluate(valid, reference), "dimensionless vectors")
  reference$y <- labels
  valid$values <- values
  expect_error(hm_evaluate(valid, reference), "dimensionless vectors")
})

test_that("prediction identities and categorical maps remain vectors", {
  task <- hm_task("y", "classification", "spectrum", "subject_id")
  expect_error(hm_prediction(c("A", "B"), task, matrix(c("o1", "o2")),
                             class_map = c("A", "B")), "identities")
  expect_error(hm_prediction(c("A", "B"), task, c("o1", "o2"),
                             class_map = matrix(c("A", "B"))), "class map")
  expect_error(hm_prediction("A", task, c("o1", "o2"),
                             class_map = c("A", "B")), "row count")
})

test_that("matrix categorical training targets are rejected before fitting", {
  obs <- data.frame(observation_id = paste0("o", 1:8),
                    subject_id = rep(paste0("s", 1:4), each = 2))
  obs$y <- cbind(rep(c("A", "B"), 4), rep(c("A", "B"), 4))
  data <- hm_dataset(matrix(seq_len(16), 8, 2), obs,
                     data.frame(wavelength = c(500, 600)))
  task <- hm_task("y", "classification", "spectrum", "subject_id")
  workflow <- hm_workflow(task, hm_recipe(), hm_model_spec("knn", task))
  set.seed(42)
  before <- .Random.seed
  expect_error(hm_fit(workflow, data, list(train = 1:4, test = 5:8)),
               "dimensionless vectors")
  expect_identical(.Random.seed, before)
})

test_that("genuine matrix-response tasks preserve row and output identity", {
  ids <- c("o1", "o2")
  values <- diag(2)
  colnames(values) <- c("A", "B")
  for (type in c("multilabel", "unmixing", "reconstruction")) {
    task <- hm_task(if (type == "reconstruction") NULL else "y", type,
                    "spectrum", "subject_id")
    prediction <- hm_prediction(values, task, ids,
                                class_map = if (type == "multilabel")
                                  c("A", "B") else NULL)
    reference <- data.frame(observation_id = ids)
    if (type == "reconstruction")
      attr(reference, "target") <- values else reference$y <- values
    result <- hm_evaluate(prediction, reference)
    expect_identical(prediction$values, values)
    expect_identical(prediction$observation_id, ids)
    expect_true(all(is.finite(result$metrics)))
    if (type == "multilabel") {
      expect_equal(result$metrics[["hamming_loss"]], 0)
    } else {
      expect_equal(result$metrics[["rmse"]], 0)
    }
  }
})
