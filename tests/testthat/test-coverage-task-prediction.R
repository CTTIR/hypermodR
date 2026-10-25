test_that("task and target schemas preserve explicit statistical meaning", {
  expect_error(hm_task("y", "unknown", "spectrum", "subject_id"), "task type")
  expect_error(
    hm_task("y", "regression", "unknown", "subject_id"),
    "task unit"
  )
  expect_error(hm_task("y", "temporal", "pixel", "subject_id"), "Temporal")
  expect_error(
    hm_task(NULL, "classification", "spectrum", "subject_id"),
    "requires an outcome"
  )
  expect_error(
    hm_task("y", "classification", "spectrum", character()),
    "grouping"
  )
  task <- function(type, reference = NULL) {
    hm_task(
      if (type %in% c(
        "unmixing", "clustering", "reconstruction"
      )) {
        NULL
      } else {
        "y"
      }, type,
      "subject", c("subject_id", "subject_id"), reference
    )
  }
  expect_identical(task("regression")$groups, "subject_id")
  expect_true(.hm_validate_target(NULL, task("clustering"), 3))
  expect_true(.hm_validate_target(ordered(c("low", "high"), levels = c(
    "low", "high"
  )), task("ordinal"), 2))
  expect_true(.hm_validate_target(data.frame(time = c(0, 2), status = c(
    0,
    1
  )), task("survival"), 2))
  expect_error(
    .hm_validate_target(c(1, NA), task("regression"), 2),
    "missingness"
  )
  expect_error(
    .hm_validate_target(1:2, task("classification"), 2),
    "explicit labels"
  )
  expect_error(
    .hm_validate_target(matrix(2, 2, 2), task("multilabel"), 2),
    "binary"
  )
  expect_error(
    .hm_validate_target(c(0, 1), task("multilabel"), 2),
    "numeric matrix"
  )
  expect_error(
    .hm_validate_target(matrix(.6, 2, 2), task("unmixing"), 2),
    "sum to one"
  )
  expect_true(.hm_validate_target(matrix(.6, 2, 2), task("unmixing", list(
    abundance_constraint = "nonnegative"
  )), 2))
  expect_error(.hm_validate_target(data.frame(time = 1:2, status = c(
    0,
    2
  )), task("survival"), 2), "binary")
})

test_that(paste0(
  "prediction schemas reject silent relabelling and ",
  "abundance violations"
), {
  classification <- hm_task("y", "classification", "spectrum", "subject_id")
  regression <- hm_task("y", "regression", "spectrum", "subject_id")
  multilabel <- hm_task("y", "multilabel", "spectrum", "subject_id")
  unmixing <- hm_task(NULL, "unmixing", "spectrum", "subject_id")
  expect_error(hm_prediction(1:2, regression, c("o1", "o1")), "identities")
  expect_error(hm_prediction(1:2, regression, "o1"), "row count")
  expect_error(hm_prediction(c(1, Inf), regression, c("o1", "o2")), "finite")
  expect_error(hm_prediction(c("a", "b"), classification, c("o1", "o2"),
                 class_map = "a"
               ), "class map")
  expect_error(
    hm_prediction(c(0, 1), multilabel, c("o1", "o2")),
    "matrix output"
  )
  labels <- matrix(c(0, 1, 1, 0), 2, dimnames = list(NULL, c("a", "b")))
  expect_error(hm_prediction(labels, multilabel, c("o1", "o2"),
                 class_map = c("b", "a")
               ), "named label map")
  label_prediction <- hm_prediction(labels, multilabel, c("o1", "o2"),
    probabilities = matrix(.8, 2, 2, dimnames = list(NULL, c("a", "b"))),
    class_map = c("a", "b")
  )
  expect_identical(label_prediction$values, labels)
  expect_error(
    hm_prediction(matrix(.6, 2, 2), unmixing, c("o1", "o2")),
    "sum to one"
  )
  expect_error(hm_prediction(matrix(c(-.1, 1.1, .4, .6), 2), unmixing, c(
    "o1", "o2"
  )), "nonnegative")
  p <- hm_prediction(c(3, 7), regression, c("o1", "o2"))
  expect_output(expect_identical(print(p), p), "regression 2 observations")
  expect_identical(summary(p), list(
    type = "regression", n = 2L,
    probabilities = FALSE
  ))
  categorical <- hm_prediction(c("a", "b"), classification, c("o1", "o2"),
    class_map = c("a", "b")
  )
  expect_error(plot(categorical), class = "hm_unsupported_error")
  output <- tempfile(fileext = ".pdf")
  grDevices::pdf(output)
  on.exit(
    {
      grDevices::dev.off()
      unlink(output)
    },
    add = TRUE
  )
  expect_null(plot(p))
})
