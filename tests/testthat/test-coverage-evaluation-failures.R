test_that(
  "evaluation refuses reference schema corruption rather than realigning it",
  {
    fixture <- eval_fixture()
    expect_error(
      hm_evaluate(fixture$p, fixture$r[6:1, ]),
      "match prediction order"
    )
    missing <- fixture$r
    missing$y <- NULL
    expect_error(
      hm_evaluate(fixture$p, missing),
      "outcome is missing"
    )
    outside <- fixture$r
    outside$y[1] <- "unknown"
    expect_error(
      hm_evaluate(fixture$p, outside),
      "outside the full class map"
    )
    expect_error(
      hm_compare(
        list(hm_evaluate(fixture$p, fixture$r)),
        unit = "subject_id"
      ),
      "Exactly two"
    )
    a <- hm_evaluate(fixture$p, fixture$r)
    b <- a
    b$hierarchy <- rev(b$hierarchy)
    expect_error(
      hm_compare(
        list(a, b),
        unit = "subject_id"
      ),
      "identical cases"
    )
    expect_error(
      hm_bootstrap(a, "subject_id", draws = 1),
      "bootstrap controls"
    )
    expect_error(
      hm_bootstrap(a, "unknown", draws = 2),
      "whole-unit identities"
    )
  }
)

test_that(
  paste0(
    "multilabel evaluation discloses absent support and ",
    "rejects mismatched label names"
  ),
  {
    task <- hm_task("y", "multilabel", "spectrum", "subject_id")
    values <- matrix(0, 3, 2, dimnames = list(NULL, c("a", "b")))
    p <- hm_prediction(
      values, task, paste0("o", 1:3),
      class_map = c("a", "b")
    )
    reference <- data.frame(observation_id = paste0("o", 1:3))
    reference$y <- values
    result <- hm_evaluate(p, reference)
    expect_equal(
      unname(
        result$metrics[c("hamming_loss", "subset_accuracy")]
      ),
      c(0, 1)
    )
    expect_true(is.na(result$metrics[["micro_f1"]]))
    expect_true(is.na(result$metrics[["macro_f1"]]))
    colnames(reference$y) <- c("b", "a")
    expect_error(
      hm_evaluate(p, reference),
      "shape-matched binary"
    )
    reference$y <- values
    p$probabilities <- matrix(2, 3, 2, dimnames = list(NULL, c("a", "b")))
    expect_error(
      hm_evaluate(p, reference),
      "per-label probabilities"
    )
  }
)

test_that(
  paste0(
    "survival and clustering evaluation require ",
    "interpretable independent references"
  ),
  {
    survival <- hm_task(
      "y", "survival", "subject", "subject_id",
      list(time = "time", status = "status")
    )
    p <- hm_prediction(
      c(3, 1),
      survival, c("o1", "o2")
    )
    ref <- data.frame(
      observation_id = c("o1", "o2"),
      time = c(1, 2),
      status = c(0, 0)
    )
    e <- hm_evaluate(p, ref)
    expect_equal(e$metrics[["comparable_pairs"]], 0)
    expect_true(is.na(e$metrics[["concordance"]]))
    ref$status[1] <- 2
    expect_error(
      hm_evaluate(p, ref),
      "Invalid survival reference"
    )
    clustering <- hm_task("y", "clustering", "spectrum", "subject_id")
    p <- hm_prediction(1, clustering, "o1")
    expect_error(
      hm_evaluate(
        p, data.frame(observation_id = "o1", y = "one")
      ),
      "at least two"
    )
    p <- hm_prediction(
      c(1, 1),
      hm_task(
        NULL, "clustering", "spectrum",
        "subject_id"
      ),
      c("o1", "o2")
    )
    expect_error(
      hm_evaluate(
        p, data.frame(observation_id = c("o1", "o2"))
      ),
      "External clustering"
    )
  }
)
