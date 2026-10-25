test_that(
  "full class space and independent proper scores are retained",
  {
    z <- eval_fixture()
    e <- hm_evaluate(z$p, z$r)
    expect_equal(
      unname(e$metrics["accuracy"]),
      4 / 6
    )
    expect_equal(
      e$confusion, table(
        factor(z$r$y, levels = c("a", "b")),
        factor(z$p$values, levels = c("a", "b"))
      )
    )
    expect_equal(
      unname(e$metrics["mean_iou"]),
      0.5
    )
    expect_equal(
      unname(e$metrics["brier"]),
      mean(2 * c(0.2, 0.7, 0.7, 0.1, 0.4, 0.4)^2)
    )
    expect_equal(
      unname(e$metrics["log_loss"]),
      -mean(log(c(0.8, 0.3, 0.3, 0.9, 0.6, 0.6)))
    )
    z$p$class_map <- c("a", "b", "c")
    z$p$probabilities <- NULL
    expect_true(is.na(hm_evaluate(z$p, z$r)$metrics["balanced_accuracy"]))
    expect_equal(
      nrow(hm_evaluate(z$p, z$r)$confusion),
      3L
    )
  }
)
test_that(
  "class then subject then acquisition is explicitly equal weighted",
  {
    z <- eval_fixture()
    # Class a: s1 .5, s2 1 => .75; b: s1 0, s2 1 => .5
    expect_equal(
      unname(hm_evaluate(
        z$p, z$r,
        "equal_class_subject_acquisition"
      )$metrics),
      0.625
    )
    expect_equal(
      nrow(hm_reliability(z$p, z$r)$bins),
      10L
    )
    expect_error(
      hm_evaluate(z$p, z$r[6:1, ]),
      "identities"
    )
  }
)
test_that(
  "numeric and survival metrics reconstruct independently",
  {
    for (kind in c("regression", "temporal", "unmixing", "reconstruction")) {
      t <- hm_task(
        if (kind == "reconstruction") {
          NULL
        } else {
          "y"
        }, kind, if (kind == "temporal") {
          "sequence"
        } else {
          "spectrum"
        }, "subject_id"
      )
      y <- if (kind %in% c("unmixing", "reconstruction")) {
        cbind(
          c(0.2, 0.8),
          c(0.8, 0.2)
        )
      } else {
        c(1, 3)
      }
      v <- if (is.matrix(y)) {
        cbind(
          c(0.4, 0.7),
          c(0.6, 0.3)
        )
      } else {
        c(2, 1)
      }
      r <- data.frame(observation_id = c("a", "b"))
      r$y <- y
      attr(r, "target") <- y
      p <- hm_prediction(v, t, r$observation_id)
      expect_equal(
        unname(hm_evaluate(p, r)$metrics["rmse"]),
        sqrt(mean((v - y)^2))
      )
    }
    t <- hm_task(
      "event", "survival", "subject", "subject_id", list(
        time = "time",
        status = "status"
      )
    )
    r <- data.frame(
      observation_id = letters[1:3], time = c(1, 2, 4),
      status = c(1, 0, 1)
    )
    p <- hm_prediction(
      c(3, 1, 2),
      t, r$observation_id
    )
    expect_equal(
      unname(hm_evaluate(p, r)$metrics["concordance"]),
      1
    )
  }
)
test_that(
  "segmentation retains errors in absent prediction classes",
  {
    t <- hm_task("y", "segmentation", "pixel", "subject_id")
    r <- data.frame(observation_id = letters[1:4], y = c("a", "a", "b", "b"))
    p <- hm_prediction(
      rep("a", 4),
      t, r$observation_id,
      class_map = c("a", "b")
    )
    e <- hm_evaluate(p, r)
    expect_equal(
      unname(e$metrics["mean_iou"]),
      0.25
    )
    expect_equal(e$per_class$recall, c(1, 0))
  }
)
test_that(
  "ARI is permutation invariant and corrected against independent counts",
  {
    t <- hm_task("y", "clustering", "spectrum", "subject_id")
    r <- data.frame(observation_id = letters[1:4], y = c("A", "A", "B", "B"))
    p <- hm_prediction(
      c("second", "second", "first", "first"),
      t, r$observation_id
    )
    expect_equal(
      unname(hm_evaluate(p, r)$metrics),
      1
    )
    p$values <- c("X", "Y", "X", "Y")
    # Four singleton cells: observed=0, row/column pair totals=2, total
    # There are six pairs.
    expect_equal(
      unname(hm_evaluate(p, r)$metrics),
      (0 - 4 / 6) / (2 - 4 / 6)
    )
    expect_match(
      hm_evaluate(p, r)$interpretation,
      "biological equivalence"
    )
  }
)
test_that(
  "anomaly ties and absent reference classes remain explicit",
  {
    t <- hm_task("y", "anomaly", "spectrum", "subject_id")
    r <- data.frame(observation_id = letters[1:4], y = c(1, 0, 1, 0))
    p <- hm_prediction(
      c(0.8, 0.8, 0.2, 0.1),
      t, r$observation_id
    )
    e <- hm_evaluate(p, r)
    # Positive .8 wins against .1 and ties .8; positive .2 wins against
    # .1.
    expect_equal(
      unname(e$metrics["roc_auc"]),
      2.5 / 4
    )
    # Threshold .8: recall .5 precision .5; threshold .2: recall 1
    # precision 2/3.
    expect_equal(
      unname(e$metrics["average_precision"]),
      0.5 * 0.5 + 0.5 * 2 / 3
    )
    r$y <- rep(0, 4)
    expect_true(all(is.na(hm_evaluate(p, r)$metrics)))
    expect_match(e$interpretation, "not calibrated risks")
  }
)
test_that(
  "multilabel scores and independent probabilities preserve label support",
  {
    t <- hm_task("y", "multilabel", "spectrum", "subject_id")
    y <- cbind(
      a = c(1, 0, 1),
      b = c(0, 1, 1)
    )
    v <- cbind(
      a = c(1, 1, 0),
      b = c(0, 1, 0)
    )
    r <- data.frame(observation_id = letters[1:3])
    r$y <- y
    p <- hm_prediction(
      v, t, r$observation_id,
      probabilities = cbind(
        a = c(0.8, 0.7, 0.2),
        b = c(0.1, 0.9, 0.3)
      ),
      class_map = c("a", "b")
    )
    e <- hm_evaluate(p, r)
    expect_equal(
      unname(e$metrics["hamming_loss"]),
      3 / 6
    )
    expect_equal(
      unname(e$metrics["subset_accuracy"]),
      1 / 3
    )
    expect_equal(
      unname(e$metrics["micro_f1"]),
      4 / 7
    )
    expect_equal(
      unname(e$metrics["macro_f1"]),
      mean(c(0.5, 2 / 3))
    )
    expect_equal(
      unname(e$metrics["brier"]),
      mean((p$probabilities - y)^2)
    )
    p$probabilities[1, 1] <- 1.1
    expect_error(
      hm_evaluate(p, r),
      "per-label probabilities"
    )
    p$probabilities <- NULL
    r$y[, 2] <- 0
    expect_true(is.na(hm_evaluate(p, r)$metrics["macro_f1"]))
  }
)
test_that(
  "reconstruction angles retain zero-norm unavailable cases",
  {
    t <- hm_task(NULL, "reconstruction", "spectrum", "subject_id")
    r <- data.frame(observation_id = c("a", "b"))
    attr(r, "target") <- rbind(
      c(1, 0),
      c(0, 1)
    )
    p <- hm_prediction(
      rbind(
        c(0, 1),
        c(0, 1)
      ),
      t, r$observation_id
    )
    expect_equal(
      unname(hm_evaluate(p, r)$metrics["spectral_angle"]),
      pi / 4
    )
    p$values[1, ] <- 0
    expect_true(is.na(hm_evaluate(p, r)$metrics["spectral_angle"]))
    expect_equal(
      unname(hm_evaluate(p, r)$metrics["zero_norm_spectra"]),
      1
    )
  }
)
