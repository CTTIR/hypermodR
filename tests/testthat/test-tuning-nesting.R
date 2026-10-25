test_that(
  "R-only inner selection excludes outer units and records every fit",
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    n <- 16L
    obs <- data.frame(
      observation_id = paste0("o", 1:n),
      subject_id = paste0("s", 1:n),
      y = seq_len(n) *
        2
    )
    d <- hm_dataset(
      cbind(
        seq_len(n),
        sin(seq_len(n))
      ),
      obs, data.frame(wavelength = c(500, 600))
    )
    t <- hm_task("y", "regression", "spectrum", "subject_id")
    w <- hm_workflow(t, hm_recipe(), hm_model_spec("lm", t))
    f <- list(
      train = 1:12, test = 13:16, inner = list(
        list(train = 1:6, test = 7:12),
        list(train = 7:12, test = 1:6)
      )
    )
    fit <- hm_tune(
      w, d, f, list(list(), list()),
      control = list(seed = 2, max_trials = 2)
    )
    expect_length(fit$tuning$ledger, 4)
    for (l in fit$tuning$ledger) {
      expect_length(
        intersect(
          c(l$train, l$validation),
          f$test
        ),
        0
      )
      expect_setequal(
        c(l$train, l$validation),
        f$train
      )
    }
    expect_equal(fit$training, 1:12)
    f$inner[[1]]$test <- 13:16
    expect_error(
      hm_tune(w, d, f, list(list())),
      "outer training"
    )
    expect_error(
      hm_tune(
        w, d, list(train = 1:12, test = 13:16),
        list(list())
      ),
      "inner folds"
    )
  }
)
test_that(
  "active learning leaves a review queue and excludes held-out labels",
  {
    z <- eval_fixture()
    pool <- z$r
    pool$y <- NA_character_
    obs <- rbind(
      z$r, transform(
        z$r[1, , drop = FALSE],
        observation_id = "outside", subject_id = "heldout",
        acquisition_id = "heldout"
      )
    )
    d <- hm_dataset(
      matrix(
        seq_len(14),
        7, 2
      ),
      obs, data.frame(wavelength = c(500, 600))
    )
    sp <- list(train = 1:6, test = 7L)
    q <- hm_active_learning(
      z$p, pool, z$r$observation_id, "outside", 2, 1, "revision1", d,
      sp
    )
    expect_false(q$labels_acquired)
    expect_false(
      identical(
        q$run_hash, hm_active_learning(
          z$p, pool, z$r$observation_id, "outside", 2, 1, "revision2",
          d, sp
        )$run_hash
      )
    )
    expect_true(all(q$queue$status == "pending_annotation_review"))
    expect_error(
      hm_active_learning(
        z$p, pool, z$r$observation_id, "o1", 2, 1,
        "revision1", d, sp
      ),
      "validated grouped split"
    )
    expect_error(
      hm_active_learning(
        z$p, z$r, z$r$observation_id, "outside", 2, 1, "revision1",
        d, sp
      ),
      "unlabeled"
    )
  }
)
test_that(
  "calibration freezes only an outer-training held-out fit",
  {
    n <- 24L
    obs <- data.frame(
      observation_id = paste0("o", 1:n),
      subject_id = paste0("s", 1:n),
      y = rep(
        c("a", "b"),
        12
      )
    )
    d <- hm_dataset(
      cbind(
        seq_len(n),
        rep(
          c(0, 1),
          12
        )
      ),
      obs, data.frame(wavelength = c(500, 600))
    )
    t <- hm_task("y", "classification", "spectrum", "subject_id")
    w <- hm_workflow(t, hm_recipe(), hm_model_spec("knn", t, list(k = 3)))
    inner <- list(train = 1:12, test = 13:18)
    outer <- list(train = 1:18, test = 19:24)
    fit <- hm_fit(w, d, inner)
    nd <- list(
      values = hm_spectra(d)$values[13:18, ,
        drop = FALSE
      ], observations = obs[13:18, , drop = FALSE],
      band_map = d@bands
    )
    p <- hm_predict(fit, nd)
    cal <- hm_calibrate(
      p, nd$observations, inner, d,
      fit = fit, outer_split = outer, abstention = 0.95
    )
    altered <- d
    altered@observations$y[1] <- "b"
    expect_error(
      hm_calibrate(p, nd$observations, inner, altered,
        fit = fit,
        outer_split = outer
      ),
      "revision differs"
    )
    bad <- p
    bad$probabilities[1, ] <- rev(bad$probabilities[1, ])
    # Altering a non-symmetric row must fail the training-only fit
    # readback.
    bad$probabilities[1, ] <- c(0.31, 0.69)
    expect_error(
      hm_calibrate(bad, nd$observations, inner, d,
        fit = fit,
        outer_split = outer
      ),
      "supplied training-only fit"
    )
    expect_equal(cal$fit_ids, obs$observation_id[1:12])
    expect_equal(cal$calibration_ids, p$observation_id)
    applied <- hm_apply_calibration(cal, p)
    expect_equal(
      rowSums(applied$probabilities),
      rep(1, 6)
    )
    expect_identical(cal, applied$provenance$calibration)
    expect_error(
      hm_calibrate(
        p, nd$observations, inner, d,
        fit = fit, outer_split = list(train = 1:12, test = 13:24)
      ),
      "outer training"
    )
  }
)
