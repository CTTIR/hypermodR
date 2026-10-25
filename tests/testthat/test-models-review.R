.review_fit <- function(
  id, x, y = NULL, type = "regression", p = list(),
  control = list()
) {
  .hm_seed(
    17, .hm_fit_engine(
      id, x, y, list(type = type),
      p, control
    )
  )
}

test_that(
  "weighted LM agrees with independent weighted normal equations",
  {
    x <- cbind(
      seq_len(12),
      rep(
        c(-1, 1, 0),
        4
      )
    )
    y <- 1 + x %*% c(2, -3) +
      sin(seq_len(12))
    w <- c(0, 1:11)
    fit <- .review_fit(
      "lm", x, as.vector(y),
      control = list(weights = w)
    )
    d <- cbind(1, x)
    expected <- solve(
      crossprod(d, w * d),
      crossprod(d, w * y)
    )
    expect_equal(
      unname(coef(fit$object)),
      as.vector(expected),
      tolerance = 1e-10
    )
    expect_error(
      .review_fit(
        "pls", x, as.vector(y),
        control = list(weights = w)
      ),
      class = "hm_capability_error"
    )
  }
)

test_that(
  "full rank PLS and PCR recover exact independent held-out plane",
  {
    skip_if_not_installed("pls")
    x <- cbind(
      seq_len(20),
      rep(
        c(-2, 1, 3, -1),
        5
      )
    )
    y <- 7 + x %*% c(0.3, -2)
    z <- rbind(
      c(21, 2),
      c(0, -4)
    )
    for (id in c("pls", "pcr")) {
      fit <- .review_fit(
        id, x, as.vector(y),
        p = list(ncomp = 2L)
      )
      expect_equal(
        .hm_predict_engine(fit, z, NULL),
        c(9.3, 15),
        tolerance = 1e-08
      )
    }
  }
)

test_that(
  "GAM exact linear fixture predicts an independent line",
  {
    skip_if_not_installed("mgcv")
    x <- matrix(
      seq(-2, 2, length.out = 30),
      ncol = 1
    )
    fit <- .review_fit("gam", x, 4 + 2 * x[, 1])
    expect_equal(
      .hm_predict_engine(
        fit, matrix(
          c(-1.5, 0.5),
          ncol = 1
        ),
        NULL
      ),
      c(1, 5),
      tolerance = 1e-06
    )
  }
)

test_that(
  "mixed model population prediction excludes group offsets",
  {
    skip_if_not_installed("lme4")
    x <- matrix(
      rep(
        seq(-2, 2, length.out = 8),
        6
      ),
      ncol = 1
    )
    group <- rep(letters[1:6], each = 8)
    offsets <- rep(
      c(-4, -2, -1, 1, 2, 4),
      each = 8
    )
    noise <- rep(
      c(1, -1, -1, 1, 1, -1, -1, 1) *
        0.05, 6
    )
    fit <- .review_fit(
      "mixed", x, 3 + 2 * x[, 1] + offsets + noise,
      p = list(group = "site"),
      control = list(observations = data.frame(site = group))
    )
    expect_equal(
      .hm_predict_engine(
        fit, matrix(
          c(-1, 1),
          ncol = 1
        ),
        NULL
      ),
      c(1, 5),
      tolerance = 1e-06
    )
  }
)

test_that(
  "glmnet orthogonal ridge agrees with its closed form objective",
  {
    skip_if_not_installed("glmnet")
    x <- cbind(
      c(-1, -1, 1, 1),
      c(-1, 1, -1, 1)
    )
    y <- as.vector(x %*% c(2, 3))
    fit <- .review_fit("glmnet", x, y, p = list(alpha = 0, lambda = 0.5))
    expected <- as.vector(
      x %*% (c(2, 3) / (1 +
                          0.5 / sqrt(mean(y^2))))
    )
    expect_equal(
      .hm_predict_engine(fit, x, NULL),
      expected,
      tolerance = 1e-06
    )
  }
)

test_that(
  "NMF and MCR exact positive-factor fixtures retain output semantics",
  {
    x <- rbind(
      c(1, 0, 1),
      c(0, 1, 1),
      c(0.2, 0.8, 1),
      c(0.7, 0.3, 1)
    )
    endmembers <- x[1:2, ]
    for (id in c("nmf", "mcr_als")) {
      fit <- .review_fit(
        id, x,
        type = if (id == "nmf") {
          "reconstruction"
        } else {
          "unmixing"
        }, p = list(rank = 2, endmembers = endmembers)
      )
      out <- .hm_predict_engine(fit, x, NULL)
      expect_equal(
        out, if (id == "nmf") {
          x
        } else {
          x[, 1:2]
        },
        tolerance = 1e-06
      )
    }
  }
)

test_that(
  "Cox censoring fixture satisfies independent partial-likelihood score",
  {
    skip_if_not_installed("survival")
    x <- matrix(
      c(-1, 1, 0, 2, -2, 1, 0, -1),
      ncol = 1
    )
    y <- data.frame(time = 1:8, status = c(1, 1, 0, 1, 1, 0, 1, 0))
    fit <- .review_fit("coxph", x, y, "survival")
    score <- function(b) {
      sum(
        vapply(
          which(y$status == 1),
          function(i) {
            risk <- which(y$time >= y$time[i])
            w <- exp(b * x[risk, 1])
            x[i, 1] - sum(w * x[risk, 1]) / sum(w)
          }, numeric(1)
        )
      )
    }
    beta <- uniroot(score, c(-5, 5))$root
    expect_equal(
      unname(coef(fit$object)),
      beta,
      tolerance = 1e-05
    )
    risk <- .hm_predict_engine(
      fit, matrix(
        c(0, 1),
        ncol = 1
      ),
      NULL
    )
    expect_equal(
      risk[2] / risk[1], exp(beta),
      tolerance = 1e-05
    )
  }
)

test_that(
  "ordinal zero association recovers empirical cumulative proportions",
  {
    skip_if_not_installed("ordinal")
    x <- matrix(
      rep(-2:2, each = 10),
      ncol = 1
    )
    y <- ordered(
      rep(
        c(
          rep("low", 2),
          rep("mid", 3),
          rep("high", 5)
        ),
        5
      ),
      levels = c("low", "mid", "high")
    )
    fit <- .review_fit("clm", x, y, "ordinal")
    expect_equal(
      .hm_predict_engine(
        fit, matrix(
          c(-1, 1),
          ncol = 1
        ),
        NULL
      ),
      matrix(
        rep(
          c(0.2, 0.3, 0.5),
          each = 2
        ),
        2,
        dimnames = list(NULL, levels(y))
      ),
      tolerance = 1e-06, ignore_attr = TRUE
    )
  }
)

test_that(
  "PAM and DBSCAN return independent known partitions with held-out support",
  {
    x <- rbind(
      c(0, 0),
      c(0.1, 0),
      c(0, 0.1),
      c(5, 5),
      c(5.1, 5),
      c(5, 5.1)
    )
    for (id in c("pam", "dbscan")) {
      if (!requireNamespace(
        if (id == "pam") {
          "cluster"
        } else {
          "dbscan"
        },
        quietly = TRUE
      )) {
        next
      }
      p <- if (id == "pam") {
        list(k = 2)
      } else {
        list(eps = 0.3, min_points = 2)
      }
      fit <- .review_fit(id, x, type = "clustering", p = p)
      out <- .hm_predict_engine(
        fit, rbind(
          c(0.05, 0.05),
          c(5.05, 5.05),
          c(0.02, 0.02)
        ),
        NULL
      )
      expect_true(out[1] > 0 && out[2] > 0 && out[1] != out[2] &&
                    out[1] == out[3])
    }
  }
)

test_that(
  "one-class SVM orders a distant held-out anomaly above training center",
  {
    skip_if_not_installed("kernlab")
    x <- as.matrix(
      expand.grid(
        seq(-0.2, 0.2, length.out = 5),
        seq(-0.2, 0.2, length.out = 5)
      )
    )
    fit <- .review_fit("one_class_svm", x, type = "anomaly")
    out <- .hm_predict_engine(
      fit, rbind(
        c(0, 0),
        c(8, 8)
      ),
      NULL
    )
    expect_true(all(is.finite(out)))
    expect_gt(out[2], out[1])
  }
)

test_that(
  "xgboost regression and multinomial APIs preserve task output shapes",
  {
    skip_if_not_installed("xgboost")
    x <- cbind(
      rep(-1:1, each = 6),
      rep(
        c(-1, 1),
        9
      )
    )
    fit <- .review_fit(
      "xgboost", x, rep(4, 18),
      p = list(nrounds = 2)
    )
    expect_equal(
      .hm_predict_engine(fit, x[1:2, , drop = FALSE], NULL),
      c(4, 4),
      tolerance = 1e-06
    )
    y <- factor(
      rep(
        c("z", "a", "m"),
        each = 6
      ),
      levels = c("z", "a", "m")
    )
    fit <- .review_fit("xgboost", x, y, "classification", list(nrounds = 2))
    out <- .hm_predict_engine(
      fit, x[c(1, 7, 13), ,
        drop = FALSE
      ], NULL
    )
    expect_identical(
      colnames(out),
      levels(y)
    )
    expect_equal(
      as.numeric(rowSums(out)),
      rep(1, 3),
      tolerance = 1e-06
    )
    expect_identical(
      max.col(out),
      1:3
    )
  }
)

test_that(
  "rank-one PCA uses training mean on held-out spectra",
  {
    x <- cbind(-2:2, 2 * (-2:2))
    fit <- .review_fit("pca", x, type = "reconstruction", p = list(rank = 1))
    expect_equal(
      unname(
        .hm_predict_engine(
          fit, matrix(
            c(1, 0),
            nrow = 1
          ),
          NULL
        )
      ),
      matrix(
        c(0.2, 0.4),
        nrow = 1
      ),
      tolerance = 1e-12
    )
  }
)

test_that(
  "ICA full-rank reconstruction has the declared matrix shape",
  {
    skip_if_not_installed("fastICA")
    x <- cbind(
      seq(-2, 2, length.out = 30),
      sin(seq_len(30))
    )
    fit <- .review_fit("ica", x, type = "reconstruction", p = list(rank = 2))
    expect_equal(
      .hm_predict_engine(fit, x[1:3, , drop = FALSE], NULL),
      x[1:3, , drop = FALSE],
      tolerance = 1e-06
    )
  }
)

test_that(
  "physical inverse rejects nonidentifiability and mutated source identity",
  {
    wave <- c(500, 600, 700)
    p <- list(
      library = hm_spectral_library(
        matrix(1, 3, 2),
        wave, "units", "fixture", "CC0"
      ),
      path_length = 1
    )
    control <- list(
      wavelength = wave, input_provenance = list(
        radiometric_unit = "reflectance", calibration = "fixture"
      )
    )
    expect_error(
      .review_fit(
        "beer_lambert", matrix(0.5, 4, 3),
        type = "unmixing", p = p, control = control
      ),
      class = "hm_identifiability_error"
    )
    p$library$values[1, 1] <- 2
    expect_error(
      .review_fit(
        "beer_lambert", matrix(0.5, 4, 3),
        type = "unmixing", p = p, control = control
      ),
      class = "hm_integrity_error"
    )
  }
)

test_that(
  "public recipe training and stochastic fitting preserve external RNG",
  {
    x <- cbind(
      seq_len(12),
      rep(
        c(0, 1, -1),
        4
      )
    )
    obs <- data.frame(
      observation_id = paste0("o", 1:12),
      subject_id = paste0("s", 1:12),
      y = 1:12
    )
    data <- hm_dataset(x, obs, data.frame(wavelength = c(500, 600)))
    task <- hm_task(NULL, "clustering", "spectrum", "subject_id")
    wf <- hm_workflow(
      task, hm_recipe(list(list(type = "scale"))),
      hm_model_spec("kmeans", task, list(centers = 2))
    )
    set.seed(734)
    before <- .Random.seed
    fit <- hm_fit(
      wf, data, list(train = 1:8, test = 9:12),
      list(seed = 42)
    )
    expect_identical(.Random.seed, before)
    expect_equal(fit$recipe$steps[[1]]$center, colMeans(x[1:8, ,
                                                          drop = FALSE
                                                        ]))
    fit2 <- hm_fit(
      wf, data, list(train = 1:8, test = 9:12),
      list(seed = 42)
    )
    expect_equal(
      hm_predict(fit, data)$values,
      hm_predict(fit2, data)$values
    )
  }
)

test_that(
  "supplied factor count must agree with declared NMF and MCR rank",
  {
    x <- rbind(
      c(1, 0, 1),
      c(0, 1, 1),
      c(0.2, 0.8, 1),
      c(0.7, 0.3, 1)
    )
    for (id in c("nmf", "mcr_als")) {
      expect_error(
        .review_fit(
          id, x,
          type = if (id == "nmf") {
            "reconstruction"
          } else {
            "unmixing"
          }, p = list(rank = 1, endmembers = x[1:2, ])
        ),
        class = "hm_error"
      )
    }
  }
)
