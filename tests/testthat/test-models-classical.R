test_that(
  "nearest neighbours use deterministic full-class probabilities",
  {
    x <- matrix(
      c(0, 1, 3, 4),
      ncol = 1
    )
    fit <- .hm_fit_engine(
      "knn", x, factor(c("a", "a", "b", "b")),
      list(type = "classification"),
      list(k = 2),
      list()
    )
    expect_equal(
      .hm_predict_engine(
        fit, matrix(
          c(0, 4, 2),
          ncol = 1
        ),
        NULL
      ),
      rbind(
        c(a = 1, b = 0),
        c(a = 0, b = 1),
        c(a = 0.5, b = 0.5)
      )
    )
    expect_error(
      .hm_fit_engine(
        "knn", x, 1:4, list(type = "regression"),
        list(k = 8),
        list()
      ),
      class = "hm_error"
    )
    gp <- .hm_fit_engine(
      "gaussian_process", x, c(0, 1, 3, 4),
      list(type = "regression"),
      list(noise_variance = 1e-08),
      list()
    )
    expect_equal(
      .hm_predict_engine(gp, x, NULL),
      c(0, 1, 3, 4),
      tolerance = 1e-06
    )
  }
)

test_that(
  "clustering and residual anomalies keep training identities",
  {
    x <- rbind(
      c(0, 0),
      c(0, 0.1),
      c(4, 4),
      c(4, 4.1)
    )
    fit <- .hm_seed(
      1, .hm_fit_engine(
        "kmeans", x, NULL, list(type = "clustering"),
        list(centers = 2),
        list()
      )
    )
    cluster <- .hm_predict_engine(fit, x, NULL)
    expect_true(
      cluster[1] == cluster[2] && cluster[3] == cluster[4] && cluster[1] !=
        cluster[3]
    )
    x <- cbind(1:8, (1:8) * 2)
    fit <- .hm_fit_engine(
      "pca_anomaly", x, NULL, list(type = "anomaly"),
      list(rank = 1),
      list()
    )
    expect_equal(
      .hm_predict_engine(fit, x, NULL),
      rep(0, 8),
      tolerance = 1e-20
    )
    expect_gt(
      .hm_predict_engine(
        fit, matrix(
          c(4, 30),
          nrow = 1
        ),
        NULL
      ),
      1
    )
  }
)

test_that(
  "optional conventional engines produce finite correctly shaped outputs",
  {
    set.seed(3)
    x <- matrix(
      stats::rnorm(120),
      40, 3
    )
    y <- factor(
      rep(
        c("a", "b"),
        each = 20
      )
    )
    packages <- c(
      lda = "MASS", qda = "MASS", naive_bayes = "e1071", svm = "e1071",
      rf = "ranger"
    )
    for (id in names(packages)) {
      if (!requireNamespace(packages[[id]], quietly = TRUE)) {
        next
      }
      fit <- .hm_fit_engine(
        id, x, y, list(type = "classification"),
        list(), list(seed = 2)
      )
      p <- .hm_predict_engine(fit, x[1:4, , drop = FALSE], NULL)
      expect_equal(
        dim(p),
        c(4L, 2L),
        info = id
      )
      expect_equal(
        rowSums(p),
        rep(1, 4),
        ignore_attr = TRUE, info = id
      )
    }
  }
)
