.engine_task <- function(type) list(type = type)

test_that(
  "linear and conjugate regression match independent algebra",
  {
    x <- matrix(
      seq_len(8),
      ncol = 1
    )
    y <- 2 + 3 * x[, 1]
    fit <- .hm_fit_engine(
      "lm", x, y, .engine_task("regression"),
      list(), list()
    )
    expect_equal(
      unname(stats::coef(fit$object)),
      c(2, 3),
      tolerance = 1e-10
    )
    expect_equal(
      .hm_predict_engine(
        fit, matrix(
          c(9, 10),
          ncol = 1
        ),
        NULL
      ),
      c(29, 32)
    )
    bayes <- .hm_fit_engine(
      "bayes_conjugate", x, y, .engine_task("regression"),
      list(noise_variance = 2, prior_variance = 4),
      list()
    )
    design <- cbind(1, x)
    expected <- solve(
      crossprod(design) / 2 +
        diag(0.25, 2),
      crossprod(design, y) / 2
    )
    expect_equal(bayes$object$mean, as.vector(expected))
    expect_true(
      all(
        eigen(bayes$object$covariance)$values >
          0
      )
    )
    expect_error(
      .hm_fit_engine(
        "bayes_conjugate", x, y, .engine_task("regression"),
        list(), list()
      ),
      class = "hm_error"
    )
  }
)

test_that(
  "binary GLM probabilities retain exact class map",
  {
    x <- matrix(
      rep(-2:2, each = 4),
      ncol = 1
    )
    y <- factor(
      rep(
        c("no", "yes", "no", "yes"),
        5
      )
    )
    fit <- .hm_fit_engine(
      "glm", x, y, .engine_task("classification"),
      list(), list()
    )
    p <- .hm_predict_engine(fit, x[1:2, , drop = FALSE], NULL)
    expect_identical(
      colnames(p),
      c("no", "yes")
    )
    expect_equal(
      p, matrix(0.5, 2, 2, dimnames = list(NULL, c("no", "yes"))),
      ignore_attr = TRUE
    )
    expect_error(
      .hm_fit_engine(
        "lm", x, y, .engine_task("classification"),
        list(), list()
      ),
      class = "hm_capability_error"
    )
  }
)

test_that(
  "FCLS abundances and nonnegative inverse recovery are exact",
  {
    skip_if_not_installed("quadprog")
    endmembers <- diag(3)
    abundance <- rbind(
      c(0.2, 0.3, 0.5),
      c(1, 0, 0),
      c(0, 0.25, 0.75),
      c(0.4, 0.4, 0.2)
    )
    fit <- .hm_fit_engine(
      "fcls", abundance, NULL, .engine_task("unmixing"),
      list(endmembers = endmembers),
      list()
    )
    expect_equal(
      .hm_predict_engine(fit, abundance, NULL),
      abundance,
      tolerance = 1e-09
    )
    expect_equal(
      .hm_nnls(
        diag(3),
        c(-1, 2, 3)
      ),
      c(0, 2, 3),
      tolerance = 1e-08
    )
    expect_error(
      .hm_fit_engine(
        "fcls", abundance, NULL, .engine_task("unmixing"),
        list(endmembers = matrix(1, 2, 3)),
        list()
      ),
      class = "hm_error"
    )
  }
)

test_that(
  "spectral inverse model requires and preserves physical declarations",
  {
    wave <- c(500, 600, 700)
    extinction <- matrix(
      c(1, 0, 1, 0, 1, 1),
      3, 2
    )
    library <- hm_spectral_library(
      extinction, wave, "per concentration per length", "synthetic fixture",
      "CC0 synthetic fixture"
    )
    concentration <- rbind(
      c(0.1, 0.2),
      c(0.2, 0.05),
      c(0.05, 0.1),
      c(0.3, 0.1)
    )
    reflectance <- exp(
      -concentration %*% t(extinction) *
        2
    )
    control <- list(
      wavelength = wave, input_provenance = list(
        radiometric_unit = "reflectance", calibration = "synthetic exact"
      )
    )
    fit <- .hm_fit_engine(
      "beer_lambert", reflectance, NULL, .engine_task("unmixing"),
      list(library = library, path_length = 2),
      control
    )
    expect_equal(
      .hm_predict_engine(fit, reflectance, NULL),
      concentration,
      tolerance = 1e-06
    )
    expect_error(
      .hm_fit_engine(
        "beer_lambert", reflectance, NULL, .engine_task("unmixing"),
        list(library = library, path_length = 2),
        list()
      ),
      class = "hm_error"
    )
    expect_error(
      .hm_predict_engine(fit, reflectance * 0, NULL),
      class = "hm_error"
    )
  }
)

test_that(
  "train-only public fitting rejects split and wavelength bypasses",
  {
    values <- matrix(
      seq_len(12),
      ncol = 2
    )
    obs <- data.frame(
      observation_id = letters[1:6], subject_id = letters[1:6],
      outcome = seq_len(6) *
        3
    )
    data <- hm_dataset(values, obs, data.frame(wavelength = c(500, 600)))
    task <- hm_task("outcome", "regression", "spectrum", "subject_id")
    workflow <- hm_workflow(task, hm_recipe(), hm_model_spec("lm", task))
    fit <- hm_fit(workflow, data, list(train = 1:4, test = 5:6))
    expect_s3_class(
      hm_predict(fit, data),
      "hm_prediction"
    )
    wrong <- hm_dataset(values, obs, data.frame(wavelength = c(501, 600)))
    expect_error(
      hm_predict(fit, wrong),
      class = "hm_band_mismatch"
    )
    expect_error(
      hm_fit(workflow, data, list(train = c(1, 2.5))),
      class = "hm_error"
    )
    obs$subject_id[5] <- obs$subject_id[1]
    leaked <- hm_dataset(values, obs, data.frame(wavelength = c(500, 600)))
    expect_error(
      hm_fit(workflow, leaked, list(train = 1:4, test = 5:6)),
      class = "hm_split_error"
    )
    expect_error(
      hm_fit(
        workflow, list(values = values, observations = obs),
        list(train = 1:4)
      ),
      class = "hm_error"
    )
  }
)
