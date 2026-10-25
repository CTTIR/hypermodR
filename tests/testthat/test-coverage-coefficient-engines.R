test_that(
  "latent coefficient tables retain unavailable uncertainty",
  {
    skip_if_not_installed("pls")
    x <- cbind(
      seq_len(18),
      rep(
        c(-2, 0, 3),
        6
      )
    )
    obs <- data.frame(
      observation_id = paste0("o", 1:18),
      subject_id = paste0("s", 1:18),
      y = 4 + 2 * x[, 1] - 3 * x[, 2]
    )
    dataset <- hm_dataset(
      x, obs, data.frame(wavelength = c(500, 650))
    )
    task <- hm_task("y", "regression", "spectrum", "subject_id")
    fit <- hm_fit(
      hm_workflow(
        task, hm_recipe(), hm_model_spec("pls", task, list(ncomp = 2))
      ),
      dataset, list(train = 1:15, test = 16:18)
    )
    expect_equal(
      as.numeric(coef(fit, scale = "original")),
      c(4, 2, -3),
      tolerance = 1e-10
    )
    table <- generics::tidy(fit, conf.int = TRUE)
    expect_identical(table$term, paste0("coefficient_", 1:3))
    expect_true(all(is.na(table$std.error)))
    expect_true(all(is.na(table$conf.low)))
    expect_true(
      all(
        table$uncertainty == "unavailable for this engine"
      )
    )
    glm <- hm_fit(
      hm_workflow(
        task,
        hm_recipe(
        ), hm_model_spec("glm", task, list(family = "gaussian"))
      ),
      dataset, list(train = 1:15, test = 16:18)
    )
    expect_identical(
      attr(
        coef(glm),
        "link"
      ),
      "identity"
    )
    expect_equal(
      predict(glm, dataset)$values,
      obs$y, tolerance = 1e-10
    )
  }
)

test_that(
  "Bayesian coefficient tables report conditional posterior spread",
  {
    x <- matrix(1:8, ncol = 1)
    obs <- data.frame(
      observation_id = paste0("o", 1:8),
      subject_id = paste0("s", 1:8),
      y = 2 + 3 * x[, 1]
    )
    data <- hm_dataset(x, obs, data.frame(wavelength = 500))
    task <- hm_task("y", "regression", "spectrum", "subject_id")
    model <- hm_fit(
      hm_workflow(
        task, hm_recipe(), hm_model_spec(
          "bayes_conjugate",
          task, list(noise_variance = 2, prior_variance = 4)
        )
      ),
      data, list(train = 1:6, test = 7:8)
    )
    table <- generics::tidy(model, conf.int = TRUE)
    covariance <- solve(
      matrix(
        c(3.25, 10.5, 10.5, 45.75),
        2, 2
      )
    )
    expect_equal(
      table$std.error, sqrt(diag(covariance)),
      tolerance = 1e-10
    )
    expect_true(
      all(
        table$uncertainty == "conditional Gaussian posterior"
      )
    )
    expect_true(all(is.na(table$p.value)))
  }
)
