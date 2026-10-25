test_that(
  paste0(
    "survival workflow uses declared reference columns and ",
    "source-aligned weights"
  ),
  {
    skip_if_not_installed("survival")
    x <- matrix(
      rep(
        c(-1, 1),
        5
      ),
      ncol = 1
    )
    observations <- data.frame(
      observation_id = paste0("o", 1:10),
      subject_id = paste0("s", 1:10),
      y = 0, time = rep(1:5, each = 2),
      status = 1
    )
    data <- hm_dataset(
      x, observations, data.frame(wavelength = 500)
    )
    task <- hm_task(
      "y", "survival", "subject", "subject_id",
      list(time = "time", status = "status")
    )
    model <- hm_fit(
      hm_workflow(
        task, hm_recipe(), hm_model_spec("coxph", task)
      ),
      data, list(train = 1:8, test = 9:10),
      list(weights = rep(1, 10))
    )
    expect_equal(
      hm_predict(model, data)$values,
      rep(1, 10),
      tolerance = 1e-10
    )
    expect_identical(
      model$engine$prediction_scope,
      "relative hazard; baseline survival not inferred"
    )
    invalid <- hm_task("y", "survival", "subject", "subject_id")
    expect_error(
      hm_fit(
        hm_workflow(
          invalid, hm_recipe(), hm_model_spec("coxph", invalid)
        ),
        data, list(train = 1:8, test = 9:10)
      ),
      "time/status reference"
    )
  }
)

test_that(
  "matrix targets retain row support and explicit augmentation vectors",
  {
    x <- rbind(
      c(0.2, 0.8),
      c(0.7, 0.3),
      c(0.4, 0.6),
      c(0.9, 0.1),
      c(0.3, 0.7),
      c(0.6, 0.4)
    )
    observations <- data.frame(
      observation_id = paste0("o", 1:6),
      subject_id = paste0("s", 1:6)
    )
    observations$abundance <- x
    data <- hm_dataset(
      x, observations, data.frame(wavelength = c(500, 600))
    )
    kernel <- hm_physics_kernel(
      function(y, p) y,
      function(x, p) x,
      list(), "synthetic identity mixture law",
      c(
        input = "relative intensity",
        output = "fraction"
      )
    )
    task <- hm_task(
      "abundance", "unmixing", "spectrum",
      "subject_id"
    )
    fit <- hm_fit(
      hm_workflow(
        task, hm_recipe(), hm_model_spec(
          "physics_inverse", task, list(kernel = kernel)
        )
      ),
      data, list(train = 1:4, test = 5:6)
    )
    expect_equal(
      hm_predict(fit, data)$values,
      x
    )
    augmented <- generics::augment(fit, data)
    expect_length(augmented$.fitted, 6)
    expect_equal(
      augmented$.fitted[[5]], x[5, , drop = FALSE]
    )
  }
)
