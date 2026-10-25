test_that("linear coefficients map back to native band units", {
  values <- cbind(seq_len(12), rep(c(-1, 1, 0), 4))
  observations <- data.frame(
    observation_id = as.character(seq_len(12)),
    subject_id = as.character(seq_len(12)),
    outcome = 4 + 2 * values[, 1] - 3 * values[, 2]
  )
  data <- hm_dataset(
    values, observations,
    data.frame(wavelength = c(500, 600))
  )
  task <- hm_task("outcome", "regression", "spectrum", "subject_id")
  workflow <- hm_workflow(
    task, hm_recipe(list(list(type = "scale"))),
    hm_model_spec("lm", task)
  )
  fit <- hm_fit(workflow, data, list(train = 1:9, test = 10:12))
  native <- stats::coef(fit, scale = "original")
  expect_equal(as.numeric(native), c(4, 2, -3), tolerance = 1e-10)
  expect_equal(attr(native, "band_map")$wavelength, c(500, 600))
  augmented <- generics::augment(fit, data)
  expect_equal(augmented$.fitted, observations$outcome, tolerance = 1e-10)
  expect_equal(augmented$.resid, rep(0, 12), tolerance = 1e-10)
  expect_false(generics::glance(fit)$scientific_qualification)
  fit$recipe$steps <- list(list(type = "snv"))
  expect_error(stats::coef(fit, scale = "original"),
    class = "hm_unsupported_error"
  )
})

test_that("tidy uncertainty agrees with a direct least squares oracle", {
  values <- matrix(seq_len(12), ncol = 1)
  y <- 1 + 2 * values[, 1] + rep(c(-0.2, 0.1, 0.1), 4)
  obs <- data.frame(
    observation_id = letters[1:12],
    subject_id = letters[1:12], outcome = y
  )
  data <- hm_dataset(values, obs, data.frame(wavelength = 500))
  task <- hm_task("outcome", "regression", "spectrum", "subject_id")
  fit <- hm_fit(
    hm_workflow(task, hm_recipe(), hm_model_spec("lm", task)),
    data, list(train = 1:9, test = 10:12)
  )
  table <- generics::tidy(fit, conf.int = TRUE)
  design <- cbind(1, values[1:9, 1])
  residual <- y[1:9] - as.vector(design %*% table$estimate)
  se <- sqrt(diag(solve(crossprod(design))) * sum(residual^2) / 7)
  expect_equal(table$std.error, se, tolerance = 1e-10)
  expect_equal(table$conf.high - table$estimate,
    stats::qt(0.975, 7) * se,
    tolerance = 1e-10
  )
  expect_error(generics::tidy(fit, conf.level = 1), class = "hm_error")
})

test_that(paste0(
  "physical kernels and residual hybrids retain separate ",
  "assumptions"
), {
  kernel <- hm_physics_kernel(
    function(y, p) matrix(y / p$scale, ncol = 1),
    function(x, p) as.vector(x[, 1] * p$scale),
    list(scale = 2), "synthetic reversible kernel",
    c(input = "arbitrary spectrum", output = "synthetic endpoint")
  )
  x <- matrix(seq_len(8), ncol = 1)
  fit <- .hm_fit_engine(
    "physics_hybrid", x, 3 + 2 * x[, 1],
    list(type = "regression"), list(kernel = kernel), list()
  )
  expect_equal(.hm_predict_engine(fit, x, NULL), 3 + 2 * x[, 1],
    tolerance = 1e-10
  )
  fit$object$kernel$parameters$scale <- 3
  expect_error(.hm_predict_engine(fit, x, NULL), class = "hm_integrity_error")
})
