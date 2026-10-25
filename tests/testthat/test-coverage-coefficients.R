test_that(paste0(
  "original-band coefficients recover an independently ",
  "specified plane"
), {
  x <- cbind(seq_len(12), rep(c(-2, 1, 3), 4))
  y <- 7 + 2 * x[, 1] - 3 * x[, 2]
  observations <- data.frame(
    observation_id = paste0("o", 1:12),
    subject_id = paste0("s", 1:12), y = y
  )
  dataset <- hm_dataset(x, observations, data.frame(wavelength = c(500, 650)))
  task <- hm_task("y", "regression", "spectrum", "subject_id")
  recipe <- hm_recipe(list(list(type = "scale")))
  workflow <- hm_workflow(task, recipe, hm_model_spec("lm", task))
  model <- hm_fit(workflow, dataset, list(train = 1:10, test = 11:12))
  beta <- coef(model, scale = "original")
  expect_equal(as.numeric(beta), c(7, 2, -3), tolerance = 1e-10)
  expect_identical(names(beta), c("(Intercept)", "band_1", "band_2"))
  expect_identical(attr(beta, "scale"), "original")
  expect_equal(unname(cbind(1, x) %*% beta), matrix(y), tolerance = 1e-10)
  expect_identical(attr(beta, "band_map"), model$band_map)
  expect_output(expect_identical(print(model), model), "hm_fit.*lm")
  expect_identical(summary(model), list(model = "lm", training_n = 10L))
  unsupported <- model
  unsupported$engine$model_id <- "ranger"
  expect_error(coef(unsupported), class = "hm_unsupported_error")
  nonlinear <- model
  nonlinear$recipe$steps[[1]]$type <- "normalize"
  expect_error(coef(nonlinear, scale = "original"),
    class = "hm_unsupported_error"
  )
  aliased <- model
  aliased$engine$object$coefficients[2] <- NA_real_
  expect_error(coef(aliased, scale = "original"),
    class = "hm_identifiability_error"
  )
  bayes <- model
  bayes$engine$model_id <- "bayes_conjugate"
  bayes$engine$object <- list(mean = c(10, 4, 6))
  expect_equal(as.numeric(coef(bayes)), c(10, 4, 6))
  expect_identical(names(coef(bayes)), c("(Intercept)", "band_1", "band_2"))
})

test_that(paste0(
  "hybrid residual predictions reconstruct a known ",
  "physical plus statistical law"
), {
  x <- matrix(seq_len(8), ncol = 1)
  kernel <- hm_physics_kernel(function(c, p) c * 2, function(
    s,
    p) {
    as.vector(s) / 2
  }, list(), "synthetic forward law", c(
    input = "signal", output = "amount"
  ))
  y <- 3 + 2.5 * as.vector(x)
  engine <- list(model_id = "physics_hybrid", parameters = list(
    kernel = kernel, residual_model = "lm"
  ))
  model <- .hm_fit_physics(engine, x, y, list())
  expect_equal(.hm_predict_physics(model, matrix(c(9, 10), ncol = 1)), c(
    25.5, 28
  ), tolerance = 1e-10)
  vector_kernel <- hm_physics_kernel(
    function(c, p) c, function(s, p) s,
    list(), "synthetic matrix", c(input = "signal", output = "amount")
  )
  engine$parameters$kernel <- vector_kernel
  expect_error(.hm_fit_physics(engine, x, y, list()), "one numeric endpoint")
})
