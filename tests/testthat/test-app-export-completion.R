test_that("exports complete only after reproduction contents are written", {
  data <- hm_example()
  task <- hm_task("response", "regression", "spectrum", "subject_id")
  workflow <- hm_workflow(task, hm_recipe(), hm_model_spec("lm", task))
  split <- hm_split(data, task, folds = 3L, seed = 42L)$folds[[1L]]
  fit <- hm_fit(workflow, data, split)
  spectra <- hm_spectra(data)
  heldout <- hm_dataset(
    spectra$values[split$test, , drop = FALSE],
    spectra$observations[split$test, , drop = FALSE],
    spectra$band_map, provenance = spectra$provenance
  )
  prediction <- hm_predict(fit, heldout)
  result <- list(
    fit = fit, prediction = prediction,
    evaluation = hm_evaluate(prediction, heldout@observations),
    workflow = workflow, split = split, control = list(),
    input_hash = digest::digest(data, algo = "sha256")
  )
  area <- tempfile("export-completion-")
  dir.create(area)
  on.exit(unlink(area, recursive = TRUE))
  target <- file.path(area, "failed")
  fail <- TRUE
  write <- base::writeLines
  local_mocked_bindings(
    writeLines = function(text, con = stdout(), ...) {
      if (is.character(con) && endsWith(con, "/reproduce.R")) {
        expect_false(file.exists(file.path(target, "COMPLETE")))
        expect_error(hm_read_run(target), "not complete")
        if (fail) cli::cli_abort("simulated reproduction write failure")
      }
      write(text, con, ...)
    }, .package = "base"
  )
  expect_error(hm_export_result(result, target), "simulated reproduction")
  expect_error(hm_read_run(target), "not complete")
  expect_identical(readRDS(file.path(target, "receipt.rds"))$state,
                   "failed")
  expect_error(hm_export_result(result, target), class = "hm_run_conflict")
  target <- file.path(area, "completed")
  fail <- FALSE
  hm_export_result(result, target)
  expect_true(file.exists(file.path(target, "reproduce.R")))
  expect_false(dir.exists(file.path(target, ".partial")))
  expect_identical(hm_read_run(target)$metrics, result$evaluation$metrics)
  environment <- new.env()
  sys.source(file.path(target, "reproduce.R"), environment)
  expect_identical(environment$reproduce(data, target), prediction)
})

test_that("temporal exports reproduce without discarding the time contract", {
  data <- hm_example()
  data@observations$measured_time <- seq_len(nrow(data@observations))
  task <- hm_task("response", "regression", "spectrum", "subject_id")
  workflow <- hm_workflow(task, hm_recipe(), hm_model_spec("lm", task))
  split <- hm_split(data, task, design = "temporal", time = "measured_time",
                    cutoff = 65, look_back = 1, embargo = 1)
  fit <- hm_fit(workflow, data, split)
  spectra <- hm_spectra(data)
  heldout <- hm_dataset(
    spectra$values[split$test, , drop = FALSE],
    spectra$observations[split$test, , drop = FALSE],
    spectra$band_map, provenance = spectra$provenance
  )
  prediction <- hm_predict(fit, heldout)
  result <- list(
    fit = fit, prediction = prediction,
    evaluation = hm_evaluate(prediction, heldout@observations),
    workflow = workflow, split = split, control = list(),
    input_hash = digest::digest(data, algo = "sha256")
  )
  target <- tempfile("temporal-export-")
  on.exit(unlink(target, recursive = TRUE))
  hm_export_result(result, target)
  saved <- hm_read_run(target)
  expect_null(saved$split$observation_id)
  expect_null(saved$split$roles)
  environment <- new.env()
  sys.source(file.path(target, "reproduce.R"), environment)
  expect_identical(environment$reproduce(data, target), prediction)
  changed <- data
  changed@observations$measured_time[split$train] <- 1000
  expect_error(environment$reproduce(changed, target), "Input hash")
  expect_error(hm_fit(saved$workflow, changed, saved$split),
               class = "hm_split_error")
})
