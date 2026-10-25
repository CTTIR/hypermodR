test_that(
  "export rejects incomplete and sensitive configurations",
  {
    expect_error(
      hm_export_result(list(), tempfile()),
      "completed"
    )
    expect_error(
      hypermodR:::.hm_app_export_safe(list(token = "x")),
      "credentials"
    )
    expect_error(
      hypermodR:::.hm_app_export_safe(list(url = "https://user:pass@host")),
      "credential"
    )
  }
)
test_that(
  "export reproduces the admitted synthetic prediction exactly",
  {
    d <- hm_example()
    task <- hm_task("response", "regression", "spectrum", "subject_id")
    w <- hm_workflow(task, hm_recipe(), hm_model_spec("lm", task))
    split <- hm_split(d, task, "grouped", seed = 42, folds = 3)$folds[[1]]
    fit <- hm_fit(w, d, split, list(seed = 42))
    s <- hm_spectra(d)
    i <- split$test
    heldout <- hm_dataset(
      s$values[i, , drop = FALSE], s$observations[i, , drop = FALSE],
      s$band_map,
      provenance = s$provenance
    )
    prediction <- hm_predict(fit, heldout)
    result <- list(
      fit = fit, prediction = prediction, evaluation = hm_evaluate(
        prediction, heldout@observations
      ),
      workflow = w, split = split, control = list(seed = 42),
      input_hash = digest::digest(d, algo = "sha256")
    )
    path <- tempfile("app-export-")
    on.exit(unlink(path, recursive = TRUE))
    hm_export_result(result, path)
    env <- new.env()
    sys.source(
      file.path(path, "reproduce.R"),
      env
    )
    expect_identical(
      env$reproduce(d, path),
      prediction
    )
    expect_null(hm_read_run(path)$private_artifacts)
    expect_error(hm_export_result(result, path))
  }
)
