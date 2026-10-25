test_that("installed reference exports relocate and explicitly reproduce", {
  skip_if_not(Sys.info()[["sysname"]] == "Linux")
  python <- Sys.getenv("HYPERMODR_TEST_TORCH_PYTHON")
  lib <- Sys.getenv("HYPERMODR_TEST_LIBRARY")
  skip_if(
    !nzchar(python) || !nzchar(lib),
    "Explicit installed reference lane required"
  )
  old <- .libPaths()
  .libPaths(c(lib, old))
  on.exit(.libPaths(old), add = TRUE)
  area <- tempfile("reference-job-")
  dir.create(area)
  on.exit(unlink(area, recursive = TRUE), add = TRUE)
  root <- normalizePath(test_path("..", ".."))
  launch <- file.path(area, "launch.py")
  writeLines(c(paste0("#!", python), readLines(file.path(
    root, "inst",
    "backends", "reference-launch.py"
  ))), launch)
  profile <- backend_test_profile(launch, model = "reference:cnn1d")
  base <- file.path(root, "inst", "python", "hypermodr_worker")
  for (file in list.files(base,
    pattern = "[.]py$", recursive = TRUE,
    full.names = TRUE
  )) {
    profile$artifacts[[length(profile$artifacts) + 1L]] <- list(path = paste0(
      "hypermodr_worker/",
      substring(file, nchar(base) + 2L)
    ), url = paste0(
      "file://",
      file
    ), bytes = unname(file.info(file)$size), sha256 = digest::digest(
      file = file,
      algo = "sha256"
    ))
  }
  backend <- hm_install_backend(
    hm_backend_plan("reference:cnn1d",
      profile = profile, cache = file.path(area, "cache")
    ), TRUE,
    TRUE, TRUE
  )
  observations <- data.frame(
    observation_id = paste0("o", 1:10), subject_id = paste0("s", 1:10),
    label = factor(rep(c("z", "a"), 5), levels = c("z", "a"))
  )
  data <- hm_dataset(
    matrix(seq_len(40) / 40, nrow = 10), observations,
    data.frame(wavelength = c(450, 500, 550, 600), unit = "nm")
  )
  task <- hm_task("label", "classification", "spectrum", "subject_id")
  workflow <- hm_workflow(task, hm_recipe(), hm_model_spec(
    "reference:cnn1d",
    task, list(epochs = 1L)
  ))
  artifacts <- file.path(area, "runs")
  job <- hm_submit_job(workflow, data, list(train = 1:8, test = 9:10),
    control = list(seed = 42, backend = backend, artifact_root = artifacts),
    resources = hm_resources(memory_mb = 8192, timeout = 120)
  )
  on.exit(hm_cancel_job(job), add = TRUE)
  for (i in seq_len(1200)) {
    status <- hm_job_status(job)
    if (status$state != "running") {
      break
    }
    Sys.sleep(0.05)
  }
  expect_identical(status$state, "completed", info = status$error)
  if (status$state != "completed") {
    return(invisible(NULL))
  }
  original <- status$result$prediction
  export <- file.path(area, "export")
  hm_export_result(status$result, export, include_private = TRUE)
  saved <- hm_read_run(export)
  expect_null(saved$control$backend)
  expect_null(saved$control$artifact_path)
  expect_null(saved$control$artifact_root)
  expect_identical(
    saved$private_artifacts$fit$engine$artifact_path,
    file.path("private", "reference-model")
  )
  moved <- file.path(area, "relocated")
  expect_true(file.rename(export, moved))
  unlink(status$result$fit$engine$artifact_path, recursive = TRUE)
  helpers <- new.env(parent = globalenv())
  sys.source(file.path(moved, "reproduce.R"), envir = helpers)
  expect_error(helpers$reproduce(data, moved), "admitted backend")
  reproduced <- helpers$reproduce(data, moved,
    backend = backend,
    artifact_root = file.path(area, "reproduced-models")
  )
  expect_identical(reproduced$values, original$values)
  expect_equal(reproduced$probabilities, original$probabilities)
  fit <- helpers$read_private_fit(moved)
  expect_identical(fit$engine$artifact_path, file.path(
    normalizePath(moved),
    "private", "reference-model"
  ))
  handle <- hm_start_backend(backend, resources = hm_resources(
    memory_mb = 8192,
    timeout = 120
  ))
  on.exit(hm_stop_backend(handle), add = TRUE)
  held <- hm_dataset(
    hm_spectra(data)$values[9:10, , drop = FALSE],
    data@observations[9:10, , drop = FALSE], data@bands
  )
  restored <- hm_predict(fit, held, control = list(backend_handle = handle))
  expect_identical(restored$values, original$values)
  expect_equal(restored$probabilities, original$probabilities)
  writeLines("tampered", file.path(fit$engine$artifact_path, "model.json"))
  expect_error(
    hm_predict(fit, held, control = list(backend_handle = handle)),
    "integrity changed"
  )
})

test_that(paste0(
  "reference export transports hashed artifacts and ",
  "explicit runtime intent"
), {
  area <- tempfile("artifact-transport-")
  dir.create(area)
  on.exit(unlink(area, recursive = TRUE), add = TRUE)
  source <- file.path(area, "learned")
  dir.create(source)
  for (file in c("model.json", "state.npz", "input.json")) {
    writeLines(paste("transport fixture", file), file.path(source, file))
  }
  task <- hm_task("label", "classification", "spectrum", "subject_id")
  workflow <- hm_workflow(
    task, hm_recipe(),
    hm_model_spec("reference:cnn1d", task)
  )
  data <- list(source = "transport metadata fixture")
  prediction <- hm_prediction("a", task, "heldout", class_map = c("a", "b"))
  evaluation <- hm_evaluate(
    prediction,
    data.frame(observation_id = "heldout", label = "a")
  )
  result <- list(
    workflow = workflow, prediction = prediction,
    evaluation = evaluation, split = list(train = 1L, test = 2L),
    control = list(
      backend = list(path = source), artifact_path = source,
      artifact_root = area, resources = hm_resources(memory_mb = 8192)
    ),
    input_hash = digest::digest(data, algo = "sha256"),
    fit = structure(
      list(
        workflow = workflow,
        engine = list(
          artifact_path = source,
          artifact_sha256 = hypermodR:::.hm_deep_artifact_hash(source)
        )
      ),
      class = "hm_fit"
    )
  )
  path <- file.path(area, "export")
  hm_export_result(result, path, include_private = TRUE)
  saved <- hm_read_run(path)
  expect_null(saved$control$backend)
  expect_null(saved$control$artifact_path)
  expect_null(saved$control$artifact_root)
  expect_identical(
    saved$private_artifacts$fit$engine$artifact_path,
    file.path("private", "reference-model")
  )
  moved <- file.path(area, "moved")
  expect_true(file.rename(path, moved))
  helpers <- new.env(parent = globalenv())
  sys.source(file.path(moved, "reproduce.R"), envir = helpers)
  fit <- helpers$read_private_fit(moved)
  expect_identical(
    hypermodR:::.hm_deep_artifact_hash(fit$engine$artifact_path),
    result$fit$engine$artifact_sha256
  )
  expect_error(helpers$reproduce(data, moved), "admitted backend")
  writeLines("changed", file.path(source, "model.json"))
  expect_error(
    hm_export_result(result, file.path(area, "tampered"), TRUE),
    "integrity changed"
  )
  result$control$resources <- NULL
  expect_error(
    hm_export_result(result, file.path(area, "undeclared")),
    "resource declaration"
  )
})
