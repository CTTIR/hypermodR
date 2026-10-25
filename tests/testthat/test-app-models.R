test_that(
  "workbench parameter objects are explicit and validated",
  {
    expect_identical(
      hypermodR:::.hm_app_parameters("{}"),
      list()
    )
    expect_identical(
      hypermodR:::.hm_app_parameters("{\"epochs\":2}"),
      list(epochs = 2L)
    )
    expect_error(
      hypermodR:::.hm_app_parameters("[]"),
      "JSON object"
    )
    expect_error(
      hypermodR:::.hm_app_parameters("{"),
      "valid JSON"
    )
    expect_error(
      hypermodR:::.hm_app_parameters("{\"x\":1,\"x\":2}"),
      "unique"
    )
  }
)

test_that(
  "reference job admission requires a receipt and explicit memory",
  {
    expect_error(
      hypermodR:::.hm_app_validate_backend(NULL, "reference:cnn1d", 4096),
      "receipt"
    )
    backend <- structure(
      list(plan = list(model_id = "reference:cnn1d")),
      class = "hm_backend"
    )
    expect_error(
      hypermodR:::.hm_app_validate_backend(backend, "reference:cnn1d", 1024),
      "4096"
    )
  }
)

test_that(
  "workbench chooses compatible engines from the actual catalogue",
  {
    catalogue <- hypermodR:::.hm_engine_catalog()
    regression <- names(
      Filter(
        function(x) {
          "regression" %in%
            x
        }, catalogue
      )
    )
    expect_true(
      all(
        c(
          "lm", "glm", "glmnet", "rf",
          "svm"
        ) %in%
          regression
      )
    )
    expect_false("reference:cnn1d" %in% regression)
    classification <- names(
      Filter(
        function(x) {
          "classification" %in%
            x
        }, catalogue
      )
    )
    expect_true("reference:cnn1d" %in% classification)
  }
)

test_that(
  "installed reference jobs own workers and retain learned artifacts",
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    python <- Sys.getenv("HYPERMODR_TEST_TORCH_PYTHON")
    lib <- Sys.getenv("HYPERMODR_TEST_LIBRARY")
    skip_if(
      !nzchar(python) ||
        !nzchar(lib),
      "Explicit installed reference lane required"
    )
    old <- .libPaths()
    .libPaths(c(lib, old))
    on.exit(
      .libPaths(old),
      add = TRUE
    )
    area <- tempfile("reference-job-")
    dir.create(area)
    on.exit(
      unlink(area, recursive = TRUE),
      add = TRUE
    )
    root <- normalizePath(test_path("..", ".."))
    launch <- file.path(area, "launch.py")
    writeLines(
      c(
        paste0("#!", python),
        readLines(
          file.path(
            root, "inst", "backends",
            "reference-launch.py"
          )
        )
      ),
      launch
    )
    profile <- backend_test_profile(launch, model = "reference:cnn1d")
    base <- file.path(root, "inst", "python", "hypermodr_worker")
    for (file in list.files(
      base, pattern = "[.]py$", recursive = TRUE,
      full.names = TRUE
    )) {
      profile$artifacts[[length(profile$artifacts) +
                           1L]] <- list(
        path = paste0(
          "hypermodr_worker/", substring(
            file, nchar(base) +
              2L
          )
        ),
        url = paste0("file://", file),
        bytes = unname(file.info(file)$size),
        sha256 = digest::digest(file = file, algo = "sha256")
      )
    }
    backend <- hm_install_backend(
      hm_backend_plan(
        "reference:cnn1d", profile = profile,
        cache = file.path(area, "cache")
      ),
      TRUE, TRUE, TRUE
    )
    data <- hm_dataset(
      matrix(
        seq_len(40) / 40,
        nrow = 10
      ),
      data.frame(
        observation_id = paste0("o", 1:10),
        subject_id = paste0("s", 1:10),
        label = factor(
          rep(
            c("z", "a"),
            5
          ),
          levels = c("z", "a")
        )
      ),
      data.frame(
        wavelength = c(450, 500, 550, 600),
        unit = "nm"
      )
    )
    task <- hm_task(
      "label", "classification", "spectrum",
      "subject_id"
    )
    workflow <- hm_workflow(
      task,
      hm_recipe(), hm_model_spec("reference:cnn1d", task, list(epochs = 1L))
    )
    artifacts <- file.path(area, "runs")
    job <- hm_submit_job(
      workflow, data, list(train = 1:8, test = 9:10),
      control = list(
        seed = 42, backend = backend,
        artifact_root = artifacts
      ),
      resources = hm_resources(memory_mb = 8192, timeout = 120)
    )
    on.exit(
      hm_cancel_job(job),
      add = TRUE
    )
    for (i in seq_len(1200)) {
      status <- hm_job_status(job)
      if (status$state != "running")
        break
      Sys.sleep(0.05)
    }
    expect_identical(
      status$state, "completed", info = status$error
    )
    if (status$state == "completed") {
      expect_null(status$result$control$backend_handle)
      expect_s3_class(status$result$evaluation, "hm_evaluation")
      expect_true(
        dir.exists(status$result$fit$engine$artifact_path)
      )
      expect_identical(
        dirname(status$result$fit$engine$artifact_path),
        normalizePath(artifacts)
      )
      artifact_path <- status$result$fit$engine$artifact_path
      hashes <- hypermodR:::.hm_deep_artifact_hash(artifact_path)
      expect_length(hashes, 3L)
    }
  }
)
