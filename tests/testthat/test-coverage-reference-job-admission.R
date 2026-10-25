test_that(
  "reference job admission binds receipts, resources and a new artifact path",
  {
    parent <- tempfile("reference-admission-")
    dir.create(parent)
    on.exit(
      unlink(parent, recursive = TRUE),
      add = TRUE
    )
    worker <- file.path(parent, "worker")
    writeLines("synthetic receipt fixture", worker)
    id <- "reference:masked_autoencoder"
    backend <- hm_install_backend(
      hm_backend_plan(
        id, profile = backend_test_profile(worker, id),
        cache = file.path(parent, "cache")
      ),
      TRUE, TRUE, TRUE
    )
    dataset <- hm_example()
    task <- hm_task(
      NULL, "reconstruction", "spectrum",
      "subject_id"
    )
    workflow <- hm_workflow(task, hm_recipe(), hm_model_spec(id, task))
    split <- hm_split(dataset, task, "grouped", folds = 3)$folds[[1]]
    captured <- NULL
    process <- new.env()
    process$alive <- TRUE
    process$is_alive <- function() process$alive
    process$kill_tree <- function() process$alive <- FALSE
    process$wait <- function(...) NULL
    testthat::local_mocked_bindings(
      .hm_resource_r_bg = function(func, args, resources, ...) {
        captured <<- list(args = args, resources = resources)
        process
      }, .package = "hypermodR"
    )
    expect_error(
      hm_submit_job(
        workflow, dataset, split, list(backend_handle = new.env())
      ),
      "live backend handle"
    )
    expect_error(
      hm_submit_job(
        workflow, dataset, split, list(backend = backend)
      ),
      class = "hm_resource_error"
    )
    resources <- hm_resources(memory_mb = 4096)
    expect_error(
      hm_submit_job(
        workflow, dataset, split, list(backend = backend),
        resources
      ),
      "artifact_root"
    )
    root <- file.path(parent, "models")
    job <- hm_submit_job(
      workflow, dataset, split, list(backend = backend, artifact_root = root),
      resources
    )
    expect_s3_class(job, "hm_job")
    expect_identical(captured$resources, resources)
    expect_identical(
      captured$args$control$backend$plan$digest,
      backend$plan$digest
    )
    expect_true(
      startsWith(
        captured$args$control$artifact_path,
        paste0(
          normalizePath(root),
          .Platform$file.sep
        )
      )
    )
    expect_false(
      file.exists(captured$args$control$artifact_path)
    )
    expect_null(job$result)
    hm_cancel_job(job)
    expect_identical(job$state, "cancelled")
  }
)

test_that(
  "workbench admission reads a trusted receipt without starting a runtime",
  {
    parent <- tempfile("ui-reference-receipt-")
    dir.create(parent)
    on.exit(
      unlink(parent, recursive = TRUE),
      add = TRUE
    )
    worker <- file.path(parent, "worker")
    writeLines("synthetic receipt fixture", worker)
    id <- "reference:masked_autoencoder"
    backend <- hm_install_backend(
      hm_backend_plan(
        id, profile = backend_test_profile(worker, id),
        cache = file.path(parent, "cache")
      ),
      TRUE, TRUE, TRUE
    )
    receipt <- file.path(parent, "installed.rds")
    saveRDS(backend, receipt)
    shiny::testServer(
      hypermodR:::.hm_app_server, {
        session$setInputs(example = 1)
        session$setInputs(
          qc = TRUE, outcome = "response",
          type = "reconstruction", groups = "subject_id",
          model = id, seed = 42, scale = FALSE,
          reference = "synthetic", backend_route = "r",
          memory = 4096
        )
        session$setInputs(configure = 1)
        expect_match(state$error, "admitted CPU reference")
        session$setInputs(backend_route = "reference")
        session$setInputs(configure = 2)
        expect_match(state$error, "installed receipt")
        session$setInputs(backend_receipt = list(datapath = receipt))
        session$setInputs(configure = 3)
        expect_null(state$error)
        expect_identical(
          state$backend$plan$digest, backend$plan$digest
        )
        expect_identical(
          state$workflow$model$model_id,
          id
        )
        expect_identical(state$status, "ready")
      }
    )
  }
)
