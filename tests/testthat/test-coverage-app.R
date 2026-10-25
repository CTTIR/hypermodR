coverage_result <- function() {
  d <- hm_example()
  task <- hm_task(
    "response", "regression", "spectrum",
    "subject_id"
  )
  w <- hm_workflow(
    task,
    hm_recipe(), hm_model_spec("lm", task)
  )
  split <- hm_split(
                    d, task,
                    "grouped",
                    seed = 42,
                    folds = 3)$folds[[1]]
  fit <- hm_fit(w, d, split, list(seed = 42))
  spectra <- hm_spectra(d)
  i <- split$test
  held <- hm_dataset(
    spectra$values[i,
                   , drop = FALSE],
    spectra$observations[i,
                         , drop = FALSE],
    spectra$band_map
  )
  p <- hm_predict(fit, held)
  list(
    fit = fit,
    prediction = p,
    evaluation = hm_evaluate(
                             p, held@observations),
    workflow = w,
    split = split,
    control = list(seed = 42),
    input_hash = digest::digest(
                                d, algo = "sha256")
  )
}

coverage_job <- function(
  value, alive = FALSE) {
  job <- new.env(parent = emptyenv())
  job$owner_pid <- Sys.getpid()
  job$started <- proc.time()[[3]]
  job$resources <- hm_resources()
  job$state <- "running"
  job$result <- NULL
  job$error <- NULL
  job$input_hash <- value$input_hash
  job$prediction_ids <- value$prediction$observation_id
  job$process <- list(
    is_alive = function(
    ) {
      alive
    }, get_result = function() value,
    kill_tree = function() {
      alive <<- FALSE
    }, wait = function(
      timeout) {
      invisible(NULL)
    }
  )
  class(job) <- "hm_job"
  job
}

test_that(
  paste0(
    "worker result admission checks identities, output ",
    "completeness and timeout"
  ),
  {
    result <- coverage_result()
    for (field in c(
      "fit", "prediction", "evaluation",
      "input_hash"
    )) {
      job <- coverage_job(result)
      bad <- result
      bad[[field]] <- NULL
      job$process$get_result <- function(
      ) {
        bad
      }
      status <- hm_job_status(job)
      expect_identical(
                       status$state,
                       "failed")
      expect_identical(
        status$error,
        "Worker returned incomplete output."
      )
      expect_null(status$result)
    }
    job <- coverage_job(result)
    job$prediction_ids <- rev(
                              job$prediction_ids)
    expect_identical(
      hm_job_status(job)$state,
      "failed"
    )
    job <- coverage_job(result)
    job$process$get_result <- function(
    ) {
      cli::cli_abort("password sensitive")
    }
    expect_match(
      hm_job_status(job)$error,
      "withheld"
    )
    job <- coverage_job(result, TRUE)
    job$started <- job$started - 100
    job$resources$timeout <- 1
    expect_identical(
      hm_job_status(job)$error,
      "The modelling job exceeded its time budget."
    )
    expect_identical(job$state, "failed")
    job <- coverage_job(result, TRUE)
    job$process$kill_tree <- function() NULL
    expect_error(
      hm_cancel_job(job),
      class = "hm_process_error"
    )
    expect_identical(job$state, "running")
    expect_error(
      hm_cancel_job(list()),
      "owned job"
    )
    expect_error(
      hm_job_status(list()),
      "owned job"
    )
  }
)

test_that(
  paste0(
    "private export strips secrets recursively and retains ",
    "verified artifacts"
  ),
  {
    result <- coverage_result()
    result$fit$diagnostic <- list(
      token = "secret", nested = c(
        "Bearer x",
        "https://u:p@example.org/resource",
        "harmless"
      )
    )
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    expect_error(
      hm_export_result(result, path, NA),
      "explicit"
    )
    hm_export_result(result, path, TRUE)
    restored <- hm_read_run(path)
    expect_true(restored$private)
    expect_true(
      isTRUE(
        all.equal(
          restored$private_artifacts$fit,
          hypermodR:::.hm_app_strip_secrets(
                                            result$fit),
          check.environment = FALSE
        )
      )
    )
    expected_nulls <- names(
                            result$fit)[vapply(
                                               result$fit,
                                               is.null,
                                               logical(
                                                       1))]
    expect_true(
      all(
        expected_nulls %in% names(
                                  restored$private_artifacts$fit)
      )
    )
    expect_true(
      all(
        vapply(
          restored$private_artifacts$fit[expected_nulls],
          is.null, logical(1)
        )
      )
    )
    d <- hm_example()
    spectra <- hm_spectra(d)
    i <- result$split$test
    held <- hm_dataset(
      spectra$values[i, , drop = FALSE],
      spectra$observations[i,
                           , drop = FALSE],
      spectra$band_map
    )
    expect_identical(
      hm_predict(
        restored$private_artifacts$fit,
        held
      ),
      result$prediction
    )
    lib <- Sys.getenv(
                      "HYPERMODR_TEST_LIBRARY")
    if (
        file.exists(
                    file.path(
                              lib,
                              "hypermodR",
                              "DESCRIPTION"))) {
      fresh <- callr::r(
        function(path, held) {
          saved <- hypermodR::hm_read_run(
                                          path)
          hypermodR::hm_predict(
            saved$private_artifacts$fit,
            held
          )
        }, args = list(
                       path = path,
                       held = held),
        libpath = .libPaths()
      )
      expect_identical(
                       fresh,
                       result$prediction)
    }
    expect_identical(
      restored$private_artifacts$fit$diagnostic$token,
      "[redacted]"
    )
    expect_identical(
      restored$private_artifacts$fit$diagnostic$nested,
      c(
        "[redacted]",
        "https://[redacted]@example.org/resource",
        "harmless"
      )
    )
    expect_identical(
      restored$private_artifacts$prediction,
      result$prediction
    )
    expect_match(
      hypermodR:::.hm_app_safe_error(
        simpleError(
          paste(
            rep("x", 600),
            collapse = ""
          )
        )
      ),
      "^x{500}$", perl = TRUE
    )
  }
)

test_that(
  paste0(
    "workbench receives completed jobs, invalidates stale ",
    "results and retries"
  ),
  {
    skip_if_not_installed("shiny")
    result <- coverage_result()
    testthat::local_mocked_bindings(
      hm_submit_job = function(
        workflow, data, split, control,
        resources
      ) {
        expect_s3_class(
                        resources,
                        "hm_resources")
        coverage_job(result)
      }, .package = "hypermodR"
    )
    shiny::testServer(
      hypermodR:::.hm_app_server, {
        session$setInputs(run = 1)
        expect_match(state$error, "Validate")
        session$setInputs(example = 1)
        session$setInputs(
          qc = TRUE, outcome = "response",
          type = "regression",
          groups = "subject_id",
          model = "lm",
          seed = 42, scale = FALSE,
          reference = "synthetic"
        )
        session$setInputs(configure = 1)
        session$setInputs(compare = 1)
        expect_match(state$error, "two runs")
        session$setInputs(run = 2)
        session$elapse(350)
        expect_identical(
                         state$status,
                         "completed")
        expect_true(
                    hypermodR:::.hm_app_current(
                                                state))
        expect_identical(
                         state$result$prediction,
                         result$prediction)
        expect_match(output$summary, "96")
        expect_match(
                     output$annotation,
                     "hyperannotatR")
        expect_match(
                     output$geometry,
                     "synthetic")
        expect_match(
                     output$configuration,
                     "lm")
        expect_match(output$runtime, "NULL")
        expect_match(
                     output$status,
                     "completed")
        archive <- output$download
        expect_true(file.exists(archive))
        expect_true(
          "reproduce.R" %in% utils::unzip(
                                          archive,
                                          list = TRUE)$Name
        )
        session$setInputs(model = "pcr")
        expect_false(
                     hypermodR:::.hm_app_current(
                                                 state))
        expect_match(output$status, "stale")
        session$setInputs(configure = 2)
        session$setInputs(run = 3)
        session$elapse(350)
        expect_identical(
                         state$status,
                         "completed")
        session$setInputs(compare = 2)
        expect_false(
          identical(
            comparison(
            ), "Run two compatible workflows to compare."
          )
        )
        session$setInputs(install = 1)
        expect_match(
                     state$error,
                     "Resolve metadata")
      }
    )
  }
)

test_that(
  paste0(
    "runtime UI submits explicit metadata and admitted ",
    "installation requests"
  ),
  {
    skip_if_not_installed("shiny")
    requests <- list()
    process_state <- new.env()
    process_state$alive <- TRUE
    process_state$killed <- FALSE
    resolution <- structure(
      list(
        backend = "reference_cpu",
        packages = "declared",
        qualification = "pending",
        profile = list(
          terms = list(
                       list(
                            text = "notice",
                            reviewed = FALSE))
        )
      ),
      class = "hm_backend_resolution"
    )
    value <- resolution
    testthat::local_mocked_bindings(
      r_bg = function(func, args, ...) {
        requests[[length(requests) +
                    1L]] <<- list(
          func = func,
          args = args, options = list(...)
        )
        list(
          is_alive = function(
          ) {
            process_state$alive
          },
          get_result = function() {
            value
          }, kill_tree = function() {
            process_state$killed <- TRUE
            process_state$alive <- FALSE
          }
        )
      }, .package = "callr"
    )
    testthat::local_mocked_bindings(
      hm_admit_backend = function(
        resolution, terms) {
        expect_true(terms[[1]]$reviewed)
        structure(
          list(terms = terms),
          class = "hm_backend_plan"
        )
      }, .package = "hypermodR"
    )
    shiny::testServer(
      hypermodR:::.hm_app_server, {
        session$setInputs(
          type = "classification",
          model = "reference:cnn1d",
          python = "python3"
        )
        session$flushReact()
        session$setInputs(resolve = 1)
        expect_match(
                     runtime(
                     )$status,
                     "no installation")
        session$setInputs(resolve = 2)
        expect_match(state$error, "Wait")
        expect_length(requests, 1)
        expect_identical(
          requests[[1]]$args, list(
            python = "python3",
            model = "reference:cnn1d"
          )
        )
        expect_identical(
          requests[[1]]$options$env, c(
            OMP_NUM_THREADS = "1",
            OPENBLAS_NUM_THREADS = "1",
            MKL_NUM_THREADS = "1"
          )
        )
        expect_match(
          paste(
            deparse(
                    body(requests[[1]]$func)),
            collapse = " "
          ),
          "hm_resolve_backend"
        )
        process_state$alive <- FALSE
        session$elapse(350)
        expect_match(
                     output$runtime,
                     "pending")
        session$setInputs(
                          terms = FALSE,
                          install = 1)
        expect_match(state$error, "review")
        process_state$alive <- TRUE
        session$setInputs(
                          terms = TRUE,
                          install = 2)
        expect_length(requests, 2)
        expect_s3_class(
                        requests[[2]]$args$plan,
                        "hm_backend_plan")
        expect_match(
          paste(
            deparse(
                    body(requests[[2]]$func)),
            collapse = " "
          ),
          "accept_terms = TRUE"
        )
        expect_match(
                     runtime(
                     )$status,
                     "qualification pending")
        session$close()
        expect_true(process_state$killed)
      }
    )
  }
)

test_that(
  paste0(
    "sequence pooling conserves independent frame means and ",
    "identities"
  ),
  {
    pooled <- hypermodR:::.hm_app_pool(
                                       core_sequence(
                                       ))
    expect_equal(
      hm_spectra(pooled)$values,
      rbind(
        c(8, 23, 38, 53),
        c(68, 83, 98, 113)
      )
    )
    expect_identical(
      pooled@observations$observation_id,
      c("pooled_f1", "pooled_f2")
    )
    expect_identical(
                     pooled@observations$frame_id,
                     c("f1", "f2"))
    expect_error(
      hypermodR:::.hm_app_pool(hm_example()),
      "already contains spectra"
    )
    expect_error(
      hm_app(launch = NA),
      "TRUE or FALSE"
    )
  }
)

test_that(
  paste0(
    "workbench file admission previews spatial frames and ",
    "requires pooling"
  ),
  {
    skip_if_not_installed("shiny")
    source <- tempfile(fileext = ".rds")
    on.exit(unlink(source))
    saveRDS(core_sequence(), source)
    shiny::testServer(
      hypermodR:::.hm_app_server, {
        session$setInputs(
                          type = "classification")
        session$setInputs(
          source = list(
                        size = 51 * 1024^2,
                        datapath = source),
          import = 1
        )
        expect_match(state$error, "50 MB")
        session$setInputs(
          source = list(
            size = file.info(source)$size,
            datapath = source
          ),
          import = 2
        )
        expect_identical(
                         state$dataset@data_kind,
                         "sequence")
        expect_match(output$frames, "f1")
        expect_true(is.list(output$spectra))
        session$setInputs(
                          qc = TRUE,
                          configure = 1)
        expect_match(
                     state$error,
                     "Spatial data require")
        session$setInputs(pool = 1)
        expect_identical(
                         state$dataset@data_kind,
                         "spectra")
        expect_equal(
          hm_spectra(state$dataset)$values[2,
          ], c(68, 83, 98, 113)
        )
        expect_error(
                     output$download,
                     "current completed")
      }
    )
  }
)

test_that(
  paste0(
    "workbench cancels owned jobs and admits retry after ",
    "launch failure"
  ),
  {
    skip_if_not_installed("shiny")
    result <- coverage_result()
    calls <- 0L
    testthat::local_mocked_bindings(
      hm_submit_job = function(...) {
        calls <<- calls + 1L
        if (calls == 1L)
          cli::cli_abort("launch denied")
        coverage_job(result, TRUE)
      }, .package = "hypermodR"
    )
    shiny::testServer(
      hypermodR:::.hm_app_server, {
        session$setInputs(example = 1)
        session$setInputs(
          qc = TRUE, outcome = "response",
          type = "regression",
          groups = "subject_id",
          model = "lm",
          seed = 42, scale = TRUE,
          reference = "synthetic"
        )
        session$setInputs(
                          configure = 1,
                          run = 1)
        expect_identical(
                         state$status,
                         "failed")
        expect_match(
                     state$error,
                     "launch denied")
        session$setInputs(run = 2)
        expect_identical(
                         state$status,
                         "running")
        expect_error(
          hypermodR:::.hm_app_import(
                                     state,
                                     core_image(
                                     )),
          "Cancel"
        )
        expect_error(
          hypermodR:::.hm_app_configure(
                                        state,
                                        result$workflow),
          "Cancel"
        )
        session$setInputs(cancel = 1)
        expect_identical(
                         state$status,
                         "cancelled")
        expect_identical(
                         state$job$state,
                         "cancelled")
        session$setInputs(run = 3)
        expect_identical(
                         state$status,
                         "running")
        expect_identical(calls, 3L)
      }
    )
  }
)
