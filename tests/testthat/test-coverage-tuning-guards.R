test_that(
  paste0(
    "tuning rejects incomplete protocols and trial budgets ",
    "before selection"
  ),
  {
    values <- matrix(
      seq_len(24),
      12, 2
    )
    observations <- data.frame(
      observation_id = paste0("o", 1:12),
      subject_id = paste0("s", 1:12),
      y = seq_len(12)
    )
    data <- hm_dataset(
      values,
      observations,
      data.frame(
                 wavelength = c(500, 650))
    )
    task <- hm_task(
                    "y",
                    "regression",
                    "spectrum", "subject_id")
    workflow <- hm_workflow(
      task,
      hm_recipe(
      ), hm_model_spec(
                       "bayes_conjugate",
                       task)
    )
    candidates <- list(
      list(
           noise_variance = 1,
           prior_variance = 10)
    )
    split <- list(
      train = 1:8, test = 9:12, inner = list(
        list(train = 1:4, test = 5:8),
        list(train = 5:8, test = 1:4)
      )
    )
    expect_error(
      hm_tune(workflow, data, split, list()),
      "Explicit candidate"
    )
    expect_error(
      hm_tune(
        workflow, data, split, candidates,
        list(
             resources = hm_resources(
                                      cpus = 2))
      ),
      "single CPU"
    )
    if (Sys.info()[["sysname"]] != "Linux") {
      expect_error(
        hm_tune(
                workflow, data, split, candidates),
        "platform is unsupported"
      )
      return(invisible(NULL))
    }
    expect_error(
      hm_tune(
        workflow, data, split, candidates,
        list(max_trials = 0)
      ),
      "trial budget"
    )
    duplicate <- split
    duplicate$inner[[2]] <- duplicate$inner[[1]]
    expect_error(
      hm_tune(
              workflow,
              data, duplicate, candidates),
      "exactly once"
    )
    expect_error(
      hm_tune(
        workflow, data, split, candidates,
        metric = "not-a-metric"
      ),
      "Selection metric"
    )
    tuned <- hm_fit(
      workflow, data, split, control = list(
        tuning = list(
          candidates = candidates,
          metric = "rmse",
          direction = "maximize"
        )
      )
    )
    expect_identical(
                     tuned$tuning$selected,
                     1L)
    expect_true(
                is.finite(
                          tuned$tuning$scores[1]))
    expect_identical(tuned$training, 1:8)
    expect_true(
      all(
        vapply(
          tuned$tuning$ledger,
          function(x) {
            !any(
              c(x$train, x$validation) %in%
                9:12
            )
          },
          logical(1)
        )
      )
    )
  }
)

test_that(
  paste0(
    "failed run reservation retains a failure receipt ",
    "without completion"
  ),
  {
    parent <- tempfile("failed-run-")
    dir.create(parent)
    on.exit(
      unlink(parent, recursive = TRUE),
      add = TRUE
    )
    expect_error(
      hm_save_run(
                  list(
                  ), file.path(
                               parent,
                               "missing",
                               "run")),
      class = "hm_io_error"
    )
    testthat::local_mocked_bindings(
      .hm_run_receipt = function(...) {
        .hm_abort(
          "Injected receipt write failure",
          "hm_io_error"
        )
      }, .package = "hypermodR"
    )
    path <- file.path(parent, "run")
    expect_error(
      hm_save_run(
        list(value = 3),
        path
      ),
      "Injected receipt"
    )
    expect_false(
                 file.exists(
                             file.path(
                                       path,
                                       "COMPLETE")))
    expect_identical(
      readRDS(
              file.path(
                        path,
                        "receipt.rds"))$state,
      "failed"
    )
  }
)
