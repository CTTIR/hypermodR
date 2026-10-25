test_that(
  "actual installed-package jobs complete, deny admission and cancel",
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    skip_if_not_installed("callr")
    lib <- Sys.getenv("HYPERMODR_TEST_LIBRARY")
    skip_if_not(
      file.exists(file.path(lib, "hypermodR", "DESCRIPTION")),
      "Explicit installed worker lane unavailable"
    )
    old <- .libPaths()
    .libPaths(c(lib, old))
    on.exit(.libPaths(old))
    d <- hm_example()
    task <- hm_task("response", "regression", "spectrum", "subject_id")
    w <- hm_workflow(task, hm_recipe(), hm_model_spec("lm", task))
    split <- hm_split(d, task, "grouped", seed = 42, folds = 3)$folds[[1]]
    expect_error(
      hm_submit_job(w, d, split, resources = hm_resources(memory_mb = 1e-04)),
      "memory"
    )
    job <- hm_submit_job(w, d, split, control = list(seed = 42))
    on.exit(
      hm_cancel_job(job),
      add = TRUE
    )
    for (i in seq_len(100)) {
      status <- hm_job_status(job)
      if (status$state != "running") {
        break
      }
      Sys.sleep(0.05)
    }
    expect_identical(status$state, "completed")
    expect_s3_class(status$result$prediction, "hm_prediction")
    second <- hm_submit_job(w, d, split, control = list(seed = 42))
    hm_cancel_job(second)
    expect_identical(
      hm_job_status(second)$state,
      "cancelled"
    )
    expect_null(hm_job_status(second)$result)
    retry <- hm_submit_job(w, d, split, control = list(seed = 42))
    on.exit(
      hm_cancel_job(retry),
      add = TRUE
    )
    expect_false(identical(retry$id, second$id))
    for (i in seq_len(100)) {
      resumed <- hm_job_status(retry)
      if (resumed$state != "running") {
        break
      }
      Sys.sleep(0.05)
    }
    expect_identical(resumed$state, "completed")
    expect_identical(resumed$result$prediction, status$result$prediction)
  }
)
