test_that(
  "state transitions, reimport and stale identities are explicit",
  {
    a <- hm_app_state()
    b <- hm_app_state()
    d <- hm_example()
    expect_error(
      hypermodR:::.hm_app_transition(a, "completed"),
      class = "hm_state_error"
    )
    hypermodR:::.hm_app_import(a, d)
    revision <- a$revision
    hypermodR:::.hm_app_import(a, d)
    expect_identical(a$revision, revision)
    expect_null(b$dataset)
    task <- hm_task("response", "regression", "spectrum", "subject_id")
    w <- hm_workflow(task, hm_recipe(), hm_model_spec("lm", task))
    hypermodR:::.hm_app_configure(a, w)
    revision <- a$revision
    hypermodR:::.hm_app_configure(a, w)
    expect_identical(a$revision, revision)
    for (s in c(
      "queued", "running",
      "completed"
    )) {
      hypermodR:::.hm_app_transition(a, s)
    }
    a$result <- list(value = 1)
    a$result_revision <- a$revision
    expect_true(hypermodR:::.hm_app_current(a))
    hypermodR:::.hm_app_import(a, hm_example(43L))
    expect_false(hypermodR:::.hm_app_current(a))
    expect_identical(a$result$value, 1)
    expect_error(hypermodR:::.hm_app_transition(a, "cancelled"))
  }
)
test_that(
  "failure and cancellation cannot finalize as complete",
  {
    for (terminal in c("failed", "cancelled")) {
      a <- hm_app_state()
      hypermodR:::.hm_app_import(a, hm_example())
      hypermodR:::.hm_app_transition(a, "ready")
      hypermodR:::.hm_app_transition(a, "queued")
      hypermodR:::.hm_app_transition(a, "running")
      hypermodR:::.hm_app_transition(a, terminal)
      expect_error(hypermodR:::.hm_app_transition(a, "completed"))
    }
  }
)
