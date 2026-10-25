test_that(
  paste0(
    "evaluation exposes summaries and explicit resampling ",
    "without changing support"
  ),
  {
    z <- eval_fixture()
    result <- hm_evaluate(
      z$p, z$r, resampling = list(
        unit = "subject_id", draws = 2L,
        seed = 1L
      )
    )
    description <- summary(result)
    expect_identical(description$metrics, result$metrics)
    expect_identical(description$support, result$per_class)
    expect_identical(description$uncertainty, result$uncertainty)
    expect_length(
      result$uncertainty$sampled_units,
      2
    )
    expect_error(
      hm_evaluate(z$p, z$r, estimand = "unknown"),
      "Unknown estimand"
    )
    expect_error(
      hm_evaluate(
        z$p, z$r, estimand = "equal_class_subject_acquisition",
        hierarchy = "subject_id"
      ),
      "hierarchy required"
    )
    bad <- z$r
    bad$subject_id[1] <- NA_character_
    expect_error(
      hm_evaluate(
        z$p, bad, estimand = "equal_class_subject_acquisition"
      ),
      "cannot be missing"
    )
    task <- hm_task(
      "y", "temporal", "subject", "subject_id",
      list(horizon = "24 hours")
    )
    prediction <- hm_prediction(
      c(1, 3),
      task, c("o1", "o2")
    )
    evaluated <- hm_evaluate(
      prediction, data.frame(
        observation_id = c("o1", "o2"),
        y = c(1, 2)
      )
    )
    expect_match(evaluated$interpretation, "24 hours")
    expect_equal(evaluated$metrics[["mae"]], 0.5)
  }
)

test_that(
  "invalid public declarations fail before expensive work",
  {
    expect_error(
      hm_endpoint_contract(
        data.frame(
          observation_id = 1, endpoint = "healing",
          value = 1
        )
      ),
      "Explicit observation"
    )
    expect_error(
      hm_model_spec(
        "lm", hm_task(
          "y", "classification", "spectrum",
          "subject_id"
        )
      ),
      class = "hm_capability_error"
    )
    data <- hm_example()
    task <- hm_task(
      "response", "regression", "spectrum",
      "subject_id"
    )
    workflow <- hm_workflow(
      task, hm_recipe(), hm_model_spec("lm", task)
    )
    expect_error(
      hm_submit_job(
        workflow, list(), list(train = 1:4, test = 5:6)
      ),
      "admitted HMDataset"
    )
    expect_error(
      hm_submit_job(
        workflow, data, list(train = 1:4, test = 5:6),
        resources = hm_resources(device = "cuda:0")
      ),
      "CPU resource budget"
    )
    expect_error(
      .hm_backend_device_env(list(), list()),
      "resource declaration"
    )
    prediction <- hm_prediction(
      c(1, 1),
      hm_task(
        "y", "clustering", "spectrum",
        "subject_id"
      ),
      c("o1", "o2")
    )
    expect_equal(
      hm_evaluate(
        prediction, data.frame(
          observation_id = c("o1", "o2"),
          y = c("same", "same")
        )
      )$metrics[["adjusted_rand_index"]],
      1
    )
  }
)
