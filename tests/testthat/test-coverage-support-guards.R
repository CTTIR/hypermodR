test_that(
  paste0(
    "benchmark declarations reject ambiguous access and ",
    "support without qualification"
  ),
  {
    p <- list(
      version = "1",
      program = "Spectralverse",
      cohort = "HeiPor",
      role = "primary_anatomy",
      dataset_revision = "fixture-v1",
      reference_revision = "fixture-reference-v1",
      license_admitted = TRUE,
      person_grouping_verified = FALSE,
      source_cases = c("case1", "case2"),
      roi_ids = c("roi1", "roi2"),
      input_track = "native_spectral",
      context_budget = 1L,
      pretraining_overlap = "unknown",
      prior_outcome_access = FALSE,
      target_access = "none",
      capabilities = list(
        list(
          model_id = "fixture",
          task = "regression",
          platform = "R-cpu",
          qualified = FALSE,
          evidence = "not-run"
        )
      )
    )
    expect_error(
      hm_benchmark_protocol(list()),
      "Incomplete"
    )
    bad <- p
    bad$license_admitted <- FALSE
    expect_error(
      hm_benchmark_protocol(bad),
      "use terms"
    )
    bad <- p
    bad$source_cases <- c("case1", "case1")
    expect_error(
      hm_benchmark_protocol(bad),
      "unique case"
    )
    bad <- p
    bad$context_budget <- 0
    expect_error(
      hm_benchmark_protocol(bad),
      "context budget"
    )
    bad <- p
    bad$capabilities <- list()
    expect_error(
      hm_benchmark_protocol(bad),
      "matrix required"
    )
    bad <- p
    bad$capabilities[[1]]$qualified <- NA
    expect_error(
      hm_benchmark_protocol(bad),
      "Invalid capability lane"
    )
    expect_false(
      hm_benchmark_protocol(
                            p, p)$scientific_qualification
    )
  }
)

test_that(
  paste0(
    "temporal protocols refuse unknown times and empty ",
    "independent support"
  ),
  {
    observations <- data.frame(
      observation_id = paste0("o", 1:4),
      subject_id = paste0("s", 1:4),
      timestamp = c(0, 1, 4, 5)
    )
    task <- hm_task(
                    "y",
                    "temporal",
                    "subject",
                    "subject_id")
    expect_error(
      hm_split(
               observations,
               task, design = "temporal"),
      "Explicit cutoff"
    )
    expect_error(
      hm_split(
        observations,
        task, design = "temporal",
        cutoff = 3, embargo = -1
      ),
      "nonnegative"
    )
    expect_error(
      hm_split(
        observations,
        task, design = "temporal",
        cutoff = 10
      ),
      "No independent"
    )
    observations$timestamp[1] <- Inf
    expect_error(
      hm_split(
        observations,
        task, design = "temporal",
        cutoff = 3
      ),
      "finite numeric times"
    )
    expect_error(
      hm_split(
               observations,
               task, design = "unknown"),
      "Unknown split"
    )
    expect_error(
      hm_split(
        observations,
        task, design = "grouped",
        folds = 10
      ),
      "Insufficient independent"
    )
  }
)

test_that(
  paste0(
    "public workflow dispatch rejects incompatible ",
    "declarations and corrupt split identities"
  ),
  {
    task <- hm_task(
                    "y",
                    "regression",
                    "spectrum", "subject_id")
    expect_error(
      hm_workflow(
        task, hm_recipe(), hm_model_spec(
          "lm", hm_task(
            "other",
            "regression", "spectrum",
            "subject_id"
          )
        )
      ),
      "Incompatible"
    )
    expect_error(
      hm_fit(list(), NULL, list()),
      "Expected workflow"
    )
    expect_error(
      .hm_model_data(1),
      "Expected HMDataset"
    )
    data <- list(
      values = matrix(1:12, 6, 2),
      observations = data.frame(
        observation_id = paste0("o", 1:6),
        subject_id = paste0("s", 1:6),
        y = 1:6
      )
    )
    expect_error(
      .hm_validate_fit_split(
                             data,
                             task, list()),
      "explicit split"
    )
    expect_error(
      .hm_validate_fit_split(
                             data,
                             task,
                             list(
                                  train = 1:3,
                                  test = 3:6)),
      "overlap"
    )
    expect_error(
      .hm_validate_fit_split(
        data, task, list(
          train = 1:3,
          test = 4:6,
          observation_id = rep(
                               "wrong", 6)
        )
      ),
      class = "hm_integrity_error"
    )
    expect_error(
      hm_prep(
        hm_recipe(list(list())),
        matrix(1:12, 6, 2)
      ),
      "requires type"
    )
  }
)
