benchmark_fixture <- function() {
  list(
    version = "1", program = "Spectralverse", cohort = "HeiPor",
    role = "primary_anatomy",
    dataset_revision = "public-v1", reference_revision = "review-v1",
    license_admitted = TRUE,
    person_grouping_verified = FALSE, source_cases = c("case1", "case2"),
    roi_ids = c("r1", "r2"),
    input_track = "native_spectral", context_budget = 1L,
    pretraining_overlap = "none_verified",
    prior_outcome_access = FALSE, target_access = "none", capabilities = list(
      list(
        model_id = "lm", task = "regression", platform = "R-cpu",
        qualified = FALSE,
        evidence = "not-run"
      )
    )
  )
}
test_that(
  "protocols retain blocked lanes, development access and matched support",
  {
    p <- benchmark_fixture()
    a <- hm_benchmark_protocol(p)
    expect_identical(a$execution, "not_run")
    expect_false(a$scientific_qualification)
    expect_identical(a$lanes[[1]]$status, "blocked_unqualified")
    p$prior_outcome_access <- TRUE
    expect_identical(
      hm_benchmark_protocol(p)$target_role,
      "development_or_overlap_unresolved"
    )
    p$source_cases <- "different"
    expect_error(
      hm_benchmark_protocol(p, benchmark_fixture()),
      "identical"
    )
    p <- benchmark_fixture()
    p$cohort <- "ManU"
    p$role <- "human_admission"
    expect_error(
      hm_benchmark_protocol(p),
      "person grouping"
    )
    p$person_grouping_verified <- TRUE
    expect_identical(
      hm_benchmark_protocol(p)$execution,
      "not_run"
    )
  }
)
