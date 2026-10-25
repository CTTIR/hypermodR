test_that(
  "endpoint declarations preserve missingness and reject conflicting facts",
  {
    r <- data.frame(
      observation_id = c("a", "a", "a"),
      endpoint = c("burn_grade", "healing", "anatomy"),
      value = c("II", NA, "arm")
    )
    x <- hm_endpoint_contract(r)
    expect_identical(x$references, r)
    expect_identical(x$source_truth, "unverified")
    expect_error(
      hm_endpoint_contract(
        rbind(
          r, data.frame(
            observation_id = "a", endpoint = "burn_grade",
            value = "III"
          )
        )
      ),
      class = "hm_reference_conflict_error"
    )
  }
)
