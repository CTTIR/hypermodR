test_that(
  "application constructs without side effects and server validates",
  {
    skip_if_not_installed("shiny")
    skip_if_not_installed("bslib")
    skip_if_not_installed("callr")
    expect_s3_class(
      hm_app(launch = FALSE),
      "shiny.appobj"
    )
    shiny::testServer(
      hypermodR:::.hm_app_server,
      {
        expect_identical(state$status, "empty")
        session$setInputs(example = 1)
        expect_identical(state$status, "imported")
        expect_null(state$job)
        session$setInputs(configure = 1)
        expect_match(state$error, "Review quality")
        session$setInputs(
          qc = TRUE, outcome = "response", type = "regression",
          groups = "subject_id",
          model = "lm", seed = 42, scale = FALSE, reference = "synthetic"
        )
        session$setInputs(configure = 2)
        expect_identical(state$status, "ready")
        expect_null(state$job)
        session$setInputs(dark = TRUE)
        expect_identical(state$status, "ready")
        session$setInputs(model = "pcr")
        expect_identical(state$status, "configuring")
      }
    )
  }
)
test_that(
  "ordinary errors with credentials are withheld",
  {
    expect_match(
      hypermodR:::.hm_app_safe_error(simpleError("password=hidden")),
      "withheld"
    )
  }
)
test_that(
  "explicit image pooling retains all eligible pixels and grouping",
  {
    d <- hm_dataset(
      array(
        seq_len(24),
        c(2, 3, 4)
      ),
      data.frame(
        observation_id = "image1", image_id = "i1", acquisition_id = "a1",
        subject_id = "s1", response = 3
      ),
      data.frame(wavelength = c(500, 600, 700, 800)),
      geometry = list(
        origin = c(1, 1),
        transform = diag(3)
      )
    )
    pooled <- hypermodR:::.hm_app_pool(d)
    expect_identical(pooled@observations$subject_id, "s1")
    expect_identical(pooled@observations$observation_id, "pooled_a1")
    expect_equal(
      hm_spectra(pooled)$values[1, ], c(3.5, 9.5, 15.5, 21.5)
    )
    expect_equal(pooled@provenance$support[[1]]$eligible_pixels, 6)
  }
)
