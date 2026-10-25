test_that(
  "public recipes propagate measured coordinates through changing dimensions",
  {
    x <- rbind(
      c(2, 6, 18),
      c(4, 12, 36),
      c(6, 18, 54)
    )
    wavelength <- c(400, 500, 800)
    selected <- hm_prep(
      hm_recipe(
        list(list(type = "select", bands = c(1, 3)))
      ),
      x, wavelength
    )
    expect_identical(selected$output_wavelength, c(400, 800))
    derivative <- hm_prep(
      hm_recipe(list(list(type = "derivative"))),
      x, wavelength
    )
    expect_identical(derivative$output_wavelength, c(450, 650))
    resampled <- hm_prep(
      hm_recipe(
        list(
          list(type = "resample", wavelength = c(450, 650))
        )
      ),
      x, wavelength
    )
    expect_identical(resampled$output_wavelength, c(450, 650))
    expect_identical(resampled$track, "adapted")
    pca <- hm_prep(
      hm_recipe(list(list(type = "pca", components = 1))),
      x, wavelength
    )
    expect_null(pca$output_wavelength)
    expect_error(
      hm_recipe(list(1)),
      "step specifications"
    )
  }
)

test_that(
  "acquisition pools reject biological identity changes and invalid budgets",
  {
    z <- eval_fixture()
    pool <- z$r
    pool$y <- NA_character_
    observations <- rbind(
      z$r, transform(
        z$r[1, , drop = FALSE], observation_id = "held",
        subject_id = "held", acquisition_id = "held"
      )
    )
    source <- hm_dataset(
      matrix(
        seq_len(14),
        7, 2
      ),
      observations, data.frame(wavelength = c(500, 600))
    )
    split <- list(train = 1:6, test = 7L)
    run <- function(
      pool = pool, prediction = z$p, budget = 1L) {
      hm_active_learning(
        prediction, pool, z$r$observation_id,
        "held", budget, 1, "fixture-revision",
        source, split
      )
    }
    bad <- pool
    bad$subject_id[1] <- "changed"
    expect_error(
      run(bad),
      "biological identities"
    )
    expect_error(
      run(pool, budget = 0),
      "acquisition budget"
    )
    expect_error(
      run(pool, prediction = list()),
      "Aligned unlabeled pool"
    )
  }
)

test_that(
  "anomaly and unknown model routes fail explicitly",
  {
    x <- matrix(1:12, 6, 2)
    expect_error(
      .hm_fit_unsupervised(
        list(
          model_id = "pca_anomaly", parameters = list(rank = 2)
        ),
        x
      ),
      "Invalid anomaly PCA"
    )
    expect_error(
      .hm_fit_unsupervised(
        list(model_id = "unknown", parameters = list()),
        x
      ),
      class = "hm_capability_error"
    )
    expect_error(
      .hm_predict_unsupervised(
        list(model_id = "unknown", object = list()),
        x
      ),
      class = "hm_capability_error"
    )
  }
)
