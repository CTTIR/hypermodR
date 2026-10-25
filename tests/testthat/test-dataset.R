test_that(
  "native spectra retain dimensions, identities and serialization",
  {
    x <- hm_dataset(
      matrix(1:8, 2, 4),
      data.frame(observation_id = c("a", "b")),
      core_bands()
    )
    expect_s4_class(x, "HMDataset")
    expect_true(validObject(x))
    expect_equal(
      hm_spectra(x, list(observation_id = "b", bands = 2))$values,
      matrix(4L, 1, 1)
    )
    expect_identical(
      unserialize(serialize(x, NULL)),
      x
    )
    expect_length(x@geometry, 0)
    expect_error(
      hm_dataset(
        matrix(1:8, 2),
        data.frame(observation_id = 1:2),
        core_bands()
      )
    )
    expect_error(
      hm_dataset(
        matrix(1:8, 2),
        data.frame(observation_id = c("a", "a")),
        core_bands()
      )
    )
    expect_error(
      hm_dataset(
        matrix(1:8, 2),
        data.frame(observation_id = c("a", "b")),
        data.frame(wavelength = c(500, 500, 700, 800))
      )
    )
    b <- core_bands()
    b$wavelength[1] <- NA_real_
    expect_error(
      hm_dataset(
        matrix(1:8, 2),
        data.frame(observation_id = c("a", "b")),
        b
      )
    )
    b$availability <- c("unavailable", rep("measured", 3))
    expect_true(
      validObject(
        hm_dataset(
          matrix(1:8, 2),
          data.frame(observation_id = c("a", "b")),
          b
        )
      )
    )
    expect_error(hm_spectra(core_image()))
    expect_error(
      hm_dataset(
        matrix(1:8, 2),
        data.frame(observation_id = c("a", "b")),
        core_bands(),
        provenance = list(handle = new.env())
      )
    )
  }
)

test_that(
  "nested hidden live handles cannot enter persistent sources",
  {
    v <- matrix(1:8, 2, 4)
    attr(v, "handle") <- new.env()
    expect_error(
      hm_dataset(
        v, data.frame(observation_id = c("a", "b")),
        core_bands()
      )
    )
  }
)
