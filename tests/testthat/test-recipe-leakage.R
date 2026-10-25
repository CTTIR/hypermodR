test_that(
  "learned transforms use only supplied training rows",
  {
    x <- matrix(
      c(1, 3, 5, 7),
      2
    )
    r <- hm_prep(
      hm_recipe(list(list(type = "scale"))),
      x
    )
    expect_equal(r$steps[[1]]$center, c(2, 6))
    expect_equal(
      hm_bake(r, x),
      matrix(
        rep(
          c(-1, 1) / sqrt(2),
          2
        ),
        2
      )
    )
    before <- serialize(r, NULL)
    hm_bake(
      r, matrix(
        c(1e+09, -1e+09, 4, 5),
        2
      )
    )
    expect_identical(
      serialize(r, NULL),
      before
    )
    expect_equal(
      hm_bake(
        unserialize(before),
        x
      ),
      hm_bake(r, x)
    )
    im <- hm_prep(
      hm_recipe(list(list(type = "impute"))),
      matrix(
        c(1, 3, 2, 4),
        2
      )
    )
    expect_equal(
      hm_bake(
        im, matrix(
          c(NA_real_, NA_real_),
          1
        )
      ),
      matrix(
        c(2, 3),
        1
      )
    )
  }
)
test_that(
  "spectral transforms have independent expected values",
  {
    x <- matrix(
      c(1, 3, 7),
      1
    )
    r <- hm_prep(
      hm_recipe(list(list(type = "derivative"))),
      x, c(400, 402, 406)
    )
    expect_equal(
      hm_bake(r, x),
      matrix(
        c(1, 1),
        1
      )
    )
    expect_error(
      hm_prep(
        hm_recipe(list(list(type = "derivative"))),
        x, c(400, 400, 406)
      )
    )
    expect_error(
      hm_prep(
        hm_recipe(list(list(type = "resample", wavelength = c(300, 500)))),
        x, c(400, 402, 406)
      )
    )
    r <- hm_prep(
      hm_recipe(list(list(type = "snv"))),
      matrix(
        c(1, 2, 3),
        1
      )
    )
    expect_equal(
      hm_bake(
        r, matrix(
          c(1, 2, 3),
          1
        )
      ),
      matrix(
        c(-1, 0, 1),
        1
      )
    )
  }
)
test_that(
  "pooling accounts for every admitted spatial pixel",
  {
    x <- hm_dataset(
      array(1:60, c(3, 5, 4)),
      data.frame(observation_id = "o", image_id = "i", acquisition_id = "a"),
      core_bands(), core_geometry()
    )
    z <- hm_pool(x)
    expect_equal(
      as.vector(z$values),
      c(8, 23, 38, 53)
    )
    expect_equal(z$provenance$eligible_pixels, 15)
    expect_equal(
      sum(z$provenance$weights),
      1
    )
    mask <- array(FALSE, c(3, 5, 1))
    mask[1, 1, 1] <- TRUE
    expect_equal(
      as.vector(hm_pool(x, mask)$values),
      c(1, 16, 31, 46)
    )
    expect_error(hm_pool(x, array(FALSE, c(3, 5, 1))))
  }
)
test_that(
  "band-changing steps carry exact measured wavelength support",
  {
    r <- hm_prep(
      hm_recipe(
        list(
          list(type = "select", bands = c(1, 3)),
          list(type = "derivative")
        )
      ),
      matrix(
        c(1, 3, 7),
        1
      ),
      c(400, 402, 406)
    )
    expect_equal(
      hm_bake(
        r, matrix(
          c(1, 3, 7),
          1
        )
      ),
      matrix(1, 1, 1)
    )
    expect_equal(r$output_wavelength, 403)
  }
)
