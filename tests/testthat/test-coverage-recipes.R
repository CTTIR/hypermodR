test_that("spectral transforms have independent asymmetric numeric oracles", {
  x <- rbind(c(3, 4, 0), c(0, 6, 8))
  expect_equal(.hm_apply_step(list(type = "normalize"), x, NULL), rbind(c(
    .6, .8, 0
  ), c(0, .6, .8)))
  expect_equal(.hm_apply_step(list(type = "snv"), rbind(c(1, 3, 5), c(
    4, 8,
    12
  )), NULL), rbind(c(-1, 0, 1), c(-1, 0, 1)))
  reflectance <- .hm_apply_step(list(type = "reflectance", dark = c(
    1, 2,
    3
  ), white = c(5, 10, 15)), rbind(c(3, 6, 9), c(5, 2, 6)), NULL)
  expect_equal(reflectance, rbind(c(.5, .5, .5), c(1, 0, .25)))
  spectrum <- rbind(c(2, 6, 18), c(4, 12, 36))
  expect_equal(.hm_apply_step(list(type = "derivative"), spectrum, c(
    400,
    500, 800
  )), rbind(c(.04, .04), c(.08, .08)))
  expect_equal(.hm_apply_step(list(type = "resample", wavelength = c(
    450,
    650
  )), spectrum, c(400, 500, 800)), rbind(c(4, 12), c(8, 24)))
  expect_equal(.hm_apply_step(list(type = "continuum"), rbind(
    c(2, 1, 2),
    c(4, 4, 4)
  ), c(400, 500, 600)), rbind(c(1, .5, 1), c(1, 1, 1)))
  expect_equal(.hm_apply_step(
    list(type = "msc", reference = c(1, 2, 3)),
    rbind(c(5, 7, 9), c(4, 7, 10)), NULL
  ), rbind(c(1, 2, 3), c(1, 2, 3)))
})

test_that(paste0(
  "spectral transforms reject undefined or extrapolated ",
  "scientific operations"
), {
  x <- matrix(1:6, 2, 3)
  expect_error(.hm_apply_step(
    list(type = "normalize"), matrix(0, 2, 3),
    NULL
  ), "zero spectra")
  expect_error(
    .hm_apply_step(list(type = "snv"), matrix(1, 2, 3), NULL),
    "nonconstant"
  )
  expect_error(.hm_apply_step(
    list(type = "normalize"), matrix(Inf, 2, 3),
    NULL
  ), "finite")
  expect_error(.hm_apply_step(
    list(type = "msc", reference = rep(1, 3)), x,
    NULL
  ), "identifiable")
  expect_error(.hm_apply_step(list(
    type = "reflectance", dark = 1:3,
    white = 1:3
  ), x, NULL), "references")
  expect_error(.hm_apply_step(list(type = "derivative"), x, c(
    500, 400,
    600
  )), "increasing")
  expect_error(.hm_apply_step(list(type = "resample", wavelength = c(
    300,
    600
  )), x, c(400, 500, 600)), "extrapolate")
  expect_error(.hm_apply_step(list(type = "continuum"), -x, c(
    400, 500,
    600
  )), "positive")
  expect_error(.hm_apply_step(list(type = "not-supported"), x, NULL),
    class = "hm_unsupported_error"
  )
})

test_that(paste0(
  "learned transforms fit training constants and preserve ",
  "exact selection"
), {
  x <- rbind(c(2, 4, 5), c(4, 4, 9), c(6, 4, 13))
  scale <- .hm_learn_step(list(type = "scale"), x)
  expect_equal(scale$center, c(4, 4, 9))
  expect_equal(scale$scale, c(2, 1, 4))
  expect_equal(.hm_apply_step(scale, x, NULL), rbind(c(-1, 0, -1), c(
    0, 0,
    0
  ), c(1, 0, 1)))
  missing <- x
  missing[1, 1] <- NA
  impute <- .hm_learn_step(list(type = "impute"), missing)
  expect_equal(impute$center, c(5, 4, 9))
  expected <- x
  expected[1, 1] <- 5
  expect_equal(.hm_apply_step(impute, missing, NULL), expected)
  selected <- .hm_learn_step(list(type = "select", bands = c(3, 1)), x)
  expect_equal(.hm_apply_step(selected, x, NULL), cbind(c(5, 9, 13), c(
    2,
    4, 6
  )))
  pca <- .hm_learn_step(list(type = "pca", components = 1L), x)
  score <- .hm_apply_step(pca, x, NULL)
  expect_equal(as.numeric(crossprod(score)), 40, tolerance = 1e-10)
  expect_equal(as.numeric(crossprod(pca$rotation)), 1, tolerance = 1e-10)
  expect_equal(.hm_learn_step(list(type = "msc"), x)$reference, c(4, 4, 9))
  for (step in list(list(type = "scale"), list(
    type = "pca"
  ))) {
    expect_error(.hm_learn_step(step, missing), "finite")
  }
  missing[, 1] <- NA
  expect_error(
    .hm_learn_step(list(type = "impute"), missing),
    "entirely missing"
  )
  expect_error(
    .hm_learn_step(list(type = "pca", components = 0), x),
    "component count"
  )
  expect_error(
    .hm_learn_step(list(type = "pca", components = 10), x),
    "component count"
  )
  expect_error(
    .hm_learn_step(list(type = "select", bands = c(1, 1)), x),
    "selected bands"
  )
})
