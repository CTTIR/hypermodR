test_that("decadic coefficients need the documented ln(10) conversion", {
  wave <- c(500, 600, 700)
  decadic <- matrix(c(1, 0, 1, 0, 1, 1), 3, 2)
  amount <- rbind(c(0.1, 0.2), c(0.2, 0.05), c(0.05, 0.1))
  reflectance <- 10^(-amount %*% t(decadic))
  control <- list(
    wavelength = wave,
    input_provenance = list(radiometric_unit = "reflectance",
                            calibration = "synthetic exact")
  )
  retrieve <- function(values) {
    library <- hm_spectral_library(values, wave, "per amount per length",
                                   "synthetic fixture", "CC0")
    fit <- hypermodR:::.hm_fit_engine(
      "beer_lambert", reflectance, NULL, list(type = "unmixing"),
      list(library = library, path_length = 1), control
    )
    hypermodR:::.hm_predict_engine(fit, reflectance, NULL)
  }
  expect_equal(retrieve(decadic * log(10)), amount, tolerance = 1e-6)
  expect_equal(retrieve(decadic), amount * log(10), tolerance = 1e-6)
})
