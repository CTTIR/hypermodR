test_that(
  "unsupported classes fail explicitly",
  {
    expect_error(
      hm_as_dataset(list()),
      class = "hm_unsupported_error"
    )
  }
)
test_that(
  "hyperspectR roundtrip retains asymmetric cube and full mask",
  {
    skip_if_not_installed("hyperspectR")
    v <- array(
      seq_len(30),
      c(2, 5, 3)
    )
    v[1, 1, 1] <- NA_real_
    mask <- matrix(
      c(TRUE, FALSE, NA, rep(TRUE, 7)),
      2, 5
    )
    x <- suppressWarnings(
      hyperspectR::hsi_cube(
        v, c(700, 500, 600),
        c(7, 5, 6),
        list(note = "native"),
        mask
      )
    )
    d <- hm_as_dataset(x)
    y <- hm_as_hsi_cube(d, "image")
    expect_identical(y$data, x$data)
    expect_identical(y$mask, mask)
    expect_identical(y$wavelengths, x$wavelengths)
    expect_identical(y$fwhm, x$fwhm)
    expect_identical(y$metadata$note, "native")
    expect_identical(y$metadata$hypermodR$sorted_to_native, 1:3)
  }
)
test_that(
  "native unsorted bands retain inverse map and mask on reimport",
  {
    skip_if_not_installed("hyperspectR")
    v <- array(
      seq_len(30),
      c(2, 5, 3)
    )
    d <- hm_dataset(
      v, data.frame(
        observation_id = "o", image_id = "i",
        acquisition_id = "a"
      ),
      data.frame(wavelength = c(700, 500, 600)),
      list(
        origin = c(0, 0),
        transform = diag(3)
      )
    )
    y <- suppressWarnings(hm_as_hsi_cube(d, "a"))
    expect_equal(
      y$data, v[, , c(2, 3, 1),
        drop = FALSE
      ]
    )
    expect_identical(y$metadata$hypermodR$sorted_to_native, c(2L, 3L, 1L))
    expect_identical(y$metadata$hypermodR$inverse_sort, c(3L, 1L, 2L))
    back <- hm_as_dataset(y)
    expect_identical(back@bands$native_index, c(2L, 3L, 1L))
  }
)
