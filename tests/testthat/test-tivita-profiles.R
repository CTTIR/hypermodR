test_that(
  "TIVITA profiles preserve unknown spectral and timezone evidence",
  {
    v <- array(
      1000 * rep(1:2, 15) +
        100 * rep(
          rep(1:5, each = 2),
          3
        ) +
        rep(1:3, each = 10),
      c(2, 5, 3)
    )
    x <- list(
      cube = v, wavelengths = c(500, 505, 510),
      metadata = list(), references = character(), timestamp = as.POSIXct(
        "2020-01-01",
        tz = "UTC"
      ),
      path = "synthetic_SpecCube.dat"
    )
    d <- hm_from_tivis(x, hm_tivita_profile())
    expect_identical(d@sources[[1]], v)
    expect_true(all(is.na(d@bands$wavelength)))
    expect_identical(d@provenance$timestamp$timezone, "unknown")
    expect_error(
      hm_from_tivis(x, list(id = "Mini")),
      class = "hm_unsupported_error"
    )
    assets <- data.frame(
      role = c(
        "SpecCube", "RGB", "DICOM", "parameter_display",
        "quantitative_export",
        "segmentation", "mask", "metadata", "sidecar"
      ),
      registration_id = "r"
    )
    d <- hm_from_tivis(
      x, hm_tivita_profile(c(500, 600, 700)),
      assets = assets, registration_id = "r"
    )
    expect_identical(d@assets, assets)
    expect_error(
      hm_from_tivis(x, hm_tivita_profile(),
        assets = assets,
        registration_id = "other"
      ),
      class = "hm_source_error"
    )
  }
)
test_that(
  "independent asymmetric BIP bytes identify implemented y-fast layout",
  {
    skip_if_not_installed("tivis.r")
    path <- tempfile(fileext = "_SpecCube.dat")
    on.exit(
      unlink(path),
      add = TRUE
    )
    con <- file(path, "wb")
    writeBin(
      as.integer(c(5, 2, 3)),
      con,
      size = 4, endian = "big"
    )
    expected <- array(NA_real_, c(2, 5, 3))
    for (xx in 1:5) {
      for (yy in 1:2) {
        for (bb in 1:3) {
          value <- yy * 1000 + xx * 100 + bb
          writeBin(
            as.double(value),
            con,
            size = 4, endian = "big"
          )
          expected[yy, xx, bb] <- value
        }
      }
    }
    close(con)
    decoded <- tivis.r::tivis_read_cube(path)
    expect_identical(
      as.numeric(decoded),
      as.numeric(expected)
    )
    expect_identical(
      dim(decoded),
      c(2L, 5L, 3L)
    )
    # Reader's generic wavelength inference is deliberately not promoted
    # to profile evidence.
    x <- list(
      cube = decoded, wavelengths = attr(decoded, "wavelengths"),
      metadata = list(), references = character(), timestamp = as.POSIXct(NA),
      path = path
    )
    expect_true(all(is.na(hm_from_tivis(x, hm_tivita_profile(
    ))@bands$wavelength)))
  }
)
