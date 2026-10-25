test_that(
  "asymmetric known values and tiles retain all pixels and singleton axes",
  {
    x <- core_sequence()
    z <- hm_read_block(x)
    expect_equal(
      dim(z$values),
      c(3L, 5L, 4L, 2L)
    )
    expect_equal(
      as.vector(z$values),
      as.numeric(1:120)
    )
    expect_true(all(z$valid))
    expect_equal(
      dim(z$valid),
      c(3L, 5L, 2L)
    )
    for (j in 1:5) {
      expect_equal(
        unname(hm_read_block(x, list(cols = j))$values),
        unname(z$values[, j, , , drop = FALSE])
      )
    }
    p <- hm_read_block(
      x, list(
        rows = c(0, 1, 3, 4),
        cols = c(0, 1, 5, 6),
        bands = 2, frame_id = "f2"
      )
    )
    expect_equal(
      dim(p$values),
      c(4L, 4L, 1L, 1L)
    )
    expect_false(
      any(
        p$valid[c(1, 4), , ]
      )
    )
    expect_true(is.na(p$values[1, 1, 1, 1]))
    expect_equal(p$values[2, 2, 1, 1], 76)
    expect_error(hm_read_block(x, list(bands = 0)))
    expect_error(hm_read_block(x, list(wrong = 1)))
    old <- options(hypermodR.block_bytes = 1)
    on.exit(options(old))
    expect_error(
      hm_read_block(x),
      class = "hm_resource_error"
    )
  }
)
test_that(
  "variable grids reject stacking but admit individual frames",
  {
    x <- core_sequence()
    x@sources[[2]] <- array(1:40, c(2, 5, 4))
    expect_error(
      hm_read_block(x),
      "Variable grids"
    )
    expect_equal(
      dim(hm_read_block(x, list(frame_id = "f2"))$values),
      c(2L, 5L, 4L, 1L)
    )
  }
)
test_that(
  "lazy RDS sources verify content before reading",
  {
    path <- tempfile(tmpdir = Sys.getenv("TMPDIR"))
    on.exit(unlink(path))
    saveRDS(
      array(1:60, c(3, 5, 4)),
      path
    )
    src <- list(
      reader = "rds", locator = path, hash = digest::digest(
        file = path,
        algo = "sha256"
      ),
      dim = c(3, 5, 4)
    )
    x <- hm_dataset(
      src, data.frame(
        observation_id = "o", image_id = "i",
        acquisition_id = "a"
      ),
      core_bands(), core_geometry()
    )
    expect_equal(
      as.vector(hm_read_block(x)$values),
      as.numeric(1:60)
    )
    saveRDS(
      array(61:120, c(3, 5, 4)),
      path
    )
    expect_error(
      hm_cache_key(x),
      class = "hm_source_error"
    )
    expect_error(
      hm_read_block(x),
      class = "hm_source_error"
    )
  }
)
test_that(
  "native selected reads avoid full-cube working allocations",
  {
    skip_if_not(capabilities("profmem"))
    x <- hm_dataset(
      array(1, c(256, 256, 32)),
      data.frame(observation_id = "o", image_id = "i", acquisition_id = "a"),
      data.frame(wavelength = seq(400, 710, 10)),
      core_geometry()
    )
    path <- tempfile(tmpdir = Sys.getenv("TMPDIR"))
    on.exit(unlink(path))
    Rprofmem(path)
    z <- hm_read_block(x, list(rows = 1:16, cols = 1:32))
    Rprofmem(NULL)
    sizes <- suppressWarnings(as.numeric(sub(" .*", "", readLines(path))))
    selected <- length(z$values) *
      8 + length(z$valid) *
      4
    expect_lte(
      max(sizes, na.rm = TRUE),
      2 * selected + 64 * 1024^2
    )
    expect_equal(
      length(z$values),
      16 * 32 * 32
    )
  }
)

test_that(
  "source masks retain NA while block usability excludes it",
  {
    x <- core_sequence()
    mask <- matrix(TRUE, 3, 5)
    mask[1, 1] <- NA
    mask[3, 5] <- FALSE
    x@provenance$spatial_masks <- list(mask, NULL)
    expect_true(validObject(x))
    z <- hm_read_block(x)
    expect_false(z$valid[1, 1, 1])
    expect_false(z$valid[3, 5, 1])
    expect_true(all(z$valid[, , 2]))
    expect_identical(x@provenance$spatial_masks[[1]], mask)
    expect_identical(z$provenance$spatial_masks[[1]], mask)
    expect_equal(z$values[1, 1, 1, 1], 1)
    x@provenance$spatial_masks <- list(
      matrix(TRUE, 2, 2),
      NULL
    )
    expect_error(validObject(x))
  }
)
