test_that("metadata rejects hidden live state and ambiguous columns", {
  x <- core_image()
  for (slot in c("observations", "bands", "assets")) {
    changed <- x
    tab <- methods::slot(changed, slot)
    if (!nrow(tab)) {
      tab <- data.frame(label = "asset")
    }
    tab$callback <- I(rep(list(function() TRUE), nrow(tab)))
    methods::slot(changed, slot) <- tab
    expect_error(methods::validObject(changed), "live handles or functions")
  }
  changed <- x
  changed@observations[["duplicate"]] <- "other"
  names(changed@observations)[4] <- "observation_id"
  expect_error(methods::validObject(changed), "unique and nonempty")
  changed <- core_sequence()
  changed@frame_index$callback <- I(rep(list(new.env()), 2))
  expect_error(methods::validObject(changed), "live handles or functions")
})

test_that("band identity and validity metadata retain explicit types", {
  x <- core_image()
  for (column in c("native_index", "canonical_index")) {
    for (value in list(c(1, 1, 3, 4), c(1, 2, 3, Inf), letters[1:4])) {
      changed <- x
      changed@bands[[column]] <- value
      expect_error(methods::validObject(changed), "positive integers")
    }
  }
  changed <- x
  changed@bands$valid <- c("yes", "no", "yes", "yes")
  expect_error(methods::validObject(changed), "validity must be logical")
  changed@bands$valid <- c(TRUE, FALSE, NA, TRUE)
  expect_identical(hm_read_block(changed)$band_map$valid,
                   c(TRUE, FALSE, NA, TRUE))
  for (width in list(c(1, 2, 3, -1), c(1, 2, 3, Inf), letters[1:4])) {
    changed@bands$fwhm <- width
    expect_error(methods::validObject(changed), "FWHM")
  }
})

test_that("selected frames preserve masks, bands and biological identity", {
  skip_if_not_installed("hyperspectR")
  x <- core_sequence()
  x@bands$wavelength <- c(800, 500, 700, 600)
  x@bands$valid <- c(FALSE, TRUE, NA, TRUE)
  x@bands$availability <- rep("declared", 4)
  x@bands$note <- c("bad detector", "good", "unknown", "good")
  x@observations$subject_id <- "subject-1"
  first <- matrix(TRUE, 3, 5)
  second <- first
  first[1, 1] <- NA
  second[2, 4] <- NA
  second[3, 2] <- FALSE
  x@provenance$spatial_masks <- list(first, second)
  cube <- suppressWarnings(hm_as_hsi_cube(x, "a", "f2"))
  back <- hm_as_dataset(cube)
  ord <- c(2L, 4L, 3L, 1L)
  expect_identical(cube$mask, second & !is.na(second))
  expect_identical(back@provenance$spatial_masks, list(second))
  expect_equal(back@sources[[1]], x@sources[[2]][, , ord])
  expect_identical(back@observations, x@observations[2, , drop = FALSE])
  expect_identical(back@bands$valid, c(TRUE, TRUE, NA, FALSE))
  expect_identical(back@bands$note, x@bands$note[ord])
  expect_identical(back@bands$canonical_index, ord)
  expect_identical(back@bands$native_index, ord)
  changed <- cube
  changed$data[1, 1, 1] <- changed$data[1, 1, 1] + 1
  expect_error(hm_as_dataset(changed), class = "hm_source_error")
  changed <- cube
  changed$metadata$hypermodR$sorted_to_native <- rep(1L, 4)
  expect_error(hm_as_dataset(changed), class = "hm_source_error")
  replacement <- back@observations
  replacement$subject_id <- "explicit-replacement"
  expect_identical(hm_as_dataset(cube, observations = replacement)@observations,
                   replacement)
})

test_that("band fields require one scalar per band", {
  for (column in c("wavelength", "native_index", "canonical_index", "fwhm",
                   "valid")) {
    x <- core_image()
    values <- if (column == "valid") rep(TRUE, 8) else seq_len(8)
    x@bands[[column]] <- I(matrix(values, 4, 2))
    expect_error(methods::validObject(x))
  }
})

test_that("restored observation and mask metadata are bound to their receipt", {
  skip_if_not_installed("hyperspectR")
  x <- core_image()
  x@bands$fwhm <- c(2, 3, 4, 5)
  mask <- matrix(TRUE, 3, 5)
  mask[1, 1] <- NA
  x@provenance$spatial_masks <- list(mask)
  x@observations$subject_id <- "subject-1"
  cube <- hm_as_hsi_cube(x, "a")
  changed <- cube
  changed$metadata$hypermodR$native_band_map$fwhm <- rep(99, 4)
  expect_error(hm_as_dataset(changed), class = "hm_source_error")
  changed <- cube
  changed$metadata$hypermodR$source_observations$subject_id <- "subject-2"
  expect_error(hm_as_dataset(changed), class = "hm_source_error")
  changed <- cube
  changed$metadata$hypermodR$spatial_masks[[1]][1, 1] <- FALSE
  expect_error(hm_as_dataset(changed), class = "hm_source_error")
  restored <- hm_as_dataset(cube)
  roundtrip <- hm_as_hsi_cube(restored, "a")
  expect_identical(hm_as_dataset(roundtrip)@observations, x@observations)
  expect_identical(hm_as_dataset(roundtrip)@provenance$spatial_masks,
                   list(mask))
})
