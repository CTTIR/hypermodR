test_that(
  "direct sequence validity retains timing and linkage contracts",
  {
    original <- core_sequence()
    validate <- hypermodR:::.hm_valid_sequence
    expect_true(validate(original))
    object <- original
    object@frame_index$sequence_id <- NULL
    expect_match(
      validate(object),
      "Frame index"
    )
    object <- original
    object@sources <- object@sources[1]
    expect_match(
      validate(object),
      "every source"
    )
    object <- original
    object@frame_index$time <- as.character(
                                            object@frame_index$time)
    expect_match(
      validate(object),
      "numeric elapsed seconds"
    )
    object <- original
    object@frame_index$time[1] <- Inf
    expect_match(
      validate(object),
      "numeric elapsed seconds"
    )
    for (quality in list(
      NULL, c(NA_character_, "ordered"),
      c("invented", "ordered"),
      c(1, 2)
    )) {
      object <- original
      object@frame_index$timing_quality <- quality
      expect_match(
        validate(object),
        "timing_quality"
      )
    }
    for (times in list(
      c(NA_real_, 2.5),
      c(0, 0),
      c(2.5, 0)
    )) {
      object <- original
      object@frame_index$time <- times
      expect_match(
        validate(object),
        "timing_quality"
      )
      quality <- hypermodR:::.hm_timing_quality(
                                                object@frame_index)
      object@frame_index$timing_quality <- quality
      expect_true(validate(object))
    }
    object <- original
    object@observations$frame_id <- NULL
    expect_match(
      validate(object),
      "known frames"
    )
    object <- original
    object@observations$frame_id[1] <- "unknown"
    expect_match(
      validate(object),
      "known frames"
    )
    object <- original
    object@observations$acquisition_id[1] <- "other"
    expect_match(
      validate(object),
      "acquisition disagrees"
    )
    object <- original
    object@observations <- object@observations[1,
                                               , drop = FALSE]
    expect_match(
      validate(object),
      "Every frame"
    )
  }
)

test_that(
  paste0(
    "dataset construction preserves explicit provenance and ",
    "geometry"
  ),
  {
    values <- matrix(
      seq_len(8),
      nrow = 2
    )
    observations <- data.frame(
                               observation_id = c(
                                                  "o1", "o2"))
    make <- function(
      provenance = list(), geometry = NULL) {
      hm_dataset(
        values, observations, core_bands(),
        geometry, provenance
      )
    }
    object <- make()
    expect_identical(
                     object@data_kind,
                     "spectra")
    expect_identical(object@geometry, list())
    expect_identical(
      object@provenance$radiometric_unit,
      "unknown"
    )
    expect_identical(
                     object@provenance$calibration,
                     "unknown")
    object <- make(
      list(
        radiometric_unit = "reflectance",
        calibration = "declared"
      )
    )
    expect_identical(
      object@provenance$radiometric_unit,
      "reflectance"
    )
    expect_identical(
                     object@provenance$calibration,
                     "declared")
    expect_error(
      make("invalid"),
      "serializable list"
    )
    expect_error(
      make(list(handle = new.env())),
      "serializable list"
    )
    expect_error(
      make(geometry = core_geometry()),
      "no geometry"
    )
    image <- core_image()
    expect_identical(
                     image@data_kind,
                     "image")
    expect_identical(
                     image@geometry,
                     core_geometry())
    expect_true(
                hypermodR:::.hm_valid_dataset(
                                              image))
  }
)
