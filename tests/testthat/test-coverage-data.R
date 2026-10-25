test_that(paste0(
  "geometry handles translated reflected and sheared ",
  "coordinate systems"
), {
  g <- list(origin = c(3, -2), transform = matrix(c(
    2, 0, 0, 1, -3, 0, 10,
    7, 1
  ), 3))
  points <- rbind(c(1, 1), c(4, 2), c(-2, 5))
  expected <- rbind(c(14, 13), c(21, 10), c(12, 1))
  expect_equal(hm_coordinates(g, points), expected)
  expect_equal(hm_coordinates(g, expected, inverse = TRUE), points)
  expect_error(hm_coordinates(g, matrix(NA_real_, 1, 2)), "finite two-column")
  expect_error(hm_coordinates(g, 1:2), "finite two-column")
  for (bad in list(
    list(), list(origin = c(0, Inf), transform = diag(3)),
    list(origin = c(0, 0), transform = matrix(0, 3, 3)), list(origin = c(
      0,
      0
    ), transform = diag(2))
  )) {
    expect_error(hm_coordinates(bad, points), "Geometry requires")
  }
})

test_that(paste0(
  "sequence validity refuses missing source timing and ",
  "cross-acquisition linkage"
), {
  original <- core_sequence()
  invalid <- function(change, message) {
    x <- original
    x <- change(x)
    expect_error(methods::validObject(x), message)
  }
  invalid(function(x) {
    x@frame_index$time <- NULL
    x
  }, "explicit time")
  invalid(
    function(x) {
      x@frame_index <- x@frame_index[1, ]
      x
    },
    "every source"
  )
  invalid(
    function(x) {
      x@frame_index$time <- c(0, Inf)
      x
    },
    "numeric elapsed"
  )
  invalid(function(x) {
    x@frame_index$timing_quality <- c(
      "ordered",
      NA
    )
    x
  }, "timing_quality")
  invalid(function(x) {
    x@frame_index$time <- c(0, 0)
    x
  }, "timing_quality")
  invalid(
    function(x) {
      x@observations$frame_id <- c("f1", "missing")
      x
    },
    "known frames"
  )
  invalid(
    function(x) {
      x@observations$acquisition_id[2] <- "other"
      x
    },
    "acquisition disagrees"
  )
  invalid(
    function(x) {
      x@observations <- x@observations[1, ]
      x
    },
    "Every frame"
  )
  expect_error(hm_sequence(
    list(), original@frame_index,
    original@observations, core_bands(), core_geometry()
  ), "nonempty")
  expect_error(hm_sequence(
    list(matrix(1, 2, 4)), original@frame_index,
    original@observations, core_bands(), core_geometry()
  ), "spatial cube")
  expect_error(hm_sequence(original@sources, original@frame_index,
                 original@observations, core_bands(), core_geometry(),
                 provenance = "bad"
               ), "provenance")
})

test_that(paste0(
  "dataset validity guards persistence, source ",
  "cardinality and spatial identities"
), {
  original <- core_image()
  invalid <- function(change, message) {
    x <- original
    x <- change(x)
    expect_error(methods::validObject(x), message)
  }
  invalid(function(x) {
    x@provenance$handle <- new.env()
    x
  }, "live handles")
  invalid(
    function(x) {
      x@observations <- x@observations[FALSE, ]
      x
    },
    "At least one observation"
  )
  invalid(function(x) {
    x@sources <- list()
    x
  }, "At least one source")
  invalid(
    function(x) {
      x@data_kind <- c("image", "spectra")
      x
    },
    "one known value"
  )
  invalid(
    function(x) {
      x@sources[[2]] <- array(1, c(3, 5, 2))
      x
    },
    "bands disagree"
  )
  invalid(function(x) {
    x@data_kind <- "unknown"
    x
  }, "spatial source shape")
  invalid(
    function(x) {
      x@observations$image_id <- NULL
      x
    },
    "Images require"
  )
  invalid(
    function(x) {
      x@observations <- rbind(x@observations, transform(
        x@observations,
        observation_id = "second", image_id = "other"
      ))
      x
    },
    "one image_id"
  )
  invalid(
    function(x) {
      x@sources[[2]] <- x@sources[[1]]
      x
    },
    "exactly one source"
  )
  invalid(
    function(x) {
      x@provenance$spatial_masks <- list()
      x
    },
    "one original mask"
  )
  invalid(function(x) {
    x@provenance$spatial_masks <- list(matrix(
      1, 3,
      5
    ))
    x
  }, "logical native-grid")
  expect_error(
    hm_dataset(matrix(1, 1, 2), data.frame(
      observation_id = "o"
    ), data.frame(wavelength = c(NA, 500))),
    "availability unavailable"
  )
  expect_error(hm_dataset(matrix(1, 1, 2), data.frame(
    observation_id = "o"
  ), data.frame(
    wavelength = c(500, 600),
    unit = "um"
  )), "unit must be nm")
  expect_error(hm_dataset(matrix(1, 1, 2), data.frame(
    observation_id = "o"
  ), data.frame(
    wavelength = c(500, 600),
    native_index = c(1, 1)
  )), "native_index")
})
