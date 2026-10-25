test_that(
  "sequences retain supplied times and reject broken linkage",
  {
    x <- core_sequence()
    expect_true(validObject(x))
    expect_equal(x@frame_index$time, c(0, 2.5))
    expect_identical(
      unserialize(serialize(x, NULL)),
      x
    )
    x@frame_index$time <- c(0, 0)
    expect_error(validObject(x))
    x <- core_sequence()
    x@observations$frame_id[2] <- "absent"
    expect_error(validObject(x))
  }
)

test_that(
  "unqualified source timing is preserved without ordinal substitution",
  {
    x <- core_sequence()
    for (times in list(
      c(NA_real_, NA_real_),
      c(2, 2),
      c(2, 1),
      c(NA_real_, 4)
    )) {
      fi <- x@frame_index
      fi$time <- times
      fi$timing_quality <- NULL
      y <- hm_sequence(x@sources, fi, x@observations, x@bands, x@geometry)
      expect_identical(y@frame_index$time, times)
      expect_identical(y@frame_index$frame_id, c("f1", "f2"))
      expect_true(any(y@frame_index$timing_quality != "ordered"))
      expect_true(validObject(y))
    }
  }
)
