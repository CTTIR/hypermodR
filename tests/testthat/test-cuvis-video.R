test_that(
  "portable sequence retains every chronology anomaly",
  {
    v <- array(
      seq_len(30),
      c(2, 5, 3)
    )
    t <- c(0, 2, 2, 1, NA_real_)
    fi <- data.frame(
      frame_id = paste0("f", 1:5),
      sequence_id = "s", acquisition_id = "a", time = t, item_type = c(
        "science", "reference", "gap", "science", "preview"
      )
    )
    obs <- data.frame(
      observation_id = paste0("o", 1:5),
      image_id = paste0("i", 1:5),
      frame_id = fi$frame_id, acquisition_id = "a"
    )
    d <- hm_portable_sequence(
      rep(
        list(v),
        5
      ),
      fi, obs, data.frame(wavelength = c(500, 600, 700)),
      list(
        origin = c(0, 0),
        transform = diag(3)
      )
    )
    expect_identical(d@frame_index$time, t)
    expect_identical(d@frame_index$item_type, fi$item_type)
    expect_true(is.na(d@provenance$timing_quality$nominal_fps))
    expect_identical(d@provenance$timing_quality$duplicate, 3L)
    expect_identical(d@provenance$timing_quality$reversed, 4L)
  }
)
