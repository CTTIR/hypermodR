test_that(
  "planned folds remain bound to the original observation order",
  {
    o <- data.frame(
      observation_id = paste0("o", 1:12),
      subject_id = paste0("s", 1:12),
      y = as.numeric(1:12)
    )
    d <- hm_dataset(
      matrix(
        as.numeric(1:24),
        12, 2
      ),
      o, data.frame(wavelength = c(500, 600))
    )
    task <- hm_task("y", "regression", "spectrum", "subject_id")
    s <- hm_split(d, task, folds = 3, inner_folds = 2)
    shuffled <- hm_dataset(d@sources[[1]][12:1, ], o[12:1, ], d@bands)
    expect_error(
      .hm_validate_fit_split(
        .hm_model_data(shuffled),
        task, s$folds[[1]]
      ),
      class = "hm_integrity_error"
    )
    expect_identical(s$folds[[1]]$observation_id, o$observation_id)
    expect_identical(s$folds[[1]]$inner[[1]]$observation_id, o$observation_id)
  }
)

test_that(
  "single image cannot alias distinct acquisition identities",
  {
    o <- data.frame(
      observation_id = c("o1", "o2"),
      image_id = c("i1", "i2"),
      acquisition_id = c("a1", "a2")
    )
    expect_error(
      hm_dataset(
        array(
          as.numeric(1:12),
          c(2, 3, 2)
        ),
        o, data.frame(wavelength = c(500, 600)),
        list(
          origin = c(0, 0),
          transform = diag(3)
        )
      )
    )
  }
)

test_that(
  "companion roundtrip preserves physical geometry",
  {
    skip_if_not_installed("hyperspectR")
    geometry <- list(
      origin = c(12, 34),
      transform = matrix(
        c(0, 2, 0, -3, 0, 0, 100, 200, 1),
        3, 3
      )
    )
    d <- hm_dataset(
      array(
        as.numeric(1:12),
        c(2, 3, 2)
      ),
      data.frame(observation_id = "o", image_id = "i", acquisition_id = "a"),
      data.frame(wavelength = c(500, 600)),
      geometry
    )
    back <- hm_as_dataset(hm_as_hsi_cube(d, "a"))
    xy <- matrix(
      c(1, 1, 3, 2),
      2, 2,
      byrow = TRUE
    )
    expect_equal(
      hm_coordinates(back@geometry, xy),
      hm_coordinates(geometry, xy)
    )
  }
)

test_that(
  "selected frame observation order matches returned time axis",
  {
    fi <- data.frame(
      frame_id = c("f1", "f2"),
      sequence_id = "s", acquisition_id = "a", time = c(0, 1)
    )
    o <- data.frame(
      observation_id = c("o1", "o2"),
      image_id = c("i1", "i2"),
      acquisition_id = "a", frame_id = fi$frame_id
    )
    d <- hm_sequence(
      list(
        array(1, c(2, 3, 2)),
        array(2, c(2, 3, 2))
      ),
      fi, o, data.frame(wavelength = c(500, 600)),
      list(
        origin = c(0, 0),
        transform = diag(3)
      )
    )
    z <- hm_read_block(d, list(frame_id = c("f2", "f1")))
    expect_identical(z$observations$frame_id, c("f2", "f1"))
  }
)
