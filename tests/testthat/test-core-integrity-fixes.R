test_that(
  "all grouped fold levels bind complete observation identities",
  {
    obs <- data.frame(
      observation_id = paste0("o", 1:12),
      subject_id = paste0("s", 1:12),
      y = 1:12
    )
    data <- hm_dataset(
      matrix(
        as.numeric(1:24),
        12, 2
      ),
      obs, data.frame(wavelength = c(500, 600))
    )
    task <- hm_task("y", "regression", "spectrum", "subject_id")
    plan <- hm_split(data, task, folds = 3, inner_folds = 2)
    model <- .hm_model_data(data)
    expect_no_error(.hm_validate_fit_split(model, task, plan$folds[[1]]))
    for (fold in plan$folds) {
      expect_identical(fold$observation_id, obs$observation_id)
      for (inner in fold$inner) {
        expect_identical(
          inner$observation_id,
          obs$observation_id
        )
      }
    }
    fold <- plan$folds[[1]]
    fold$inner[[1]]$observation_id <- rev(obs$observation_id)
    expect_error(
      .hm_validate_fit_split(model, task, fold),
      class = "hm_integrity_error"
    )
    fold <- plan$folds[[1]]
    fold$inner[[1]]$train <- fold$test
    fold$inner[[1]]$test <- integer()
    expect_error(
      .hm_validate_fit_split(model, task, fold),
      "outer training"
    )
    obs$timestamp <- seq_len(12)
    anonymous <- obs
    anonymous$observation_id <- NULL
    expect_error(
      hm_split(anonymous, task, design = "temporal", cutoff = 7),
      "metadata key"
    )
    expect_error(
      hm_split(anonymous, task, folds = 3, inner_folds = 2),
      "metadata key"
    )
    temporal <- hm_split(obs, task, design = "temporal", cutoff = 7)
    expect_identical(temporal$observation_id, obs$observation_id)
    reversed <- model
    reversed$observations <- model$observations[12:1, ]
    expect_error(
      .hm_validate_fit_split(reversed, task, temporal),
      class = "hm_integrity_error"
    )
  }
)

test_that(
  "image metadata maps exactly one source identity",
  {
    obs <- data.frame(
      observation_id = c("o1", "o2"),
      image_id = "image", acquisition_id = "a"
    )
    values <- array(
      as.numeric(1:12),
      c(2, 3, 2)
    )
    bands <- data.frame(wavelength = c(500, 600))
    geometry <- list(
      origin = c(0, 0),
      transform = diag(3)
    )
    expect_no_error(hm_dataset(values, obs, bands, geometry))
    obs$acquisition_id[2] <- "b"
    expect_error(
      hm_dataset(values, obs, bands, geometry),
      "one acquisition_id"
    )
    obs$acquisition_id <- "a"
    obs$image_id[2] <- "other"
    expect_error(
      hm_dataset(values, obs, bands, geometry),
      "one image_id"
    )
  }
)

test_that(
  "companion metadata carries exact geometry and respects explicit overrides",
  {
    skip_if_not_installed("hyperspectR")
    geometry <- list(
      origin = c(12, 34),
      transform = matrix(
        c(0, 2, 0, -3, 0, 0, 100, 200, 1),
        3, 3
      ),
      axes = c("y", "x"),
      units = "mm"
    )
    data <- hm_dataset(
      array(
        as.numeric(1:12),
        c(2, 3, 2)
      ),
      .hm_image_observations(), data.frame(wavelength = c(500, 600)),
      geometry
    )
    cube <- hm_as_hsi_cube(data, "image")
    expect_identical(cube$metadata$hypermodR$geometry, geometry)
    expect_identical(
      hm_as_dataset(cube)@geometry,
      geometry
    )
    override <- .hm_native_geometry()
    expect_identical(
      hm_as_dataset(cube, geometry = override)@geometry,
      override
    )
  }
)

test_that(
  "frame selection orders observations and values together",
  {
    fi <- data.frame(
      frame_id = c("f1", "f2"),
      sequence_id = "s", acquisition_id = "a", time = c(0, 1)
    )
    obs <- data.frame(
      observation_id = c("o1", "o2", "o3"),
      image_id = c("i1", "i2", "i1"),
      acquisition_id = "a", frame_id = c("f1", "f2", "f1")
    )
    data <- hm_sequence(
      list(
        array(1, c(2, 3, 2)),
        array(2, c(2, 3, 2))
      ),
      fi, obs, data.frame(wavelength = c(500, 600)),
      list(
        origin = c(0, 0),
        transform = diag(3)
      )
    )
    block <- hm_read_block(data, list(frame_id = c("f2", "f1")))
    expect_identical(block$observations$observation_id, c("o2", "o1", "o3"))
    expect_equal(
      as.numeric(block$values[1, 1, 1, ]),
      c(2, 1)
    )
    expect_identical(
      hm_read_block(data)$observations$observation_id,
      c("o1", "o3", "o2")
    )
  }
)
