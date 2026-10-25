test_that(
  "deep routes declare explicit supported tasks and blocked tensor routes",
  {
    expect_identical(
      .hm_deep_catalog()[["reference:cnn1d"]],
      "classification"
    )
    expect_identical(
      .hm_deep_catalog()[["reference:masked_autoencoder"]],
      "reconstruction"
    )
    expect_error(
      .hm_fit_deep(
        "reference:unet2d", matrix(1, 2, 4),
        NULL, list(type = "segmentation"),
        list(), list()
      ),
      class = "hm_capability_error"
    )
    expect_error(
      hm_extract_native("spectralgpt", NULL, NULL, NULL, list()),
      class = "hm_blocked_error"
    )
    expect_error(
      hm_extract_native(
        "htc", matrix(1, 2, 4),
        1:4, matrix(TRUE, 2, 2),
        list()
      ),
      class = "hm_capability_error"
    )
    expect_error(
      hm_extract_native(
        "htc", array(1, c(2, 2, 4)),
        1:4, matrix(TRUE, 2, 2),
        list()
      ),
      class = "hm_blocked_error"
    )
  }
)
test_that(
  "reference artifact integrity is independent of runtime handles",
  {
    path <- tempfile()
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE))
    for (f in c("model.json", "state.npz", "input.json")) {
      writeLines(
        f,
        file.path(path, f)
      )
    }
    a <- .hm_deep_artifact_hash(path)
    expect_length(a, 3)
    writeLines("changed", file.path(path, "state.npz"))
    expect_false(identical(a, .hm_deep_artifact_hash(path)))
  }
)
test_that(
  paste0(
    "real reference workflows persist and restore with ",
    "explicit runtime control"
  ),
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    python <- Sys.getenv("HYPERMODR_TEST_TORCH_PYTHON")
    skip_if(
      !nzchar(python),
      "Explicit optional Torch interpreter not provided"
    )
    root <- normalizePath(test_path("..", ".."))
    launcher <- file.path(root, "inst", "backends", "reference-launch.py")
    base <- file.path(root, "inst", "python", "hypermodr_worker")
    area <- tempfile()
    dir.create(area)
    on.exit(
      unlink(area, recursive = TRUE),
      add = TRUE
    )
    for (family in c("cnn1d", "spectral_vit", "masked_autoencoder")) {
      id <- paste0("reference:", family)
      launch <- file.path(area, paste0(family, ".py"))
      writeLines(
        c(
          paste0("#!", python),
          readLines(launcher)
        ),
        launch
      )
      p <- backend_test_profile(launch, model = id)
      for (f in list.files(base,
        pattern = "[.]py$", recursive = TRUE,
        full.names = TRUE
      )) {
        p$artifacts[[length(p$artifacts) +
                       1L]] <- list(
          path = paste0(
            "hypermodr_worker/", substring(
              f, nchar(base) +
                2L
            )
          ),
          url = paste0("file://", f),
          bytes = unname(file.info(f)$size),
          sha256 = digest::digest(file = f, algo = "sha256")
        )
      }
      b <- hm_install_backend(
        hm_backend_plan(id, profile = p, cache = file.path(area, paste0(
          family, "cache"
        ))),
        TRUE, TRUE, TRUE
      )
      h <- hm_start_backend(
        b,
        resources = hm_resources(memory_mb = 8192, timeout = 120)
      )
      x <- matrix(
        seq_len(40) / 40,
        nrow = 10
      )
      obs <- data.frame(
        observation_id = paste0("o", 1:10),
        subject_id = paste0("s", 1:10),
        label = factor(
          rep(
            c("z", "a"),
            5
          ),
          levels = c("z", "a")
        )
      )
      data <- list(
        values = x, observations = obs, band_map = data.frame(
          wavelength = c(450, 500, 550, 600),
          unit = "nm", native_index = 1:4, canonical_index = 1:4
        )
      )
      reconstruction <- family == "masked_autoencoder"
      task <- hm_task(
        if (reconstruction) {
          NULL
        } else {
          "label"
        }, if (reconstruction) {
          "reconstruction"
        } else {
          "classification"
        }, "spectrum", "subject_id"
      )
      wf <- hm_workflow(task, hm_recipe(), hm_model_spec(id, task, list(
        epochs = 1L
      )))
      fit <- hm_fit(
        wf, data, list(train = 1:8, test = 9:10),
        control = list(
          backend_handle = h, artifact_path = file.path(area, paste0(
            family, "model"
          ))
        )
      )
      expect_identical(
        fit$engine$training_hash, .hm_hash(
          list(x[1:8, , drop = FALSE], if (
                                           reconstruction) {
            NULL
          } else {
            obs$label[1:8]
          })
        )
      )
      pred <- hm_predict(fit, data, control = list(backend_handle = h))
      hm_stop_backend(h)
      expect_false(any(vapply(fit$engine, is.environment, logical(1))))
      restored <- unserialize(serialize(fit, NULL))
      h <- hm_start_backend(
        b,
        resources = hm_resources(memory_mb = 8192, timeout = 120)
      )
      pred2 <- hm_predict(restored, data, control = list(backend_handle = h))
      expect_equal(pred2, pred)
      if (!reconstruction) {
        expect_identical(
          colnames(pred$probabilities),
          levels(obs$label)
        )
        expect_equal(
          rowSums(pred$probabilities),
          rep(1, 10)
        )
      }
      writeLines("tampered", file.path(
        fit$engine$artifact_path,
        "input.json"
      ))
      expect_error(
        hm_predict(fit, data, control = list(backend_handle = h)),
        class = "hm_integrity_error"
      )
      hm_stop_backend(h)
    }
  }
)
