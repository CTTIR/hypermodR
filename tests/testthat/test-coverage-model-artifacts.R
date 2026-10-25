test_that(
  "artifact identity detects corruption, missing files and symlinks",
  {
    directory <- tempfile("model-artifact-")
    dir.create(directory)
    on.exit(
      unlink(directory, recursive = TRUE),
      add = TRUE
    )
    files <- c("model.json", "state.npz", "input.json")
    for (f in files) {
      writeLines(
        paste("fixture", f),
        file.path(directory, f)
      )
    }
    original <- .hm_deep_artifact_hash(directory)
    expect_identical(
      names(original),
      files
    )
    expect_identical(
      unname(original[1]),
      digest::digest(
        file = file.path(directory, files[1]),
        algo = "sha256"
      )
    )
    writeLines("corrupt model", file.path(directory, files[1]))
    expect_false(identical(original, .hm_deep_artifact_hash(directory)))
    unlink(file.path(directory, files[2]))
    expect_error(
      .hm_deep_artifact_hash(directory),
      class = "hm_integrity_error"
    )
    if (.Platform$OS.type == "unix") {
      expect_true(
        file.symlink(
          file.path(directory, files[1]),
          file.path(directory, files[2])
        )
      )
      expect_error(
        .hm_deep_artifact_hash(directory),
        class = "hm_integrity_error"
      )
    }
  }
)

test_that(
  "native routes refuse invalid scientific inputs before acquiring runtime",
  {
    cube <- array(
      seq_len(24),
      c(2, 3, 4)
    )
    mask <- matrix(TRUE, 2, 3)
    wave <- c(450, 500, 600, 750)
    admission <- list(
      execute_source_acknowledged = TRUE, code_terms_admitted = TRUE,
      weight_terms_admitted = TRUE
    )
    expect_error(
      hm_extract_native("spectralgpt", cube, wave, mask, admission),
      class = "hm_blocked_error"
    )
    expect_error(
      hm_extract_native(
        "htc", cube, rev(wave),
        mask, admission
      ),
      class = "hm_capability_error"
    )
    expect_error(
      hm_extract_native(
        "htc", cube, wave, t(mask),
        admission
      ),
      class = "hm_capability_error"
    )
    expect_error(
      hm_extract_native("htc", cube, wave, mask & FALSE, admission),
      class = "hm_capability_error"
    )
    expect_error(
      hm_extract_native("htc", cube, wave, mask, list()),
      class = "hm_blocked_error"
    )
    bad <- cube
    bad[1] <- Inf
    expect_error(
      hm_extract_native("htc", bad, wave, mask, admission),
      class = "hm_capability_error"
    )
    expect_error(
      hm_extract_native("htc", cube, wave, mask, admission),
      class = "hm_validation_error"
    )
  }
)

test_that(
  paste0(
    "reference spectral model contracts reject unsupported ",
    "routes and parameters"
  ),
  {
    x <- matrix(1:12, 3, 4)
    classification <- hm_task("y", "classification", "spectrum", "subject_id")
    regression <- hm_task("y", "regression", "spectrum", "subject_id")
    fit <- function(id = "reference:cnn1d", task = classification,
                    params = list(),
                    control = list(), input = x) {
      .hm_fit_deep(
        id, input, c("a", "b", "a"),
        task, params, control
      )
    }
    expect_error(
      fit("reference:unet2d"),
      class = "hm_capability_error"
    )
    expect_error(
      fit(task = regression),
      class = "hm_capability_error"
    )
    expect_error(
      fit(control = list(weights = rep(1, 3))),
      class = "hm_capability_error"
    )
    expect_error(
      fit(input = matrix(Inf, 3, 4)),
      class = "hm_validation_error"
    )
    expect_error(
      fit(params = list(unknown = 1)),
      class = "hm_validation_error"
    )
    for (epochs in list(0, 101, 1.5, Inf)) {
      expect_error(
        fit(params = list(epochs = epochs)),
        class = "hm_validation_error"
      )
    }
    expect_error(
      fit(params = list(domain = c(0, 2, 1))),
      class = "hm_validation_error"
    )
    expect_error(fit(), class = "hm_validation_error")
  }
)

test_that(
  "named candidates declare intent without upstream execution claims",
  {
    task <- hm_task("y", "classification", "spectrum", "subject_id")
    constructors <- list(
      htc = hm_htc, hyperfree = hm_hyperfree, hypersigma = hm_hypersigma,
      hypervision = hm_hypervision, spectralearth = hm_spectralearth,
      spectralgpt = hm_spectralgpt, dual_stream_mhsi = hm_dual_stream_mhsi
    )
    for (id in names(constructors)) {
      specification <- constructors[[id]](task, list(
        source = "declared fixture"
      ))
      expect_identical(specification$model_id, id)
      expect_identical(specification$task, task)
      expect_identical(specification$parameters$source, "declared fixture")
    }
    temporal <- hm_temporal_model("gru", task, list(
      causal = FALSE,
      pretrained = TRUE
    ))
    expect_identical(temporal$model_id, "temporal_reference:gru")
    expect_true(temporal$parameters$causal)
    expect_false(temporal$parameters$pretrained)
    expect_error(hm_temporal_model("unsupported", task))
  }
)

test_that(
  "physical kernels preserve an independent inverse and detect mutation",
  {
    forward <- function(concentration, parameters) {
      concentration *
        parameters$scale
    }
    inverse <- function(spectrum, parameters) {
      rowMeans(
        spectrum
      ) / parameters$scale
    }
    kernel <- hm_physics_kernel(
      forward, inverse, list(scale = 2),
      "synthetic linear law", c(input = "intensity", output = "concentration")
    )
    spectra <- rbind(
      c(2, 6),
      c(8, 12),
      c(4, 4)
    )
    expect_equal(
      .hm_kernel_predict(kernel, spectra),
      c(2, 5, 2)
    )
    changed <- kernel
    changed$parameters$scale <- 3
    expect_error(
      .hm_kernel_predict(changed, spectra),
      class = "hm_integrity_error"
    )
    expect_error(
      hm_physics_kernel(1, inverse, list(), "fixture", c(
        input = "x",
        output = "y"
      )),
      class = "hm_validation_error"
    )
    invalid <- hm_physics_kernel(
      forward, function(x, p) rep(Inf, nrow(x)),
      list(), "fixture", c(input = "x", output = "y")
    )
    expect_error(
      .hm_kernel_predict(invalid, spectra),
      class = "hm_backend_output_error"
    )
    engine <- list(model_id = "physics_inverse", parameters = list(
      kernel = kernel
    ))
    fitted <- .hm_fit_physics(
      engine, spectra, c(2, 5, 2),
      list()
    )
    expect_equal(
      .hm_predict_physics(fitted, spectra),
      c(2, 5, 2)
    )
    recursive <- list(
      model_id = "physics_hybrid", parameters = list(
        kernel = kernel,
        residual_model = "physics_hybrid"
      )
    )
    expect_error(
      .hm_fit_physics(
        recursive, spectra, c(2, 5, 2),
        list()
      ),
      "Recursive"
    )
  }
)
