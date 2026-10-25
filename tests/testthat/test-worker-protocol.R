test_that(
  "independent CPU worker reconstructs arrays and cancellation is owned",
  {
    skip_if_not_installed("processx")
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    skip_if(!nzchar(Sys.which("python3")))
    source <- test_path("..", "integration", "models", "toy_worker.py")
    cache <- tempfile()
    on.exit(unlink(cache, recursive = TRUE))
    p <- backend_test_profile(normalizePath(source))
    backend <- hm_install_backend(
      hm_backend_plan("test-identity", profile = p, cache = cache),
      TRUE, TRUE, TRUE
    )
    handle <- hm_start_backend(backend)
    on.exit(
      hm_stop_backend(handle),
      add = TRUE
    )
    expect_identical(
      hm_worker_request(handle)$status,
      "ok"
    )
    x <- array(
      seq_len(24) / 7,
      c(2, 3, 4)
    )
    original <- x
    answer <- hm_worker_request(handle, "extract", x, c(450, 500, 550, 600))
    expect_equal(answer$array, x, tolerance = 1e-14)
    expect_identical(x, original)
    expect_error(
      hm_worker_request(handle, "predict", x, 1:3),
      "channel"
    )
    expect_error(
      hm_worker_request(handle, "extract", x, 1:4, max_bytes = 128),
      "Invalid finite|request exceeds"
    )
    expect_error(
      hm_worker_request(handle, "predict", x, 1:4,
        timeout = 0.05,
        metadata = list(sleep = TRUE)
      ),
      class = "hm_timeout_error"
    )
    expect_false(hm_backend_status(handle)$alive)
    forged <- new.env()
    class(forged) <- "hm_backend_handle"
    forged$owner_pid <- -1L
    expect_error(
      hm_stop_backend(forged),
      "not owned"
    )
  }
)
test_that(
  "packaged reference training is reachable through R worker transport",
  {
    skip_if_not_installed("processx")
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    python <- Sys.getenv("HYPERMODR_TEST_TORCH_PYTHON")
    skip_if(
      !nzchar(python),
      "Explicit optional Torch interpreter not provided"
    )
    launch <- tempfile()
    cache <- tempfile()
    on.exit(
      unlink(
        c(launch, cache),
        recursive = TRUE
      )
    )
    launcher <- system.file("backends", "reference-launch.py",
      package = "hypermodR"
    )
    writeLines(
      c(
        paste0("#!", python),
        readLines(launcher)
      ),
      launch
    )
    p <- backend_test_profile(launch, model = "reference:cnn1d")
    base <- system.file("python", "hypermodr_worker", package = "hypermodR")
    files <- list.files(base, recursive = TRUE, full.names = TRUE)
    for (f in files[grepl("[.]py$", files)]) {
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
      hm_backend_plan("reference:cnn1d", profile = p, cache = cache),
      TRUE, TRUE, TRUE
    )
    h <- hm_start_backend(
      b,
      resources = hm_resources(memory_mb = 8192, timeout = 120)
    )
    on.exit(
      hm_stop_backend(h),
      add = TRUE
    )
    expect_identical(
      hm_worker_request(h)$status,
      "ok"
    )
    x <- matrix(
      seq_len(24) / 24,
      nrow = 6
    )
    ans <- hm_worker_request(
      h, "fit", x, 1:4,
      metadata = list(
        y = c(0L, 1L, 0L, 1L, 0L, 1L),
        roles = rep("train", 6),
        groups = paste0("subject", 1:6),
        epochs = 2L
      )
    )
    expect_identical(ans$status, "ok")
    expect_false(ans$receipt$pretrained)
    prediction <- hm_worker_request(h, "predict", x, 1:4)
    expect_equal(
      dim(prediction$array),
      c(6, 2)
    )
    expect_true(all(is.finite(prediction$array)))
    expect_error(
      hm_worker_request(h, "predict", x, c(450, 500, 550, 600)),
      class = "hm_worker_error"
    )
    expect_false(hm_backend_status(h)$alive)
  }
)
