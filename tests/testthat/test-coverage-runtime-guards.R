test_that(
  "backend plans refuse unknown, mismatched or incomplete profiles",
  {
    worker <- tempfile("plan-guard-")
    writeLines("synthetic worker fixture", worker)
    on.exit(unlink(worker), add = TRUE)
    expect_error(hm_backend_plan("not-a-model"), "Unknown model identity")
    expect_error(
      hm_backend_plan(
        "test-identity",
        profile = backend_test_profile(worker, "other-identity")
      ),
      "identity mismatch"
    )
    incomplete <- backend_test_profile(worker)
    incomplete$terms <- NULL
    expect_error(
      hm_backend_plan("test-identity", profile = incomplete),
      "required exact manifest fields"
    )
    gpu <- backend_test_profile(worker)
    gpu$device <- "cuda"
    expect_error(
      hm_backend_plan("test-identity", profile = gpu),
      class = "hm_platform_error"
    )
    expect_error(.hm_validate_plan(list()), "backend plan is required")
  }
)

test_that(
  "runtime start refuses mismatched, moved or modified installations",
  {
    skip_if_not_installed("processx")
    parent <- tempfile("start-guard-")
    dir.create(parent)
    on.exit(unlink(parent, recursive = TRUE), add = TRUE)
    worker <- file.path(parent, "worker")
    writeLines("synthetic worker fixture", worker)
    backend <- hm_install_backend(
      hm_backend_plan(
        "test-identity",
        profile = backend_test_profile(worker),
        cache = file.path(parent, "cache")
      ),
      TRUE, TRUE, TRUE
    )
    expect_error(
      hm_start_backend(backend, model = "other-identity"),
      "model mismatch"
    )
    expect_error(hm_start_backend(backend, host = "0.0.0.0"), "Loopback")
    moved <- backend
    moved$path <- file.path(parent, "elsewhere")
    expect_error(hm_start_backend(moved), "differs from declared")
    writeLines("modified worker", file.path(backend$path, "worker"))
    expect_error(hm_start_backend(backend), class = "hm_integrity_error")
  }
)

test_that(
  "installation reports a missing local artifact before activation",
  {
    parent <- tempfile("missing-artifact-")
    dir.create(parent)
    on.exit(unlink(parent, recursive = TRUE), add = TRUE)
    worker <- file.path(parent, "worker")
    writeLines("synthetic worker fixture", worker)
    profile <- backend_test_profile(worker)
    profile$artifacts[[1]]$url <- paste0(
      "file://", file.path(parent, "missing")
    )
    plan <- hm_backend_plan(
      "test-identity", profile = profile, cache = file.path(parent, "cache")
    )
    expect_error(
      suppressWarnings(hm_install_backend(plan, TRUE, TRUE, TRUE)),
      "Local artifact is unavailable"
    )
    expect_false(dir.exists(file.path(plan$cache, plan$digest)))
  }
)

test_that(
  "the spectral reference route refuses image and sequence tasks",
  {
    for (unit in c("image", "sequence")) {
      expect_error(
        .hm_fit_deep(
          "reference:cnn1d", matrix(0, 2, 3), c("a", "b"),
          list(unit = unit, type = "classification"), list(), list()
        ),
        class = "hm_capability_error"
      )
    }
  }
)

test_that(
  "checkpoint staging requires a new directory before any request",
  {
    skip_if_not_installed("httr2")
    existing <- tempfile("checkpoint-staging-")
    dir.create(existing)
    on.exit(unlink(existing, recursive = TRUE), add = TRUE)
    expect_error(
      hm_resolve_checkpoint(
        "fixture/model", strrep("a", 40), directory = existing
      ),
      "new and exclusive"
    )
  }
)
