test_that(
  "symlink checks cover only components below the trusted root",
  {
    root <- tempfile("trusted-root-")
    dir.create(file.path(root, "cache", "runtime"), recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    expect_true(.hm_no_symlink(root, root))
    expect_true(.hm_no_symlink(file.path(root, "cache", "runtime"), root))
    expect_true(.hm_no_symlink(file.path(root, "cache", "new", "file"), root))
    expect_true(
      .hm_no_symlink(file.path(normalizePath(root), "cache"), root)
    )
    expect_error(
      .hm_no_symlink(tempfile("elsewhere-"), root),
      class = "hm_integrity_error"
    )
    expect_error(
      .hm_no_symlink(file.path(root, "cache", "..", "..", "x"), root),
      class = "hm_integrity_error"
    )
    withr::local_dir(root)
    expect_true(.hm_no_symlink(file.path("cache", "runtime"), "cache"))
    expect_true(.hm_no_symlink("cache", "."))
  }
)

test_that(
  "a symlink below the trusted root is refused",
  {
    skip_on_os("windows")
    root <- tempfile("trusted-root-")
    outside <- tempfile("outside-")
    dir.create(root)
    dir.create(outside)
    on.exit(unlink(c(root, outside), recursive = TRUE), add = TRUE)
    expect_true(file.symlink(outside, file.path(root, "linked")))
    expect_error(
      .hm_no_symlink(file.path(root, "linked"), root),
      class = "hm_integrity_error"
    )
    expect_error(
      .hm_no_symlink(file.path(root, "linked", "artifact"), root),
      class = "hm_integrity_error"
    )
    expect_true(
      file.symlink(file.path(outside, "missing"), file.path(root, "dangling"))
    )
    expect_error(
      .hm_no_symlink(file.path(root, "dangling"), root),
      class = "hm_integrity_error"
    )
  }
)

test_that(
  "a symlinked prefix above the trusted root is accepted",
  {
    skip_on_os("windows")
    real <- tempfile("real-prefix-")
    alias <- tempfile("linked-prefix-")
    dir.create(file.path(real, "root"), recursive = TRUE)
    on.exit(unlink(c(alias, real), recursive = TRUE), add = TRUE)
    expect_true(file.symlink(real, alias))
    root <- file.path(alias, "root")
    expect_true(.hm_no_symlink(file.path(root, "cache", "artifact"), root))
    expect_true(file.symlink(real, file.path(root, "loop")))
    expect_error(
      .hm_no_symlink(file.path(root, "loop", "artifact"), root),
      class = "hm_integrity_error"
    )

    source <- file.path(real, "worker")
    writeLines("synthetic worker fixture", source)
    plan <- hm_backend_plan(
      "test-identity",
      profile = backend_test_profile(file.path(alias, "worker")),
      cache = file.path(alias, "cache")
    )
    backend <- hm_install_backend(plan, TRUE, TRUE, TRUE)
    expect_true(file.exists(file.path(backend$path, "receipt.json")))

    linked <- hm_backend_plan(
      "test-identity",
      profile = backend_test_profile(source),
      cache = file.path(alias, "swapped")
    )
    dir.create(file.path(real, "elsewhere"))
    expect_true(file.symlink(file.path(real, "elsewhere"), linked$cache))
    expect_error(
      hm_install_backend(linked, TRUE, TRUE, TRUE),
      class = "hm_integrity_error"
    )
  }
)

test_that(
  "runtime start and checkpoint staging refuse relinked directories",
  {
    skip_on_os("windows")
    skip_if_not_installed("processx")
    parent <- tempfile("relinked-runtime-")
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
    # Same bytes behind a link: only the symlink check can refuse this.
    runtime <- file.path(backend$path, "worker")
    unlink(runtime)
    expect_true(file.symlink(worker, runtime))
    expect_error(hm_start_backend(backend), class = "hm_integrity_error")

    skip_if_not_installed("httr2")
    dir.create(file.path(parent, "outside"))
    staging <- file.path(parent, "checkpoint")
    expect_true(file.symlink(file.path(parent, "outside"), staging))
    expect_error(
      hm_resolve_checkpoint(
        "fixture/model", strrep("a", 40), directory = staging
      ),
      class = "hm_integrity_error"
    )
  }
)
