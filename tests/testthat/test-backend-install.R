test_that(
  "explicit install is reusable and failed update retains prior runtime",
  {
    source <- tempfile()
    writeLines("#!/bin/sh\nexit 0", source)
    cache <- tempfile()
    on.exit(
      unlink(
        c(source, cache),
        recursive = TRUE
      )
    )
    p <- backend_test_profile(source)
    plan <- hm_backend_plan("test-identity", profile = p, cache = cache)
    expect_error(
      hm_install_backend(plan),
      class = "hm_terms_error"
    )
    b <- hm_install_backend(plan,
      accept_terms = TRUE, offline = TRUE,
      acknowledge_custom_code = TRUE
    )
    expect_true(file.exists(file.path(b$path, "receipt.json")))
    expect_identical(
      hm_install_backend(plan, TRUE, TRUE, TRUE)$path,
      b$path
    )
    p$artifacts[[1]]$sha256 <- paste(
      rep("a", 64),
      collapse = ""
    )
    bad <- hm_backend_plan("test-identity", profile = p, cache = cache)
    expect_error(
      hm_install_backend(bad, TRUE, TRUE, TRUE),
      class = "hm_integrity_error"
    )
    expect_true(file.exists(file.path(b$path, "worker")))
    expect_false(dir.exists(file.path(cache, bad$digest)))
    changed <- plan
    changed$model_id <- "mutated"
    expect_error(
      hm_install_backend(changed, TRUE, acknowledge_custom_code = TRUE),
      class = "hm_integrity_error"
    )
    dir.create(
      paste0(
        file.path(cache, plan$digest),
        ".lock"
      )
    )
    expect_error(
      hm_install_backend(plan, TRUE, acknowledge_custom_code = TRUE),
      class = "hm_install_locked"
    )
  }
)
test_that(
  "offline miss and symlink source are rejected before activation",
  {
    source <- tempfile()
    writeLines("test", source)
    cache <- tempfile()
    link <- tempfile()
    on.exit(
      unlink(
        c(source, cache, link),
        recursive = TRUE
      )
    )
    p <- backend_test_profile(source)
    p$artifacts[[1]]$url <- "https://example.invalid/pinned"
    plan <- hm_backend_plan("test-identity", profile = p, cache = cache)
    expect_error(
      hm_install_backend(plan, TRUE, TRUE, TRUE),
      class = "hm_offline_error"
    )
    skip_on_os("windows")
    file.symlink(source, link)
    p <- backend_test_profile(source)
    p$artifacts[[1]]$url <- paste0("file://", link)
    expect_error(
      hm_install_backend(
        hm_backend_plan("test-identity", profile = p, cache = cache),
        TRUE,
        acknowledge_custom_code = TRUE
      ),
      class = "hm_integrity_error"
    )
  }
)
test_that(
  "minimal isolated CPU Python environment installs and reuses offline",
  {
    skip_if_not_installed("processx")
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    python <- unname(Sys.which("python3"))
    skip_if(!nzchar(python))
    source <- tempfile()
    writeLines("print('cpu-environment-ok')", source)
    lock <- tempfile()
    writeLines(
      "# No third party packages needed for this protocol fixture", lock
    )
    cache <- tempfile()
    on.exit(
      unlink(
        c(source, lock, cache),
        recursive = TRUE
      )
    )
    version <- trimws(
      processx::run(python, c(
        "-c",
        "import platform; print(platform.python_version())"
      ))$stdout
    )
    p <- backend_test_profile(source)
    p$artifacts[[2]] <- list(
      path = "requirements.txt", url = paste0("file://", lock),
      bytes = unname(file.info(lock)$size),
      sha256 = digest::digest(file = lock, algo = "sha256")
    )
    p$python <- list(
      executable = python, version = version,
      lock = "requirements.txt"
    )
    plan <- hm_backend_plan("test-identity", profile = p, cache = cache)
    b <- hm_install_backend(plan, TRUE, TRUE, TRUE)
    expect_true(file.exists(file.path(
      b$path, "environment", "bin",
      "python"
    )))
    handle <- hm_start_backend(b)
    handle$process$wait(3000)
    expect_equal(handle$process$get_exit_status(), 0L)
    expect_match(handle$process$read_output(), "cpu-environment-ok")
    expect_identical(
      hm_install_backend(plan, TRUE, TRUE, TRUE)$path,
      b$path
    )
  }
)
