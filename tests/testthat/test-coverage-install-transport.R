test_that(
  paste0(
    "download transport stages only verified bytes and ",
    "preserves corrupt prior runtime"
  ),
  {
    source <- tempfile("install-source-")
    cache <- tempfile("install-cache-")
    writeLines("verified synthetic artifact", source)
    on.exit(
      unlink(
        c(source, cache),
        recursive = TRUE
      ),
      add = TRUE
    )
    profile <- backend_test_profile(source)
    profile$artifacts[[1]]$url <- "https://example.invalid/exact-artifact"
    plan <- hm_backend_plan(
      "test-identity", profile = profile,
      cache = cache
    )
    expect_error(
      hm_install_backend(plan, TRUE),
      "custom-code acknowledgement"
    )
    requests <- character()
    testthat::local_mocked_bindings(
      download.file = function(url, destfile, ...) {
        requests <<- c(requests, url)
        file.copy(source, destfile)
        0L
      }, .package = "utils"
    )
    backend <- hm_install_backend(plan, TRUE, acknowledge_custom_code = TRUE)
    expect_identical(
      requests, "https://example.invalid/exact-artifact"
    )
    expect_identical(
      readLines(file.path(backend$path, "worker")),
      "verified synthetic artifact"
    )
    writeLines(
      "corrupted installed bytes", file.path(backend$path, "worker")
    )
    expect_error(
      hm_install_backend(plan, TRUE, acknowledge_custom_code = TRUE),
      "Existing runtime failed integrity"
    )
    expect_identical(
      readLines(file.path(backend$path, "worker")),
      "corrupted installed bytes"
    )
  }
)

test_that(
  paste0(
    "cached exact artifact installs offline and failed ",
    "transfer cannot activate"
  ),
  {
    source <- tempfile("cached-source-")
    cache <- tempfile("artifact-cache-")
    writeLines("cached fixture bytes", source)
    on.exit(
      unlink(
        c(source, cache),
        recursive = TRUE
      ),
      add = TRUE
    )
    profile <- backend_test_profile(source)
    profile$artifacts[[1]]$url <- "https://example.invalid/exact-artifact"
    dir.create(
      file.path(cache, "artifacts"),
      recursive = TRUE
    )
    file.copy(
      source, file.path(
        cache, "artifacts", profile$artifacts[[1]]$sha256
      )
    )
    plan <- hm_backend_plan(
      "test-identity", profile = profile,
      cache = cache
    )
    backend <- hm_install_backend(
      plan, TRUE, offline = TRUE, acknowledge_custom_code = TRUE
    )
    expect_identical(
      readLines(file.path(backend$path, "worker")),
      "cached fixture bytes"
    )
    failed_cache <- tempfile("failed-transfer-")
    on.exit(
      unlink(failed_cache, recursive = TRUE),
      add = TRUE
    )
    failed_plan <- hm_backend_plan(
      "test-identity", profile = profile,
      cache = failed_cache
    )
    testthat::local_mocked_bindings(
      download.file = function(...) 1L,
      .package = "utils"
    )
    expect_error(
      hm_install_backend(
        failed_plan, TRUE, acknowledge_custom_code = TRUE
      ),
      class = "hm_download_error"
    )
    expect_false(
      dir.exists(file.path(failed_cache, failed_plan$digest))
    )
    expect_false(
      dir.exists(
        paste0(
          file.path(failed_cache, failed_plan$digest),
          ".lock"
        )
      )
    )
    expect_length(
      list.files(
        failed_cache, all.files = TRUE,
        no.. = TRUE
      ),
      0
    )
  }
)
