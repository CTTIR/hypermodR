test_that("fresh installed namespace loads packaged resources offline", {
  library <- Sys.getenv("HYPERMODR_TEST_LIBRARY")
  paths <- unique(c(if (nzchar(library)) library, .libPaths()))
  package_files <- file.path(paths, "hypermodR", "Meta", "package.rds")
  skip_if(!any(file.exists(package_files)), "Installed library required")
  result <- callr::r(function() {
    trace(
      "url",
      where = baseenv(),
      tracer = quote({
        cli::cli_abort("Network URL access forbidden in offline load test")
      }),
      print = FALSE
    )
    trace(
      "download.file",
      where = asNamespace("utils"),
      tracer = quote({
        cli::cli_abort("Download forbidden in offline load test")
      }),
      print = FALSE
    )
    suppressPackageStartupMessages(library(hypermodR))
    registry_path <- system.file(
      "models", "registry.json", package = "hypermodR", mustWork = TRUE
    )
    registry <- jsonlite::read_json(registry_path)
    backend_resource <- system.file(
      "backends", "reference-launch.py", package = "hypermodR"
    )
    worker_resource <- system.file(
      "python", "hypermodr_worker", "__main__.py", package = "hypermodR"
    )
    qualified <- vapply(hm_models(), function(x) {
      isTRUE(x$scientific_qualification)
    }, logical(1))
    list(
      schema = registry$schema_version,
      release = registry$release_status,
      resources = file.exists(backend_resource, worker_resource),
      qualified = qualified,
      core_count = length(hm_models(backend = "r_core"))
    )
  }, libpath = paths,
  env = c(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1"))
  expect_equal(result$schema, 1)
  expect_identical(result$release, "unsigned_development")
  expect_true(all(result$resources))
  expect_false(any(result$qualified))
  expect_gt(result$core_count, 5)
})
