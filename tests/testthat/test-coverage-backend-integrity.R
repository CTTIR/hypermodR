test_that("runtime manifest schema denies ambiguous or unsafe artifacts", {
  source <- tempfile("runtime-source-")
  writeLines("fixture runtime bytes", source)
  on.exit(unlink(source), add = TRUE)
  original <- backend_test_profile(source)
  expect_identical(.hm_validate_profile(original), original)
  invalid <- function(change, pattern) {
    profile <- change(original)
    expect_error(.hm_validate_profile(profile), pattern)
  }
  invalid(function(p) {
    p$runtime_version <- "latest"
    p
  }, "Moving revisions")
  invalid(function(p) {
    p$artifacts[[1]]$path <- "../worker"
    p
  }, "artifact pin")
  invalid(function(p) {
    p$artifacts[[1]]$bytes <- -1
    p
  }, "artifact pin")
  invalid(function(p) {
    p$artifacts[[1]]$url <- "http://example.invalid/worker"
    p
  }, "artifact pin")
  invalid(function(p) {
    p$artifacts[[2]] <- p$artifacts[[1]]
    p
  }, "overlap")
  invalid(function(p) {
    p$artifacts[[1]]$path <- "environment/bin/python"
    p
  }, "reserved")
  invalid(function(p) {
    p$artifacts[[2]] <- p$artifacts[[1]]
    p$artifacts[[2]]$path <- "worker/subfile"
    p
  }, "overlap")
  invalid(function(p) {
    p$command <- "missing"
    p
  }, "Command")
  invalid(function(p) {
    p$custom_code <- NA
    p
  }, "explicit")
  invalid(function(p) {
    p$custom_code_reviewed <- FALSE
    p
  }, "hash-bound")
  invalid(function(p) {
    p$terms[[1]]$text <- "changed"
    p
  }, "Terms")
  invalid(function(p) {
    p$artifacts <- list()
    p
  }, "Artifacts")
})

test_that(paste0(
  "backend availability rehashes installed artifacts ",
  "while parity stays reported"
), {
  source <- tempfile("qualification-source-")
  cache <- tempfile("qualification-cache-")
  writeLines("fixture runtime bytes", source)
  on.exit(unlink(c(source, cache), recursive = TRUE), add = TRUE)
  profile <- backend_test_profile(source)
  plan <- hm_backend_plan("test-identity", profile = profile, cache = cache)
  backend <- hm_install_backend(plan, accept_terms = TRUE, offline = TRUE,
                                acknowledge_custom_code = TRUE)
  report <- hm_check_backend("test-identity", backend = backend,
                             parity = list(plan_digest = plan$digest,
                                           passed = TRUE))
  expect_identical(report$availability, "pass")
  expect_identical(report$parity, "reported_pass")
  expect_false(report$scientific_qualification)
  expect_identical(hm_check_backend("test-identity", backend = backend,
                                    parity = list(plan_digest = "wrong",
                                                  passed = TRUE))$parity,
                   "not_established")
  writeLines("different bytes", file.path(backend$path, "worker"))
  corrupted <- hm_check_backend("test-identity", backend = backend,
                                parity = list(plan_digest = plan$digest,
                                              passed = TRUE))
  expect_identical(corrupted$availability, "unavailable")
  expect_identical(corrupted$parity, "not_established")
})

test_that(paste0(
  "provenance receipts reject missing inputs and altered ",
  "source identity"
), {
  source <- tempfile("provenance-")
  writeLines("original", source)
  on.exit(unlink(source), add = TRUE)
  objects <- list(measurement = c(3, 7))
  receipt <- hm_provenance(objects, source)
  expect_true(hm_verify_provenance(receipt, objects))
  expect_identical(unname(receipt$files), digest::digest(file = source,
                                                         algo = "sha256"))
  expect_error(hm_provenance(list(1)), "require names")
  expect_error(hm_provenance(files = paste0(source, "missing")),
               "Invalid provenance")
  expect_error(hm_verify_provenance(list()), "Invalid provenance receipt")
  expect_error(hm_verify_provenance(receipt, list(measurement = c(3, 8))),
               class = "hm_integrity_error")
  writeLines("altered", source)
  expect_error(hm_verify_provenance(receipt, objects),
               class = "hm_integrity_error")
})
