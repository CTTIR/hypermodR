test_that("availability and start share installation integrity admission", {
  skip_on_cran()
  skip_if_not_installed("processx")
  root <- tempfile("availability-review-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  source <- file.path(root, "source")
  writeLines("synthetic runtime", source)
  plan <- hm_backend_plan(
    "test-identity", profile = backend_test_profile(source),
    cache = file.path(root, "cache")
  )
  backend <- hm_install_backend(plan, TRUE, TRUE, TRUE)
  parity <- list(plan_digest = plan$digest, passed = TRUE)
  report <- function(x) {
    hm_check_backend("test-identity", backend = x, parity = parity)
  }
  expect_identical(report(backend)$availability, "pass")
  expect_identical(report(backend)$parity, "reported_pass")
  moved <- backend
  moved$path <- file.path(root, "elsewhere")
  dir.create(moved$path)
  file.copy(source, file.path(moved$path, "worker"))
  expect_identical(report(moved)$availability, "unavailable")
  expect_identical(report(moved)$parity, "not_established")
  expect_error(hm_start_backend(moved), class = "hm_integrity_error")
  skip_on_os("windows")
  artifact <- file.path(backend$path, "worker")
  unlink(artifact)
  expect_true(file.symlink(source, artifact))
  expect_identical(report(backend)$availability, "unavailable")
  expect_identical(report(backend)$parity, "not_established")
  expect_error(hm_start_backend(backend), class = "hm_integrity_error")
})

test_that("Python environment root cannot be replaced with a matching link", {
  skip_on_os("windows")
  root <- tempfile("environment-review-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  path <- file.path(root, "runtime")
  dir.create(file.path(path, "environment"), recursive = TRUE)
  writeLines("synthetic environment bytes", file.path(path, "environment", "x"))
  manifest <- .hm_environment_inventory(file.path(path, "environment"))
  jsonlite::write_json(list(environment_manifest = manifest),
                       file.path(path, "receipt.json"), auto_unbox = TRUE)
  expect_true(.hm_environment_ok(path))
  outside <- file.path(root, "moved-environment")
  expect_true(file.rename(file.path(path, "environment"), outside))
  expect_true(file.symlink(outside, file.path(path, "environment")))
  expect_error(.hm_environment_ok(path), class = "hm_integrity_error")
})

test_that("worker array schema failures stop an actual owned process", {
  skip_on_cran()
  skip_if_not_installed("processx")
  python <- unname(Sys.which("python3"))
  skip_if(!nzchar(python))
  root <- tempfile("worker-schema-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  script <- file.path(root, "worker.py")
  writeLines(c(
    "import sys,json", "mode=sys.argv[1]", "for line in sys.stdin:",
    "    req=json.loads(line)",
    paste0("    ans={k:req[k] for k in ",
           "('protocol','id','model','plan_digest')}"),
    paste0("    ans.update(status='ok',operations=",
           "['fit','predict','extract','cancel'])"),
    paste0("    ans['array']={'dtype':'float64','order':'F',",
           "'shape':[1,2],'values':[3,7],'channels':['a','b']}"),
    "    if mode=='shape': ans['array']['shape']=['one',2]",
    "    if mode=='scalar': ans['array']=7",
    "    if mode=='channels': ans['array']['channels']=['a','a']",
    "    if mode=='null-channel': ans['array']['channels']=['a',None]",
    "    if mode=='nested': ans['array']['values']=[[3],7]",
    "    if mode=='root': ans=7",
    "    print(json.dumps(ans),flush=True)"
  ), script)
  for (mode in c("shape", "scalar", "channels", "null-channel", "nested",
                 "root", "valid")) {
    process <- processx::process$new(
      python, c(script, mode), stdin = "|", stdout = "|", stderr = "|",
      cleanup = TRUE, cleanup_tree = TRUE
    )
    handle <- new.env(parent = emptyenv())
    handle$process <- process
    handle$pid <- process$get_pid()
    handle$owner_pid <- Sys.getpid()
    handle$model <- "fixture"
    handle$plan_digest <- "fixture-plan"
    class(handle) <- "hm_backend_handle"
    if (mode == "valid") {
      expect_equal(hm_worker_request(handle)$array, matrix(c(3, 7), nrow = 1))
      expect_true(process$is_alive())
      hm_stop_backend(handle)
    } else {
      expect_error(hm_worker_request(handle), class = "hm_worker_error")
      expect_false(process$is_alive())
    }
  }
})

test_that("retired generated profiles cannot be planned, installed or reused", {
  source <- tempfile("retired-profile-")
  writeLines("synthetic artifact retained during admission failure", source)
  root <- tempfile("retired-cache-")
  dir.create(root)
  on.exit(unlink(c(source, root), recursive = TRUE), add = TRUE)
  original <- backend_test_profile(source)
  current <- hm_backend_plan("test-identity", profile = original, cache = root)
  for (profile in list(
    c("reference_cpu-3.11.9", "torch-2.6.0-numpy-2.2.6"),
    c("vllm_linux_cuda-3.11.9", "vllm-0.11.0")
  )) {
    retired <- original
    retired$profile_id <- profile[1L]
    retired$runtime_version <- profile[2L]
    expect_error(hm_backend_plan("test-identity", profile = retired),
                 class = "hm_backend_security_error")
    # Reconstruct a previously serialized, hash-valid plan. Current admission
    # must still deny it rather than trusting an earlier decision forever.
    saved <- unclass(current)
    saved$profile <- retired
    saved$digest <- NULL
    saved$digest <- .hm_hash(saved)
    class(saved) <- "hm_backend_plan"
    path <- file.path(root, saved$digest)
    dir.create(path)
    sentinel <- file.path(path, "existing-user-file")
    writeLines("preserve", sentinel)
    expect_error(hm_install_backend(saved, TRUE, TRUE, TRUE),
                 class = "hm_backend_security_error")
    backend <- structure(list(path = path, plan = saved), class = "hm_backend")
    if (requireNamespace("processx", quietly = TRUE)) {
      expect_error(hm_start_backend(backend),
                   class = "hm_backend_security_error")
    }
    expect_error(hm_check_backend("test-identity", backend = backend),
                 class = "hm_backend_security_error")
    expect_identical(readLines(sentinel), "preserve")
  }
  expect_identical(.hm_runtime_security(original), TRUE)
  expect_error(hm_resolve_backend(
    "vllm_linux_cuda", "fixture", "missing-python", "missing-uv",
    directory = file.path(root, "must-not-be-created")
  ), class = "hm_backend_security_error")
  expect_false(dir.exists(file.path(root, "must-not-be-created")))
})
