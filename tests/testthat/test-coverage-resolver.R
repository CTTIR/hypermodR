test_that(paste0(
  "checkpoint metadata resolution binds complete exact ",
  "artifacts without weights download"
), {
  state <- new.env(parent = emptyenv())
  state$tree <- list(list(type = "file", path = "config.json"), list(
    type = "file", path = "LICENSE"
  ), list(
    type = "file",
    path = "weights.safetensors", lfs = list(oid = paste0(
      "sha256:",
      strrep("a", 64)
    ), size = 128)
  ))
  state$config <- list(architectures = list("Qwen2ForCausalLM"))
  state$pagination <- NULL
  state$requests <- character()
  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_timeout = function(req, seconds) req,
    req_options = function(req, ...) req,
    req_perform = function(req) {
      state$requests <- c(
        state$requests,
        req$url
      )
      req
    },
    resp_body_raw = function(resp) {
      if (grepl("/tree/", resp$url, fixed = TRUE)) {
        return(charToRaw(
          jsonlite::toJSON(state$tree, auto_unbox = TRUE)
        ))
      }
      if (endsWith(resp$url, "config.json")) {
        return(charToRaw(
          jsonlite::toJSON(state$config, auto_unbox = TRUE)
        ))
      }
      if (endsWith(resp$url, "LICENSE")) {
        return(charToRaw(
          "Synthetic fixture reuse terms"
        ))
      }
      cli::cli_abort("Unexpected metadata request")
    },
    resp_header = function(resp, header) {
      if (grepl("/tree/", resp$url,
        fixed = TRUE
      )) {
        state$pagination
      } else {
        NULL
      }
    },
    .package = "httr2"
  )
  parent <- tempfile("checkpoint-metadata-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  revision <- strrep("b", 40)
  resolve <- function(name) {
    hm_resolve_checkpoint(
      "fixture/model",
      revision, file.path(parent, name)
    )
  }
  out <- resolve("valid")
  expect_identical(out$repository, "fixture/model")
  expect_identical(out$revision, revision)
  expect_identical(out$directory, "checkpoint")
  expect_length(out$artifacts, 3)
  weights <- out$artifacts[[3]]
  expect_identical(weights$path, "checkpoint/weights.safetensors")
  expect_identical(weights$sha256, strrep("a", 64))
  expect_equal(weights$bytes, 128)
  expect_false(any(endsWith(state$requests, ".safetensors")))
  expect_false(out$terms[[1]]$reviewed)
  expect_identical(out$terms[[1]]$text, "Synthetic fixture reuse terms")
  expect_true(file.exists(file.path(
    parent, "valid",
    "checkpoint-manifest.json"
  )))
  expect_false(file.exists(file.path(parent, "valid", "weights.safetensors")))
  state$pagination <- '<url>; rel="next"'
  expect_error(resolve("paginated"), "pagination")
  state$pagination <- NULL
  state$config$auto_map <- list(AutoModel = "remote.module")
  expect_error(resolve("remote-code"), "remote custom code")
  state$config <- list(architectures = list("NotAnAdmittedArchitecture"))
  expect_error(resolve("architecture"), "architecture")
  state$config <- list(architectures = list("Qwen2ForCausalLM"))
  state$tree[[3]]$lfs$oid <- "sha256:invalid"
  expect_error(resolve("invalid-sha"), "SHA256")
  state$tree <- state$tree[1:2]
  expect_error(resolve("no-weights"), class = "hm_terms_error")
  state$tree <- list(list(type = "file", path = "LICENSE"))
  expect_error(resolve("no-config"), "config.json")
})

test_that(paste0(
  "runtime metadata resolution records pinned artifacts ",
  "and explicit pending admission"
), {
  state <- new.env(parent = emptyenv())
  state$python <- "3.11.9"
  state$uv <- "uv 0.11.26 (fixture)"
  state$requests <- list()
  expected_sha <- strrep("c", 64)
  testthat::local_mocked_bindings(run = function(command, args, ...) {
    if (identical(args, "--version")) {
      return(list(
        stdout = state$uv,
        status = 0L
      ))
    }
    if (identical(args[1], "-c")) {
      return(list(
        stdout = state$python,
        status = 0L
      ))
    }
    input <- jsonlite::read_json(args[2])
    state$requests[[length(state$requests) + 1L]] <- input
    resolved <- list(
      requirements = "fixture==1.2.3 --hash=sha256:exact",
      artifacts = list(list(
        path = "wheels/fixture.whl",
        url = "https://example.invalid/fixture.whl", bytes = 42,
        sha256 = expected_sha
      )), terms = list(list(
        package = "fixture",
        version = "1.2.3", artifact_sha256 = expected_sha,
        text = "Exact fixture distribution terms",
        reviewed = FALSE
      )), packages = list(list(
        name = "fixture",
        version = "1.2.3"
      ))
    )
    list(stdout = jsonlite::toJSON(resolved, auto_unbox = TRUE), status = 0L)
  }, .package = "processx")
  testthat::local_mocked_bindings(hm_backend_capabilities = function() {
    list(os = "Linux", arch = "x86_64")
  }, .package = "hypermodR")
  parent <- tempfile("runtime-metadata-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  python <- unname(Sys.which("true"))
  uv <- unname(Sys.which("true"))
  out <- hm_resolve_backend(
    "reference_cpu", "reference:cnn1d", python, uv,
    file.path(parent, "reference")
  )
  expect_s3_class(out, "hm_backend_resolution")
  expect_identical(out$profile$model_id, "reference:cnn1d")
  expect_identical(out$profile$runtime_version, "torch-2.6.0-numpy-2.2.6")
  expect_identical(out$profile$artifacts[[1]]$sha256, expected_sha)
  expect_equal(out$profile$artifacts[[1]]$bytes, 42)
  expect_identical(unlist(state$requests[[1]]$requirements), c(
    "numpy==2.2.6", "torch==2.6.0"
  ))
  expect_identical(state$requests[[1]]$torch_backend, "cpu")
  expect_identical(readLines(file.path(
    parent, "reference",
    "requirements.txt"
  )), "fixture==1.2.3 --hash=sha256:exact")
  expect_false(out$profile$terms[[1]]$reviewed)
  expect_identical(out$profile$terms[[1]]$sha256, digest::digest(
    "Exact fixture distribution terms",
    algo = "sha256", serialize = FALSE
  ))
  expect_true(any(vapply(out$profile$artifacts, function(a) {
    startsWith(
      a$path, "hypermodr_worker/"
    )
  }, logical(1))))
  expect_error(hm_admit_backend(out, out$profile$terms),
    class = "hm_terms_error"
  )
  reviewed <- out$profile$terms
  reviewed[[1]]$reviewed <- TRUE
  plan <- hm_admit_backend(out, reviewed, cache = file.path(parent, "cache"))
  expect_s3_class(plan, "hm_backend_plan")
  vllm <- hm_resolve_backend(
    "vllm_linux_cuda", "fixture/model", python,
    uv, file.path(parent, "vllm")
  )
  expect_identical(vllm$profile$device, "cuda")
  expect_identical(unlist(state$requests[[2]]$requirements), "vllm==0.11.0")
  reviewed <- vllm$profile$terms
  reviewed[[1]]$reviewed <- TRUE
  expect_error(hm_admit_backend(vllm, reviewed), "vLLM requires")
  state$uv <- "uv 0.12.0"
  expect_error(hm_resolve_backend(
    "reference_cpu", "fixture", python, uv,
    file.path(parent, "wrong-uv")
  ), "exactly")
  state$python <- "3.10.0"
  expect_error(hm_resolve_backend(
    "reference_cpu", "fixture", python, uv,
    file.path(parent, "wrong-python")
  ), class = "hm_platform_error")
})
