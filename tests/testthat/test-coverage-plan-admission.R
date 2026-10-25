test_that(paste0(
  "GPU metadata admission requires exact checkpoint paths ",
  "and reviewed terms"
), {
  source <- tempfile("checkpoint-plan-")
  writeLines("local fixture", source)
  on.exit(unlink(source), add = TRUE)
  profile <- backend_test_profile(source, "fixture/model")
  profile$device <- "cuda"
  profile$terms[[1]]$package <- "fixture"
  profile$terms[[1]]$version <- "1.0.0"
  profile$terms[[1]]$artifact_sha256 <- profile$artifacts[[1]]$sha256
  resolution <- structure(list(
    backend = "vllm_linux_cuda",
    profile = profile
  ), class = "hm_backend_resolution")
  checkpoint <- list(directory = "checkpoint", artifacts = list(list(
    path = "checkpoint/weights.safetensors", url = paste0(
      "file://",
      source
    ), bytes = file.info(source)$size, sha256 = digest::digest(
      file = source, algo = "sha256"
    )
  )), terms = profile$terms)
  plan <- hm_admit_backend(resolution, profile$terms,
    hardware_verified = TRUE, checkpoint = checkpoint, port = 8123L
  )
  expect_s3_class(plan, "hm_backend_plan")
  expect_identical(plan$profile$args, c(
    "checkpoint", "fixture/model",
    "8123"
  ))
  expect_identical(
    plan$profile$artifacts[[2]]$path,
    "checkpoint/weights.safetensors"
  )
  expect_true(plan$profile$hardware_verified)
  expect_false(plan$scientific_qualification)
  expect_true(.hm_validate_plan(plan))
  expect_error(hm_admit_backend(resolution, profile$terms,
                 hardware_verified = FALSE, checkpoint = checkpoint
               ), "admitted device")
  checkpoint$directory <- "../checkpoint"
  expect_error(hm_admit_backend(resolution, profile$terms,
                 hardware_verified = TRUE, checkpoint = checkpoint
               ), "checkpoint")
  checkpoint$directory <- "checkpoint"
  expect_error(
    hm_admit_backend(resolution, profile$terms,
      hardware_verified = TRUE, checkpoint = checkpoint, port = 80
    ),
    "unprivileged"
  )
  expect_error(
    hm_admit_backend(resolution, profile$terms,
      hardware_verified = TRUE, checkpoint = checkpoint, port = 8123.5
    ),
    "unprivileged"
  )
  changed <- profile$terms
  changed[[1]]$package <- "other"
  expect_error(
    hm_admit_backend(resolution, changed,
      hardware_verified = TRUE, checkpoint = checkpoint
    ),
    class = "hm_terms_error"
  )
})
