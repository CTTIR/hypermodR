test_that(
  "CUDA selection cannot broaden inherited visibility",
  {
    plan <- list(
      profile = list(
                     device = "cuda",
                     hardware_verified = TRUE)
    )
    resources <- hm_resources(
                              device = "cuda:2")
    select <- hypermodR:::.hm_backend_device_env
    withr::local_envvar(
                        CUDA_VISIBLE_DEVICES = NA_character_)
    expect_identical(
      select(plan, resources),
      c(CUDA_VISIBLE_DEVICES = "2")
    )
    Sys.setenv(CUDA_VISIBLE_DEVICES = "2,3")
    expect_identical(
      select(plan, resources),
      c(CUDA_VISIBLE_DEVICES = "2")
    )
    expect_error(
      select(
             plan,
             hm_resources(
                          device = "cuda:0")),
      "inherited"
    )
    Sys.setenv(
               CUDA_VISIBLE_DEVICES = "GPU-uuid")
    expect_error(
      select(plan, resources),
      "inherited"
    )
    for (device in c(
      "cpu", "cuda", "cuda:-1", "cuda:1.5",
      "cuda:01"
    )) {
      expect_error(
        select(
               plan,
               hm_resources(
                            device = device)),
        "explicit cuda:N"
      )
    }
    plan$profile$hardware_verified <- FALSE
    expect_error(
      select(plan, resources),
      "verified device ABI"
    )
    plan$profile$device <- "cpu"
    expect_error(
      select(plan, resources),
      "CPU resource"
    )
    expect_identical(
      select(plan, hm_resources()),
      c(CUDA_VISIBLE_DEVICES = "")
    )
    if (Sys.info()[["sysname"]] != "Linux") {
      expect_error(
        hypermodR:::.hm_resource_admit(
                                       resources),
        "platform is unsupported"
      )
    } else {
      expect_error(
        hypermodR:::.hm_resource_admit(
                                       resources),
        "Only CPU"
      )
      expect_type(
        hypermodR:::.hm_resource_admit(
                                       resources,
                                       allow_cuda = TRUE),
        "list"
      )
    }
  }
)

test_that(
  "an empty inherited CUDA list admits no device",
  {
    # Windows cannot hold an empty environment variable: assigning "" removes
    # it, so an empty inherited list cannot occur there.
    skip_on_os("windows")
    plan <- list(profile = list(device = "cuda", hardware_verified = TRUE))
    withr::local_envvar(CUDA_VISIBLE_DEVICES = "")
    expect_identical(
      Sys.getenv("CUDA_VISIBLE_DEVICES", unset = NA_character_), ""
    )
    expect_error(
      .hm_backend_device_env(plan, hm_resources(device = "cuda:2")),
      "inherited"
    )
  }
)

test_that(
  paste0(
    "only verified CUDA backend start opts into CUDA ",
    "process launch"
  ),
  {
    withr::local_envvar(
                        CUDA_VISIBLE_DEVICES = "2,3")
    requested <- NULL
    testthat::local_mocked_bindings(
      .hm_validate_plan = function(
        plan) {
        invisible(TRUE)
      },
      .hm_no_symlink = function(
        path, root) {
        invisible(TRUE)
      },
      .hm_resource_process = function(
        command, args, resources, wd,
        env, allow_cuda
      ) {
        requested <<- list(
          resources = resources, env = env,
          allow_cuda = allow_cuda
        )
        list(get_pid = function() 123L)
      }, .package = "hypermodR"
    )
    backend <- structure(
      list(
        path = file.path(
                         tempdir(
                         ), "cuda-plan"),
        plan = list(
          cache = tempdir(
          ), digest = "cuda-plan",
          model_id = "cuda-test",
          profile = list(
            device = "cuda",
            hardware_verified = TRUE,
            command = "worker",
            args = character(),
            artifacts = list()
          )
        )
      ),
      class = "hm_backend"
    )
    expect_error(
      hm_start_backend(backend),
      "explicit cuda:N"
    )
    expect_null(requested)
    handle <- hm_start_backend(
      backend,
      resources = hm_resources(
                               device = "cuda:2")
    )
    expect_s3_class(
                    handle,
                    "hm_backend_handle")
    expect_true(requested$allow_cuda)
    expect_identical(
      unname(
             requested$env["CUDA_VISIBLE_DEVICES"]),
      "2"
    )
    expect_identical(
                     requested$resources$device,
                     "cuda:2")
  }
)
