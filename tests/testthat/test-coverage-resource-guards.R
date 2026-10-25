test_that(
  paste0(
    "resource admission refuses unsupported device and ",
    "impossible inherited CPU budget"
  ),
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    expect_error(
      .hm_resource_admit(list()),
      "resource declaration"
    )
    expect_error(
      .hm_resource_admit(hm_resources(device = "cuda")),
      class = "hm_resource_error"
    )
    expect_error(
      .hm_resource_admit(hm_resources(cpus = 100000L)),
      "inherited affinity"
    )
    expect_error(
      hm_resources(cpus = 1.5),
      "thread or network"
    )
    expect_error(
      hm_resources(network = NA),
      "thread or network"
    )
    expect_error(
      hm_resources(memory_mb = Inf),
      "positive finite"
    )
    expect_error(
      hm_with_resources(list(), 42),
      "resource declaration"
    )
  }
)

test_that(
  "actual nested child evaluation must retain identical admitted budget",
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    resource <- hm_resources(memory_mb = 1024, timeout = 20)
    result <- hm_with_resources(
      resource,
      {
        value <- hm_with_resources(resource, 42)
        changed <- tryCatch(
          hm_with_resources(
            hm_resources(memory_mb = 2048),
            42
          ),
          error = function(e) inherits(e, "hm_resource_error")
        )
        list(
          value = value, changed_budget_rejected = changed
        )
      }
    )
    expect_identical(result$value, 42)
    expect_true(result$changed_budget_rejected)
  }
)
