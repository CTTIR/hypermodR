test_that(
  "bounded expression preserves caller data and parent environment",
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    before <- Sys.getenv("OMP_NUM_THREADS")
    parent_pid <- Sys.getpid()
    value <- 17L
    out <- hm_with_resources(
      hm_resources(),
      {
        list(
          value = value, pid = Sys.getpid(), threads = Sys.getenv(
            "OMP_NUM_THREADS"
          ),
          affinity = grep(
            "^Cpus_allowed_list:", readLines("/proc/self/status"),
            value = TRUE
          )
        )
      }
    )
    expect_identical(out$value, value)
    expect_false(identical(out$pid, parent_pid))
    expect_identical(out$threads, "1")
    expect_identical(
      Sys.getenv("OMP_NUM_THREADS"),
      before
    )
    admitted <- hypermodR:::.hm_resource_admit(hm_resources())
    expect_identical(
      sub("^Cpus_allowed_list:[[:space:]]*", "", out$affinity),
      admitted$affinity
    )
    expect_error(hm_resources(cpus = 1, threads = 2))
  }
)

test_that(
  "timeout terminates a child without status polling",
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    child <- hypermodR:::.hm_resource_r_bg(
      function() {
        Sys.sleep(30)
        TRUE
      },
      resources = hm_resources(timeout = 1)
    )
    on.exit(if (child$is_alive()) child$kill_tree())
    child$wait(5000L)
    expect_false(child$is_alive())
    expect_error(child$get_result(), "without output")
  }
)

test_that(
  "address-space limit rejects an allocation in a small owned child",
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    skip_if(!nzchar(Sys.which("python3")))
    p <- hypermodR:::.hm_resource_process(
      Sys.which("python3"),
      c(
        "-c", paste0(
          "import sys\ntry: x=bytearray(64*1024*1024)\nexcept ",
          "MemoryError: sys.exit(23)\nsys.exit(24)"
        )
      ),
      hm_resources(memory_mb = 48, timeout = 5)
    )
    on.exit(if (p$is_alive()) p$kill_tree())
    p$wait(7000L)
    expect_identical(p$get_exit_status(), 23L)
  }
)

test_that(
  "cancellation requires ownership and leaves unrelated children alone",
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    one <- hypermodR:::.hm_resource_r_bg(
      function() Sys.sleep(30),
      resources = hm_resources()
    )
    two <- hypermodR:::.hm_resource_r_bg(
      function() Sys.sleep(30),
      resources = hm_resources()
    )
    on.exit({
      if (one$is_alive()) one$kill_tree()
      if (two$is_alive()) two$kill_tree()
    })
    one$owner_pid <- -1L
    expect_error(one$kill_tree(), "not owned")
    expect_true(one$is_alive())
    expect_true(two$is_alive())
    one$owner_pid <- Sys.getpid()
    one$kill_tree()
    one$wait(5000L)
    expect_false(one$is_alive())
    expect_true(two$is_alive())
  }
)

test_that(
  "nested work reuses its admitted budget and refuses changes",
  {
    skip_if_not(Sys.info()[["sysname"]] == "Linux")
    expect_true(
      hm_with_resources(hm_resources(), hm_with_resources(
        hm_resources(),
        TRUE
      ))
    )
    expect_error(
      hm_with_resources(
        hm_resources(), hm_with_resources(
          hm_resources(cpus = 2),
          TRUE
        )
      ),
      "reuse"
    )
    expect_error(
      hypermodR:::.hm_resource_admit(hm_resources(device = "cuda")),
      "Only CPU"
    )
  }
)

test_that("unsupported systems refuse real bounded child routes explicitly", {
  if (identical(Sys.info()[["sysname"]], "Linux")) {
    expect_type(hypermodR:::.hm_resource_admit(hm_resources()), "list")
  } else {
    expect_error(
      hm_with_resources(hm_resources(), 42),
      "platform is unsupported"
    )
  }
})
