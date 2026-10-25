test_that(
  "exclusive storage and readback protect successful runs",
  {
    path <- tempfile()
    on.exit(unlink(path, recursive = TRUE))
    x <- list(a = 1:5)
    hm_save_run(x, path)
    expect_identical(
      hm_read_run(path),
      x
    )
    expect_error(
      hm_save_run(
        list(a = 0),
        path
      ),
      class = "hm_run_conflict"
    )
    expect_identical(
      hm_read_run(path),
      x
    )
    saveRDS(0, file.path(path, "object.rds"))
    expect_error(
      hm_read_run(path),
      class = "hm_integrity_error"
    )
  }
)
test_that(
  "provenance catches changed inputs",
  {
    p <- hm_provenance(list(input = 1))
    expect_true(hm_verify_provenance(p, list(input = 1)))
    expect_error(
      hm_verify_provenance(p, list(input = 2)),
      class = "hm_integrity_error"
    )
  }
)
