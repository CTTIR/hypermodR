test_that(
  "states cannot share data, results or queue handles",
  {
    a <- hm_app_state()
    b <- hm_app_state()
    hypermodR:::.hm_app_import(a, hm_example())
    a$job <- list(id = "owned")
    a$result <- list(secret = "private")
    expect_null(b$dataset)
    expect_null(b$result)
    expect_null(b$job)
    b$error <- "independent"
    expect_null(a$error)
  }
)
