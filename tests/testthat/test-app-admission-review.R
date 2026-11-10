test_that("new runtime notices require a fresh explicit review", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("callr")
  fixture <- new.env()
  fixture$value <- structure(list(
    backend = "reference_cpu", packages = "first",
    qualification = "pending",
    profile = list(terms = list(list(text = "first notice")))
  ), class = "hm_backend_resolution")
  admitted <- list()
  local_mocked_bindings(
    r_bg = function(func, args, ...) {
      list(is_alive = function() FALSE,
           get_result = function() fixture$value,
           kill_tree = function() NULL)
    }, .package = "callr"
  )
  local_mocked_bindings(
    hm_admit_backend = function(resolution, terms) {
      admitted[[length(admitted) + 1L]] <<- terms
      structure(list(terms = terms), class = "hm_backend_plan")
    }, .package = "hypermodR"
  )
  shiny::testServer(hypermodR:::.hm_app_server, {
    session$setInputs(type = "classification", model = "reference:cnn1d",
                      python = "python3", terms = FALSE)
    session$setInputs(resolve = 1)
    session$elapse(350)
    session$setInputs(terms = TRUE)
    fixture$value$profile$terms[[1]]$text <- "different notice"
    fixture$value$packages <- "second"
    session$setInputs(resolve = 2)
    session$elapse(350)
    expect_identical(runtime()$profile$terms[[1]]$text, "different notice")
    # Simulate an old checked value before the browser applies the reset.
    expect_true(input$terms)
    session$setInputs(install = 1)
    expect_length(admitted, 0L)
    expect_match(state$error, "review every artifact")
    session$setInputs(terms = FALSE)
    session$setInputs(terms = TRUE)
    session$setInputs(install = 2)
    expect_length(admitted, 1L)
    expect_identical(admitted[[1]][[1]]$text, "different notice")
    expect_true(admitted[[1]][[1]]$reviewed)
  })
})

test_that("workbench construction does not change the upload limit", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("callr")
  withr::local_options(shiny.maxRequestSize = 12345)
  app <- hm_app(launch = FALSE)
  expect_identical(getOption("shiny.maxRequestSize"), 12345)
  cleanup <- NULL
  local_mocked_bindings(
    onStop = function(fun, session) cleanup <<- fun,
    .package = "shiny"
  )
  app$onStart()
  expect_identical(getOption("shiny.maxRequestSize"), 50 * 1024^2)
  cleanup()
  expect_identical(getOption("shiny.maxRequestSize"), 12345)
})
