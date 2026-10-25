test_that(paste0(
  "native extraction preserves complete spatial metadata ",
  "without qualification"
), {
  state <- new.env(parent = emptyenv())
  state$corrupt <- FALSE
  state$request <- NULL
  features <- matrix(c(2, 5, 7, 11), 2, 2)
  testthat::local_mocked_bindings(
    .hm_deep_handle = function(control, id) list(model = id),
    hm_worker_request = function(handle, operation, array, channels,
                                 timeout, metadata) {
      state$request <- list(model = handle$model, operation = operation,
                            channels = channels, array = array,
                            metadata = metadata)
      input_digest <- if (state$corrupt) {
        "mismatch"
      } else {
        .hm_hash(list(array, channels))
      }
      list(
        array = features,
        identity = list(source_revision = "synthetic-contract-v1"),
        input_digest = input_digest
      )
    }, .package = "hypermodR"
  )
  cube <- array(seq_len(24), c(2, 3, 4))
  mask <- matrix(c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE), 2, 3)
  admission <- list(execute_source_acknowledged = TRUE,
                    code_terms_admitted = TRUE, weight_terms_admitted = TRUE)
  result <- hm_extract_native("htc", cube, c(450, 500, 600, 750), mask,
                              admission)
  expect_s3_class(result, "hm_native_features")
  expect_identical(result$features, features)
  expect_false(result$qualified)
  expect_identical(state$request$array, cube)
  expect_identical(state$request$channels, c("450", "500", "600", "750"))
  expect_identical(state$request$operation, "extract")
  expect_identical(unlist(state$request$metadata$mask$shape), c(2L, 3L))
  expect_identical(unlist(state$request$metadata$mask$values), as.logical(
                                                                          mask))
  expect_identical(state$request$metadata$admission, admission)
  state$corrupt <- TRUE
  expect_error(hm_extract_native("htc", cube, c(450, 500, 600, 750), mask,
                                 admission), class = "hm_integrity_error")
})
