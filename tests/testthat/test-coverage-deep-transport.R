test_that(paste0(
  "reference adapter validates transport binding and ",
  "independently decodes logits"
), {
  state <- new.env(parent = emptyenv())
  state$corrupt <- FALSE
  state$shape <- FALSE
  state$requests <- list()
  testthat::local_mocked_bindings(
    .hm_deep_handle = function(control, id) list(model = id),
    hm_worker_request = function(handle, operation, array, channels,
                                 timeout, metadata) {
      state$requests[[length(state$requests) + 1L]] <- list(
        operation = operation, channels = channels, metadata = metadata
      )
      if (operation == "fit") {
        dir.create(metadata$artifact_path)
        for (f in c("model.json", "state.npz", "input.json")) {
          writeLines(
            paste("synthetic artifact", f), file.path(
              metadata$artifact_path,
              f
            )
          )
        }
      }
      output <- if (
                    handle$model == "reference:masked_autoencoder") {
        array * 2
      } else {
        matrix(rep(c(log(3), 0), nrow(array)), ncol = 2, byrow = TRUE)
      }
      if (state$shape) output <- matrix(0, 1, 1)
      list(
        input_digest = if (state$corrupt) {
          "wrong-digest"
        } else {
          .hm_hash(
            list(array, channels)
          )
        }, receipt = list(fixture = TRUE),
        array = output
      )
    }, .package = "hypermodR"
  )
  parent <- tempfile("deep-transport-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  x <- matrix(1:12, 3, 4)
  task <- hm_task("y", "classification", "spectrum", "subject_id")
  control <- list(
    artifact_path = file.path(parent, "classification"),
    observations = data.frame(subject_id = c("s1", "s2", "s3")),
    wavelength = c(450, 500, 600, 750)
  )
  model <- .hm_fit_deep("reference:cnn1d", x, c("b", "a", "b"), task, list(
    epochs = 3L, domain = c(0L, 1L, 0L)
  ), control)
  expect_identical(model$classes, c("a", "b"))
  expect_equal(model$n_features, 4)
  expect_identical(state$requests[[1]]$channels, c(
    "450", "500", "600",
    "750"
  ))
  expect_identical(unlist(state$requests[[1]]$metadata$roles), rep(
    "train",
    3
  ))
  expect_identical(unlist(state$requests[[1]]$metadata$y), c(1L, 0L, 1L))
  expect_identical(unlist(state$requests[[1]]$metadata$domain), c(0L, 1L, 0L))
  expect_length(unique(unlist(state$requests[[1]]$metadata$groups)), 3)
  prediction <- .hm_predict_deep(model, x)
  expect_equal(prediction, matrix(rep(c(.75, .25), 3), 3, 2,
    byrow = TRUE,
    dimnames = list(NULL, c("a", "b"))
  ))
  state$corrupt <- TRUE
  expect_error(.hm_predict_deep(model, x), class = "hm_integrity_error")
  state$corrupt <- FALSE
  state$shape <- TRUE
  expect_error(.hm_predict_deep(model, x), class = "hm_backend_output_error")
  state$shape <- FALSE
  writeLines("corrupted", file.path(model$artifact_path, "model.json"))
  expect_error(.hm_predict_deep(model, x), class = "hm_integrity_error")
  control$artifact_path <- file.path(parent, "reconstruction")
  reconstruction <- .hm_fit_deep(
    "reference:masked_autoencoder", x, NULL,
    hm_task("y", "reconstruction", "spectrum", "subject_id"), list(), control
  )
  expect_equal(.hm_predict_deep(reconstruction, x), x * 2)
  state$shape <- TRUE
  expect_error(.hm_predict_deep(reconstruction, x),
    class = "hm_backend_output_error"
  )
  state$shape <- FALSE
  state$corrupt <- TRUE
  control$artifact_path <- file.path(parent, "corrupt-fit")
  expect_error(.hm_fit_deep(
    "reference:cnn1d", x, c("b", "a", "b"), task,
    list(), control
  ), class = "hm_integrity_error")
})

test_that(paste0(
  "reference adapter rejects missing metadata before ",
  "contacting transport"
), {
  testthat::local_mocked_bindings(.hm_deep_handle = function(
    control,
    id) {
    list(model = id)
  }, .package = "hypermodR")
  parent <- tempfile("deep-admission-")
  dir.create(parent)
  on.exit(unlink(parent, recursive = TRUE), add = TRUE)
  x <- matrix(1:12, 3, 4)
  task <- hm_task("y", "classification", "spectrum", "subject_id")
  control <- list(artifact_path = parent, observations = data.frame(
    subject_id = c("s1", "s2", "s3")
  ), wavelength = 1:4)
  fit <- function(control) {
    .hm_fit_deep("reference:cnn1d", x, c(
      "a", "b",
      "a"
    ), task, list(), control)
  }
  expect_error(fit(control), "new artifact directory")
  control$artifact_path <- file.path(parent, "model")
  control$observations <- data.frame(other = 1:3)
  expect_error(fit(control), class = "hm_split_error")
  control$observations <- data.frame(subject_id = c("s1", "s2", "s3"))
  control$wavelength <- 1:3
  expect_error(fit(control), "Measured output channels")
})
