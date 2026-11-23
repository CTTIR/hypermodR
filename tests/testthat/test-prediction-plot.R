recorded_calls <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grDevices::dev.control("enable")
  value <- withVisible(force(expr))
  calls <- grDevices::recordPlot()[[1]]
  names(calls) <- vapply(calls, function(e) e[[2]][[1]]$name, "")
  list(value = value, calls = calls)
}

axis_titles <- function(calls) {
  title <- calls[[which(names(calls) == "C_title")[1]]][[2]]
  c(xlab = title[[4]], ylab = title[[5]])
}

test_that("numeric prediction plots label their axes and return the input", {
  task <- hm_task("response", "regression", "spectrum", "subject_id")
  prediction <- hm_prediction(c(1.2, 0.8, 1.9), task, c("o1", "o2", "o3"))
  drawn <- recorded_calls(plot(prediction))
  expect_false(drawn$value$visible)
  expect_identical(drawn$value$value, prediction)
  expect_identical(
    axis_titles(drawn$calls),
    c(xlab = "Observation (prediction order)", ylab = "Predicted response")
  )
})

test_that("caller graphics arguments override the defaults", {
  task <- hm_task("response", "regression", "spectrum", "subject_id")
  prediction <- hm_prediction(c(1.2, 0.8), task, c("o1", "o2"))
  drawn <- recorded_calls(plot(prediction, ylab = "Haemoglobin (g/dL)"))
  expect_identical(
    axis_titles(drawn$calls),
    c(xlab = "Observation (prediction order)", ylab = "Haemoglobin (g/dL)")
  )
})

test_that("matrix predictions draw one line per output with a legend", {
  task <- hm_task("abundance", "unmixing", "spectrum", "subject_id")
  abundances <- cbind(water = c(0.2, 0.5, 0.7), lipid = c(0.8, 0.5, 0.3))
  prediction <- hm_prediction(abundances, task, c("o1", "o2", "o3"))
  drawn <- recorded_calls(plot(prediction))
  expect_identical(drawn$value$value, prediction)
  # One series per output plus the legend's point symbols.
  expect_identical(sum(names(drawn$calls) == "C_plotXY"), 3L)
  legend_text <- unlist(lapply(
    drawn$calls[names(drawn$calls) == "C_text"],
    function(e) e[[2]][[3]]
  ))
  expect_true(all(c("water", "lipid") %in% legend_text))
})

test_that("the series palette stays colour-blind safe for many outputs", {
  expect_identical(
    hypermodR:::.hm_plot_palette(2L),
    unname(grDevices::palette.colors(8L, palette = "Okabe-Ito"))[2:3]
  )
  expect_identical(
    hypermodR:::.hm_plot_palette(12L),
    grDevices::hcl.colors(12L, "viridis")
  )
})
