naive_window_components <- function(obs) {
  n <- nrow(obs)
  adjacency <- outer(seq_len(n), seq_len(n), function(a, b) {
    obs$source_id[a] == obs$source_id[b] &
      obs$window_start[a] <= obs$window_end[b] &
      obs$window_start[b] <= obs$window_end[a]
  })
  label <- seq_len(n)
  repeat {
    updated <- vapply(seq_len(n), function(i) min(label[adjacency[i, ]]),
                      numeric(1))
    if (identical(updated, as.numeric(label))) break
    label <- updated
  }
  label
}

same_partition <- function(a, b) {
  identical(outer(a, a, "=="), outer(b, b, "=="))
}

test_that("overlapping source windows form the same components as all pairs", {
  set.seed(11)
  for (replicate in 1:20) {
    n <- 40
    start <- sample(0:60, n, replace = TRUE)
    obs <- data.frame(
      observation_id = paste0("o", seq_len(n)),
      source_id = sample(c("left", "right", "third"), n, replace = TRUE),
      window_start = start,
      window_end = start + sample(0:6, n, replace = TRUE)
    )
    expect_true(same_partition(
      hypermodR:::.hm_components(obs, character()),
      naive_window_components(obs)
    ))
  }
})

test_that("touching windows connect and separated windows do not", {
  obs <- data.frame(
    observation_id = paste0("o", 1:5),
    source_id = c("s", "s", "s", "s", "t"),
    window_start = c(10, 0, 5, 20, 0),
    window_end = c(12, 5, 8, 25, 30)
  )
  component <- hypermodR:::.hm_components(obs, character())
  expect_identical(component[2], component[3])
  expect_false(component[1] == component[2])
  expect_false(component[4] == component[1])
  expect_false(component[5] == component[2])
})

test_that("many source windows are grouped without all-pairs comparisons", {
  n <- 20000
  obs <- data.frame(
    observation_id = paste0("o", seq_len(n)),
    source_id = rep(c("x", "y"), each = n / 2),
    window_start = rep(seq_len(n / 2) * 10, 2),
    window_end = rep(seq_len(n / 2) * 10 + 5, 2)
  )
  elapsed <- system.time(
    component <- hypermodR:::.hm_components(obs, character())
  )[["elapsed"]]
  expect_length(unique(component), n)
  expect_lt(elapsed, 20)
})
