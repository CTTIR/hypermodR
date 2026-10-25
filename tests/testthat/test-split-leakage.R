test_that(
  "connected grouping and nested folds never leak",
  {
    o <- data.frame(
      observation_id = as.character(1:12),
      subject_id = rep(letters[1:6], each = 2),
      wound_id = c("w", "w", "w", "x", rep(LETTERS[3:6], each = 2))
    )
    t <- hm_task("y", "regression", "spectrum", c("subject_id", "wound_id"))
    a <- hm_split(o, t, folds = 3, inner_folds = 2, seed = 9)
    expect_identical(a, hm_split(o, t, folds = 3, inner_folds = 2, seed = 9))
    expect_length(
      unique(a$component[1:4]),
      1
    )
    expect_equal(
      sort(unlist(lapply(a$folds, `[[`, "test"))),
      1:12
    )
    for (f in a$folds) {
      expect_length(
        intersect(a$component[f$train], a$component[f$test]),
        0
      )
      for (g in f$inner) {
        expect_true(
          all(
            c(g$train, g$test) %in%
              f$train
          )
        )
        expect_length(
          intersect(a$component[g$train], a$component[g$test]),
          0
        )
      }
    }
    o$y <- c(
      rep("rare", 2),
      rep("common", 10)
    )
    expect_error(
      hm_split(
        o, hm_task("y", "classification", "spectrum", "subject_id"),
        design = "grouped", folds = 3
      ),
      class = "hm_split_error"
    )
  }
)
test_that(
  "temporal embargo excludes future groups",
  {
    o <- data.frame(
      observation_id = paste0("o", 1:5),
      subject_id = c("a", "b", "c", "a", "d"),
      timestamp = c(1, 2, 3, 10, 11)
    )
    t <- hm_task("y", "temporal", "subject", "subject_id")
    s <- hm_split(o, t,
      design = "temporal", cutoff = 10, look_back = 2,
      embargo = 1
    )
    expect_equal(s$train, c(2L, 3L))
    expect_equal(s$test, c(4L, 5L))
  }
)
