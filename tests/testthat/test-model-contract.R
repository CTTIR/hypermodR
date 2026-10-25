test_that(
  "task and probabilities reject incompatible schemas",
  {
    expect_error(
      hm_task("y", "segmentation", "subject", "subject_id"),
      class = "hm_validation_error"
    )
    t <- hm_task("y", "classification", "spectrum", "subject_id")
    p <- matrix(
      c(0.2, 0.8, 0.7, 0.3),
      2,
      byrow = TRUE, dimnames = list(NULL, c("a", "b"))
    )
    x <- hm_prediction(
      c("b", "a"),
      t, c("1", "2"),
      p, c("a", "b")
    )
    expect_equal(
      summary(x)$n,
      2
    )
    expect_error(
      hm_prediction(
        c("b", "a"),
        t, c("1", "2"),
        p * 2, c("a", "b")
      ),
      class = "hm_validation_error"
    )
    expect_error(
      hm_prediction(
        c("b", "a"),
        t, c("1", "2"),
        p, c("b", "a")
      ),
      class = "hm_validation_error"
    )
  }
)
test_that(
  "target schemas reject incompatible response shapes",
  {
    expect_error(
      .hm_validate_target(
        1:3, hm_task("y", "ordinal", "spectrum", "subject_id"),
        3
      )
    )
    expect_error(
      .hm_validate_target(
        matrix(1, 3, 2),
        hm_task("y", "regression", "spectrum", "subject_id"),
        3
      )
    )
    expect_error(
      .hm_validate_target(
        data.frame(
          time = c(1, -1),
          status = c(0, 1)
        ),
        hm_task("y", "survival", "subject", "subject_id"),
        2
      )
    )
  }
)
