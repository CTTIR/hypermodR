test_that(
  "paired bootstrap preserves whole units and reconstructible draws",
  {
    z <- eval_fixture()
    e <- hm_evaluate(z$p, z$r)
    b <- hm_bootstrap(e, "subject_id", draws = 20, seed = 4)
    for (i in seq_along(b$indices)) {
      ix <- b$indices[[i]]
      expect_equal(
        unname(b$resamples[i, "accuracy"]),
        mean(z$p$values[ix] == z$r$y[ix])
      )
      expect_equal(
        ix, unlist(
          lapply(b$sampled_units[[i]], function(u) {
            which(
              z$r$subject_id == u
            )
          }),
          use.names = FALSE
        )
      )
    }
    paired <- hm_compare(
      list(e, e),
      unit = "subject_id", draws = 20, seed = 4
    )
    expect_equal(
      unname(paired$uncertainty$interval),
      matrix(0, 2, length(e$metrics))
    )
    expect_true(all(paired$uncertainty$resamples == 0))
  }
)
test_that(
  "missing class draws are retained and cannot produce an interval",
  {
    z <- eval_fixture()
    z$r$subject_id <- ifelse(z$r$y == "a", "A", "B")
    b <- hm_bootstrap(
      hm_evaluate(z$p, z$r),
      "subject_id",
      draws = 50, seed = 7
    )
    expect_gt(b$invalid_draws, 0)
    expect_equal(b$invalid_draws, sum(!b$valid))
    expect_true(all(is.na(b$interval)))
    expect_match(b$conditioning, "fixed predictions")
  }
)
