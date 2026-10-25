test_that(
  "conditions distinguish validation and dependencies",
  {
    expect_error(
      .hm_abort("invalid"),
      class = "hm_validation_error"
    )
    expect_error(
      .hm_require("hypermodRmissingpackage"),
      class = "hm_dependency_error"
    )
    expect_true(.hm_require("stats"))
    expect_error(
      .hm_scalar_character(NA_character_, "x"),
      class = "hm_error"
    )
    expect_identical(
      .hm_scalar_character("x", "x"),
      "x"
    )
  }
)

test_that(
  "hashes cover contents and seeded operations preserve caller state",
  {
    expect_false(
      identical(
        .hm_hash(1:4),
        .hm_hash(c(1:3, 5L))
      )
    )
    set.seed(42)
    before <- .Random.seed
    a <- .hm_seed(7, stats::runif(4))
    expect_identical(before, .Random.seed)
    expect_identical(a, .hm_seed(7, stats::runif(4)))
    expect_error(
      .hm_seed(-1, 1),
      class = "hm_validation_error"
    )
  }
)
