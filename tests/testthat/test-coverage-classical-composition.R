test_that(
  paste0(
    "nearest-neighbour regression and ensemble means have ",
    "exact independent values"
  ),
  {
    x <- matrix(
      c(0, 2, 7, 10),
      ncol = 1
    )
    y <- c(1, 5, 15, 21)
    fit <- .hm_fit_engine(
      "knn", x, y, list(type = "regression"),
      list(k = 2),
      list()
    )
    expect_equal(
      .hm_predict_engine(
        fit, matrix(
          c(1, 9),
          ncol = 1
        ),
        NULL
      ),
      c(3, 18)
    )
    members <- list(
      list(model_id = "knn", parameters = list(k = 1)),
      list(model_id = "knn", parameters = list(k = 2))
    )
    ensemble <- .hm_fit_engine(
      "ensemble", x, y, list(type = "regression"),
      list(members = members),
      list()
    )
    expect_equal(
      .hm_predict_engine(
        ensemble, matrix(
          c(0, 10),
          ncol = 1
        ),
        NULL
      ),
      c(2, 19.5)
    )
    labels <- c("a", "a", "b", "b")
    ensemble <- .hm_fit_engine(
      "ensemble", x, labels, list(type = "classification"),
      list(members = members),
      list()
    )
    expect_equal(
      .hm_predict_engine(
        ensemble, matrix(
          c(0, 10),
          ncol = 1
        ),
        NULL
      ),
      diag(2),
      ignore_attr = TRUE
    )
    expect_error(
      .hm_fit_engine(
        "ensemble", x, y, list(type = "regression"),
        list(), list()
      ),
      "nonempty list"
    )
    expect_error(
      .hm_fit_engine(
        "ensemble", x, y, list(type = "regression"),
        list(members = list(list(model_id = "ensemble"))),
        list()
      ),
      "Nested ensembles"
    )
    expect_error(
      .hm_fit_engine(
        "gaussian_process", x, y, list(type = "regression"),
        list(length_scale = 0),
        list()
      ),
      "GP scales"
    )
  }
)
