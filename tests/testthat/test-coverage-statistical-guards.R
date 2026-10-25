test_that(
  paste0(
    "engine admission rejects malformed data and ",
    "unsupported class support"
  ),
  {
    fit <- function(
      x, y, id = "lm", task = list(type = "regression"),
      parameters = list(), control = list()
    ) {
      .hm_fit_engine(id, x, y, task, parameters, control)
    }
    x <- matrix(1:12, 6, 2)
    expect_error(
      fit(
        matrix(Inf, 6, 2),
        1:6
      ),
      "finite numeric"
    )
    expect_error(
      fit(
        matrix(1:2, 1, 2),
        1
      ),
      "too small"
    )
    expect_error(
      fit(x, c(1:5, Inf)),
      "finite numeric"
    )
    expect_error(
      fit(
        x, c("a", "a", "b", "b", "c", NA),
        "glm", list(type = "classification")
      ),
      "missing"
    )
    expect_error(
      fit(
        x, factor(
          rep(
            c("a", "b"),
            3
          )
        ),
        "clm", list(type = "ordinal")
      ),
      "ordered factor"
    )
    expect_error(
      fit(
        x, factor(
          rep(
            c("a", "b"),
            3
          ),
          levels = c("a", "b", "c")
        ),
        "glm", list(type = "classification")
      ),
      "training support"
    )
    expect_error(
      fit(
        x, rep(
          c("a", "b", "c"),
          2
        ),
        "glm", list(type = "classification")
      ),
      class = "hm_capability_error"
    )
    expect_error(
      fit(
        x, rep(
          c("a", "b"),
          3
        ),
        "glm", list(type = "classification"),
        list(family = "gaussian")
      ),
      "binomial"
    )
    expect_error(
      fit(
        x, 1:6, "glm", parameters = list(family = "unknown")
      ),
      "Unknown GLM"
    )
    engine <- fit(x, 1:6)
    expect_error(
      .hm_predict_engine(
        engine, matrix(1, 3, 1),
        NULL
      ),
      "wrong band count"
    )
    expect_error(
      .hm_probabilities(
        matrix(0.5, 2, 3),
        c("a", "b"),
        2
      ),
      class = "hm_backend_output_error"
    )
    expect_error(
      .hm_probabilities(
        c(-0.1, 1.1),
        c("a", "b"),
        2
      ),
      class = "hm_backend_output_error"
    )
  }
)

test_that(
  paste0(
    "Gaussian GLM and binary regularized intercept ",
    "reproduce known predictions"
  ),
  {
    x <- cbind(
      rep(-2:2, each = 6),
      0
    )
    regression <- .hm_fit_engine(
      "glm", x, 3 + 2 * x[, 1], list(type = "regression"),
      list(family = "gaussian"),
      list()
    )
    expect_equal(
      .hm_predict_engine(
        regression, cbind(
          c(-3, 4),
          0
        ),
        NULL
      ),
      c(-3, 11),
      tolerance = 1e-10
    )
    skip_if_not_installed("glmnet")
    binary <- factor(
      rep(
        c("a", "b"),
        15
      )
    )
    model <- .hm_fit_engine(
      "glmnet", x, binary, list(type = "classification"),
      list(lambda = 0.1),
      list()
    )
    expect_equal(
      .hm_predict_engine(
        model, cbind(
          c(-3, 4),
          0
        ),
        NULL
      ),
      matrix(
        0.5, 2, 2, dimnames = list(NULL, c("a", "b"))
      ),
      tolerance = 1e-06
    )
    multi <- factor(
      rep(
        c("a", "b", "c"),
        10
      )
    )
    model <- .hm_fit_engine(
      "glmnet", x, multi, list(type = "classification"),
      list(lambda = 0.1),
      list()
    )
    expect_equal(
      .hm_predict_engine(
        model, cbind(
          c(-3, 4),
          0
        ),
        NULL
      ),
      matrix(
        1 / 3, 2, 3, dimnames = list(NULL, c("a", "b", "c"))
      ),
      tolerance = 1e-06
    )
    expect_error(
      .hm_fit_engine(
        "glmnet", x, binary, list(type = "classification"),
        list(lambda = 0),
        list()
      ),
      "positive value"
    )
  }
)

test_that(
  "mixed models reject missing or singleton biological groups",
  {
    skip_if_not_installed("lme4")
    x <- matrix(1:12, ncol = 1)
    expect_error(
      .hm_fit_engine(
        "mixed", x, 1:12, list(type = "regression"),
        list(group = "subject_id"),
        list()
      ),
      "declared group"
    )
    expect_error(
      .hm_fit_engine(
        "mixed", x, 1:12, list(type = "regression"),
        list(group = "subject_id"),
        list(
          observations = data.frame(subject_id = rep("s1", 12))
        )
      ),
      "grouping is invalid"
    )
  }
)
