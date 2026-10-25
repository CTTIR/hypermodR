.hm_fit_classical <- function(engine, x, y, weights, control) {
  id <- engine$model_id
  p <- engine$parameters
  d <- .hm_engine_frame(x)
  if (!is.null(y) &&
        !is.data.frame(y)) {
    d$outcome <- y
  }
  formula <- stats::as.formula("outcome ~ .")
  classification <- engine$task_type == "classification"
  if (id %in% c("lda", "qda")) {
    .hm_require("MASS")
    fun <- if (id == "lda") {
      MASS::lda
    } else {
      MASS::qda
    }
    engine$object <- fun(
      as.matrix(.hm_engine_frame(x)),
      grouping = y
    )
  } else if (id == "knn") {
    k <- .hm_parameter(p, "k", 5L)
    if (length(k) !=
          1L || !is.finite(k) ||
          k < 1L || k > nrow(x) ||
          k != as.integer(k)) {
      .hm_abort("k must be an integer within the training sample count.")
    }
    engine$object <- list(x = x, y = y, k = k)
  } else if (id == "naive_bayes") {
    .hm_require("e1071")
    engine$object <- e1071::naiveBayes(x, y, laplace = .hm_parameter(
      p,
      "laplace", 1
    ))
  } else if (id == "svm") {
    .hm_require("e1071")
    engine$object <- e1071::svm(
      x, y,
      kernel = .hm_parameter(p, "kernel", "radial"),
      cost = .hm_parameter(p, "cost", 1),
      gamma = .hm_parameter(p, "gamma", 1 / ncol(x)),
      probability = classification, scale = FALSE
    )
  } else if (id == "rf") {
    .hm_require("ranger")
    engine$object <- ranger::ranger(
      formula,
      data = d, num.trees = .hm_parameter(p, "num_trees", 100L),
      mtry = .hm_parameter(p, "mtry", max(1L, floor(sqrt(ncol(x))))),
      probability = classification, case.weights = weights, num.threads = 1L,
      seed = .hm_parameter(control, "seed", 1L)
    )
  } else if (id == "xgboost") {
    .hm_require("xgboost")
    label <- if (classification) {
      as.integer(y) -
        1L
    } else {
      y
    }
    train <- xgboost::xgb.DMatrix(x, label = label, weight = weights)
    params <- list(
      objective = if (
                      classification) {
        "multi:softprob"
      } else {
        "reg:squarederror"
      },
      nthread = 1L, max_depth = .hm_parameter(p, "max_depth", 3L),
      eta = .hm_parameter(p, "eta", 0.1),
      seed = .hm_parameter(control, "seed", 1L)
    )
    if (classification) {
      params$num_class <- length(engine$classes)
    }
    engine$object <- xgboost::xgb.train(params, train,
      nrounds = .hm_parameter(p, "nrounds", 50L)
    )
  } else if (id == "gaussian_process") {
    sigma <- .hm_parameter(p, "length_scale", 1)
    noise <- .hm_parameter(p, "noise_variance", 0.001)
    if (length(sigma) !=
          1L || !is.finite(sigma) ||
          sigma <= 0 || length(noise) !=
          1L || !is.finite(noise) ||
          noise <= 0) {
      .hm_abort("Invalid GP scales.")
    }
    kernel <- .hm_rbf(x, x, sigma)
    ch <- chol(kernel + diag(noise, nrow(x)))
    engine$object <- list(
      x = x, length_scale = sigma, chol = ch, alpha = backsolve(
        ch, forwardsolve(
          t(ch),
          y
        )
      )
    )
  } else if (id == "coxph") {
    return(.hm_fit_survival(engine, x, y, weights))
  } else if (id == "clm") {
    .hm_require("ordinal")
    engine$object <- ordinal::clm(
      formula,
      data = d, weights = weights, link = .hm_parameter(p, "link", "logit")
    )
  } else if (id == "ensemble") {
    members <- p$members
    if (!is.list(members) ||
          !length(members)) {
      .hm_abort("Supply a nonempty list of member specs.")
    }
    if (any(
      vapply(
        members, function(m) identical(m$model_id, "ensemble"),
        logical(1)
      )
    )) {
      .hm_abort("Nested ensembles are not supported.")
    }
    engine$object <- lapply(
      members, function(m) {
        .hm_fit_engine(
          m$model_id, x, y, list(type = engine$task_type),
          .hm_parameter(m, "parameters", list()),
          control
        )
      }
    )
  } else {
    return(.hm_fit_unsupervised(engine, x))
  }
  engine
}

.hm_rbf <- function(x, y, length_scale) {
  distance <- outer(
    rowSums(x^2),
    rowSums(y^2),
    "+"
  ) -
    2 * tcrossprod(x, y)
  exp(
    -pmax(distance, 0) / (2 *
                            length_scale^2)
  )
}

.hm_predict_classical <- function(engine, x, ...) {
  id <- engine$model_id
  object <- engine$object
  d <- .hm_engine_frame(x)
  if (id %in% c("lda", "qda")) {
    out <- stats::predict(object, newdata = d)$posterior
  } else if (id == "knn") {
    distances <- outer(
      rowSums(x^2),
      rowSums(object$x^2),
      "+"
    ) -
      2 * tcrossprod(x, object$x)
    neighbors <- lapply(
      seq_len(nrow(x)),
      function(i) {
        order(distances[i, ], seq_len(nrow(object$x)))[seq_len(object$k)]
      }
    )
    if (engine$task_type == "regression") {
      return(
        vapply(
          neighbors, function(i) mean(object$y[i]),
          numeric(1)
        )
      )
    }
    out <- t(
      vapply(
        neighbors, function(i) {
          as.numeric(table(factor(object$y[i],
                       levels = engine$classes
                     ))) / length(i)
        }, numeric(length(engine$classes))
      )
    )
    colnames(out) <- engine$classes
  } else if (id == "naive_bayes") {
    out <- stats::predict(object, newdata = x, type = "raw")
  } else if (id == "svm") {
    out <- stats::predict(object,
      newdata = x,
      probability = engine$task_type == "classification"
    )
    if (engine$task_type == "classification") {
      out <- attr(out, "probabilities")
    }
  } else if (id == "rf") {
    out <- stats::predict(object, data = d, num.threads = 1L)$predictions
  } else if (id == "xgboost") {
    out <- stats::predict(object, newdata = x)
    if (engine$task_type == "classification" && is.null(dim(out))) {
      out <- matrix(
        out,
        ncol = length(engine$classes),
        byrow = TRUE
      )
    }
  } else if (id == "gaussian_process") {
    return(
      as.vector(
        .hm_rbf(x, object$x, object$length_scale) %*%
          object$alpha
      )
    )
  } else if (id == "coxph") {
    return(as.vector(stats::predict(object, newdata = d, type = "risk")))
  } else if (id == "clm") {
    out <- stats::predict(object, newdata = d, type = "prob")$fit
  } else if (id == "ensemble") {
    outputs <- lapply(object, .hm_predict_engine, x = x, task = list(
      type = engine$task_type
    ))
    out <- Reduce(`+`, outputs) / length(outputs)
  } else {
    return(.hm_predict_unsupervised(engine, x))
  }
  if (engine$task_type %in% c("classification", "ordinal")) {
    return(.hm_probabilities(out, engine$classes, nrow(x)))
  }
  as.vector(out)
}
