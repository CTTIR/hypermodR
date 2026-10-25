.hm_fit_unsupervised <- function(engine, x) {
  id <- engine$model_id
  p <- engine$parameters
  if (id == "kmeans") {
    engine$object <- stats::kmeans(
      x,
      centers = .hm_parameter(p, "centers", 2L),
      nstart = .hm_parameter(p, "nstart", 10L),
      iter.max = .hm_parameter(p, "maxit", 100L)
    )
  } else if (id == "pam") {
    .hm_require("cluster")
    engine$object <- cluster::pam(x, k = .hm_parameter(p, "k", 2L))
  } else if (id == "dbscan") {
    .hm_require("dbscan")
    engine$object <- dbscan::dbscan(
      x,
      eps = .hm_parameter(p, "eps", 0.5),
      minPts = .hm_parameter(p, "min_points", 5L)
    )
    engine$training_x <- x
  } else if (id == "isolation_forest") {
    .hm_require("isotree")
    engine$object <- isotree::isolation.forest(
      x,
      ntrees = .hm_parameter(p, "num_trees", 100L),
      nthreads = 1L, seed = .hm_parameter(p, "seed", 1L)
    )
  } else if (id == "one_class_svm") {
    .hm_require("kernlab")
    engine$object <- kernlab::ksvm(
      x,
      type = "one-svc", kernel = "rbfdot", scaled = FALSE,
      nu = .hm_parameter(p, "nu", 0.1),
      kpar = list(sigma = .hm_parameter(p, "sigma", 1 / ncol(x)))
    )
  } else if (id == "pca_anomaly") {
    rank <- .hm_parameter(p, "rank", 1L)
    if (rank < 1L || rank >= min(
      ncol(x),
      nrow(x)
    )) {
      .hm_abort("Invalid anomaly PCA rank.")
    }
    engine$object <- stats::prcomp(x, rank. = rank, center = TRUE)
    score <- .hm_predict_unsupervised(engine, x)
    engine$threshold <- as.numeric(
      stats::quantile(
        score,
        probs = .hm_parameter(p, "quantile", 0.99),
        names = FALSE
      )
    )
  } else {
    .hm_abort("Unknown engine.", "hm_capability_error")
  }
  engine
}

.hm_predict_unsupervised <- function(engine, x) {
  id <- engine$model_id
  object <- engine$object
  if (id %in% c("kmeans", "pam")) {
    centers <- if (id == "kmeans") {
      object$centers
    } else {
      object$medoids
    }
    distances <- outer(
      rowSums(x^2),
      rowSums(centers^2),
      "+"
    ) -
      2 * tcrossprod(x, centers)
    return(max.col(-distances, ties.method = "first"))
  }
  if (id == "dbscan") {
    return(stats::predict(object, newdata = x, data = engine$training_x))
  }
  if (id == "isolation_forest") {
    return(as.numeric(stats::predict(object, x, type = "score")))
  }
  if (id == "one_class_svm") {
    return(-as.numeric(kernlab::predict(object, x, type = "decision")))
  }
  if (id == "pca_anomaly") {
    centered <- sweep(x, 2L, object$center)
    reconstructed <- centered %*% object$rotation %*% t(object$rotation)
    return(rowMeans((centered - reconstructed)^2))
  }
  .hm_abort("Unknown unsupervised engine.", "hm_capability_error")
}
