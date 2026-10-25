.hm_engine_catalog <- function() {
  conventional <- list(
    lm = "regression", glm = c("classification", "regression"),
    glmnet = c("classification", "regression"),
    gam = "regression", mixed = "regression", pls = "regression",
    pcr = "regression",
    pca = "reconstruction", ica = "reconstruction", nmf = "reconstruction",
    mcr_als = "unmixing", fcls = "unmixing", beer_lambert = "unmixing",
    bayes_conjugate = "regression", brms = "regression",
    physics_inverse = c("regression", "unmixing"),
    physics_hybrid = "regression", lda = "classification",
    qda = "classification",
    knn = c("classification", "regression"),
    naive_bayes = "classification", svm = c("classification", "regression"),
    rf = c("classification", "regression"),
    xgboost = c("classification", "regression"),
    gaussian_process = "regression", coxph = "survival", clm = "ordinal",
    ensemble = c("classification", "regression"),
    kmeans = "clustering", pam = "clustering", dbscan = "clustering",
    isolation_forest = "anomaly",
    one_class_svm = "anomaly", pca_anomaly = "anomaly"
  )
  if (exists(".hm_deep_catalog", mode = "function")) {
    c(conventional, .hm_deep_catalog())
  } else {
    conventional
  }
}

.hm_parameter <- function(parameters, name, default) {
  if (is.null(parameters[[name]])) {
    default
  } else {
    parameters[[name]]
  }
}

.hm_engine_frame <- function(x) {
  out <- as.data.frame(x)
  names(out) <- paste0("band_", seq_len(ncol(x)))
  out
}

.hm_fit_engine <- function(model_id, x, y, task, parameters, control) {
  catalog <- .hm_engine_catalog()
  if (!model_id %in% names(catalog) ||
        !task$type %in% catalog[[model_id]]) {
    .hm_abort(
      "Model {.val {model_id}} cannot fit this task.",
      "hm_capability_error"
    )
  }
  if (startsWith(model_id, "reference:")) {
    return(.hm_fit_deep(model_id, x, y, task, parameters, control))
  }
  if (!is.matrix(x) ||
        !is.numeric(x) ||
        any(!is.finite(x))) {
    .hm_abort("Model input must be a finite numeric matrix.")
  }
  if (nrow(x) <
        2L || ncol(x) <
        1L) {
    .hm_abort("Training data is too small.")
  }
  weights <- control$weights
  if (!is.null(weights)) {
    if (!is.numeric(weights) ||
          length(weights) !=
            nrow(x) ||
          any(!is.finite(weights)) ||
          any(weights < 0) ||
          sum(weights) <=
            0) {
      .hm_abort(paste0(
        "Training weights must be finite, nonnegative and have ",
        "positive sum."
      ))
    }
    supported <- c(
      "lm", "glm", "glmnet", "gam", "mixed", "rf", "xgboost", "coxph",
      "clm"
    )
    if (!model_id %in% supported) {
      .hm_abort(
        "This engine does not implement weighted fitting.",
        "hm_capability_error"
      )
    }
  }
  classes <- NULL
  if (task$type %in% c("classification", "ordinal")) {
    if (anyNA(y)) {
      .hm_abort("Training labels cannot be missing.")
    }
    if (task$type == "ordinal" && !is.ordered(y)) {
      .hm_abort("Ordinal outcomes require an ordered factor.")
    }
    if (!is.factor(y)) {
      y <- factor(y)
    }
    classes <- levels(y)
    if (length(classes) <
      2L || any(
      table(y) ==
        0L
    )) {
      .hm_abort("Every declared class needs training support.")
    }
  }
  if (task$type == "regression" && (!is.numeric(y) ||
                                      length(y) !=
                                        nrow(x) ||
                                      any(!is.finite(y)))) {
    .hm_abort("Regression outcomes must be finite numeric values.")
  }
  engine <- list(
    model_id = model_id, task_type = task$type, parameters = parameters,
    classes = classes, n_features = ncol(x),
    training_hash = .hm_hash(list(x, y)),
    object = NULL
  )
  if (model_id %in% c("physics_inverse", "physics_hybrid")) {
    return(.hm_fit_physics(engine, x, y, control))
  }
  if (model_id %in% c(
    "lm", "glm", "glmnet", "gam", "mixed", "pls", "pcr", "bayes_conjugate",
    "brms"
  )) {
    return(.hm_fit_statistical(engine, x, y, weights, control))
  }
  if (model_id %in% c(
    "pca", "ica", "nmf", "mcr_als", "fcls",
    "beer_lambert"
  )) {
    return(.hm_fit_chemometric(engine, x, y, control))
  }
  .hm_fit_classical(engine, x, y, weights, control)
}

.hm_predict_engine <- function(engine, x, task, ...) {
  if (!is.matrix(x) ||
        !is.numeric(x) ||
        any(!is.finite(x)) ||
        ncol(x) !=
          engine$n_features) {
    .hm_abort("Prediction input is nonfinite or has the wrong band count.")
  }
  if (startsWith(engine$model_id, "reference:")) {
    return(.hm_predict_deep(engine, x, ...))
  }
  id <- engine$model_id
  if (id %in% c("physics_inverse", "physics_hybrid")) {
    return(.hm_predict_physics(engine, x))
  }
  if (id %in% c(
    "lm", "glm", "glmnet", "gam", "mixed", "pls", "pcr", "bayes_conjugate",
    "brms"
  )) {
    return(.hm_predict_statistical(engine, x, ...))
  }
  if (id %in% c("pca", "ica", "nmf", "mcr_als", "fcls", "beer_lambert")) {
    return(.hm_predict_chemometric(engine, x))
  }
  .hm_predict_classical(engine, x, ...)
}

.hm_probabilities <- function(p, classes, n) {
  if (is.null(dim(p))) {
    if (length(classes) !=
          2L || length(p) !=
          n) {
      .hm_abort(
        "Backend returned an invalid probability shape.",
        "hm_backend_output_error"
      )
    }
    p <- cbind(1 - p, p)
    colnames(p) <- classes
  }
  if (!is.matrix(p) || ncol(p) != length(classes)) {
    .hm_abort("Backend returned an invalid probability shape.",
              "hm_backend_output_error")
  }
  if (is.null(colnames(p))) {
    colnames(p) <- classes
  }
  if (!setequal(
    colnames(p),
    classes
  ) ||
    nrow(p) !=
      n) {
    .hm_abort(
      "Backend class map differs from the trained map.",
      "hm_backend_output_error"
    )
  }
  p <- p[, classes, drop = FALSE]
  if (any(!is.finite(p)) ||
    any(p < 0) ||
    any(p > 1) ||
    any(
      abs(
        rowSums(p) -
          1
      ) >
        1e-06
    )) {
    .hm_abort("Backend probabilities are invalid.", "hm_backend_output_error")
  }
  p
}
