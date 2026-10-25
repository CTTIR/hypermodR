.hm_learn_step <- function(s, x) {
  if (s$type == "impute") {
    s$center <- colMeans(x, na.rm = TRUE)
    if (any(!is.finite(s$center))) {
      .hm_abort("Cannot impute an entirely missing training band.")
    }
  }
  if (s$type == "scale") {
    if (any(!is.finite(x))) {
      .hm_abort("Scaling requires finite training values.")
    }
    s$center <- colMeans(x)
    s$scale <- apply(x, 2, stats::sd)
    s$scale[s$scale == 0 | is.na(s$scale)] <- 1
  }
  if (s$type == "pca") {
    if (any(!is.finite(x))) {
      .hm_abort("PCA requires finite training values.")
    }
    p <- stats::prcomp(x, center = TRUE, scale. = FALSE)
    k <- if (is.null(s$components)) {
      ncol(p$rotation)
    } else {
      s$components
    }
    if (length(k) !=
          1 || k < 1 || k > ncol(p$rotation) ||
          k != as.integer(k)) {
      .hm_abort("Invalid PCA component count.")
    }
    s$center <- p$center
    s$rotation <- p$rotation[, seq_len(k),
      drop = FALSE
    ]
  }
  if (s$type == "select") {
    if (!is.numeric(s$bands) ||
      !length(s$bands) ||
      anyNA(s$bands) ||
      anyDuplicated(s$bands) ||
      any(
        s$bands < 1 | s$bands > ncol(x) |
          s$bands != as.integer(s$bands)
      )) {
      .hm_abort("Invalid explicitly selected bands.")
    }
  }
  if (s$type == "msc") {
    s$reference <- colMeans(x)
  }
  s
}
