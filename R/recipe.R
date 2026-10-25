#' Declare preprocessing steps
#' @param steps List of step lists, each with a type and its required controls;
#' see Details.
#' @return An unfitted `hm_recipe` with native input track.
#' @details Steps execute in list order. Learned quantities are estimated only
#' by `hm_prep()` on its supplied training matrix. Supported types are impute
#' (training column means), scale (training means and standard deviations), pca
#' (optional integer components), select (explicit unique integer bands),
#' normalize (row Euclidean norm), snv (row mean/standard deviation), msc
#' (training mean reference), reflectance (explicit dark and white band
#' vectors), derivative (adjacent finite differences per nm), resample
#' (explicit wavelength grid within measured support), and continuum (positive
#' upper concave envelope division). Missing values must be imputed before
#' steps requiring finite data.
#' @export
hm_recipe <- function(steps = list()) {
  if (!is.list(steps) ||
        any(!vapply(steps, is.list, logical(1)))) {
    .hm_abort("steps must be a list of step specifications.")
  }
  structure(
    list(steps = steps, fitted = FALSE, track = "native"),
    class = "hm_recipe"
  )
}
#' Fit preprocessing on training spectra
#' @param recipe Unfitted `hm_recipe`.
#' @param training Numeric observation-by-feature training matrix.
#' @param wavelength Optional measured wavelength vector in nm, aligned to
#' input columns; required by wavelength-dependent steps.
#' @return A fitted `hm_recipe` containing learned step state, input/output
#' wavelength grids, feature count and training hash.
#' @details Call only on the training partition. Selection, derivatives,
#' resampling and PCA can change output dimensions; resampling marks the track
#' as adapted.
#' @export
hm_prep <- function(recipe, training, wavelength = NULL) {
  if (!inherits(recipe, "hm_recipe") ||
        recipe$fitted || !is.matrix(training) ||
        !is.numeric(training)) {
    .hm_abort("Expected an unfitted recipe and numeric training matrix.")
  }
  out <- recipe
  out$n_features <- ncol(training)
  out$wavelength <- wavelength
  x <- training
  for (i in seq_along(out$steps)) {
    s <- out$steps[[i]]
    if (is.null(s$type)) {
      .hm_abort("Every step requires type.")
    }
    s$input_wavelength <- wavelength
    s <- .hm_learn_step(s, x)
    x <- .hm_apply_step(s, x, wavelength)
    if (s$type == "select" && !is.null(wavelength)) {
      wavelength <- wavelength[s$bands]
    }
    if (s$type == "derivative") {
      wavelength <- (wavelength[-1] + wavelength[-length(wavelength)]) / 2
    }
    if (s$type == "resample") {
      wavelength <- s$wavelength
    }
    if (s$type == "pca") {
      wavelength <- NULL
    }
    out$steps[[i]] <- s
    if (s$type == "resample") {
      out$track <- "adapted"
    }
  }
  out$output_wavelength <- wavelength
  out$fitted <- TRUE
  out$training_hash <- .hm_hash(training)
  out
}
#' Apply immutable training transformations
#' @param recipe Fitted recipe returned by `hm_prep()`.
#' @param newdata Numeric observation-by-feature matrix with the original input
#' column count and ordering.
#' @return Numeric transformed matrix; rows retain input order and columns
#' follow the fitted steps.
#' @details Reuses frozen training transformations without learning from new
#' data.
#' @export
hm_bake <- function(recipe, newdata) {
  if (!inherits(recipe, "hm_recipe") ||
        !isTRUE(recipe$fitted) ||
        !is.matrix(newdata) ||
        !is.numeric(newdata) ||
        ncol(newdata) !=
          recipe$n_features) {
    .hm_abort("Fitted recipe and matching numeric matrix required.")
  }
  x <- newdata
  for (s in recipe$steps) x <- .hm_apply_step(s, x, s$input_wavelength)
  x
}
#' Explicitly pool all valid spatial pixels
#' @param x Spectral dataset accepted by `hm_read_block()`.
#' @param mask Optional logical array matching the block validity dimensions;
#' no missing entries.
#' @param method Pooling rule; currently only mean is supported.
#' @param selection Named block selection passed to `hm_read_block()`.
#' @return List with a one-row band matrix `values`, `band_map`, and provenance
#' including eligible pixel count, equal weights and mask hash.
#' @details All eligible finite pixels in the selected support contribute
#' equally; an empty eligible support is rejected.
#' @export
hm_pool <- function(x, mask = NULL, method = "mean", selection = list()) {
  if (!identical(method, "mean")) {
    .hm_abort(
      "Only explicit mean pooling is supported.",
      "hm_unsupported_error"
    )
  }
  block <- hm_read_block(x, selection)
  valid <- block$valid
  if (!is.null(mask)) {
    if (!is.logical(mask) ||
      !identical(
        dim(mask),
        dim(valid)
      ) ||
      anyNA(mask)) {
      .hm_abort("Pooling mask must match spatial/time support.")
    }
    valid <- valid & mask
  }
  n <- sum(valid)
  if (!n) {
    .hm_abort("No eligible pixels for pooling.")
  }
  v <- block$values
  values <- vapply(
    seq_len(dim(v)[3]),
    function(j) {
      z <- array(v[, , j, , drop = FALSE], dim = dim(valid))
      if (any(!is.finite(z[valid]))) {
        .hm_abort("Eligible pooling pixels must be finite.")
      }
      mean(z[valid])
    }, numeric(1)
  )
  list(
    values = matrix(values, nrow = 1),
    band_map = block$band_map, provenance = list(
      source = block$provenance, method = method, eligible_pixels = n,
      weights = rep(1 / n, n),
      mask_hash = .hm_hash(valid),
      track = "native"
    )
  )
}
