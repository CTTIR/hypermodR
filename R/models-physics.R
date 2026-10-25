.hm_fit_beer_lambert <- function(engine, x, control) {
  p <- engine$parameters
  library <- p$library
  if (!inherits(library, "hm_spectral_library") ||
        nrow(library$values) !=
          ncol(x)) {
    .hm_abort(paste0(
      "A wavelength-matched, declared extinction library is ",
      "required."
    ))
  }
  if (!identical(library$wavelength, control$wavelength) ||
        !identical(control$input_provenance$radiometric_unit,
                   "reflectance") ||
        is.null(control$input_provenance$calibration) ||
        identical(control$input_provenance$calibration, "unknown")) {
    .hm_abort(
      paste0(
        "Beer-Lambert needs matched wavelengths and explicitly ",
        "calibrated reflectance."
      )
    )
  }
  if (!identical(
    library$hash, .hm_hash(
      library[setdiff(
        names(library),
        "hash"
      )]
    )
  )) {
    .hm_abort(
      "Spectral library changed after declaration.",
      "hm_integrity_error"
    )
  }
  path_length <- p$path_length
  if (is.null(path_length) ||
        length(path_length) !=
          1L || !is.finite(path_length) ||
        path_length <= 0) {
    .hm_abort("Declare positive path_length.")
  }
  if (qr(library$values)$rank !=
        ncol(library$values)) {
    .hm_abort(
      "The supplied extinction design is not identifiable.",
      "hm_identifiability_error"
    )
  }
  engine$object <- list(
    design = library$values * path_length, library = library,
    assumptions = "supplied path length; scattering already corrected",
    uncertainty = NA_real_
  )
  engine
}

.hm_predict_beer_lambert <- function(engine, x) {
  if (any(x <= 0) ||
        any(x > 1)) {
    .hm_abort("Beer-Lambert input must be declared reflectance in (0, 1].")
  }
  t(
    vapply(
      seq_len(nrow(x)),
      function(i) {
        .hm_nnls(engine$object$design, -log(x[i, ]))
      }, numeric(ncol(engine$object$design))
    )
  )
}
