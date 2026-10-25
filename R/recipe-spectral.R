.hm_apply_step <- function(s, x, wavelength) {
  t <- s$type
  if (t == "impute") {
    for (j in seq_len(ncol(x))) {
      x[
        is.na(x[, j]),
        j
      ] <- s$center[j]
    }
    return(x)
  }
  if (t == "scale") {
    return(
      sweep(
        sweep(x, 2, s$center, "-"),
        2, s$scale, "/"
      )
    )
  }
  if (t == "pca") {
    return(
      sweep(x, 2, s$center, "-") %*%
        s$rotation
    )
  }
  if (t == "select") {
    return(x[, s$bands, drop = FALSE])
  }
  if (any(!is.finite(x))) {
    .hm_abort("Spectral transforms require finite values.")
  }
  if (t == "normalize") {
    norm <- sqrt(rowSums(x * x))
    if (any(norm == 0)) {
      .hm_abort("Cannot normalize zero spectra.")
    }
    return(x / norm)
  }
  if (t == "snv") {
    v <- apply(x, 1, stats::sd)
    if (any(
      !is.finite(v) |
        v == 0
    )) {
      .hm_abort("SNV requires nonconstant spectra.")
    }
    return((x - rowMeans(x)) / v)
  }
  if (t == "msc") {
    z <- t(
      vapply(
        seq_len(nrow(x)),
        function(i) {
          fit <- stats::lm.fit(
            cbind(1, s$reference),
            x[i, ]
          )
          b <- fit$coefficients
          if (any(!is.finite(b)) ||
                b[2] == 0) {
            .hm_abort("MSC reference is not identifiable.")
          }
          (x[i, ] - b[1]) / b[2]
        }, numeric(ncol(x))
      )
    )
    return(z)
  }
  if (t == "reflectance") {
    if (!is.numeric(s$dark) ||
          !is.numeric(s$white) ||
          length(s$dark) !=
            ncol(x) ||
          length(s$white) !=
            ncol(x) ||
          any(!is.finite(s$white - s$dark)) ||
          any(s$white <= s$dark)) {
      .hm_abort("Explicit valid dark and white references required.")
    }
    return(
      sweep(
        sweep(x, 2, s$dark, "-"),
        2, s$white - s$dark, "/"
      )
    )
  }
  if (t %in% c("derivative", "resample", "continuum")) {
    if (!is.numeric(wavelength) ||
      length(wavelength) !=
        ncol(x) ||
      any(!is.finite(wavelength)) ||
      any(
        diff(wavelength) <=
          0
      )) {
      .hm_abort("Strictly increasing measured wavelength grid required.")
    }
    if (t == "derivative") {
      return(
        sweep(
          x[, -1, drop = FALSE] - x[, -ncol(x),
            drop = FALSE
          ], 2, diff(wavelength),
          "/"
        )
      )
    }
    if (t == "resample") {
      if (!is.numeric(s$wavelength) ||
        !length(s$wavelength) ||
        anyNA(s$wavelength) ||
        any(
          diff(s$wavelength) <=
            0
        ) ||
        any(
          s$wavelength < min(wavelength) |
            s$wavelength > max(wavelength)
        )) {
        .hm_abort("Resampling cannot extrapolate missing spectral support.")
      }
      return(
        t(
          vapply(
            seq_len(nrow(x)),
            function(i) {
              stats::approx(wavelength, x[i, ],
                xout = s$wavelength
              )$y
            },
            numeric(length(s$wavelength))
          )
        )
      )
    }
    return(
      t(
        vapply(
          seq_len(nrow(x)),
          function(i) {
            # Upper concave envelope, retaining only clockwise turns.
            keep <- integer()
            for (j in seq_along(wavelength)) {
              keep <- c(keep, j)
              while (length(keep) >=
                       3) {
                a <- tail(keep, 3)
                slopes <- diff(x[i, a]) / diff(wavelength[a])
                if (slopes[1] >= slopes[2]) break
                keep <- keep[-(length(keep) -
                                 1)]
              }
            }
            envelope <- stats::approx(wavelength[keep], x[i, keep],
              xout = wavelength
            )$y
            if (any(envelope <= 0)) .hm_abort("Continuum must be positive.")
            x[i, ] / envelope
          }, numeric(ncol(x))
        )
      )
    )
  }
  .hm_abort("Unsupported preprocessing step.", "hm_unsupported_error")
}
