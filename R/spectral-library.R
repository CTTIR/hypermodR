#' Declare an extinction or response library
#'
#' Values must be supplied from measured or otherwise justified sources. This
#' constructor records a declaration; it cannot verify the physical reference.
#' @param values Numeric wavelength-by-component matrix.
#' @param wavelength Strictly increasing wavelength centres in nm.
#' @param units Character description of coefficient units.
#' @param source Character source citation or persistent identifier.
#' @param license Character declaration of reuse terms.
#' @return An immutable-by-identity `hm_spectral_library` list.
#' @export
hm_spectral_library <- function(values, wavelength, units, source, license) {
  if (!is.matrix(values) ||
    !is.numeric(values) ||
    any(!is.finite(values)) ||
    any(values < 0) ||
    nrow(values) !=
      length(wavelength) ||
    !is.numeric(wavelength) ||
    any(!is.finite(wavelength)) ||
    any(
      diff(wavelength) <=
        0
    )) {
    .hm_abort("Invalid spectral library grid or values.")
  }
  .hm_scalar_character(units, "units")
  .hm_scalar_character(source, "source")
  .hm_scalar_character(license, "license")
  out <- list(
    values = values, wavelength = wavelength, units = units, source = source,
    license = license
  )
  out$hash <- .hm_hash(out)
  structure(out, class = "hm_spectral_library")
}
