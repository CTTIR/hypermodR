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
#' @details The `beer_lambert` engine solves
#' \eqn{-\ln R = L E c}{-ln(R) = L E c} for nonnegative component amounts
#' \eqn{c}, where \eqn{R} is calibrated reflectance, \eqn{L} the declared
#' `path_length` and \eqn{E} these library values. The attenuation uses the
#' natural logarithm, so library values must be natural-log (Napierian)
#' coefficients per unit of path length. Decadic coefficients, such as
#' tabulated molar extinction coefficients, must first be multiplied by
#' \eqn{\ln 10 \approx 2.303}{ln(10), about 2.303} (because
#' \eqn{10^{-A} = e^{-A \ln 10}}{10^-A = exp(-A ln(10))}) and converted to the
#' intended amount units; otherwise the retrieved amounts are too large by
#' that factor.
#' @examples
#' lib <- hm_spectral_library(
#'   cbind(component_a = c(1.2, 0.8, 0.3), component_b = c(0.9, 1.1, 0.7)),
#'   wavelength = c(450, 550, 750), units = "arbitrary",
#'   source = "synthetic illustration", license = "CC0"
#' )
#' lib$wavelength
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
