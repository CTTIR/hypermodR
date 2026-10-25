#' Declare supplied physical forward and inverse kernels
#' @param forward Function of concentrations and a parameter list, producing
#' spectra.
#' @param inverse Function of spectra and a parameter list, producing
#' concentrations.
#' @param parameters Serializable physical constants and assumptions.
#' @param source Source identifier for the implemented physical model.
#' @param units Named units for input and output quantities.
#' @return A declared `hm_physics_kernel`. Supplying a function does not
#' validate
#'   its physiological assumptions; independent standards remain necessary.
#' @export
hm_physics_kernel <- function(
  forward, inverse, parameters = list(),
  source, units
) {
  if (!is.function(forward) ||
        !is.function(inverse) ||
        !is.list(parameters) ||
        !.hm_plain(parameters) ||
        !is.character(units) ||
        length(units) <
          2L || is.null(names(units)) ||
        anyNA(units)) {
    .hm_abort("Invalid physical kernel declaration.")
  }
  .hm_scalar_character(source, "source")
  structure(
    list(
      forward = forward, inverse = inverse, parameters = parameters,
      source = source,
      units = units, code_hash = .hm_hash(
        list(
          body(forward),
          formals(forward),
          body(inverse),
          formals(inverse),
          parameters
        )
      )
    ),
    class = "hm_physics_kernel"
  )
}

.hm_kernel_predict <- function(kernel, x) {
  if (!inherits(kernel, "hm_physics_kernel") ||
    !identical(
      kernel$code_hash, .hm_hash(
        list(
          body(kernel$forward),
          formals(kernel$forward),
          body(kernel$inverse),
          formals(kernel$inverse),
          kernel$parameters
        )
      )
    )) {
    .hm_abort(
      "Physical kernel changed after declaration.",
      "hm_integrity_error"
    )
  }
  out <- kernel$inverse(x, kernel$parameters)
  if (!is.numeric(out) ||
        NROW(out) !=
          nrow(x) ||
        any(!is.finite(out))) {
    .hm_abort(
      "Physical inverse returned incompatible output.",
      "hm_backend_output_error"
    )
  }
  out
}

.hm_fit_physics <- function(engine, x, y, control) {
  kernel <- engine$parameters$kernel
  baseline <- .hm_kernel_predict(kernel, x)
  engine$object <- list(kernel = kernel)
  if (engine$model_id == "physics_hybrid") {
    if (!is.null(dim(baseline))) {
      .hm_abort("Residual hybrid reference requires one numeric endpoint.")
    }
    residual_id <- .hm_parameter(engine$parameters, "residual_model", "lm")
    if (residual_id %in% c("physics_hybrid", "physics_inverse")) {
      .hm_abort("Recursive physical hybrids are unsupported.")
    }
    engine$object$residual <- .hm_fit_engine(
      residual_id, x, y - baseline, list(type = "regression"),
      .hm_parameter(engine$parameters, "residual_parameters", list()),
      control
    )
  }
  engine
}

.hm_predict_physics <- function(engine, x) {
  baseline <- .hm_kernel_predict(engine$object$kernel, x)
  if (engine$model_id == "physics_hybrid") {
    baseline <- baseline + .hm_predict_engine(
      engine$object$residual, x,
      list(type = "regression")
    )
  }
  baseline
}
