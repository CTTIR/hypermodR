.hm_abort <- function(message, class = "hm_validation_error", ...) {
  cli::cli_abort(
    message,
    class = c(class, "hm_error"),
    .envir = parent.frame(), ...
  )
}

.hm_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    .hm_abort(
      "Optional package {.pkg {package}} is required for this operation.",
      "hm_dependency_error"
    )
  }
  invisible(TRUE)
}

.hm_hash <- function(object) {
  digest::digest(object, algo = "sha256", serialize = TRUE)
}

.hm_scalar_character <- function(x, name) {
  if (!is.character(x) ||
        length(x) !=
          1L || is.na(x) ||
        !nzchar(x)) {
    .hm_abort("{name} must be one nonempty character string.")
  }
  invisible(x)
}

.hm_seed <- function(seed, code) {
  if (!is.numeric(seed) ||
        length(seed) !=
          1L || is.na(seed) ||
        !is.finite(seed) ||
        seed != as.integer(seed) ||
        seed < 0) {
    .hm_abort("seed must be one nonnegative integer.")
  }
  # Restore the caller's random-number state; only `.Random.seed` is touched.
  present <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (present) {
    old <- get(".Random.seed", envir = .GlobalEnv)
  }
  on.exit({
    if (present) {
      # nolint start: object_name_linter.
      assign(".Random.seed", old, envir = .GlobalEnv)
      # nolint end
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(seed)
  force(code)
}
