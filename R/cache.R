#' Compute a complete cache identity
#' @param data Data, including all source values and band metadata.
#' @param state Reader, calibration, model and preprocessing state.
#' @return SHA-256 identity. Lazy sources include declared content hashes.
#' @examples
#' data <- hm_example()
#' key <- hm_cache_key(data, list(recipe = "scale", model = "lm"))
#' key
#' # Any change of state yields a different identity
#' identical(key, hm_cache_key(data, list(recipe = "snv", model = "lm")))
#' @export
hm_cache_key <- function(data, state = list()) {
  if (!.hm_plain(state)) {
    .hm_abort("Cache state must contain no live handles.")
  }
  if (methods::is(data, "HMDataset")) {
    methods::validObject(data)
    for (source in data@sources) {
      if (!is.numeric(source)) {
        if (!file.exists(source$locator) ||
          !identical(
            digest::digest(file = source$locator, algo = "sha256"),
            source$hash
          )) {
          .hm_abort(
            "Lazy source changed before cache lookup.",
            "hm_source_error"
          )
        }
      }
    }
  }
  .hm_hash(list(schema = 1L, data = data, state = state))
}
#' Create independent in-memory cached values
#' @return A list with get and set functions; get returns an independent copy.
#' @examples
#' cache <- hm_cache()
#' key <- hm_cache_key(hm_example(), list(recipe = "scale"))
#' cache$set(key, 1:3)
#' cache$get(key)
#' @export
hm_cache <- function() {
  store <- new.env(parent = emptyenv())
  list(
    set = function(key, value) {
      .hm_scalar_character(key, "key")
      if (!.hm_plain(value)) .hm_abort("Cached values must be serializable.")
      assign(
        key, serialize(value, NULL, version = 3),
        envir = store
      )
      invisible(key)
    }, get = function(key) {
      .hm_scalar_character(key, "key")
      if (!exists(key, envir = store, inherits = FALSE)) {
        return(NULL)
      }
      unserialize(get(key, envir = store, inherits = FALSE))
    }
  )
}
