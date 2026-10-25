#' Exchange bounded typed arrays with an owned worker
#' @param handle Managed process handle.
#' @param operation One of handshake, predict, extract, fit or cancel.
#' @param array Numeric array; dimensions and channel metadata are explicit.
#' @param channels Expected measured channel identifiers.
#' @param timeout Maximum seconds.
#' @param max_bytes Maximum request or response bytes.
#' @param metadata Additional JSON metadata.
#' @return Decoded worker response list; any array payload is reconstructed as
#' a finite numeric R array in declared dimensions. Cancellation returns
#' backend status invisibly.
#' @export
hm_worker_request <- function(
  handle, operation = "handshake", array = NULL, channels = NULL,
  timeout = 30,
  max_bytes = 16 * 1024^2, metadata = list()
) {
  .hm_owned_handle(handle)
  if (!operation %in% c("handshake", "predict", "extract", "fit", "cancel") ||
        !is.numeric(timeout) ||
        length(timeout) !=
          1L || !is.finite(timeout) ||
        timeout <= 0 || !is.numeric(max_bytes) ||
        length(max_bytes) !=
          1L || !is.finite(max_bytes) ||
        max_bytes < 128) {
    .hm_abort("Invalid worker operation or bounds.")
  }
  if (operation == "cancel") {
    return(hm_stop_backend(handle))
  }
  req <- list(
    protocol = "hypermodr-worker/1", id = .hm_hash(list(
      Sys.time(),
      Sys.getpid(), operation
    )),
    operation = operation, model = handle$model,
    plan_digest = handle$plan_digest,
    metadata = metadata
  )
  if (!length(metadata)) {
    req$metadata <- NULL
  }
  if (!is.null(array)) {
    if (!is.numeric(array) ||
      length(array) >
        max_bytes / 8 || any(!is.finite(array)) ||
      is.null(dim(array)) ||
      any(
        dim(array) <=
          0
      ) ||
      is.null(channels) ||
      length(channels) !=
        tail(
          dim(array),
          1L
        ) ||
      anyNA(channels) ||
      anyDuplicated(channels)) {
      .hm_abort("Invalid finite typed array, shape or channel metadata.")
    }
    req$array <- list(
      dtype = "float64", order = "F", shape = as.list(dim(array)),
      channels = as.list(channels),
      values = as.list(as.numeric(array))
    )
    req$input_digest <- .hm_hash(list(array, channels))
  }
  payload <- jsonlite::toJSON(req,
    auto_unbox = TRUE, digits = NA,
    null = "null"
  )
  if (nchar(payload, type = "bytes") >
        max_bytes) {
    .hm_abort("Worker request exceeds byte limit.")
  }
  handle$process$write_input(paste0(payload, "\n"))
  started <- proc.time()[[3]]
  response <- ""
  diagnostic_bytes <- 0
  repeat {
    if (proc.time()[[3]] - started > timeout) {
      hm_stop_backend(handle)
      .hm_abort("Worker timed out and was stopped.", "hm_timeout_error")
    }
    handle$process$poll_io(20L)
    chunk <- handle$process$read_output()
    response <- paste0(response, chunk)
    # Drain bounded diagnostics without forwarding potentially sensitive
    # worker text.
    errors <- handle$process$read_error()
    diagnostic_bytes <- diagnostic_bytes + nchar(errors, type = "bytes")
    if (diagnostic_bytes > max_bytes || nchar(response, type = "bytes") >
          max_bytes) {
      hm_stop_backend(handle)
      .hm_abort("Worker response exceeds byte limit.")
    }
    if (grepl("\n", response, fixed = TRUE)) {
      break
    }
    if (!handle$process$is_alive()) {
      .hm_abort("Worker exited with a partial response.", "hm_worker_error")
    }
  }
  lines <- strsplit(response, "\n", fixed = TRUE)[[1]]
  if (length(lines) !=
        1L) {
    hm_stop_backend(handle)
    .hm_abort("Unexpected extra worker output.")
  }
  ans <- tryCatch(
    jsonlite::fromJSON(lines[[1]], simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(ans) ||
        !identical(ans$protocol, req$protocol) ||
        !identical(ans$id, req$id) ||
        !identical(ans$model, req$model) ||
        !identical(ans$plan_digest, req$plan_digest) ||
        !identical(ans$status, "ok")) {
    hm_stop_backend(handle)
    .hm_abort(
      "Worker identity, protocol or operation failure.",
      "hm_worker_error"
    )
  }
  if (operation == "handshake" && !all(
    c("fit", "predict", "extract", "cancel") %in%
      unlist(ans$operations)
  )) {
    hm_stop_backend(handle)
    .hm_abort("Worker capability handshake failed.")
  }
  if (!is.null(ans$array)) {
    a <- ans$array
    shape <- unlist(a$shape)
    values <- unlist(a$values)
    if (!identical(a$dtype, "float64") ||
      !identical(a$order, "F") ||
      !length(shape) ||
      any(
        !is.finite(shape) |
          shape <= 0 | shape != floor(shape)
      ) ||
      prod(shape) !=
        length(values) ||
      length(values) >
        max_bytes / 8 || !is.numeric(values) ||
      any(!is.finite(values)) ||
      length(unlist(a$channels)) !=
        tail(shape, 1L)) {
      hm_stop_backend(handle)
      .hm_abort("Malformed worker output array.")
    }
    ans$array <- array(values, dim = shape)
  }
  ans
}
