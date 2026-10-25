#' Bounded causal frame streaming
#' @param predict_frame Function accepting a frame and its timestamp.
#' @param max_frames Maximum frames before explicit reset.
#' @return Mutable stream handle, with per-frame outputs and latency
#' diagnostics.
#' @export
hm_stream <- function(predict_frame, max_frames = 256L) {
  if (!is.function(predict_frame) ||
        length(max_frames) !=
          1L || !is.finite(max_frames) ||
        max_frames < 1 || max_frames != as.integer(max_frames)) {
    .hm_abort("Invalid stream callback or bound.")
  }
  e <- new.env(parent = emptyenv())
  e$predict_frame <- predict_frame
  e$max_frames <- as.integer(max_frames)
  e$sequence <- NULL
  e$time <- -Inf
  e$count <- 0L
  e$latency <- numeric()
  e$cancelled <- FALSE
  class(e) <- "hm_stream"
  e
}
#' Push ordered frames into a stream
#' @param stream An hm_stream.
#' @param frames List of native frames; geometry may vary.
#' @param timestamps Strictly increasing finite elapsed seconds.
#' @param sequence_id Explicit recording identity.
#' @return Outputs in source order, with timestamp and latency metadata.
#' @export
hm_stream_push <- function(stream, frames, timestamps, sequence_id) {
  if (!inherits(stream, "hm_stream") ||
        stream$cancelled) {
    .hm_abort("Stream unavailable or cancelled.")
  }
  .hm_scalar_character(sequence_id, "sequence_id")
  if (!is.list(frames) ||
    !is.numeric(timestamps) ||
    length(frames) !=
      length(timestamps) ||
    any(!is.finite(timestamps)) ||
    any(
      diff(timestamps) <=
        0
    )) {
    .hm_abort(paste0(
      "Frames require finite ordered timestamps; preserve ",
      "gaps explicitly."
    ))
  }
  if (!identical(sequence_id, stream$sequence)) {
    stream$sequence <- sequence_id
    stream$time <- -Inf
    stream$count <- 0L
    stream$latency <- numeric()
  }
  if (length(timestamps) &&
        timestamps[1] <= stream$time) {
    .hm_abort("Duplicate or reversed frame timestamp.")
  }
  if (stream$count + length(frames) >
        stream$max_frames) {
    .hm_abort("Stream frame limit reached; explicit new sequence required.")
  }
  out <- vector("list", length(frames))
  for (i in seq_along(frames)) {
    started <- proc.time()[[3]]
    out[[i]] <- stream$predict_frame(frames[[i]], timestamps[i])
    stream$latency <- c(stream$latency, proc.time()[[3]] - started)
    stream$time <- timestamps[i]
    stream$count <- stream$count + 1L
  }
  structure(
    out,
    timestamps = timestamps, sequence_id = sequence_id, dropped = 0L,
    real_time_qualified = FALSE
  )
}
#' Cancel a frame stream
#' @param stream An hm_stream.
#' @return The cancelled stream invisibly.
#' @export
hm_stream_cancel <- function(stream) {
  if (!inherits(stream, "hm_stream")) {
    .hm_abort("Invalid stream.")
  }
  stream$cancelled <- TRUE
  invisible(stream)
}
