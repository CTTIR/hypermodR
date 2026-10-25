#' Index a portable spectral sequence without repairing chronology
#' @param frames Numeric cubes or admitted RDS locators.
#' @param frame_index Explicit frame table; time is source elapsed seconds,
#' including missing or reversed values.
#' @param observations Observation identities.
#' @param bands Native band metadata.
#' @param geometry Source grid.
#' @param provenance Source evidence.
#' @return HMSequence retaining original ordering and timing quality.
#' @export
hm_portable_sequence <- function(
  frames, frame_index, observations, bands,
  geometry, provenance = list()
) {
  if (!is.numeric(frame_index$time)) {
    .hm_abort("Explicit numeric source time required.")
  }
  t <- frame_index$time
  provenance$timing_quality <- list(
    missing = which(is.na(t)),
    duplicate = which(
      duplicated(t) &
        !is.na(t)
    ),
    reversed = which(
      diff(t) <
        0
    ) +
      1L, nominal_fps = NA_real_, time_policy = "source values unchanged"
  )
  hm_sequence(frames, frame_index, observations, bands, geometry, provenance)
}
