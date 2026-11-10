#' Index a portable spectral sequence without repairing chronology
#' @param frames Numeric cubes or admitted RDS locators.
#' @param frame_index Explicit frame table; time is source elapsed seconds,
#' including missing or reversed values.
#' @param observations Observation identities.
#' @param bands Native band metadata.
#' @param geometry Source grid.
#' @param provenance Source evidence.
#' @return HMSequence retaining original ordering and timing quality.
#' @examples
#' frames <- list(array(1:60, c(3, 5, 4)), array(61:120, c(3, 5, 4)))
#' frame_index <- data.frame(
#'   frame_id = c("f1", "f2"), sequence_id = "s1", acquisition_id = "a1",
#'   time = c(0, 2.5)
#' )
#' observations <- data.frame(
#'   observation_id = c("o1", "o2"), image_id = c("i1", "i2"),
#'   acquisition_id = "a1", frame_id = c("f1", "f2")
#' )
#' bands <- data.frame(wavelength = c(500, 600, 700, 800))
#' geometry <- list(origin = c(0, 0), transform = diag(3))
#' # Reversed source timestamps are retained and flagged, not repaired
#' frame_index$time <- c(2.5, 0)
#' sequence <- hm_portable_sequence(frames, frame_index, observations, bands,
#'                                  geometry)
#' sequence@frame_index
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
