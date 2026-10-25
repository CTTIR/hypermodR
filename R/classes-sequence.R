.hm_timing_quality <- function(f) {
  quality <- rep("ordered", nrow(f))
  quality[is.na(f$time)] <- "unavailable"
  groups <- interaction(f$sequence_id, f$acquisition_id, drop = TRUE)
  for (idx in split(
    seq_len(nrow(f)),
    groups
  )) {
    t <- f$time[idx]
    dup <- !is.na(t) &
      (duplicated(t) |
         duplicated(t, fromLast = TRUE))
    quality[idx[dup]] <- "duplicate"
    if (length(idx) >
          1L) {
      reversed <- which(
        !is.na(diff(t)) &
          diff(t) <
            0
      ) +
        1L
      quality[idx[reversed]] <- "reversed"
    }
  }
  quality
}
.hm_valid_sequence <- function(object) {
  tryCatch(
    {
      f <- object@frame_index
      .hm_table(f, "frame_id")
      if (!all(
        c("sequence_id", "acquisition_id", "time") %in%
          names(f)
      ) ||
        nrow(f) !=
          length(object@sources)) {
        .hm_abort(
          paste0(
            "Frame index requires frame_id, sequence_id, ",
            "acquisition_id and explicit time for every source."
          )
        )
      }
      if (!is.numeric(f$time) ||
            any(is.infinite(f$time))) {
        .hm_abort(paste0(
          "Frame time must be numeric elapsed seconds; missing ",
          "times remain NA."
        ))
      }
      expected <- .hm_timing_quality(f)
      if (!is.character(f$timing_quality) ||
        length(f$timing_quality) !=
          nrow(f) ||
        anyNA(f$timing_quality) ||
        any(
          !f$timing_quality %in% c(
            "ordered", "unavailable", "duplicate",
            "reversed"
          )
        ) ||
        any(f$timing_quality == "ordered" & expected != "ordered")) {
        .hm_abort(
          paste0(
            "Frame timing_quality must explicitly retain ",
            "unavailable, duplicate or reversed timing."
          )
        )
      }
      if (!"frame_id" %in% names(object@observations) ||
            any(!object@observations$frame_id %in% f$frame_id)) {
        .hm_abort("Observations must link to known frames.")
      }
      linked <- match(object@observations$frame_id, f$frame_id)
      if (any(
        object@observations$acquisition_id != f$acquisition_id[linked]
      )) {
        .hm_abort("Observation acquisition disagrees with frame identity.")
      }
      if (any(!f$frame_id %in% object@observations$frame_id)) {
        .hm_abort("Every frame must have observation linkage.")
      }
      TRUE
    },
    error = function(e) conditionMessage(e)
  )
}
#' Validated time-indexed spectral sequence
#' @slot frame_index Explicit frame identity and elapsed seconds; gaps are
#' retained.
#' @exportClass HMSequence
methods::setClass(
  "HMSequence",
  contains = "HMDataset", slots = c(frame_index = "data.frame"),
  validity = .hm_valid_sequence
)
#' Construct a spectral sequence
#' @param frames List of native y,x,wavelength arrays or declarative RDS
#' sources.
#' @param frame_index Frame metadata with character frame_id, sequence_id,
#' acquisition_id and numeric source time in elapsed seconds (NA allowed).
#' timing_quality marks ordered, unavailable, duplicate or reversed source
#' timing; source order is retained.
#' @param observations Observation metadata linked by frame_id.
#' @param bands Band metadata in nm.
#' @param geometry Explicit spatial geometry.
#' @param provenance Source and timing declarations.
#' @return An HMSequence.
#' @export
hm_sequence <- function(
  frames, frame_index, observations, bands, geometry,
  provenance = list()
) {
  if (!is.list(frames) ||
        !length(frames)) {
    .hm_abort("frames must be a nonempty list.")
  }
  .hm_table(frame_index, "frame_id")
  if (!all(
    c("sequence_id", "acquisition_id", "time") %in%
      names(frame_index)
  ) ||
    !is.numeric(frame_index$time)) {
    .hm_abort("Frame index needs identities and numeric source time.")
  }
  if (!"timing_quality" %in% names(frame_index)) {
    frame_index$timing_quality <- .hm_timing_quality(frame_index)
  }
  dims <- lapply(frames, .hm_source)
  if (any(
    lengths(dims) !=
      3L
  )) {
    .hm_abort("Every frame must be a spatial cube.")
  }
  if (!is.list(provenance)) {
    .hm_abort("provenance must be a list.")
  }
  if (is.null(provenance$radiometric_unit)) {
    provenance$radiometric_unit <- "unknown"
  }
  if (is.null(provenance$calibration)) {
    provenance$calibration <- "unknown"
  }
  methods::new(
    "HMSequence",
    sources = frames, observations = observations, bands = .hm_bands(
      bands,
      dims[[1]][3]
    ),
    geometry = geometry, provenance = provenance, data_kind = "sequence",
    assets = data.frame(), frame_index = frame_index
  )
}
