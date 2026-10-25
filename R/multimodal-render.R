#' Render a declared three-band auxiliary image
#' @param cube Finite height by width by channel numeric array.
#' @param wavelengths Measured wavelength vector.
#' @param bands Three distinct channel indices.
#' @param limits Explicit common intensity range used for clipping and scaling.
#' @param source_id Source provenance identity.
#' @param timestamp Optional acquisition time.
#' @return An `hm_rendering` list with a height-by-width-by-3 image scaled to
#' `[0,1]` and a hash-bound rendering manifest.
#' @export
hm_render_hsi <- function(
  cube, wavelengths, bands, limits, source_id,
  timestamp = NULL
) {
  .hm_scalar_character(source_id, "source_id")
  d <- dim(cube)
  if (!is.numeric(cube) ||
    length(d) !=
      3L || any(d <= 0) ||
    any(!is.finite(cube)) ||
    !is.numeric(wavelengths) ||
    length(wavelengths) !=
      d[3] || any(!is.finite(wavelengths)) ||
    anyDuplicated(wavelengths) ||
    !is.numeric(bands) ||
    length(bands) !=
      3L || any(
    !is.finite(bands) |
      bands != floor(bands) |
      bands < 1 | bands > d[3]
  ) ||
    anyDuplicated(bands) ||
    !is.numeric(limits) ||
    length(limits) !=
      2L || any(!is.finite(limits)) ||
    limits[1] >= limits[2]) {
    .hm_abort(paste0(
      "Invalid cube, measured wavelengths, bands or explicit ",
      "scaling range."
    ))
  }
  selected <- cube[, , bands, drop = FALSE]
  rgb <- pmin(1, pmax(0, (selected - limits[1]) / diff(limits)))
  dim(rgb) <- c(d[1], d[2], 3L)
  manifest <- list(
    source_id = source_id, input_digest = .hm_hash(cube),
    original_shape = d, output_shape = dim(rgb),
    band_indices = bands, wavelengths = wavelengths[bands],
    colour_mapping = c("red", "green", "blue"),
    scaling_limits = limits, clipped_values = sum(
      selected < limits[1] | selected > limits[2]
    ),
    spatial_transform = "none", frame_selection = "single",
    timestamp = timestamp,
    discarded_bands = setdiff(
      seq_len(d[3]),
      bands
    ),
    information_loss = paste0(
      "Three channels only; intensity clipping/scaling; ",
      "auxiliary image cannot replace full-spectrum data."
    )
  )
  manifest$digest <- .hm_hash(manifest)
  structure(
    list(image = rgb, manifest = manifest),
    class = "hm_rendering"
  )
}
