#' Convert a qualified companion object
#' @param x Companion object.
#' @param profile Explicit source profile where required.
#' @param ... Adapter-specific arguments.
#' @return HMDataset or HMSequence for an admitted class; unsupported classes
#' and
#'   currently unqualified CUVIS sources raise typed errors.
#' @details hyperspectR cube conversion preserves measured bands, masks and
#' declared
#'   geometry; hyperannotatR conversion uses a bounded public tile and records
#'   manifest/revision identity. CUVIS session enumeration and SDK scaling
#' remain
#'   blocked pending source-backed fixtures. Conversion does not infer
#' calibration.
#' @export
hm_as_dataset <- function(x, profile = NULL, ...) UseMethod("hm_as_dataset")
#' @rdname hm_as_dataset
#' @export
hm_as_dataset.default <- function(x, profile = NULL, ...) {
  .hm_abort(
    "No admitted adapter for this object class.", "hm_unsupported_error"
  )
}
.hm_image_observations <- function(id = "image") {
  data.frame(
    observation_id = id, image_id = id, acquisition_id = id,
    stringsAsFactors = FALSE
  )
}
.hm_native_geometry <- function() {
  list(
    origin = c(0, 0),
    transform = diag(3),
    axes = c("y", "x"),
    coordinate_convention = "pixel centres", y_direction = "down"
  )
}
#' @rdname hm_as_dataset
#' @param observations Explicit acquisition and biological identities.
#' @param geometry Source spatial geometry.
#' @export
hm_as_dataset.hsi_cube <- function(
  x, profile = NULL, observations = .hm_image_observations(),
  geometry = .hm_native_geometry(),
  ...
) {
  .hm_require("hyperspectR")
  if (missing(geometry) &&
        !is.null(x$metadata$hypermodR$geometry)) {
    geometry <- x$metadata$hypermodR$geometry
  }
  # The public constructor validates the foreign structure without
  # modifying it.
  hyperspectR::hsi_cube(x$data, x$wavelengths, x$fwhm, x$metadata, x$mask)
  p <- list(
    adapter = "hyperspectR", package_version = as.character(
      utils::packageVersion("hyperspectR")
    ),
    companion_metadata = x$metadata, spatial_masks = list(x$mask),
    radiometric_unit = "unknown"
  )
  if (!is.null(x$metadata$hypermodR)) {
    p <- utils::modifyList(p, x$metadata$hypermodR)
  }
  # hyperspectR masks are binary. Restore the tri-state native mask written by
  # hm_as_hsi_cube() when it collapses exactly onto the cube mask.
  mask <- x$mask
  native <- x$metadata$hypermodR$spatial_masks
  if (!is.null(mask) && is.list(native) && length(native) == 1L &&
        is.logical(native[[1L]]) &&
        identical(dim(native[[1L]]), dim(mask)) &&
        isTRUE(all((native[[1L]] & !is.na(native[[1L]])) == mask))) {
    mask <- native[[1L]]
  }
  p$spatial_masks <- list(mask)
  b <- data.frame(wavelength = x$wavelengths)
  if (!is.null(p$sorted_to_native) &&
        length(p$sorted_to_native) ==
          nrow(b)) {
    b$native_index <- p$sorted_to_native
  }
  if (!is.null(x$fwhm)) {
    b$fwhm <- rep_len(x$fwhm, length(x$wavelengths))
  }
  hm_dataset(x$data, observations, b, geometry, p)
}
#' Materialise one frame through hyperspectR
#' @param x Spatial HMDataset.
#' @param acquisition_id One acquisition ID.
#' @param frame_id One frame ID for sequences; NULL for an image.
#' @param ... Reserved.
#' @return An hsi_cube with explicit native band sorting map in metadata.
#' @export
hm_as_hsi_cube <- function(x, acquisition_id, frame_id = NULL, ...) {
  .hm_require("hyperspectR")
  s <- list(acquisition_id = acquisition_id)
  if (methods::is(x, "HMSequence")) {
    if (!is.character(frame_id) ||
          length(frame_id) !=
            1L) {
      .hm_abort("Select one frame_id.")
    }
    s$frame_id <- frame_id
  } else if (!is.null(frame_id)) {
    .hm_abort("Images have no frame_id.")
  }
  z <- hm_read_block(x, s)
  if (dim(z$values)[4] !=
        1L) {
    .hm_abort("Select one frame.")
  }
  b <- z$band_map
  ord <- order(b$wavelength)
  p <- x@provenance
  p$native_band_map <- b
  p$sorted_to_native <- b$native_index[ord]
  p$inverse_sort <- order(ord)
  m <- p$companion_metadata
  if (is.null(m)) {
    m <- list()
  }
  p$companion_metadata <- NULL
  p$geometry <- x@geometry
  m$hypermodR <- p
  fwhm <- if ("fwhm" %in% names(b) &&
                all(is.finite(b$fwhm))) {
    b$fwhm
  } else {
    NULL
  }
  mask <- NULL
  if (!is.null(x@provenance$spatial_masks)) {
    i <- if (methods::is(x, "HMSequence")) {
      match(frame_id, x@frame_index$frame_id)
    } else {
      1L
    }
    mask <- x@provenance$spatial_masks[[i]]
  }
  if (!is.null(mask)) {
    # hyperspectR (>= 0.2.0) rejects NA in masks. Unknown pixels are not
    # usable, as in hm_read_block(); the native mask stays in metadata.
    mask <- mask & !is.na(mask)
  }
  hyperspectR::hsi_cube(
    array(z$values, dim(z$values)[1:3]),
    b$wavelength, fwhm, m, mask
  )
}
