#' Convert a qualified companion object
#' @param x Companion object.
#' @param profile Explicit source profile where required.
#' @param ... Adapter-specific arguments.
#' @return HMDataset or HMSequence for an admitted class; unsupported classes
#' and
#'   currently unqualified CUVIS sources raise typed errors.
#' @details hyperspectR cube conversion preserves measured bands, masks and
#' declared
#'   geometry. A cube exported by this package also retains selected-frame
#'   observations and band metadata; recorded content identity must still match
#'   on reimport. Explicit observations override restored identities.
#'   hyperannotatR conversion uses a bounded public tile and records
#'   manifest/revision identity. CUVIS session enumeration and SDK scaling
#' remain
#'   blocked pending source-backed fixtures. Conversion does not infer
#' calibration.
#' @examples
#' \donttest{
#' if (requireNamespace("hyperspectR", quietly = TRUE)) {
#'   cube <- hyperspectR::hs_example_cube()
#'   data <- hm_as_dataset(cube)
#'   dim(data@sources[[1]])
#' }
#' }
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
.hm_cube_identity <- function(x) {
  restoration <- x$metadata$hypermodR
  restoration$cube_identity <- NULL
  .hm_hash(list(
    data = x$data, wavelengths = x$wavelengths, fwhm = x$fwhm,
    mask = x$mask, restoration = restoration
  ))
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
  stored <- x$metadata$hypermodR
  bound <- !is.null(stored$cube_identity)
  if (bound && !identical(stored$cube_identity, .hm_cube_identity(x))) {
    .hm_abort("Cube content or restoration metadata changed.",
              "hm_source_error")
  }
  if (missing(observations) && bound &&
        !is.null(stored$source_observations)) {
    observations <- stored$source_observations
  }
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
  if (bound && !is.null(p$native_band_map)) {
    native_bands <- .hm_bands(p$native_band_map, nrow(b))
    selected <- match(p$sorted_to_native, native_bands$native_index)
    if (length(selected) != nrow(b) || anyNA(selected) ||
          anyDuplicated(selected) ||
          !isTRUE(all.equal(native_bands$wavelength[selected], x$wavelengths,
                            check.attributes = FALSE))) {
      .hm_abort("Stored band identities disagree with the cube.",
                "hm_source_error")
    }
    b <- native_bands[selected, , drop = FALSE]
  }
  hm_dataset(x$data, observations, b, geometry, p)
}
#' Materialise one frame through hyperspectR
#' @param x Spatial HMDataset.
#' @param acquisition_id One acquisition ID.
#' @param frame_id One frame ID for sequences; NULL for an image.
#' @param ... Reserved.
#' @return An hsi_cube with explicit native band sorting map in metadata.
#' @examples
#' image <- hm_dataset(
#'   array(seq_len(60) / 60, c(3, 5, 4)),
#'   data.frame(observation_id = "o1", image_id = "i1", acquisition_id = "a1"),
#'   data.frame(wavelength = c(500, 600, 700, 800)),
#'   geometry = list(origin = c(0, 0), transform = diag(3))
#' )
#' \donttest{
#' if (requireNamespace("hyperspectR", quietly = TRUE)) {
#'   cube <- hm_as_hsi_cube(image, acquisition_id = "a1")
#'   print(class(cube))
#'   # The round trip restores the native dataset
#'   hm_as_dataset(cube)@data_kind
#' }
#' }
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
  p$source_observations <- z$observations
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
  p$spatial_masks <- list(mask)
  m$hypermodR <- p
  if (!is.null(mask)) {
    # hyperspectR (>= 0.2.0) rejects NA in masks. Unknown pixels are not
    # usable, as in hm_read_block(); the native mask stays in metadata.
    mask <- mask & !is.na(mask)
  }
  out <- hyperspectR::hsi_cube(
    array(z$values, dim(z$values)[1:3]),
    b$wavelength, fwhm, m, mask
  )
  out$metadata$hypermodR$cube_identity <- .hm_cube_identity(out)
  out
}
