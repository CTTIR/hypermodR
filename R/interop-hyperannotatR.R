#' @rdname hm_as_dataset
#' @param observations Explicit acquisition and biological identities.
#' @export
hm_as_dataset.annot_image <- function(
  x, profile = NULL,
  observations = .hm_image_observations(), ...
) {
  .hm_require("hyperannotatR")
  d <- hyperannotatR::at_dims(x, 0L)
  b <- as.data.frame(hyperannotatR::at_bands(x))
  if (any(
    !is.na(b$wavelength) &
      !b$unit %in% "nm"
  )) {
    .hm_abort(
      "Only source-declared nm wavelengths are admitted.",
      "hm_unsupported_error"
    )
  }
  b$availability <- ifelse(
    is.na(b$wavelength),
    "unavailable", "declared"
  )
  b$unit <- "nm"
  b$native_index <- b$index
  bytes <- prod(as.double(c(d, nrow(b)))) *
    8
  if (bytes > getOption("hypermodR.block_bytes", 256 * 1024^2)) {
    .hm_abort(
      "Annotation materialisation exceeds block budget.",
      "hm_resource_error"
    )
  }
  v <- hyperannotatR::at_tile(
    x,
    xrange = c(1L, d[1]),
    yrange = c(1L, d[2]),
    bands = seq_len(nrow(b)),
    level = 0L
  )
  hm_dataset(
    v, observations, b, .hm_native_geometry(), list(
      adapter = "hyperannotatR",
      manifest = hyperannotatR::at_interop_manifest(x, hash_sources = TRUE),
      hsi_meta = hyperannotatR::at_hsi_meta(x),
      materialisation = "bounded public tile"
    )
  )
}
#' @rdname hm_as_dataset
#' @export
hm_as_dataset.annot_project <- function(x, profile = NULL, ...) {
  out <- hm_as_dataset(x$image, profile, ...)
  out@provenance$annotation_manifest <- hyperannotatR::at_interop_manifest(
    x,
    hash_sources = TRUE
  )
  out@provenance$annotation_revision <- hyperannotatR::at_annotation_revision(x)
  methods::validObject(out)
  out
}
