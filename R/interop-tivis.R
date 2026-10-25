#' Import an explicitly profiled TIVITA measurement list
#' @param x Unclassed tivis_measurement result.
#' @param profile Explicit hm_tivita_profile declaration.
#' @param observations Explicit source/group identity.
#' @param assets Asset inventory with role and registration_id columns.
#' @param registration_id Expected registration identity.
#' @param mask Optional logical y,x validity mask; NA remains unknown.
#' @return HMDataset; no device calibration is inferred.
#' @export
hm_from_tivis <- function(
  x, profile, observations = .hm_image_observations(), assets = data.frame(),
  registration_id = NULL, mask = NULL
) {
  if (!is.list(x) ||
    !is.null(class(x)) &&
      !identical(
        class(x),
        "list"
      ) ||
    !all(
      c(
        "cube", "wavelengths", "metadata", "references", "timestamp",
        "path"
      ) %in%
        names(x)
    )) {
    .hm_abort("Expected an unclassed tivis measurement list.")
  }
  if (!is.list(profile) ||
        !identical(profile$id, "tivita-synthetic-yxb-v1") ||
        !identical(profile$layout, "y,x,band")) {
    .hm_abort(
      "Unqualified TIVITA device/export profile.",
      "hm_unsupported_error"
    )
  }
  d <- dim(x$cube)
  if (!is.numeric(x$cube) ||
        length(d) !=
          3L) {
    .hm_abort("Expected y,x,band cube.")
  }
  wl <- profile$wavelengths
  if (is.null(wl)) {
    wl <- rep(NA_real_, d[3])
  }
  if (length(wl) !=
        d[3]) {
    .hm_abort("Profile wavelength count disagrees with cube.")
  }
  roles <- c(
    "SpecCube", "RGB", "DICOM", "parameter_display", "quantitative_export",
    "segmentation", "mask", "metadata", "sidecar", "unknown"
  )
  if (!is.data.frame(assets)) {
    .hm_abort("assets must be a data frame.")
  }
  if (nrow(assets)) {
    if (!all(
      c("role", "registration_id") %in%
        names(assets)
    ) ||
      any(!assets$role %in% roles)) {
      .hm_abort("Unknown asset role or missing registration.")
    }
    if (is.null(registration_id) ||
          anyNA(assets$registration_id) ||
          any(assets$registration_id != registration_id)) {
      .hm_abort("Asset registration mismatch.", "hm_source_error")
    }
  }
  b <- data.frame(
    wavelength = wl, availability = ifelse(
      is.na(wl),
      "unavailable", "declared"
    )
  )
  out <- hm_dataset(
    x$cube, observations, b, .hm_native_geometry(), list(
      adapter = "tivis.r", profile = profile, radiometric_unit = "unknown",
      timestamp = list(
        value = as.character(x$timestamp),
        timezone = "unknown", reader_assumption = "UTC",
        evidence = "filename assumption only"
      ),
      metadata = x$metadata, references = x$references,
      spatial_masks = list(mask)
    )
  )
  out@assets <- assets
  methods::validObject(out)
  out
}
