#' Inspect admitted instrument capabilities
#' @return A data frame. Synthetic profiles are not device qualification.
#' @examples
#' hm_instrument_profiles()
#' @export
hm_instrument_profiles <- function() {
  data.frame(
    profile = c(
      "tivita-synthetic-yxb-v1", "Tissue", "Mini", "Surgery",
      "cuvis-sdk"
    ),
    status = c("synthetic_only", rep("unqualified", 4)),
    reason = c(
      "Explicit portable arrays; no device claim",
      "Device/export fixture required",
      "Device/export fixture required", "Device/export fixture required",
      "SDK scaling and complete enumeration fixture required"
    )
  )
}
#' Declare an explicit TIVITA array profile
#' @param wavelengths Independently established wavelengths in nm; NULL
#' preserves unknown.
#' @param device Device declaration, not qualification.
#' @param software Export software declaration.
#' @param evidence Evidence identifier.
#' @return Serializable profile list.
#' @examples
#' profile <- hm_tivita_profile(c(500, 600, 700), device = "declared device",
#'                              software = "export-v1", evidence = "fixture-1")
#' profile$qualification
#' # Unknown wavelengths stay unknown
#' is.null(hm_tivita_profile()$wavelengths)
#' @export
hm_tivita_profile <- function(
  wavelengths = NULL, device = "unknown", software = "unknown",
  evidence = "unavailable"
) {
  list(
    id = "tivita-synthetic-yxb-v1", layout = "y,x,band",
    wavelengths = wavelengths,
    device = device, software = software, evidence = evidence,
    qualification = "synthetic_only"
  )
}
