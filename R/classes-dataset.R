#' Validated spectral data container
#' @slot sources Native arrays or declarative RDS locators, never live handles.
#' @slot observations Identity and observation metadata.
#' @slot bands Native band mapping; wavelengths are in nm.
#' @slot geometry Explicit spatial transform; empty for native spectra.
#' @slot provenance Source, radiometry and calibration declarations.
#' @slot data_kind One of spectra, image or sequence.
#' @slot assets Distinct asset-role metadata.
#' @examples
#' methods::showClass("HMDataset")
#' data <- hm_example()
#' methods::is(data, "HMDataset")
#' data@data_kind
#' @exportClass HMDataset
methods::setClass(
  "HMDataset",
  slots = c(
    sources = "list", observations = "data.frame", bands = "data.frame",
    geometry = "list", provenance = "list", data_kind = "character",
    assets = "data.frame"
  ),
  validity = function(object) .hm_valid_dataset(object)
)
#' Construct a native spectral dataset
#' @param values Numeric observation-by-band matrix or y,x,wavelength array.
#' @param observations Data frame with unique character observation_id; images
#' also require image_id and acquisition_id.
#' @param bands Band metadata with numeric wavelength in nm. Unknown
#' wavelengths require availability='unavailable'.
#' @param geometry Explicit origin and invertible homogeneous transform for
#' images.
#' @param provenance Reproducible source and calibration metadata.
#' @return An HMDataset.
#' @examples
#' # Native spectra: one row per observation, one column per band
#' spectra <- hm_dataset(
#'   matrix(runif(12), 3, 4),
#'   data.frame(observation_id = c("o1", "o2", "o3"),
#'              subject_id = c("s1", "s1", "s2")),
#'   data.frame(wavelength = c(500, 600, 700, 800))
#' )
#' spectra@data_kind
#'
#' # A y, x, wavelength image with explicit geometry
#' image <- hm_dataset(
#'   array(seq_len(60), c(3, 5, 4)),
#'   data.frame(observation_id = "o1", image_id = "i1", acquisition_id = "a1"),
#'   data.frame(wavelength = c(500, 600, 700, 800)),
#'   geometry = list(origin = c(0, 0), transform = diag(3))
#' )
#' image@data_kind
#' @export
hm_dataset <- function(
  values, observations, bands, geometry = NULL,
  provenance = list()
) {
  d <- .hm_source(values)
  kind <- if (length(d) ==
                2L) {
    "spectra"
  } else {
    "image"
  }
  if (!is.list(provenance) ||
        !.hm_plain(provenance)) {
    .hm_abort("provenance must be a serializable list.")
  }
  if (is.null(provenance$radiometric_unit)) {
    provenance$radiometric_unit <- "unknown"
  }
  if (is.null(provenance$calibration)) {
    provenance$calibration <- "unknown"
  }
  methods::new(
    "HMDataset",
    sources = list(values),
    observations = observations, bands = .hm_bands(bands, tail(d, 1)),
    geometry = if (is.null(geometry)) {
      list()
    } else {
      geometry
    }, provenance = provenance, data_kind = kind, assets = data.frame()
  )
}
