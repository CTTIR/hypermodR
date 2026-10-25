#' @rdname hm_as_dataset
#' @export
hm_as_dataset.cuvis_session <- function(x, profile = NULL, ...) {
  .hm_require("cuvis.r")
  .hm_abort(
    paste0(
      "CUVIS session enumeration is unqualified: public ",
      "item-type counts and source-backed timing/scaling ",
      "evidence are required. Use explicitly indexed portable ",
      "exports."
    ),
    "hm_unsupported_error"
  )
}
#' @rdname hm_as_dataset
#' @export
hm_as_dataset.cuvis_measurement <- function(x, profile = NULL, ...) {
  .hm_require("cuvis.r")
  .hm_abort(
    paste0(
      "CUVIS SDK scaling is unqualified. Export an explicitly ",
      "declared numeric cube; processing mode alone cannot ",
      "establish scaling."
    ),
    "hm_unsupported_error"
  )
}
