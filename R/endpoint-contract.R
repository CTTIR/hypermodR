#' Declare independent reference endpoints
#' @param references Data frame with observation_id, endpoint and value.
#' Missing values remain missing.
#' @return Validated declaration, without claiming source truth.
#' @export
hm_endpoint_contract <- function(references) {
  allowed <- c("burn_grade", "healing", "injury", "anatomy")
  if (!is.data.frame(references) ||
    !all(
      c("observation_id", "endpoint", "value") %in%
        names(references)
    ) ||
    !is.character(references$observation_id) ||
    anyNA(references$observation_id) ||
    any(!nzchar(references$observation_id)) ||
    anyNA(references$endpoint) ||
    any(!references$endpoint %in% allowed)) {
    .hm_abort(paste0(
      "Explicit observation and independent endpoint ",
      "declarations required."
    ))
  }
  key <- paste(references$observation_id, references$endpoint, sep = "\r")
  if (any(
    vapply(
      split(references$value, key),
      function(v) {
        length(unique(v[!is.na(v)])) >
          1L
      }, logical(1)
    )
  )) {
    .hm_abort("Conflicting reference labels.", "hm_reference_conflict_error")
  }
  structure(
    list(
      references = references, validation = "declaration only",
      source_truth = "unverified"
    ),
    class = "hm_endpoint_contract"
  )
}
