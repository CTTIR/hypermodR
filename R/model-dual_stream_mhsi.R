#' Declare the dual_stream_mhsi upstream candidate
#' @param task An hm_task.
#' @param parameters Explicit mode, provenance and preprocessing declarations.
#' @return An hm_model_spec; upstream execution remains admission-gated.
#' @export
hm_dual_stream_mhsi <- function(task, parameters = list()) {
  hm_model_spec("dual_stream_mhsi", task, parameters)
}
