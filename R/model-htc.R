#' Declare the htc upstream candidate
#' @param task An hm_task.
#' @param parameters Explicit mode, provenance and preprocessing declarations.
#' @return An hm_model_spec; upstream execution remains admission-gated.
#' @export
hm_htc <- function(task, parameters = list()) {
  hm_model_spec("htc", task, parameters)
}
