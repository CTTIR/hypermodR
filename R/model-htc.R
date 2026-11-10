#' Declare the htc upstream candidate
#' @param task An hm_task.
#' @param parameters Explicit mode, provenance and preprocessing declarations.
#' @return An hm_model_spec; upstream execution remains admission-gated.
#' @examples
#' task <- hm_task("y", "classification", "image", "subject_id")
#' spec <- hm_htc(task)
#' spec$model_id
#' # Declaring a candidate neither installs nor executes the upstream model
#' @export
hm_htc <- function(task, parameters = list()) {
  hm_model_spec("htc", task, parameters)
}
