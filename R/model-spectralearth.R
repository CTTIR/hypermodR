#' Declare the spectralearth upstream candidate
#' @param task An hm_task.
#' @param parameters Explicit mode, provenance and preprocessing declarations.
#' @return An hm_model_spec; upstream execution remains admission-gated.
#' @examples
#' task <- hm_task("y", "classification", "image", "subject_id")
#' spec <- hm_spectralearth(task)
#' spec$model_id
#' # Declaring a candidate neither installs nor executes the upstream model
#' @export
hm_spectralearth <- function(task, parameters = list()) {
  hm_model_spec("spectralearth", task, parameters)
}
