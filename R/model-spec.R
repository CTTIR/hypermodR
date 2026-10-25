#' Declare a model specification
#' @param model_id Single model identifier from `hm_models()` or an explicitly
#' admitted custom identity.
#' @param task An `hm_task` declaration.
#' @param parameters Named list of engine-specific parameters; interpreted when
#' fitting.
#' @return An `hm_model_spec` list containing `model_id`, `task` and
#' `parameters`.
#' @details This constructor declares intent and checks known core task
#' compatibility. It neither installs a backend nor qualifies a model
#' scientifically.
#' @export
hm_model_spec <- function(model_id, task, parameters = list()) {
  .hm_scalar_character(model_id, "model_id")
  if (!inherits(task, "hm_task") ||
        !is.list(parameters)) {
    .hm_abort("Invalid task or parameters.")
  }
  if (exists(".hm_engine_catalog", mode = "function")) {
    catalog <- .hm_engine_catalog()
    if (model_id %in% names(catalog) &&
          !task$type %in% catalog[[model_id]]) {
      .hm_abort("Model does not support this task.", "hm_capability_error")
    }
  }
  structure(
    list(model_id = model_id, task = task, parameters = parameters),
    class = "hm_model_spec"
  )
}
