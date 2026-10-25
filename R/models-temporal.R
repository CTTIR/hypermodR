#' Declare a causal temporal reference
#' @param family Framewise, timestamp_mean, temporal_cnn, lstm, gru,
#' transformer or state_space.
#' @param task An hm_task.
#' @param parameters Explicit sequence and training declarations.
#' @return An hm_model_spec with a separate package reference identity.
#' @export
hm_temporal_model <- function(family, task, parameters = list()) {
  family <- match.arg(
    family, c(
      "framewise", "timestamp_mean", "temporal_cnn", "lstm", "gru",
      "transformer",
      "state_space"
    )
  )
  parameters$causal <- TRUE
  parameters$pretrained <- FALSE
  hm_model_spec(
    paste0("temporal_reference:", family),
    task, parameters
  )
}
