#' Declare auxiliary language-model prompt provenance
#' @param text Prompt text.
#' @param origin Origin of instructions, independent of held-out outcomes.
#' @param rendering Rendering manifest for image content.
#' @param images Optional PNG/JPEG data URLs from an explicitly manifested
#' rendering.
#' @param timestamps Frame timestamps retained in order.
#' @return An `hm_prompt` list retaining text, origin, rendering, timestamps,
#' optional images, auxiliary_only flag and content digest.
#' @export
hm_prompt <- function(
  text, origin, rendering = NULL, timestamps = NULL,
  images = NULL
) {
  .hm_scalar_character(text, "text")
  if (!origin %in% c("prespecified", "training_only", "user_inference")) {
    .hm_abort("Prompt origin must exclude held-out reference labels.")
  }
  if (!is.null(timestamps) &&
        (!is.numeric(timestamps) ||
           any(!is.finite(timestamps)) ||
           is.unsorted(timestamps, strictly = TRUE))) {
    .hm_abort("Frame timestamps must be finite and strictly ordered.")
  }
  if (!is.null(images) &&
    (is.null(rendering) ||
      !is.character(images) ||
      !length(images) ||
      anyNA(images) ||
      any(!grepl(
        "^data:image/(png|jpeg);base64,[A-Za-z0-9+/=]+$",
        images
      )))) {
    .hm_abort("Images require PNG/JPEG data URLs and a rendering manifest.")
  }
  structure(
    list(
      text = text, origin = origin, rendering = rendering,
      timestamps = timestamps,
      images = images, auxiliary_only = TRUE, digest = .hm_hash(list(
        text,
        origin, rendering, timestamps, images
      ))
    ),
    class = "hm_prompt"
  )
}
