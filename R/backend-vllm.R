.hm_vllm_transport <- function(url, body, token, timeout, max_bytes) {
  .hm_require("httr2")
  req <- httr2::request(url)
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_options(req,
    maxfilesize = max_bytes,
    followlocation = FALSE
  )
  req <- httr2::req_headers(req, `Content-Type` = "application/json")
  if (!is.null(token)) {
    req <- httr2::req_auth_bearer_token(req, token)
  }
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    .hm_abort(
      "Endpoint request failed or exceeded time/byte limits.",
      "hm_http_error"
    )
  }
  if (httr2::resp_status(resp) !=
        200L) {
    return(
      list(
        status = httr2::resp_status(resp),
        body = ""
      )
    )
  }
  raw <- httr2::resp_body_raw(resp)
  if (length(raw) >
        max_bytes) {
    .hm_abort("Endpoint response exceeds byte limit.")
  }
  list(
    status = httr2::resp_status(resp),
    body = rawToChar(raw)
  )
}
#' Query an explicitly configured OpenAI-compatible vLLM endpoint
#' @param endpoint Explicit server base URL.
#' @param model Exact served model identity.
#' @param prompt A provenance-bearing prompt.
#' @param allow_remote Explicit transmission consent for non-loopback endpoints.
#' @param disclosure Description of the data transmitted to a remote endpoint.
#' @param token Optional authentication secret, excluded from receipts and
#' errors.
#' @param timeout Maximum seconds.
#' @param max_bytes Maximum request/response bytes.
#' @param generation Deterministic generation settings.
#' @param .transport Internal transport override for protocol testing.
#' @return List with response text, exact model, prompt digest/origin,
#' rendering/timestamps, generation settings, remote/disclosure fields and
#' auxiliary-only, uncalibrated semantics.
#' @details The endpoint must include the API base path expected before
#' /chat/completions (for example /v1). Remote transmission requires HTTPS,
#' allow_remote=TRUE and disclosure. Stock native HSI architectures are
#' rejected. Responses must identify the exact served model, contain one choice
#' and finish with stop. Text is auxiliary output with no calibrated
#' probability interpretation.
#' @export
hm_vllm_chat <- function(
  endpoint, model, prompt, allow_remote = FALSE, disclosure = NULL,
  token = NULL,
  timeout = 30, max_bytes = 1024^2, generation = list(
    temperature = 0,
    max_tokens = 256L
  ),
  .transport = .hm_vllm_transport
) {
  .hm_scalar_character(endpoint, "endpoint")
  .hm_scalar_character(model, "model")
  if (model %in% c(
    "htc", "hypervision", "hyperfree", "hypersigma", "dual_stream_mhsi",
    "spectralearth", "spectralgpt"
  )) {
    .hm_abort("Native HSI architectures have no stock vLLM route.")
  }
  if (!grepl("^https?://[^/@?#]+(:[0-9]+)?(/[^?#]*)?$", endpoint) ||
        grepl("@", endpoint, fixed = TRUE)) {
    .hm_abort(
      paste0(
        "Endpoint must be an explicit HTTP(S) URL without ",
        "credentials or query parameters."
      )
    )
  }
  local <- grepl(
    "^https?://(127[.]0[.]0[.]1|localhost|\\[::1\\])(:[0-9]+)?(/|$)", endpoint
  )
  if (!local && (!isTRUE(allow_remote) ||
                   is.null(disclosure) ||
                   !is.character(disclosure) ||
                   !nzchar(disclosure))) {
    .hm_abort(
      "Remote transmission requires explicit consent and a data disclosure.",
      "hm_remote_consent_error"
    )
  }
  if (!local && !startsWith(endpoint, "https://")) {
    .hm_abort("Remote endpoints require HTTPS.")
  }
  if (!inherits(prompt, "hm_prompt") ||
    !is.numeric(timeout) ||
    length(timeout) !=
      1L || !is.finite(timeout) ||
    timeout <= 0 || !is.numeric(max_bytes) ||
    length(max_bytes) !=
      1L || !is.finite(max_bytes) ||
    max_bytes < 128 || any(
    !names(generation) %in%
      c("temperature", "max_tokens", "seed", "top_p")
  )) {
    .hm_abort("Invalid prompt, generation settings or request bounds.")
  }
  if (!identical(
    prompt$digest, .hm_hash(
      list(
        prompt$text, prompt$origin, prompt$rendering, prompt$timestamps,
        prompt$images
      )
    )
  )) {
    .hm_abort("Prompt provenance changed after declaration.")
  }
  content <- prompt$text
  if (!is.null(prompt$images)) {
    content <- c(
      list(list(type = "text", text = prompt$text)),
      lapply(
        prompt$images, function(x) {
          list(
            type = "image_url",
            image_url = list(url = x)
          )
        }
      )
    )
  }
  body <- c(
    list(
      model = model, messages = list(list(role = "user", content = content)),
      stream = FALSE
    ),
    generation
  )
  if (nchar(
    jsonlite::toJSON(body, auto_unbox = TRUE),
    type = "bytes"
  ) >
    max_bytes) {
    .hm_abort("Endpoint request exceeds byte limit.")
  }
  response <- tryCatch(
    .transport(
      paste0(
        sub("/+$", "", endpoint),
        "/chat/completions"
      ),
      body, token, timeout, max_bytes
    ),
    error = function(e) {
      .hm_abort(
        paste0(
          "Endpoint request failed; inspect server diagnostics ",
          "without credentials."
        ),
        "hm_http_error"
      )
    }
  )
  if (!is.list(response) ||
    !identical(
      as.integer(response$status),
      200L
    )) {
    .hm_abort(
      "Endpoint returned an unsuccessful HTTP status.",
      "hm_http_error"
    )
  }
  if (!is.character(response$body) ||
        length(response$body) !=
          1L || nchar(response$body, type = "bytes") >
        max_bytes) {
    .hm_abort("Endpoint response exceeds bounds.")
  }
  answer <- tryCatch(
    jsonlite::fromJSON(response$body, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(answer) ||
        !identical(answer$model, model) ||
        length(answer$choices) !=
          1L || !identical(answer$choices[[1]]$finish_reason, "stop") ||
        !is.character(answer$choices[[1]]$message$content) ||
        length(answer$choices[[1]]$message$content) !=
          1L) {
    .hm_abort(
      "Wrong served model, malformed or incomplete response.",
      "hm_http_error"
    )
  }
  list(
    text = answer$choices[[1]]$message$content, model = model,
    prompt_digest = prompt$digest,
    prompt_origin = prompt$origin, rendering = prompt$rendering,
    timestamps = prompt$timestamps,
    generation = generation, remote = !local, disclosure = disclosure,
    auxiliary_only = TRUE,
    probability_semantics = "not_calibrated", status = "complete"
  )
}
