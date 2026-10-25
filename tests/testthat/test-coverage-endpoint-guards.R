test_that(
  "endpoint consent and payload bounds fail before transport",
  {
    prompt <- hm_prompt(
                        "fixture",
                        "prespecified")
    forbidden <- function(
      ...) {
      cli::cli_abort(
                     "Transport should not be called")
    }
    expect_error(
      hm_vllm_chat(
        "http://user:secret@localhost/v1",
        "fixture",
        prompt, .transport = forbidden
      ),
      "credentials"
    )
    expect_error(
      hm_vllm_chat(
        "http://example.invalid/v1",
        "fixture",
        prompt,
        allow_remote = TRUE,
        disclosure = "synthetic test",
        .transport = forbidden
      ),
      "HTTPS"
    )
    expect_error(
      hm_vllm_chat(
        "http://localhost/v1", "fixture",
        prompt,
        timeout = -1,
        .transport = forbidden
      ),
      "bounds"
    )
    expect_error(
      hm_vllm_chat(
        "http://localhost/v1", "fixture",
        prompt,
        generation = list(
                          unsupported = TRUE),
        .transport = forbidden
      ),
      "generation"
    )
    corrupted <- prompt
    corrupted$text <- "changed"
    expect_error(
      hm_vllm_chat(
        "http://localhost/v1", "fixture",
        corrupted, .transport = forbidden
      ),
      "provenance changed"
    )
    long <- hm_prompt(
      paste(
        rep("x", 1000),
        collapse = ""
      ),
      "prespecified"
    )
    expect_error(
      hm_vllm_chat(
        "http://localhost/v1", "fixture",
        long,
        max_bytes = 128,
        .transport = forbidden
      ),
      "request exceeds"
    )
    oversized <- function(...) {
      list(
        status = 200L, body = paste(
          rep("x", 1000),
          collapse = ""
        )
      )
    }
    expect_error(
      hm_vllm_chat(
        "http://localhost/v1", "fixture",
        prompt,
        max_bytes = 512,
        .transport = oversized
      ),
      "response exceeds"
    )
    malformed <- function(
      ...) {
      list(
           status = 200L, body = "{not-json")
    }
    expect_error(
      hm_vllm_chat(
        "http://localhost/v1", "fixture",
        prompt, .transport = malformed
      ),
      class = "hm_http_error"
    )
  }
)

test_that(
  paste0(
    "multimodal endpoint contracts preserve manifested ",
    "images and transmission declaration"
  ),
  {
    prompt <- hm_prompt(
      "synthetic image", "user_inference",
      rendering = list(
                       source_id = "fixture"),
      timestamps = c(0, 2),
      images = "data:image/png;base64,AAAA"
    )
    request <- NULL
    transport <- function(
      url, body, token, timeout, max_bytes) {
      request <<- list(
                       url = url,
                       body = body)
      list(
        status = 200L, body = paste0(
          "{\"model\":\"fixture\",\"choices\":[{\"finish_reason\":\"sto",
          "p\",\"message\":{\"content\":\"synthetic answer\"}}]}"
        )
      )
    }
    result <- hm_vllm_chat(
      "https://example.invalid/v1/",
      "fixture",
      prompt,
      allow_remote = TRUE,
      disclosure = "synthetic manifested image",
      .transport = transport
    )
    expect_true(result$remote)
    expect_true(result$auxiliary_only)
    expect_identical(
      result$probability_semantics,
      "not_calibrated"
    )
    expect_identical(
      request$url,
      "https://example.invalid/v1/chat/completions"
    )
    expect_identical(
      request$body$messages[[1]]$content[[2]]$image_url$url,
      prompt$images
    )
    expect_identical(
                     result$timestamps,
                     c(0, 2))
    expect_identical(
      result$rendering,
      list(source_id = "fixture")
    )
  }
)
