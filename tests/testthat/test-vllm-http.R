test_that(
  "vLLM enforces identity, remote disclosure, completion and redaction",
  {
    prompt <- hm_prompt("Describe the auxiliary image", "prespecified")
    mock <- function(url, body, token, timeout, max_bytes) {
      list(
        status = 200L, body = jsonlite::toJSON(
          list(
            model = body$model, choices = list(list(
              finish_reason = "stop",
              message = list(content = "answer")
            ))
          ),
          auto_unbox = TRUE
        )
      )
    }
    out <- hm_vllm_chat("http://127.0.0.1:8099/v1", "pinned-test", prompt,
      .transport = mock
    )
    expect_identical(out$text, "answer")
    expect_false(out$remote)
    expect_error(
      hm_vllm_chat("https://example.invalid/v1", "pinned-test", prompt,
        .transport = mock
      ),
      class = "hm_remote_consent_error"
    )
    expect_error(
      hm_vllm_chat("http://localhost/v1", "spectralgpt", prompt,
        .transport = mock
      ),
      "Native HSI"
    )
    wrong <- function(...) {
      list(
        status = 200L,
        body = "{\"model\":\"wrong\",\"choices\":[]}"
      )
    }
    expect_error(
      hm_vllm_chat("http://localhost/v1", "pinned-test", prompt,
        .transport = wrong
      ),
      "Wrong served"
    )
    leak <- function(...) cli::cli_abort("secret-token-value")
    expect_error(
      hm_vllm_chat(
        "http://localhost/v1", "pinned-test", prompt,
        token = "secret-token-value",
        .transport = leak
      ),
      "^Endpoint request failed;"
    )
    auth <- function(...) list(status = 401L, body = "denied")
    expect_error(
      hm_vllm_chat("http://localhost/v1", "pinned-test", prompt,
        .transport = auth
      ),
      "HTTP status"
    )
  }
)
test_that(
  "rendering records exact discarded channels and transformation",
  {
    cube <- array(
      seq_len(24),
      c(2, 3, 4)
    )
    out <- hm_render_hsi(
      cube, c(450, 500, 550, 600),
      c(1, 2, 4),
      c(0, 24),
      "fixture"
    )
    expect_equal(
      dim(out$image),
      c(2, 3, 3)
    )
    expect_equal(out$manifest$discarded_bands, 3)
    expect_identical(out$manifest$spatial_transform, "none")
    expect_error(
      hm_prompt("label", "test_reference"),
      "origin"
    )
    expect_error(
      hm_prompt("video", "prespecified", timestamps = c(2, 1)),
      "ordered"
    )
  }
)
test_that(
  "real HTTP transport works against an independent loopback fixture",
  {
    skip_if_not_installed("processx")
    skip_if_not_installed("httr2")
    python <- unname(Sys.which("python3"))
    skip_if(!nzchar(python))
    fixture <- normalizePath(test_path(
      "..", "integration", "vllm",
      "http_fixture.py"
    ))
    server <- processx::process$new(python, fixture,
      stdout = "|",
      stderr = "|", cleanup = TRUE
    )
    on.exit(server$kill_tree())
    server$poll_io(3000)
    port <- server$read_output_lines(n = 1L)
    expect_match(port, "^[0-9]+$")
    endpoint <- paste0("http://127.0.0.1:", port, "/v1")
    prompt <- hm_prompt("Auxiliary description", "prespecified")
    response <- hm_vllm_chat(endpoint, "test-model", prompt)
    expect_identical(response$text, "loopback reply")
    expect_error(
      hm_vllm_chat(endpoint, "auth-failure", prompt),
      "HTTP status"
    )
  }
)
