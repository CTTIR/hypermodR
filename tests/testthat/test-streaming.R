test_that(
  "streaming preserves order, irregular time and bounded lifecycle",
  {
    frames <- list(
      matrix(1, 2, 3),
      matrix(2, 3, 2),
      matrix(3, 1, 4)
    )
    stream <- hm_stream(
      function(frame, time) {
        sum(frame) +
          time
      },
      max_frames = 3
    )
    result <- hm_stream_push(
      stream, frames, c(0, 0.2, 0.8),
      "recording-a"
    )
    expect_equal(
      unlist(result),
      c(6, 12.2, 12.8),
      ignore_attr = TRUE
    )
    expect_identical(
      attr(result, "timestamps"),
      c(0, 0.2, 0.8)
    )
    expect_error(
      hm_stream_push(stream, frames[1], 0.8, "recording-a"),
      "Duplicate"
    )
    expect_error(
      hm_stream_push(stream, frames[1], 1, "recording-a"),
      "limit"
    )
    expect_length(
      hm_stream_push(stream, frames[1], 0, "recording-b"),
      1
    )
    hm_stream_cancel(stream)
    expect_error(
      hm_stream_push(stream, frames[1], 1, "recording-b"),
      "cancelled"
    )
  }
)
