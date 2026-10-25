test_that(
  "coordinates invert exactly and padding stays explicit",
  {
    g <- list(
      origin = c(0, 0),
      transform = matrix(
        c(2, 0, 0, 0, 3, 0, 10, 20, 1),
        3
      )
    )
    p <- rbind(
      c(1, 1),
      c(5, 3),
      c(0, 0)
    )
    q <- hm_coordinates(g, p)
    expect_equal(q[1, ], c(10, 20))
    expect_equal(q[2, ], c(18, 26))
    expect_equal(
      hm_coordinates(g, q, TRUE),
      p
    )
    expect_error(
      hm_coordinates(
        list(
          origin = c(0, 0),
          transform = matrix(0, 3, 3)
        ),
        p
      )
    )
  }
)
