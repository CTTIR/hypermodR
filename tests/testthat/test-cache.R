test_that(
  paste0(
    "full values and all state change cache identity and ",
    "cached reads are independent"
  ),
  {
    x <- core_image()
    key <- hm_cache_key(x, list(reader = "1", model = "m"))
    y <- x
    y@sources[[1]][59] <- 999
    expect_false(identical(key, hm_cache_key(y, list(
      reader = "1",
      model = "m"
    ))))
    y <- x
    y@bands$wavelength[1] <- 501
    expect_false(identical(key, hm_cache_key(y, list(
      reader = "1",
      model = "m"
    ))))
    for (state in list(
      list(reader = "2", model = "m"),
      list(reader = "1", model = "n")
    )) {
      expect_false(identical(key, hm_cache_key(x, state)))
    }
    y <- x
    y@provenance$calibration <- "new"
    expect_false(identical(key, hm_cache_key(y, list(
      reader = "1",
      model = "m"
    ))))
    cache <- hm_cache()
    cache$set(key, hm_read_block(x))
    a <- cache$get(key)
    a$values[] <- 0
    expect_equal(
      as.vector(cache$get(key)$values),
      as.numeric(1:60)
    )
    expect_equal(
      as.vector(hm_read_block(x)$values),
      as.numeric(1:60)
    )
  }
)
