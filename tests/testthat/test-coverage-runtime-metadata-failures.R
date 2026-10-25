test_that(
  paste0(
    "Python setup refuses absent pins, unreviewed resolver ",
    "options and version drift"
  ),
  {
    parent <- tempfile("python-metadata-")
    dir.create(parent)
    on.exit(
      unlink(parent, recursive = TRUE),
      add = TRUE
    )
    expect_error(
      .hm_setup_python(
        list(python = list()),
        parent
      ),
      "pinned interpreter"
    )
    lock <- file.path(parent, "requirements.txt")
    writeLines(
      "--index-url https://example.invalid",
      lock
    )
    hash <- paste(
      rep("a", 64),
      collapse = ""
    )
    profile <- list(
      python = list(
        executable = "fixture-python",
        version = "3.11.9", lock = "requirements.txt"
      ),
      artifacts = list(
        list(path = "requirements.txt", sha256 = hash)
      )
    )
    expect_error(
      .hm_setup_python(profile, parent),
      "only exact packages"
    )
    writeLines(
      paste0(
        "fixture==1.2.3 --hash=sha256:",
        hash
      ),
      lock
    )
    testthat::local_mocked_bindings(
      run = function(...) list(stdout = "3.11.8"),
      .package = "processx"
    )
    expect_error(
      .hm_setup_python(profile, parent),
      "version mismatch"
    )
    jsonlite::write_json(
      list(schema = "1.0"),
      file.path(parent, "receipt.json")
    )
    expect_false(.hm_environment_ok(parent))
    if (.Platform$OS.type == "unix") {
      environment <- file.path(parent, "environment")
      dir.create(environment)
      outside <- file.path(parent, "outside")
      writeLines("must not be admitted", outside)
      expect_true(
        file.symlink(outside, file.path(environment, "escape"))
      )
      expect_error(
        .hm_environment_inventory(environment),
        class = "hm_integrity_error"
      )
    }
  }
)

test_that(
  "public preprocessing and pooling reject malformed support",
  {
    expect_error(
      hm_prep(hm_recipe(), 1:4),
      "unfitted recipe"
    )
    expect_error(
      hm_bake(hm_recipe(), matrix(1:4, 2)),
      "Fitted recipe"
    )
    image <- core_image()
    expect_error(
      hm_pool(image, method = "median"),
      class = "hm_unsupported_error"
    )
    expect_error(
      hm_pool(image, matrix(TRUE, 1, 1)),
      "mask must match"
    )
    testthat::local_mocked_bindings(
      hm_read_block = function(...) {
        list(
          values = array(Inf, c(1, 1, 1, 1)),
          valid = array(TRUE, c(1, 1, 1))
        )
      },
      .package = "hypermodR"
    )
    expect_error(
      hm_pool(image),
      "pixels must be finite"
    )
  }
)
