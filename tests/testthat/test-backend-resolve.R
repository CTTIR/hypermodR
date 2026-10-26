test_that(
  paste0(
    "resolver and checkpoint admission reject unavailable ",
    "contracts before network"
  ),
  {
    expect_error(
      hm_resolve_checkpoint("owner/model", "main"),
      "immutable"
    )
    expect_error(
      hm_resolve_checkpoint(
        "../model", paste(
          rep("a", 40),
          collapse = ""
        )
      ),
      "immutable"
    )
    expect_error(
      hm_admit_backend(list(), list()),
      "per-artifact"
    )
    python <- unname(Sys.which("python3"))
    skip_if(!nzchar(python))
    caps <- hm_backend_capabilities()
    skip_if_not(identical(caps$os, "Linux") && identical(caps$arch, "x86_64"))
    version <- trimws(
      processx::run(python, c(
        "-c",
        "import platform; print(platform.python_version())"
      ))$stdout
    )
    if (!grepl("^3[.](11|12)[.]", version)) {
      expect_error(
        hm_resolve_backend("reference_cpu", "reference:cnn1d",
          python = python, uv = "uv"
        ),
        "3.11 or 3.12"
      )
    }
  }
)
test_that(
  "resolved admission binds full reviewed terms to exact artifact identity",
  {
    source <- tempfile()
    writeLines("fixture", source)
    on.exit(unlink(source))
    p <- backend_test_profile(source)
    p$terms[[1]]$package <- "fixture"
    p$terms[[1]]$version <- "1.0.0"
    p$terms[[1]]$artifact_sha256 <- p$artifacts[[1]]$sha256
    resolution <- structure(
      list(profile = p, backend = "reference_cpu"),
      class = "hm_backend_resolution"
    )
    terms <- p$terms
    terms[[1]]$reviewed <- FALSE
    expect_error(
      hm_admit_backend(resolution, terms),
      class = "hm_terms_error"
    )
    terms <- p$terms
    terms[[1]]$artifact_sha256 <- "tamper"
    expect_error(
      hm_admit_backend(resolution, terms),
      class = "hm_terms_error"
    )
    expect_s3_class(
      hm_admit_backend(resolution, p$terms),
      "hm_backend_plan"
    )
  }
)
