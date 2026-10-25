test_that(
  "exact profiles reject unsupported platforms, mutable pins and terms",
  {
    path <- tempfile()
    writeLines("test", path)
    p <- backend_test_profile(path)
    plan <- hm_backend_plan("test-identity", profile = p)
    expect_s3_class(plan, "hm_backend_plan")
    expect_false(plan$scientific_qualification)
    p$runtime_version <- "latest"
    expect_error(
      hm_backend_plan("test-identity", profile = p),
      "Moving"
    )
    p <- backend_test_profile(path)
    p$terms[[1]]$text <- "tampered"
    expect_error(
      hm_backend_plan("test-identity", profile = p),
      class = "hm_terms_error"
    )
    p <- backend_test_profile(path)
    p$os <- "OtherOS"
    expect_error(
      hm_backend_plan("test-identity", profile = p),
      class = "hm_platform_error"
    )
    p <- backend_test_profile(path)
    p$artifacts[[1]]$path <- "../escape"
    expect_error(
      hm_backend_plan("test-identity", profile = p),
      "Invalid artifact"
    )
    unlink(path)
  }
)
test_that(
  "registry retains unavailable families without implied qualification",
  {
    models <- hm_models()
    expect_true(
      all(
        c(
          "htc", "hypervision", "hyperfree", "hypersigma", "dual_stream_mhsi",
          "spectralearth", "spectralgpt"
        ) %in%
          vapply(
            models, function(x) x$model_id,
            ""
          )
      )
    )
    expect_true(
      length(hm_models(include_unavailable = FALSE)) >
        0L
    )
    expect_true(
      "lm" %in% vapply(
        hm_models(task = "regression"),
        function(x) x$model_id,
        ""
      )
    )
    expect_error(
      hm_backend_plan("spectralgpt"),
      class = "hm_backend_unavailable"
    )
  }
)
test_that(
  "signature verification uses only trusted nonrevoked provisioned keys",
  {
    skip_if_not_installed("processx")
    skip_if(!nzchar(Sys.which("openssl")))
    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE))
    key <- file.path(dir, "private.pem")
    pub <- file.path(dir, "public.pem")
    registry <- file.path(dir, "registry.json")
    signature <- file.path(dir, "registry.sig")
    processx::run(
      "openssl", c(
        "genpkey", "-algorithm", "RSA", "-pkeyopt", "rsa_keygen_bits:2048",
        "-out", key
      )
    )
    processx::run("openssl", c("pkey", "-in", key, "-pubout", "-out", pub))
    jsonlite::write_json(
      list(release_sequence = 2, expires = "2099-01-01"),
      registry,
      auto_unbox = TRUE
    )
    processx::run(
      "openssl", c(
        "dgst", "-sha256", "-sign", key, "-out", signature,
        registry
      )
    )
    fingerprint <- digest::digest(file = pub, algo = "sha256")
    expect_true(hm_verify_registry(
      registry, signature, pub,
      fingerprint
    )$verified)
    expect_error(
      hm_verify_registry(registry, signature, pub, fingerprint,
        revoked_keys = fingerprint
      ),
      class = "hm_signature_error"
    )
    expect_error(
      hm_verify_registry(registry, signature, pub, fingerprint,
        minimum_version = 3L
      ),
      class = "hm_signature_error"
    )
    writeLines("tampered", registry)
    expect_error(
      hm_verify_registry(registry, signature, pub, fingerprint),
      class = "hm_signature_error"
    )
  }
)
