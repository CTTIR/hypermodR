test_that(
  "catalog filters preserve availability and qualification distinctions",
  {
    models <- hm_models(backend = "r_core", include_unavailable = FALSE)
    expect_true(
      length(models) >
        5L
    )
    expect_true(
      all(
        vapply(
          models, function(x) isTRUE(x$package_enabled),
          logical(1)
        )
      )
    )
    expect_false(
      any(
        vapply(
          models, function(x) isTRUE(x$scientific_qualification),
          logical(1)
        )
      )
    )
    expect_length(
      hm_models(task = "not-a-task"),
      0
    )
    expect_length(
      hm_models(backend = "not-a-backend"),
      0
    )
    expect_true(
      all(
        vapply(
          hm_models(task = "regression"),
          function(x) {
            "regression" %in%
              x$tasks
          }, logical(1)
        )
      )
    )
    expect_error(
      hm_model_spec("ridge", list()),
      class = "hm_validation_error"
    )
    expect_error(
      hm_model_spec(
        "ridge", hm_task("y", "regression", "spectrum", "subject_id"),
        1
      ),
      class = "hm_validation_error"
    )
    expect_error(
      .hm_resource("does-not-exist"),
      class = "hm_resource_error"
    )
  }
)

test_that(
  "prompt provenance preserves temporal and image inputs exactly",
  {
    manifest <- list(
      channels = c(1L, 3L, 2L),
      source = "synthetic"
    )
    p <- hm_prompt(
      "Describe the rendering", "prespecified", manifest, c(0, 2.5),
      "data:image/png;base64,AAAA"
    )
    expect_s3_class(p, "hm_prompt")
    expect_identical(p$rendering, manifest)
    expect_identical(p$timestamps, c(0, 2.5))
    expect_true(p$auxiliary_only)
    expect_false(
      identical(
        p$digest, hm_prompt(
          "Different text", "prespecified", manifest, c(0, 2.5),
          "data:image/png;base64,AAAA"
        )$digest
      )
    )
    expect_error(
      hm_prompt("x", "held_out_labels"),
      class = "hm_validation_error"
    )
    expect_error(
      hm_prompt("x", "training_only", timestamps = c(2, 1)),
      class = "hm_validation_error"
    )
    expect_error(
      hm_prompt("x", "training_only", timestamps = c(1, 1)),
      class = "hm_validation_error"
    )
    expect_error(
      hm_prompt("x", "training_only", timestamps = c(1, Inf)),
      class = "hm_validation_error"
    )
    expect_error(
      hm_prompt("x", "user_inference", images = "data:image/png;base64,AAAA"),
      class = "hm_validation_error"
    )
    expect_error(
      hm_prompt("x", "user_inference", manifest,
        images = "file:///private.png"
      ),
      class = "hm_validation_error"
    )
  }
)

test_that(
  paste0(
    "spectral library declarations reject corrupt grids and ",
    "retain source identity"
  ),
  {
    values <- matrix(
      c(1, 3, 2, 7, 5, 4),
      3, 2
    )
    lib <- hm_spectral_library(
      values, c(450, 550, 750),
      "cm^-1", "synthetic measured fixture", "CC0"
    )
    expect_identical(lib$values, values)
    expect_identical(lib$wavelength, c(450, 550, 750))
    expect_identical(lib$license, "CC0")
    expect_false(
      identical(
        lib$hash, hm_spectral_library(
          values, c(450, 550, 750),
          "cm^-1", "different source", "CC0"
        )$hash
      )
    )
    for (grid in list(
      c(450, 450, 750),
      c(750, 550, 450),
      c(450, NA, 750),
      c(450, 550)
    )) {
      expect_error(
        hm_spectral_library(values, grid, "cm^-1", "fixture", "CC0"),
        class = "hm_validation_error"
      )
    }
    bad <- values
    bad[1, 1] <- -1
    expect_error(
      hm_spectral_library(
        bad, c(450, 550, 750),
        "cm^-1", "fixture", "CC0"
      ),
      class = "hm_validation_error"
    )
    bad[1, 1] <- Inf
    expect_error(
      hm_spectral_library(
        bad, c(450, 550, 750),
        "cm^-1", "fixture", "CC0"
      ),
      class = "hm_validation_error"
    )
  }
)

test_that(
  "backend qualification never infers parity from missing installation",
  {
    result <- hm_check_backend("absent")
    expect_identical(result$availability, "unavailable")
    expect_identical(result$parity, "not_established")
    expect_false(result$scientific_qualification)
    expect_identical(
      hm_check_backend("absent", tests = "availability")$parity,
      "not_requested"
    )
    expect_error(
      hm_check_backend("absent", tests = "invented"),
      class = "hm_validation_error"
    )
  }
)

test_that(
  "confidence bins and retained risk have independent expected values",
  {
    task <- hm_task("y", "classification", "spectrum", "subject_id")
    probability <- rbind(
      c(a = 0.9, b = 0.1),
      c(a = 0.4, b = 0.6),
      c(a = 0.2, b = 0.8)
    )
    prediction <- hm_prediction(
      c("a", "b", "b"),
      task, c("o1", "o2", "o3"),
      probability, c("a", "b")
    )
    reference <- data.frame(
      observation_id = c("o1", "o2", "o3"),
      y = c("a", "a", "b"),
      subject_id = c("s1", "s2", "s3"),
      acquisition_id = c("q1", "q2", "q3")
    )
    result <- hm_reliability(prediction, reference, c(0, 0.5, 0.75, 1))
    expect_identical(result$bins$n, c(0L, 1L, 2L))
    expect_equal(result$bins$accuracy, c(NA, 0, 1))
    expect_equal(result$bins$confidence, c(NA, 0.6, 0.85))
    expect_equal(result$coverage_risk$threshold, c(0, 0.6, 0.8, 0.9, 1))
    expect_equal(result$coverage_risk$coverage, c(1, 1, 2 / 3, 1 / 3, 0))
    expect_equal(result$coverage_risk$risk, c(1 / 3, 1 / 3, 0, 0, NA))
    expect_error(
      hm_reliability(prediction, reference, c(0.1, 1)),
      class = "hm_validation_error"
    )
    expect_error(
      hm_reliability(prediction, reference, c(0, 0.5, 0.5, 1)),
      class = "hm_validation_error"
    )
  }
)

test_that(
  "registry signatures bind content, trust, revocation and release validity",
  {
    skip_if(
      !nzchar(Sys.which("openssl")),
      "openssl executable unavailable"
    )
    directory <- tempfile("registry-signature-")
    dir.create(directory)
    on.exit(
      unlink(directory, recursive = TRUE),
      add = TRUE
    )
    private <- file.path(directory, "private.pem")
    public <- file.path(directory, "public.pem")
    registry <- file.path(directory, "registry.json")
    signature <- file.path(directory, "registry.sig")
    processx::run(
      "openssl", c(
        "genpkey", "-algorithm", "RSA", "-pkeyopt", "rsa_keygen_bits:2048",
        "-out", private
      )
    )
    processx::run("openssl", c(
      "pkey", "-in", private, "-pubout", "-out",
      public
    ))
    fingerprint <- digest::digest(file = public, algo = "sha256")
    write_registry <- function(sequence, expiry) {
      jsonlite::write_json(
        list(release_sequence = sequence, expires = expiry),
        registry,
        auto_unbox = TRUE
      )
      processx::run(
        "openssl", c(
          "dgst", "-sha256", "-sign", private, "-out",
          signature, registry
        )
      )
    }
    now <- as.POSIXct("2026-01-01", tz = "UTC")
    write_registry(4, "2027-01-01T00:00:00Z")
    verified <- hm_verify_registry(registry, signature, public,
      fingerprint,
      now = now
    )
    expect_true(verified$verified)
    expect_equal(verified$release_sequence, 4)
    expect_identical(verified$digest, digest::digest(
      file = registry,
      algo = "sha256"
    ))
    expect_error(
      hm_verify_registry(registry, signature, public, character(), now = now),
      class = "hm_signature_error"
    )
    expect_error(
      hm_verify_registry(
        registry, signature, public, fingerprint,
        revoked_keys = fingerprint,
        now = now
      ),
      class = "hm_signature_error"
    )
    writeLines("tampered", registry)
    expect_error(
      hm_verify_registry(registry, signature, public, fingerprint, now = now),
      class = "hm_signature_error"
    )
    write_registry(0, "2027-01-01T00:00:00Z")
    expect_error(
      hm_verify_registry(registry, signature, public, fingerprint, now = now),
      class = "hm_signature_error"
    )
    write_registry(4, "2025-01-01T00:00:00Z")
    expect_error(
      hm_verify_registry(registry, signature, public, fingerprint, now = now),
      class = "hm_signature_error"
    )
  }
)
