test_that(
  "spectra and block reads refuse ambiguous or unknown selections",
  {
    spectra <- hm_example()
    first <- spectra@observations$observation_id[[1]]
    expect_error(
      hm_spectra(spectra, list(observation_id = c(first, first))),
      "unique character IDs"
    )
    expect_error(
      hm_spectra(spectra, list(observation_id = "not-an-observation")),
      "Unknown observation_id"
    )
    expect_error(hm_read_block(spectra), "hm_spectra")
    image <- hm_dataset(
      array(as.numeric(seq_len(30)), c(2, 5, 3)),
      .hm_image_observations(),
      data.frame(wavelength = c(500, 600, 700)),
      .hm_native_geometry()
    )
    expect_error(
      hm_read_block(image, list(acquisition_id = "other")),
      "known acquisition"
    )
    expect_error(
      hm_read_block(image, list(frame_id = "frame")),
      "no frame selection"
    )
  }
)

test_that(
  "splits require a task and an observation table",
  {
    task <- hm_task(NULL, "reconstruction", "spectrum", "subject_id")
    expect_error(hm_split(hm_example(), list()), "Expected task")
    expect_error(hm_split(list(), task), "Observation table required")
  }
)

test_that(
  "run storage refuses unknown states and incomplete runs",
  {
    path <- tempfile("run-guard-")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    expect_error(.hm_run_receipt(path, "unknown"), "Unknown run state")
    expect_error(hm_read_run(path), class = "hm_integrity_error")
  }
)
