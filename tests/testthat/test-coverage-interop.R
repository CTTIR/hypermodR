test_that(paste0(
  "annotation public bridge binds content and stages ",
  "uncommitted masks"
), {
  skip_if_not_installed("hyperannotatR")
  skip_if_not_installed("magick")
  skip_if_not_installed("tiff")
  project <- hyperannotatR::at_example_project()
  labels <- c(tumour = 1L, necrosis = 2L, stroma = 3L)
  contract <- hm_annotation_contract(project, labels)
  expect_identical(contract, hm_annotation_contract(project, labels))
  expect_identical(contract$dimensions, c(512L, 512L))
  expect_identical(contract$background, 0L)
  expect_identical(contract$unknown, NA_integer_)
  pred <- matrix(0L, 512, 512)
  pred[1, 1] <- NA_integer_
  pred[2, 3] <- 1L
  identity <- list(name = "test-mask", version = "1", sha256 = paste(rep(
    "a", 64
  ), collapse = ""))
  staged <- hm_stage_annotations(pred, project, labels, contract, identity)
  receipt <- attr(staged, "hypermodR_receipt")
  expect_identical(receipt$unknown_mask, is.na(pred))
  expect_identical(receipt$unknown_count, 1L)
  expect_equal(receipt$background_count, 512^2 - 2)
  expect_identical(receipt$review_status, "unreviewed")
  expect_identical(hm_annotation_contract(project, labels), contract)
  changed <- contract
  changed$annotation_revision <- "stale"
  expect_error(hm_stage_annotations(
    pred, project, labels, changed,
    identity
  ), class = "hm_stale_annotation_error")
  expect_error(hm_stage_annotations(
    pred[1:2, ], project, labels, contract,
    identity
  ), "exact integer")
  bad <- pred
  bad[2, 2] <- 0.5
  expect_error(hm_stage_annotations(
    bad, project, labels, contract,
    identity
  ), "exact integer")
  bad[2, 2] <- 99
  expect_error(hm_stage_annotations(
    bad, project, labels, contract,
    identity
  ), "Unknown prediction")
  expect_error(hm_stage_annotations(pred, project, labels, contract, list(
    name = "x"
  )), "model identity")
  expect_error(hm_annotation_contract(list(), labels), "annot_project")
  for (bad in list(
    c(a = 0), c(a = 1, b = 1), c(a = NA_real_), c(1, 2),
    setNames(1, ""), c(a = Inf), c(a = 1.5)
  )) {
    expect_error(hm_annotation_contract(project, bad), "label_map")
  }
  d <- hm_as_dataset(project)
  expect_identical(d@provenance$adapter, "hyperannotatR")
  expect_identical(
    d@provenance$annotation_revision,
    contract$annotation_revision
  )
  expect_equal(dim(hm_read_block(d)$values)[1:2], c(512, 512))
  withr::local_options(hypermodR.block_bytes = 1)
  expect_error(hm_as_dataset(project$image), class = "hm_resource_error")
})

test_that("annotation export refuses ambiguous biological grouping", {
  skip_if_not_installed("hyperannotatR")
  skip_if_not_installed("magick")
  skip_if_not_installed("tiff")
  for (g in list(
    data.frame(), data.frame(
      subject_id = "s",
      procedure_id = "p"
    ),
    data.frame(name = "n", subject_id = NA_character_, procedure_id = "p"),
    data.frame(
      name = character(), subject_id = character(),
      procedure_id = character()
    ),
    data.frame(name = "n", subject_id = "s", procedure_id = 1)
  )) {
    expect_error(hm_export_annotations(NULL, tempfile(), g), "group|Grouping")
  }
})

test_that(paste0(
  "TIVITA explicit profile preserves source and rejects ",
  "registration ambiguity"
), {
  x <- list(
    cube = array(1:30, c(2, 5, 3)), wavelengths = NULL,
    metadata = list(note = "source"), references = list(),
    timestamp = "2024-01-02", path = "source"
  )
  profile <- list(
    id = "tivita-synthetic-yxb-v1", layout = "y,x,band",
    wavelengths = c(500, 600, 700)
  )
  mask <- matrix(c(NA, FALSE, rep(TRUE, 8)), 2, 5)
  assets <- data.frame(role = c("SpecCube", "RGB"), registration_id = "r")
  d <- hm_from_tivis(x, profile,
    assets = assets, registration_id = "r",
    mask = mask
  )
  expect_identical(d@sources[[1]], x$cube)
  expect_identical(d@assets, assets)
  expect_identical(d@provenance$spatial_masks[[1]], mask)
  expect_identical(d@provenance$radiometric_unit, "unknown")
  expect_identical(
    d@provenance$timestamp$evidence,
    "filename assumption only"
  )
  profile$wavelengths <- NULL
  expect_true(all(is.na(hm_from_tivis(x, profile)@bands$wavelength)))
  expect_error(
    hm_from_tivis(structure(x, class = "foreign"), profile),
    "unclassed"
  )
  expect_error(hm_from_tivis(x, list()), class = "hm_unsupported_error")
  broken <- x
  broken$cube <- matrix(1, 2, 3)
  expect_error(hm_from_tivis(broken, profile), "y,x,band")
  profile$wavelengths <- 1:2
  expect_error(hm_from_tivis(x, profile), "count")
  profile$wavelengths <- NULL
  expect_error(hm_from_tivis(x, profile, assets = list()), "data frame")
  expect_error(hm_from_tivis(x, profile, assets = data.frame(
    role = "bad",
    registration_id = "r"
  )), "Unknown asset")
  expect_error(hm_from_tivis(x, profile,
                 assets = assets,
                 registration_id = "other"
               ), class = "hm_source_error")
  expect_error(
    hm_as_hsi_cube(core_image(), "a", frame_id = "f"),
    "Images have no"
  )
  expect_error(hm_as_hsi_cube(core_sequence(), "a"), "Select one frame_id")
  cube <- hm_as_hsi_cube(core_sequence(), "a", "f2")
  expect_equal(cube$data, array(61:120, c(3, 5, 4)))
})

test_that(paste0(
  "annotation grouping merges connected identities before ",
  "public export"
), {
  skip_if_not_installed("hyperannotatR")
  skip_if_not_installed("magick")
  skip_if_not_installed("tiff")
  project <- hyperannotatR::at_example_project()
  destination <- tempfile()
  on.exit(unlink(destination, recursive = TRUE))
  groups <- data.frame(
    name = "example", subject_id = "subject-1",
    procedure_id = "procedure-1"
  )
  result <- hm_export_annotations(project, destination, groups,
    tile_size = 512L, mask_format = "npy", label_map = c(
      tumour = 1L,
      necrosis = 2L, stroma = 3L
    )
  )
  expect_true(dir.exists(destination))
  expect_true(length(result$checks) > 0L)
  expect_true(length(list.files(destination, recursive = TRUE)) > 0L)
})

test_that("unqualified CUVIS SDK inputs remain explicitly blocked", {
  required <- character()
  testthat::local_mocked_bindings(.hm_require = function(package) {
    required <<- c(required, package)
    invisible(TRUE)
  }, .package = "hypermodR")
  expect_error(hm_as_dataset(structure(list(), class = "cuvis_session")),
    "enumeration is unqualified",
    class = "hm_unsupported_error"
  )
  expect_error(
    hm_as_dataset(structure(list(),
      class = "cuvis_measurement"
    )), "scaling is unqualified",
    class = "hm_unsupported_error"
  )
  expect_identical(required, c("cuvis.r", "cuvis.r"))
  profile <- hm_tivita_profile(
    c(500, 600), "declared device", "export-v1",
    "fixture-1"
  )
  expect_identical(profile$qualification, "synthetic_only")
  expect_identical(profile$evidence, "fixture-1")
  expect_identical(profile$wavelengths, c(500, 600))
  profiles <- hm_instrument_profiles()
  expect_identical(
    profiles$status[profiles$profile == "cuvis-sdk"],
    "unqualified"
  )
})

test_that("transitive biological connections remain one export component", {
  skip_if_not_installed("hyperannotatR")
  skip_if_not_installed("magick")
  skip_if_not_installed("tiff")
  received <- NULL
  testthat::local_mocked_bindings(
    at_training_export = function(
      x,
      destination, group_by, groups, ...
    ) {
      received <<- groups
      expect_identical(group_by, "subject_id")
      list(marker = "export")
    }, at_training_check = function(destination) list(integrity = TRUE),
    .package = "hyperannotatR"
  )
  groups <- data.frame(name = letters[1:4], subject_id = c(
    "s1", "s2",
    "s2", "s4"
  ), procedure_id = c("p1", "p1", "p3", "p4"))
  out <- hm_export_annotations(NULL, "declared-export", groups)
  expect_identical(received$subject_id, c(
    "connected-1", "connected-1",
    "connected-1", "connected-4"
  ))
  expect_identical(received$procedure_id, groups$procedure_id)
  expect_identical(groups$subject_id, c("s1", "s2", "s2", "s4"))
  expect_identical(out$checks, list(integrity = TRUE))
})
