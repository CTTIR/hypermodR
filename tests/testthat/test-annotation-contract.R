test_that(
  "class labels cannot conflate background or unknown",
  {
    expect_error(hm_annotation_contract(NULL, c(tissue = 0)))
    expect_error(hm_export_annotations(NULL, "unused", data.frame(
      subject_id = "s"
    )))
  }
)
test_that(
  paste0(
    "installed annotation bridge binds content revision ",
    "geometry and staged masks"
  ),
  {
    skip_if_not_installed("hyperannotatR")
    skip_if_not_installed("tiff")
    p <- hyperannotatR::at_example_project()
    map <- c(tumour = 1L, necrosis = 2L, stroma = 3L)
    contract <- hm_annotation_contract(p, map)
    d <- hm_as_dataset(p)
    expect_identical(
      dim(d@sources[[1]])[1:2],
      rev(contract$dimensions)
    )
    expect_identical(
      d@provenance$annotation_revision,
      contract$annotation_revision
    )
    m <- matrix(0L, contract$dimensions[2], contract$dimensions[1])
    m[1:3, 2:5] <- 1L
    m[10, 11] <- NA_integer_
    before <- serialize(p, NULL)
    patch <- hm_stage_annotations(
      m, p, map, contract, list(
        name = "fixture", version = "1", sha256 = paste(
          rep("a", 64),
          collapse = ""
        )
      )
    )
    expect_s3_class(patch, "at_staged_patch")
    expect_identical(
      serialize(p, NULL),
      before
    )
    expect_identical(
      attr(patch, "hypermodR_receipt")$unknown_count,
      1L
    )
    expect_true(
      attr(patch, "hypermodR_receipt")$unknown_mask[
        10,
        11
      ]
    )
    stale <- contract
    stale$annotation_revision <- "stale"
    expect_error(
      hm_stage_annotations(m, p, map, stale, list()),
      class = "hm_stale_annotation_error"
    )
    expect_error(
      hm_stage_annotations(
        m, p, c(tumour = 2L, necrosis = 1L, stroma = 3L),
        contract, list()
      ),
      class = "hm_stale_annotation_error"
    )
    dest <- tempfile("annotation-export-")
    on.exit(
      unlink(dest, recursive = TRUE),
      add = TRUE
    )
    ex <- hm_export_annotations(
      p, dest, data.frame(
        entry_id = "entry-0001", subject_id = "subject",
        procedure_id = "procedure"
      ),
      tile_size = 128L, fractions = c(train = 1),
      label_map = map
    )
    expect_true(all(ex$checks$status == "ok"))
  }
)
