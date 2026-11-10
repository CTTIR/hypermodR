#' Stage a bound integer prediction for human review
#' @param predictions Integer y,x mask. NA is unknown; zero is background.
#' @param project Destination annot_project.
#' @param label_map Named class codes.
#' @param contract hm_annotation_contract captured before prediction.
#' @param model Model name, version and sha256 provenance.
#' @return Companion staged patch with unknown mask receipt; never committed.
#' @examples
#' \donttest{
#' if (requireNamespace("hyperannotatR", quietly = TRUE) &&
#'     requireNamespace("tiff", quietly = TRUE)) {
#'   project <- hyperannotatR::at_example_project()
#'   label_map <- c(tumour = 1L, necrosis = 2L, stroma = 3L)
#'   contract <- hm_annotation_contract(project, label_map)
#'   mask <- matrix(0L, contract$dimensions[2], contract$dimensions[1])
#'   mask[1:3, 2:5] <- 1L
#'   mask[10, 11] <- NA_integer_
#'   patch <- hm_stage_annotations(
#'     mask, project, label_map, contract,
#'     list(name = "example-model", version = "1", sha256 = strrep("a", 64))
#'   )
#'   attr(patch, "hypermodR_receipt")$unknown_count
#' }
#' }
#' @export
hm_stage_annotations <- function(
  predictions, project, label_map, contract,
  model
) {
  .hm_require("hyperannotatR")
  .hm_label_map(label_map)
  current <- hm_annotation_contract(project, label_map)
  if (!identical(contract, current)) {
    .hm_abort(
      "Image content, revision, geometry or label map changed.",
      "hm_stale_annotation_error"
    )
  }
  if (!is.matrix(predictions) ||
    !is.numeric(predictions) ||
    any(
      !is.na(predictions) &
        (!is.finite(predictions) |
           predictions != floor(predictions))
    ) ||
    !identical(
      as.integer(dim(predictions)),
      rev(contract$dimensions)
    )) {
    .hm_abort("Predictions require exact integer y,x grid.")
  }
  if (any(
    !is.na(predictions) &
      !predictions %in% c(0, unname(label_map))
  )) {
    .hm_abort("Unknown prediction class.")
  }
  if (!is.list(model) ||
    !all(
      c("name", "version", "sha256") %in%
        names(model)
    ) ||
    any(
      vapply(
        model[c("name", "version", "sha256")],
        function(z) {
          !is.character(z) ||
            length(z) !=
              1L || is.na(z) ||
            !nzchar(z)
        }, logical(1)
      )
    )) {
    .hm_abort("Explicit model identity required.")
  }
  unknown <- is.na(predictions)
  out <- hyperannotatR::at_training_import(
    project, predictions, label_map,
    model = model, stage = TRUE, replace = FALSE
  )
  attr(out, "hypermodR_receipt") <- list(
    unknown_mask = unknown, background_count = sum(predictions == 0,
      na.rm = TRUE
    ),
    unknown_count = sum(unknown),
    contract = contract, review_status = "unreviewed"
  )
  out
}
