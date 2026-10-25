#' Bind an annotation project to its immutable export contract
#' @param project annot_project.
#' @param label_map Named positive integer class codes; zero is background.
#' @return Serializable exact manifest binding and label map.
#' @export
hm_annotation_contract <- function(project, label_map) {
  .hm_require("hyperannotatR")
  .hm_label_map(label_map)
  if (!inherits(project, "annot_project")) {
    .hm_abort("Expected annot_project.")
  }
  m <- hyperannotatR::at_interop_manifest(project, hash_sources = TRUE)
  # Manifest issue time changes on every call; content and revision do
  # not.
  m$created <- NULL
  list(
    manifest_hash = .hm_hash(m),
    annotation_revision = hyperannotatR::at_annotation_revision(project),
    label_map = label_map, dimensions = as.integer(hyperannotatR::at_dims(
      project$image, 0L
    )),
    unknown = NA_integer_, background = 0L
  )
}
.hm_label_map <- function(x) {
  if (!is.numeric(x) ||
    !length(x) ||
    is.null(names(x)) ||
    anyNA(x) ||
    any(
      !is.finite(x) |
        x < 1 | x != floor(x)
    ) ||
    anyDuplicated(x) ||
    anyNA(names(x)) ||
    any(!nzchar(names(x))) ||
    anyDuplicated(names(x))) {
    .hm_abort("label_map must have unique names and positive integer codes.")
  }
}
#' Export grouped annotations through hyperannotatR
#' @param x Annotation project or session.
#' @param destination Export directory.
#' @param groups Explicit biological grouping table accepted by
#' at_training_export.
#' @param ... Additional public export arguments.
#' @return Companion export including integrity checks.
#' @export
hm_export_annotations <- function(x, destination, groups, ...) {
  .hm_require("hyperannotatR")
  if (!is.data.frame(groups) ||
    !"subject_id" %in% names(groups) ||
    !any(
      c("procedure_id", "sequence_id") %in%
        names(groups)
    ) ||
    anyNA(groups$subject_id) ||
    any(!nzchar(groups$subject_id))) {
    .hm_abort(
      paste0(
        "Explicit subject_id plus procedure_id or sequence_id ",
        "grouping is required."
      )
    )
  }
  if (!any(
    c("entry_id", "name") %in%
      names(groups)
  )) {
    .hm_abort("Grouping rows require entry_id or name.")
  }
  keys <- intersect(
    c("subject_id", "procedure_id", "sequence_id"),
    names(groups)
  )
  if (!nrow(groups) ||
    any(
      vapply(
        groups[keys], function(v) {
          !is.character(v) ||
            anyNA(v) ||
            any(!nzchar(v))
        }, logical(1)
      )
    )) {
    .hm_abort(paste0(
      "Grouping identities must be explicit nonmissing ",
      "character values."
    ))
  }
  # Connected rows sharing any biological or sequence identity stay
  # together.
  component <- seq_len(nrow(groups))
  repeat {
    old <- component
    for (key in keys) {
      for (rows in split(
        seq_len(nrow(groups)),
        groups[[key]]
      )) {
        ids <- unique(component[rows])
        component[component %in% ids] <- min(ids)
      }
    }
    if (identical(old, component)) {
      break
    }
  }
  groups$subject_id <- paste0("connected-", component)
  out <- hyperannotatR::at_training_export(x, destination,
    group_by = "subject_id", groups = groups, ...
  )
  out$checks <- hyperannotatR::at_training_check(destination)
  out
}
