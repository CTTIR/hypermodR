#' Admit a bounded Spectralverse benchmark protocol without accessing data
#' @param protocol List declaring version=1, program=Spectralverse, cohort,
#' role, dataset_revision, reference_revision, license_admitted,
#' person_grouping_verified, source_cases, roi_ids, input_track,
#' context_budget, pretraining_overlap, prior_outcome_access, target_access and
#' capabilities.
#' @param comparison Optional second protocol requiring identical
#' cohort/revisions/cases/ROIs/input/context and target access.
#' @return List with validated protocol, content hash, target-role
#' classification, capability-lane admission states, execution=not_run and
#' scientific_qualification=FALSE.
#' @details Cohorts are HeiPor (primary_anatomy), xeno
#' (development_species_perfusion) and ManU (human_admission). ManU requires
#' verified person grouping. Each capability lane supplies model_id, task,
#' platform, logical qualified and evidence. Admission is declarative and does
#' not access data or execute a benchmark.
#' @export
hm_benchmark_protocol <- function(protocol, comparison = NULL) {
  p <- protocol
  required <- c(
    "version", "program", "cohort", "role", "dataset_revision",
    "reference_revision",
    "license_admitted", "person_grouping_verified", "source_cases", "roi_ids",
    "input_track", "context_budget", "pretraining_overlap",
    "prior_outcome_access",
    "target_access", "capabilities"
  )
  if (!is.list(p) ||
        !all(required %in% names(p))) {
    .hm_abort("Incomplete benchmark protocol.")
  }
  if (!identical(p$version, "1") ||
        !identical(p$program, "Spectralverse") ||
        !p$cohort %in% c("HeiPor", "xeno", "ManU") ||
        !isTRUE(p$license_admitted)) {
    .hm_abort("Benchmark version, cohort or use terms not admitted.")
  }
  role <- c(
    HeiPor = "primary_anatomy", xeno = "development_species_perfusion",
    ManU = "human_admission"
  )
  if (!identical(p$role, unname(role[p$cohort])) ||
        (p$cohort == "ManU" && !isTRUE(p$person_grouping_verified))) {
    .hm_abort("Cohort role/person grouping admission failed.")
  }
  for (k in c(
    "dataset_revision",
    "reference_revision"
  )) {
    .hm_scalar_character(p[[k]], k)
  }
  for (k in c("source_cases", "roi_ids")) {
    if (!is.character(p[[k]]) ||
          !length(p[[k]]) ||
          anyNA(p[[k]]) ||
          any(!nzchar(p[[k]])) ||
          anyDuplicated(p[[k]])) {
      .hm_abort("Explicit unique case/ROI support required.")
    }
  }
  if (!p$input_track %in% c(
    "native_spectral", "documented_adaptation",
    "auxiliary_rendering"
  ) ||
    !p$pretraining_overlap %in% c("none_verified", "known", "unknown") ||
    !p$target_access %in% c("none", "unlabelled", "labelled") ||
    !is.logical(p$prior_outcome_access) ||
    length(p$prior_outcome_access) !=
      1 || is.na(p$prior_outcome_access) ||
    length(p$context_budget) !=
      1 || !is.finite(p$context_budget) ||
    p$context_budget < 1 || p$context_budget != as.integer(
    p$context_budget
  )) {
    .hm_abort("Invalid access ledger or context budget.")
  }
  if (!is.list(p$capabilities) ||
        !length(p$capabilities)) {
    .hm_abort("Capability qualification matrix required.")
  }
  for (lane in p$capabilities) {
    if (!is.list(lane) ||
      !all(
        c("model_id", "task", "platform", "qualified", "evidence") %in%
          names(lane)
      ) ||
      !is.logical(lane$qualified) ||
      length(lane$qualified) !=
        1 || is.na(lane$qualified)) {
      .hm_abort("Invalid capability lane.")
    }
    for (k in c(
      "model_id", "task", "platform",
      "evidence"
    )) {
      .hm_scalar_character(lane[[k]], k)
    }
  }
  if (!is.null(comparison)) {
    other <- hm_benchmark_protocol(comparison)$protocol
    for (k in c(
      "cohort", "dataset_revision", "reference_revision", "source_cases",
      "roi_ids", "input_track", "context_budget", "target_access"
    )) {
      if (!identical(p[[k]], other[[k]])) {
        .hm_abort(paste0(
          "Compared methods require identical ",
          "source/ROI/input/context support."
        ))
      }
    }
  }
  untouched <- p$cohort != "xeno" && !p$prior_outcome_access &&
    p$pretraining_overlap ==
      "none_verified" && p$target_access == "none"
  list(
    protocol = p, hash = .hm_hash(p),
    target_role = if (
                      untouched) {
      "eligible_prospective_target"
    } else {
      "development_or_overlap_unresolved"
    },
    lanes = lapply(
      p$capabilities, function(x) {
        c(
          x, list(
            status = if (
                         x$qualified) {
              "eligible_requires_execution"
            } else {
              "blocked_unqualified"
            }
          )
        )
      }
    ),
    execution = "not_run", scientific_qualification = FALSE
  )
}
