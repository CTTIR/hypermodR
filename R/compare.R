.hm_check_pair <- function(a, b) {
  if (!inherits(a, "hm_evaluation") ||
    !inherits(b, "hm_evaluation") ||
    !identical(a$reference, b$reference) ||
    !identical(a$predictions$task, b$predictions$task) ||
    !identical(a$predictions$class_map, b$predictions$class_map) ||
    !identical(a$estimand, b$estimand) ||
    !identical(a$hierarchy, b$hierarchy) ||
    !identical(
      names(a$metrics),
      names(b$metrics)
    )) {
    .hm_abort(
      paste0(
        "Paired comparison requires identical cases, ",
        "references, tasks and estimands."
      )
    )
  }
}
#' Compare matched predictions using paired whole-unit resampling
#' @param evaluations List of exactly two matched `hm_evaluation` objects,
#' ordered first minus second.
#' @param paired Must be TRUE.
#' @param unit Reference column identifying independent whole units.
#' @param draws Integer number of paired bootstrap draws, at least two.
#' @param seed Reproducible resampling seed.
#' @param family Optional descriptive comparison-family declaration, retained
#' without multiplicity adjustment.
#' @return An `hm_evaluation` list containing metric differences, paired
#' bootstrap uncertainty, family and interpretation.
#' @details Requires identical cases, references, task, class map, estimand,
#' hierarchy and metric names. This is descriptive inference without
#' multiplicity-adjusted significance claims.
#' @examples
#' data <- hm_example(seed = 42L)
#' task <- hm_task("response", "regression", "spectrum", "subject_id")
#' fold <- hm_split(data, task, folds = 3L, seed = 7L)$folds[[1]]
#' workflow <- hm_workflow(
#'   task, hm_recipe(list(list(type = "scale"))), hm_model_spec("lm", task)
#' )
#' fit <- hm_fit(workflow, data, fold)
#' heldout <- hm_spectra(data, list(
#'   observation_id = data@observations$observation_id[fold$test]
#' ))
#' knn <- hm_workflow(
#'   task, hm_recipe(list(list(type = "scale"))),
#'   hm_model_spec("knn", task, list(k = 5))
#' )
#' first <- hm_evaluate(hm_predict(fit, heldout), heldout$observations)
#' second <- hm_evaluate(
#'   hm_predict(hm_fit(knn, data, fold), heldout), heldout$observations
#' )
#' comparison <- hm_compare(list(first, second), unit = "subject_id",
#'                          draws = 50L, seed = 1L)
#' comparison$metrics
#' @export
hm_compare <- function(
  evaluations, paired = TRUE, unit, draws = 200L,
  seed = 1L, family = NULL
) {
  if (!isTRUE(paired) ||
        length(evaluations) !=
          2) {
    .hm_abort("Exactly two paired evaluations are required.")
  }
  a <- evaluations[[1]]
  b <- evaluations[[2]]
  .hm_check_pair(a, b)
  structure(
    list(
      metrics = a$metrics - b$metrics, uncertainty = hm_bootstrap(a, unit,
        draws, seed,
        comparison = b
      ),
      family = family, inference = paste0(
        "Descriptive paired differences; no ",
        "multiplicity-adjusted significance claim."
      )
    ),
    class = "hm_evaluation"
  )
}
