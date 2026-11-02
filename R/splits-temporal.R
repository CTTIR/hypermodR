.hm_time_hash <- function(obs, time) {
  .hm_hash(list(observation_id = unname(as.character(obs$observation_id)),
                time = unname(as.double(obs[[time]]) + 0)))
}
.hm_temporal_split <- function(
  obs, task, seed, time = "timestamp", cutoff = NULL, look_back = 0,
  embargo = 0
) {
  .hm_scalar_character(time, "time")
  if (!time %in% names(obs) ||
        !is.numeric(obs[[time]]) ||
        any(!is.finite(obs[[time]]))) {
    .hm_abort("Explicit finite numeric times required.")
  }
  if (is.null(cutoff) ||
        length(cutoff) !=
          1 || !is.finite(cutoff) ||
        length(look_back) !=
          1 || !is.finite(look_back) ||
        look_back < 0 || length(embargo) !=
        1 || !is.finite(embargo) ||
        embargo < 0) {
    .hm_abort("Explicit cutoff and nonnegative look-back/embargo required.")
  }
  component <- .hm_components(obs, .hm_split_groups(obs, task))
  train <- which(obs[[time]] < cutoff - embargo - look_back)
  test <- which(obs[[time]] >= cutoff)
  # Any group exposed in the future is excluded entirely from training.
  train <- train[!component[train] %in% component[test]]
  if (!length(train) ||
        !length(test)) {
    .hm_abort("No independent temporal train/test support.", "hm_split_error")
  }
  structure(
    list(
      design = "temporal", seed = seed, observation_id = obs$observation_id,
      train = train, test = test, excluded = setdiff(
        seq_len(nrow(obs)),
        c(train, test)
      ),
      component = component, time = time,
      time_hash = .hm_time_hash(obs, time),
      cutoff = cutoff, look_back = look_back,
      embargo = embargo,
      roles = obs[, intersect(
        c("role", "previous_outcome_access"),
        names(obs)
      ),
      drop = FALSE
      ]
    ),
    class = "hm_split"
  )
}
