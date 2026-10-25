.hm_observations <- function(data) {
  if (is.data.frame(data)) {
    return(data)
  }
  if (methods::is(data, "HMDataset")) {
    return(data@observations)
  }
  if (is.list(data) &&
        is.data.frame(data$observations)) {
    return(data$observations)
  }
  .hm_abort("Observation table required.")
}
.hm_components <- function(obs, groups) {
  n <- nrow(obs)
  parent <- seq_len(n)
  root <- function(i) {
    while (parent[i] != i) i <- parent[i]
    i
  }
  merge <- function(a, b) {
    ra <- root(a)
    rb <- root(b)
    if (ra != rb) {
      parent[rb] <<- ra
    }
  }
  for (g in groups) {
    if (!g %in% names(obs) ||
          !is.character(obs[[g]]) ||
          anyNA(obs[[g]]) ||
          any(!nzchar(obs[[g]]))) {
      .hm_abort(paste0(
        "Declared grouping columns must contain complete ",
        "character identities."
      ))
    }
    for (ids in split(
      seq_len(n),
      obs[[g]]
    )) {
      if (length(ids) >
            1) {
        for (j in ids[-1]) merge(ids[1], j)
      }
    }
  }
  if (all(
    c("source_id", "window_start", "window_end") %in%
      names(obs)
  )) {
    if (anyNA(obs[, c("source_id", "window_start", "window_end")]) ||
          any(obs$window_start > obs$window_end)) {
      .hm_abort("Invalid source windows.")
    }
    for (ids in split(
      seq_len(n),
      obs$source_id
    )) {
      for (a in ids) {
        for (b in ids) {
          if (obs$window_start[a] <= obs$window_end[b] &&
                obs$window_start[b] <=
                  obs$window_end[a]) {
            merge(a, b)
          }
        }
      }
    }
  }
  vapply(
    seq_len(n),
    root, integer(1)
  )
}
#' Plan grouped resampling
#' @param data Observation data frame, HMDataset, or list containing
#' observations.
#' @param task An `hm_task` declaring grouping columns.
#' @param design One of grouped, nested_grouped or temporal.
#' @param seed Seed for reproducible grouped assignment.
#' @param folds Integer outer-fold count, at least two.
#' @param inner_folds Integer inner-fold count for nested_grouped, at least two.
#' @param ... Temporal controls: time is the numeric timestamp-column name
#' (default timestamp), cutoff is required, and nonnegative look_back/embargo
#' use the same units as time.
#' @return An `hm_split`. Grouped designs contain folds (train/test row indices
#' and optional inner folds), component membership, observation identities and
#' hash. Temporal designs contain train/test/excluded indices, cutoff,
#' look-back and embargo.
#' @details Groups connected through any task grouping column or known
#' biological/source identities stay together; overlapping source windows are
#' connected. Grouped assignment balances component counts rather than
#' stratifying outcomes and rejects outer categorical folds without full class
#' support. Temporal training precedes cutoff minus embargo and look-back;
#' groups exposed at/after cutoff are removed entirely from training. Source
#' times are not repaired or reinterpreted.
#' @export
hm_split <- function(
  data, task, design = "nested_grouped", seed = 1L, folds = 5L,
  inner_folds = 3L,
  ...
) {
  if (!inherits(task, "hm_task")) {
    .hm_abort("Expected task.")
  }
  obs <- .hm_observations(data)
  .hm_table(obs, "observation_id")
  if (design == "temporal") {
    return(.hm_temporal_split(obs, task, seed, ...))
  }
  if (!design %in% c("grouped", "nested_grouped")) {
    .hm_abort("Unknown split design.")
  }
  groups <- unique(
    c(
      task$groups, intersect(
        c(
          "subject_id", "procedure_id", "wound_id", "sequence_id",
          "acquisition_id",
          "source_lineage_id"
        ),
        names(obs)
      )
    )
  )
  component <- .hm_components(obs, groups)
  units <- unique(component)
  if (length(folds) !=
        1 || folds < 2 || folds != as.integer(folds) ||
        length(units) <
          folds) {
    .hm_abort("Insufficient independent groups for requested folds.")
  }
  membership <- .hm_seed(
    seed,
    {
      z <- sample(units)
      setNames(
        rep(
          seq_len(folds),
          length.out = length(z)
        ),
        z
      )
    }
  )
  make <- function(train, test) {
    list(
      train = train, test = test,
      observation_id = obs$observation_id
    )
  }
  plans <- lapply(
    seq_len(folds),
    function(k) {
      test <- unname(
        which(
          membership[as.character(component)] ==
            k
        )
      )
      train <- setdiff(
        seq_len(nrow(obs)),
        test
      )
      p <- make(train, test)
      if (task$type %in% c("classification", "ordinal", "segmentation")) {
        y <- obs[[task$outcome]]
        if (is.null(y) ||
          anyNA(y) ||
          !setequal(
            unique(y[train]),
            unique(y)
          ) ||
          !setequal(
            unique(y[test]),
            unique(y)
          )) {
          .hm_abort(
            "Impossible class support in grouped folds.",
            "hm_split_error"
          )
        }
      }
      if (design == "nested_grouped") {
        u <- unique(component[train])
        if (length(inner_folds) !=
          1 || inner_folds < 2 || inner_folds != as.integer(
                                                            inner_folds) ||
          length(u) <
            inner_folds) {
          .hm_abort(
            "Insufficient independent training groups for inner folds."
          )
        }
        m <- .hm_seed(
          seed + k, setNames(
            rep(
              seq_len(inner_folds),
              length.out = length(u)
            ),
            sample(u)
          )
        )
        p$inner <- lapply(
          seq_len(inner_folds),
          function(j) {
            make(
              train[m[as.character(component[train])] !=
                      j], train[m[as.character(component[train])] ==
                                  j]
            )
          }
        )
      }
      p
    }
  )
  structure(
    list(
      design = design, seed = seed, folds = plans, component = component,
      observation_id = obs$observation_id, roles = obs[, intersect(
        c("role", "previous_outcome_access"),
        names(obs)
      ),
      drop = FALSE
      ], hash = .hm_hash(list(component, plans))
    ),
    class = "hm_split"
  )
}
