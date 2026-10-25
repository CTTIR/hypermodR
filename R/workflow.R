#' Assemble a modelling workflow
#' @param task An `hm_task`.
#' @param recipe An unfitted `hm_recipe`.
#' @param model An `hm_model_spec` whose task is identical to task.
#' @return An `hm_workflow` list containing the three declarations.
#' @details Assembly validates compatible declarations; fitting and backend
#' admission occur separately.
#' @export
hm_workflow <- function(task, recipe, model) {
  if (!inherits(task, "hm_task") ||
        !inherits(recipe, "hm_recipe") ||
        !inherits(model, "hm_model_spec") ||
        !identical(task, model$task)) {
    .hm_abort("Incompatible workflow components.")
  }
  structure(
    list(task = task, recipe = recipe, model = model),
    class = "hm_workflow"
  )
}
.hm_model_data <- function(data) {
  if (methods::is(data, "HMDataset")) {
    return(hm_spectra(data))
  }
  if (is.list(data) &&
        is.matrix(data$values) &&
        is.data.frame(data$observations)) {
    if (is.null(data$band_map)) {
      .hm_abort("Explicit band_map is required.")
    }
    normalized <- hm_dataset(
      data$values, data$observations, data$band_map,
      provenance = if (is.null(data$provenance)) {
        list()
      } else {
        data$provenance
      }
    )
    return(hm_spectra(normalized))
  }
  .hm_abort("Expected HMDataset or explicit values/observations list.")
}
#' Fit a workflow using training indices only
#' @param workflow An `hm_workflow`.
#' @param data An HMDataset of native spectra, or list with numeric values
#' matrix, observations data frame, band_map and optional provenance.
#' @param split One explicit train/test fold, normally from `hm_split()`;
#' indices are source row positions.
#' @param control Engine-control list: seed defaults to one; weights may align
#' to all source rows or training rows. Optional tuning contains candidates,
#' metric and direction. Reference workers require backend_handle and a new
#' artifact_path.
#' @return An `hm_fit` containing workflow, fitted recipe, engine, training
#' indices, validated split, original band map and hash-bound provenance.
#' @details Preprocessing and engine fitting use training rows only. Connected
#' biological/source identities cannot cross train/test; inner folds must
#' remain inside outer training. Spatial images/sequences require explicit
#' supported routes and are never silently flattened. R reference spectral
#' training currently admits reference:cnn1d, reference:spectral_vit,
#' reference:masked_autoencoder, reference:vae, reference:distillation,
#' reference:multitask and reference:domain_adversarial with the appropriate
#' task; worker-level tensor families have a separate protocol. A fitted model
#' is not independent scientific qualification.
#' @export
hm_fit <- function(workflow, data, split, control = list()) {
  if (!inherits(workflow, "hm_workflow")) {
    .hm_abort("Expected workflow.")
  }
  if (!is.null(control$tuning)) {
    tuning <- control$tuning
    control$tuning <- NULL
    return(
      hm_tune(
        workflow, data, split, tuning$candidates, control,
        metric = tuning$metric,
        direction = if (is.null(
          tuning$direction
        )) {
          "minimize"
        } else {
          tuning$direction
        }
      )
    )
  }
  d <- .hm_model_data(data)
  split <- .hm_validate_fit_split(d, workflow$task, split)
  idx <- split$train
  recipe <- hm_prep(
    workflow$recipe, d$values[idx, , drop = FALSE],
    d$band_map$wavelength
  )
  x <- hm_bake(recipe, d$values[idx, , drop = FALSE])
  control$observations <- d$observations[idx, , drop = FALSE]
  control$wavelength <- recipe$output_wavelength
  control$input_provenance <- d$provenance
  if (!is.null(control$weights) &&
        length(control$weights) ==
          nrow(d$values)) {
    control$weights <- control$weights[idx]
  }
  target <- if (is.null(workflow$task$outcome)) {
    NULL
  } else {
    d$observations[[workflow$task$outcome]]
  }
  y <- if (is.matrix(target)) {
    target[idx, , drop = FALSE]
  } else {
    target[idx]
  }
  if (workflow$task$type == "survival") {
    ref <- workflow$task$reference
    if (!is.list(ref) ||
      !all(
        c("time", "status") %in%
          names(ref)
      ) ||
      !all(
        unlist(ref[c("time", "status")]) %in%
          names(d$observations)
      )) {
      .hm_abort("Survival requires time/status reference columns.")
    }
    y <- data.frame(
      time = d$observations[[ref$time]][idx],
      status = d$observations[[ref$status]][idx]
    )
  }
  .hm_validate_target(y, workflow$task, length(idx))
  if (!exists(".hm_fit_engine", mode = "function")) {
    .hm_abort("No fitting engine is available.", "hm_unsupported_error")
  }
  seed <- if (is.null(control$seed)) {
    1L
  } else {
    control$seed
  }
  engine <- .hm_seed(
    seed, .hm_fit_engine(
      workflow$model$model_id, x, y, workflow$task, workflow$model$parameters,
      control
    )
  )
  structure(
    list(
      workflow = workflow, recipe = recipe, engine = engine, training = idx,
      split = split, band_map = d$band_map, provenance = hm_provenance(
        list(data = data, split = split, recipe = recipe)
      )
    ),
    class = "hm_fit"
  )
}
#' Predict with a fitted workflow
#' @param object An `hm_fit`.
#' @param newdata Native spectral data in the same form as hm_fit data, with
#' exactly matching measured band semantics.
#' @param ... Engine prediction controls; reference workers require control
#' with the owned backend_handle.
#' @return An `hm_prediction` in newdata observation order, with task-specific
#' values and optional class probabilities.
#' @details Applies the frozen training recipe. Measured
#' wavelength/unit/fwhm/native/canonical band metadata present during training
#' must match exactly. Prediction does not fit preprocessing or use new
#' outcomes.
#' @export
hm_predict <- function(object, newdata, ...) UseMethod("hm_predict")
#' @rdname hm_predict
#' @export
hm_predict.hm_fit <- function(object, newdata, ...) {
  d <- .hm_model_data(newdata)
  fields <- intersect(
    c("wavelength", "unit", "fwhm", "native_index", "canonical_index"),
    names(object$band_map)
  )
  if (!all(fields %in% names(d$band_map)) ||
    !identical(
      object$band_map[, fields, drop = FALSE], d$band_map[, fields,
        drop = FALSE
      ]
    )) {
    .hm_abort(
      "Prediction band semantics differ from training.",
      "hm_band_mismatch"
    )
  }
  x <- hm_bake(object$recipe, d$values)
  out <- .hm_predict_engine(object$engine, x, object$workflow$task, ...)
  if (inherits(out, "hm_prediction")) {
    return(out)
  }
  if (!is.list(out)) {
    if (is.matrix(out) &&
      object$workflow$task$type %in% c(
        "classification", "ordinal",
        "segmentation"
      )) {
      classes <- object$engine$classes
      out <- list(
        values = classes[max.col(out, ties.method = "first")],
        probabilities = out, class_map = classes
      )
    } else {
      out <- list(values = out)
      if (object$workflow$task$type %in% c(
        "classification", "ordinal",
        "segmentation"
      )) {
        out$class_map <- object$engine$classes
      }
    }
  }
  do.call(
    hm_prediction, c(
      out, list(
        task = object$workflow$task,
        observation_id = d$observations$observation_id
      )
    )
  )
}
#' @rdname hm_predict
#' @export
predict.hm_fit <- function(object, newdata, ...) {
  hm_predict(
    object,
    newdata, ...
  )
}
#' @param object An `hm_fit`.
#' @param ... Arguments forwarded to the fitted engine coefficient method where
#' supported.
#' @param scale Either fitted coordinates or original measured-band coordinates.
#' @return Engine coefficients with scale and link attributes. Original-scale
#' output is a named intercept/band vector (or engine list of vectors), with
#' band_map attached.
#' @details Fitted coefficient availability depends on the engine.
#' Original-band mapping is restricted to lm, glm, glmnet, pls, pcr and
#' bayes_conjugate with globally affine scale, pca, select, derivative,
#' resample or reflectance recipes. Nonlinear/missingness-dependent recipes,
#' aliased coefficients and incompatible dimensions raise typed errors.
#' Coefficients describe a fitted coordinate system and do not establish
#' biological causality.
#' @rdname hm_fit
#' @export
coef.hm_fit <- function(object, ..., scale = c("fitted", "original")) {
  scale <- match.arg(scale)
  engine <- object$engine
  if (engine$model_id == "bayes_conjugate") {
    out <- engine$object$mean
    names(out) <- c(
      "(Intercept)", paste0(
        "band_", seq_len(
          length(out) -
            1L
        )
      )
    )
  } else {
    supported <- c(
      "lm", "glm", "gam", "mixed", "pls", "pcr", "glmnet", "coxph", "clm",
      "brms"
    )
    if (!engine$model_id %in% supported) {
      .hm_abort(
        "Coefficients unavailable for this engine.",
        "hm_unsupported_error"
      )
    }
    if (engine$model_id %in% c("pls", "pcr")) {
      out <- as.vector(stats::coef(engine$object,
        ncomp = engine$ncomp,
        intercept = TRUE
      ))
    } else if (engine$model_id == "glmnet") {
      out <- stats::coef(engine$object, s = engine$lambda)
    } else {
      out <- stats::coef(engine$object, ...)
    }
  }
  if (scale == "original") {
    if (!engine$model_id %in% c(
      "lm", "glm", "glmnet", "pls", "pcr",
      "bayes_conjugate"
    )) {
      .hm_abort(
        "This engine has no single original-band coefficient map.",
        "hm_unsupported_error"
      )
    }
    map <- .hm_recipe_affine_map(object$recipe)
    transform <- function(beta) {
      beta <- as.numeric(beta)
      if (length(beta) !=
            ncol(map$matrix) +
              1L || any(!is.finite(beta))) {
        .hm_abort(
          "Aliased or incompatible coefficients cannot be mapped.",
          "hm_identifiability_error"
        )
      }
      c(
        `(Intercept)` = beta[1L] + sum(map$offset * beta[-1L]),
        stats::setNames(
          as.vector(map$matrix %*% beta[-1L]),
          paste0("band_", seq_len(nrow(map$matrix)))
        )
      )
    }
    out <- if (is.list(out)) {
      lapply(out, transform)
    } else {
      transform(out)
    }
    attr(out, "band_map") <- object$band_map
  }
  attr(out, "scale") <- scale
  attr(out, "link") <- if (engine$model_id ==
                             "glm") {
    engine$object$family$link
  } else {
    "identity or engine linear predictor"
  }
  out
}

.hm_recipe_affine_map <- function(recipe) {
  allowed <- c(
    "scale", "pca", "select", "derivative", "resample",
    "reflectance"
  )
  if (any(
    !vapply(
      recipe$steps, function(x) {
        x$type %in% allowed
      }, logical(1)
    )
  )) {
    .hm_abort(
      paste0(
        "Nonlinear or missingness-dependent recipes have no ",
        "global affine band map."
      ),
      "hm_unsupported_error"
    )
  }
  n <- recipe$n_features
  offset <- as.vector(hm_bake(recipe, matrix(0, 1L, n)))
  matrix <- sweep(
    hm_bake(recipe, diag(n)),
    2L, offset
  )
  list(matrix = matrix, offset = offset)
}
#' @rdname hm_fit
#' @param x An `hm_fit`.
#' @return Print methods return the input invisibly.
#' @export
print.hm_fit <- function(x, ...) {
  cat("<hm_fit>", x$workflow$model$model_id, "\n")
  invisible(x)
}
#' @rdname hm_fit
#' @return Summary methods return model identity and training-row count.
#' @export
summary.hm_fit <- function(object, ...) {
  list(
    model = object$workflow$model$model_id, training_n = length(
      object$training
    )
  )
}

.hm_validate_fit_split <- function(data, task, split) {
  n <- nrow(data$values)
  if (!is.list(split) ||
        is.null(split$train)) {
    .hm_abort("Select one explicit split fold.")
  }
  train <- split$train
  test <- if (is.null(split$test)) {
    setdiff(
      seq_len(n),
      train
    )
  } else {
    split$test
  }
  for (index in list(train, test)) {
    if (!is.numeric(index) ||
          anyNA(index) ||
          any(!is.finite(index)) ||
          any(index != as.integer(index)) ||
          anyDuplicated(index) ||
          any(index < 1 | index > n)) {
      .hm_abort("Split indices must be unique valid integers.")
    }
  }
  if (!length(train) ||
        length(intersect(train, test))) {
    .hm_abort("Training and test indices overlap or are empty.")
  }
  obs <- data$observations
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
  if (length(intersect(component[train], component[test]))) {
    .hm_abort(
      "Connected biological/source units cross training and test.",
      "hm_split_error"
    )
  }
  if (!is.null(split$observation_id) &&
        !identical(split$observation_id, obs$observation_id)) {
    .hm_abort("Split observation identities changed.", "hm_integrity_error")
  }
  if (!is.null(split$inner)) {
    split$inner <- lapply(
      split$inner, function(inner) {
        checked <- .hm_validate_fit_split(data, task, inner)
        if (any(
          !c(checked$train, checked$test) %in%
            train
        )) {
          .hm_abort(
            "Inner fold indices leave the outer training set.",
            "hm_split_error"
          )
        }
        checked
      }
    )
  }
  split$train <- as.integer(train)
  split$test <- as.integer(test)
  split$observation_id <- obs$observation_id
  split$component <- component
  split
}
