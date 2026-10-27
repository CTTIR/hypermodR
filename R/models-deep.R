.hm_deep_catalog <- function() {
  list(
    `reference:cnn1d` = "classification",
    `reference:spectral_vit` = "classification",
    `reference:masked_autoencoder` = "reconstruction",
    `reference:vae` = "reconstruction",
    `reference:distillation` = "classification",
    `reference:multitask` = "classification",
    `reference:domain_adversarial` = "classification"
  )
}
.hm_deep_handle <- function(control, id) {
  handle <- control$backend_handle
  .hm_owned_handle(handle)
  if (!identical(handle$model, id) ||
        !handle$process$is_alive()) {
    .hm_abort(
      "An owned live backend matching the model is required.",
      "hm_backend_error"
    )
  }
  handle
}
.hm_deep_artifact_hash <- function(path) {
  .hm_no_symlink(path, dirname(path))
  files <- c("model.json", "state.npz", "input.json")
  stats::setNames(
    vapply(
      files, function(f) {
        target <- file.path(path, f)
        if (!file.exists(target) ||
              nzchar(Sys.readlink(target))) {
          .hm_abort("Model artifact missing or linked.", "hm_integrity_error")
        }
        digest::digest(file = target, algo = "sha256")
      }, character(1)
    ),
    files
  )
}
.hm_fit_deep <- function(model_id, x, y, task, parameters, control) {
  if (!model_id %in% names(.hm_deep_catalog())) {
    .hm_abort(
      paste0(
        "Temporal and image models require their explicit ",
        "tensor/sequence route."
      ),
      "hm_capability_error"
    )
  }
  if (task$unit %in% c("image", "sequence") ||
        task$type == "temporal") {
    .hm_abort(
      "Reference spectral route cannot flatten images or sequences.",
      "hm_capability_error"
    )
  }
  if (!task$type %in% .hm_deep_catalog()[[model_id]]) {
    .hm_abort("Incompatible reference task.", "hm_capability_error")
  }
  if (!is.null(control$weights)) {
    .hm_abort(
      "Reference weighted fitting is unavailable.",
      "hm_capability_error"
    )
  }
  if (!is.matrix(x) ||
        any(!is.finite(x))) {
    .hm_abort("Finite spectral matrix required.")
  }
  if (length(
    setdiff(
      names(parameters),
      c("epochs", "domain")
    )
  )) {
    .hm_abort("Unknown reference parameters.")
  }
  epochs <- .hm_parameter(parameters, "epochs", 2L)
  if (!is.numeric(epochs) ||
        length(epochs) !=
          1L || !is.finite(epochs) ||
        epochs != as.integer(epochs) ||
        epochs < 1 || epochs > 100) {
    .hm_abort("Reference epochs must be an integer from 1 to 100.")
  }
  if (!is.null(parameters$domain) &&
        (length(parameters$domain) !=
           nrow(x) ||
           anyNA(parameters$domain) ||
           any(!parameters$domain %in% c(0L, 1L)))) {
    .hm_abort("Domains require one binary label per training observation.")
  }
  handle <- .hm_deep_handle(control, model_id)
  path <- control$artifact_path
  .hm_scalar_character(path, "artifact_path")
  if (file.exists(path) ||
        !dir.exists(dirname(path))) {
    .hm_abort("Provide a new artifact directory under an existing parent.")
  }
  .hm_no_symlink(path, dirname(path))
  path <- file.path(
    normalizePath(dirname(path)),
    basename(path)
  )
  obs <- control$observations
  groups <- intersect(task$groups, names(obs))
  if (!length(groups) ||
        anyNA(obs[, groups, drop = FALSE])) {
    .hm_abort(
      "Explicit biological training groups required.",
      "hm_split_error"
    )
  }
  group_ids <- apply(obs[, groups, drop = FALSE], 1L, function(z) {
    .hm_hash(
      as.list(z)
    )
  })
  classes <- if (task$type == "classification") {
    levels(factor(y))
  } else {
    NULL
  }
  channels <- as.character(control$wavelength)
  if (length(channels) !=
        ncol(x)) {
    .hm_abort("Measured output channels required.")
  }
  metadata <- list(
    epochs = as.integer(epochs),
    roles = as.list(rep("train", nrow(x))),
    groups = as.list(group_ids),
    seed = as.integer(.hm_parameter(control, "seed", 1L)),
    outputs = if (is.null(classes)) 2L else length(classes),
    artifact_path = path
  )
  if (!is.null(classes)) {
    metadata$y <- as.list(
      as.integer(factor(y, levels = classes)) -
        1L
    )
  }
  if (!is.null(parameters$domain)) {
    metadata$domain <- as.list(parameters$domain)
  }
  result <- hm_worker_request(
    handle, "fit", x, channels,
    timeout = .hm_parameter(control, "timeout", 120),
    metadata = metadata
  )
  if (!identical(result$input_digest, .hm_hash(list(x, channels)))) {
    .hm_abort("Training input digest mismatch.", "hm_integrity_error")
  }
  list(
    model_id = model_id, task_type = task$type, parameters = parameters,
    classes = classes, n_features = ncol(x),
    training_hash = .hm_hash(list(x, y)),
    channels = channels, artifact_path = path,
    artifact_sha256 = .hm_deep_artifact_hash(path),
    receipt = result$receipt, object = NULL
  )
}
.hm_predict_deep <- function(engine, x, control = list(), ...) {
  handle <- .hm_deep_handle(control, engine$model_id)
  if (!identical(
    .hm_deep_artifact_hash(engine$artifact_path),
    engine$artifact_sha256
  )) {
    .hm_abort("Model artifact integrity changed.", "hm_integrity_error")
  }
  ans <- hm_worker_request(
    handle, "predict", x, engine$channels,
    timeout = .hm_parameter(control, "timeout", 120),
    metadata = list(
      artifact_path = engine$artifact_path, artifact_sha256 = as.list(
        engine$artifact_sha256
      )
    )
  )
  if (!identical(ans$input_digest, .hm_hash(list(x, engine$channels)))) {
    .hm_abort("Prediction input digest mismatch.", "hm_integrity_error")
  }
  out <- ans$array
  if (engine$task_type == "classification") {
    if (!identical(
      dim(out),
      c(
        nrow(x),
        length(engine$classes)
      )
    )) {
      .hm_abort("Invalid reference class shape.", "hm_backend_output_error")
    }
    p <- exp(out - apply(out, 1L, max))
    p <- p / rowSums(p)
    colnames(p) <- engine$classes
    return(.hm_probabilities(p, engine$classes, nrow(x)))
  }
  if (!identical(
    dim(out),
    dim(x)
  )) {
    .hm_abort("Invalid reconstruction shape.", "hm_backend_output_error")
  }
  out
}

#' Extract admitted native upstream image features
#' @param model_id Named candidate identity (htc, hyperfree or hypersigma).
#' @param cube Numeric full-frame HWC array; spectral channels remain measured.
#' @param wavelengths Strictly increasing measured wavelengths in nm.
#' @param mask Logical full-frame eligible-pixel mask.
#' @param admission Explicit exact source, checkpoint and separate rights
#' admission.
#' @param control Runtime list with an owned backend_handle and optional
#' timeout.
#' @return An `hm_native_features` list with worker-returned numeric
#' `features`, identity metadata, qualified=FALSE and input digest. Feature
#' shape is adapter-specific.
#' @details Only htc, hyperfree and hypersigma extraction are admitted by this
#' entry point.
#'   The worker must verify its adapter-specific exact source/checkpoint/input
#' contract.
#'   Admission requires execute_source_acknowledged, code_terms_admitted and
#'   weight_terms_admitted flags. Extraction is not upstream downstream-task
#' training
#'   or scientific qualification; other named architectures remain blocked.
#' @export
hm_extract_native <- function(
  model_id, cube, wavelengths, mask, admission,
  control = list()
) {
  if (!model_id %in% c("htc", "hyperfree", "hypersigma")) {
    .hm_abort(
      paste0(
        "Named route is blocked pending executable source, ",
        "weights and rights admission."
      ),
      "hm_blocked_error"
    )
  }
  if (!is.array(cube) ||
    length(dim(cube)) !=
      3L || !is.numeric(cube) ||
    any(!is.finite(cube)) ||
    !is.numeric(wavelengths) ||
    length(wavelengths) !=
      dim(cube)[3L] ||
    any(!is.finite(wavelengths)) ||
    any(
      diff(wavelengths) <=
        0
    ) ||
    !is.logical(mask) ||
    !identical(
      dim(mask),
      dim(cube)[1:2]
    ) ||
    anyNA(mask) ||
    !any(mask)) {
    .hm_abort(
      paste0(
        "Native extraction requires finite HWC cube, measured ",
        "nm channels and full-frame logical mask."
      ),
      "hm_capability_error"
    )
  }
  if (!is.list(admission) ||
        !isTRUE(admission$execute_source_acknowledged) ||
        !isTRUE(admission$code_terms_admitted) ||
        !isTRUE(admission$weight_terms_admitted)) {
    .hm_abort(
      paste0(
        "Explicit source execution and separate code/weight ",
        "admission required."
      ),
      "hm_blocked_error"
    )
  }
  handle <- .hm_deep_handle(control, model_id)
  ans <- hm_worker_request(
    handle, "extract", cube, as.character(wavelengths),
    timeout = .hm_parameter(control, "timeout", 120),
    metadata = list(
      admission = admission, wavelengths = as.list(wavelengths),
      mask = list(
        shape = as.list(dim(mask)),
        values = as.list(as.logical(mask))
      )
    )
  )
  if (!identical(ans$input_digest, .hm_hash(list(cube, as.character(
    wavelengths
  ))))) {
    .hm_abort("Native input identity mismatch.", "hm_integrity_error")
  }
  structure(
    list(
      features = ans$array, identity = ans$identity, qualified = FALSE,
      input_digest = ans$input_digest
    ),
    class = "hm_native_features"
  )
}
