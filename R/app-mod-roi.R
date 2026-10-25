.hm_app_pool <- function(dataset) {
  if (dataset@data_kind == "spectra") {
    .hm_abort("This source already contains spectra.")
  }
  keys <- if (methods::is(dataset, "HMSequence")) {
    dataset@frame_index$frame_id
  } else {
    unique(dataset@observations$acquisition_id)
  }
  if (length(keys) >
        128L) {
    .hm_abort(
      paste0(
        "Workbench pooling is bounded to 128 images/frames. Use ",
        "the API for a larger declared operation."
      )
    )
  }
  values <- vector("list", length(keys))
  observations <- vector("list", length(keys))
  support <- vector("list", length(keys))
  for (i in seq_along(keys)) {
    obs <- dataset@observations
    if (methods::is(dataset, "HMSequence")) {
      obs <- obs[obs$frame_id == keys[i], , drop = FALSE]
      selection <- list(
        acquisition_id = obs$acquisition_id[1],
        frame_id = keys[i]
      )
    } else {
      obs <- obs[obs$acquisition_id == keys[i], , drop = FALSE]
      selection <- list(acquisition_id = keys[i])
    }
    varying <- vapply(
      obs, function(x) {
        length(unique(x)) >
          1L
      }, logical(1)
    )
    varying[names(varying) %in%
              c("observation_id", "x", "y", "row", "col")] <- FALSE
    if (any(varying)) {
      .hm_abort(
        paste0(
          "Image/frame metadata or references vary within pooling ",
          "support. Define a validated ROI and reference with the ",
          "API first."
        )
      )
    }
    source <- dataset@sources[[i]]
    if (prod(as.double(.hm_source(source))) *
          8 > 64 * 1024^2) {
      .hm_abort(
        paste0(
          "Image exceeds the 64 MB interactive pooling bound. ",
          "Prepare explicit pooled spectra with the API."
        )
      )
    }
    pooled <- hm_pool(dataset, method = "mean", selection = selection)
    values[[i]] <- pooled$values
    observations[[i]] <- obs[1L, , drop = FALSE]
    support[[i]] <- pooled$provenance
  }
  obs <- do.call(rbind, observations)
  obs$observation_id <- paste0("pooled_", keys)
  hm_dataset(
    do.call(rbind, values),
    obs, pooled$band_map,
    provenance = list(
      source_hash = .hm_hash(dataset),
      method = "explicit whole-image mean", support = support
    )
  )
}
