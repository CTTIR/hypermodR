.hm_select <- function(s, allowed) {
  if (!is.list(s) ||
    (length(s) &&
      (is.null(names(s)) ||
        any(!nzchar(names(s))) ||
        anyDuplicated(names(s)) ||
        any(
          !names(s) %in%
            allowed
        )))) {
    .hm_abort("Unknown or duplicated selection fields.")
  }
}
.hm_indices <- function(x, n, name, padding = FALSE) {
  if (is.null(x)) {
    return(seq_len(n))
  }
  if (!is.numeric(x) ||
    !length(x) ||
    any(!is.finite(x)) ||
    any(x != floor(x)) ||
    any(
      abs(x) >
        .Machine$integer.max
    ) ||
    anyDuplicated(x) ||
    (!padding && any(x < 1 | x > n))) {
    .hm_abort("Invalid 1-based {name} indices.")
  }
  as.integer(x)
}
.hm_band_map <- function(x, b) {
  out <- x@bands[b, , drop = FALSE]
  out$selected_index <- seq_along(b)
  if (!"valid" %in% names(out)) {
    out$valid <- !is.na(out$wavelength)
  }
  out
}
.hm_load_source <- function(s) {
  if (is.numeric(s)) {
    return(s)
  }
  if (!file.exists(s$locator) ||
    !identical(
      digest::digest(file = s$locator, algo = "sha256"),
      s$hash
    )) {
    .hm_abort(
      "Lazy source missing or content hash changed.",
      "hm_source_error"
    )
  }
  v <- readRDS(s$locator)
  if (!is.numeric(v) ||
    !identical(
      as.integer(dim(v)),
      as.integer(s$dim)
    )) {
    .hm_abort("Lazy source shape or type changed.", "hm_source_error")
  }
  v
}
#' Read native spectra without spatial reduction
#' @param x HMDataset containing native spectra.
#' @param selection Named list with character observation_id and positive bands.
#' @return Named list with numeric observation-by-selected-band values matrix,
#'   selected observations in request order, band_map and source provenance.
#' @details Only native spectral matrices are admitted; images require explicit
#'   spatial reduction. Bands are unique positive 1-based indices in requested
#' order.
#' @export
methods::setGeneric(
  "hm_spectra", function(x, selection = list()) {
    methods::standardGeneric(
      "hm_spectra"
    )
  }
)
#' @rdname hm_spectra
#' @export
methods::setMethod(
  "hm_spectra", "HMDataset", function(x, selection = list()) {
    methods::validObject(x)
    if (x@data_kind != "spectra") {
      .hm_abort("Spatial reduction requires an explicit recipe.")
    }
    .hm_select(selection, c("observation_id", "bands"))
    i <- seq_len(nrow(x@observations))
    if (!is.null(selection$observation_id)) {
      if (!is.character(selection$observation_id) ||
            !length(selection$observation_id) ||
            anyNA(selection$observation_id) ||
            anyDuplicated(selection$observation_id)) {
        .hm_abort("observation_id selection must be unique character IDs.")
      }
      i <- match(selection$observation_id, x@observations$observation_id)
      if (anyNA(i)) {
        .hm_abort("Unknown observation_id.")
      }
    }
    b <- .hm_indices(
      selection$bands, nrow(x@bands),
      "bands"
    )
    list(
      values = x@sources[[1]][i, b, drop = FALSE],
      observations = x@observations[i, ,
        drop = FALSE
      ], band_map = .hm_band_map(x, b),
      provenance = x@provenance
    )
  }
)
#' Read a bounded spatial block
#' @param x Spatial HMDataset or HMSequence.
#' @param selection Named acquisition_id, frame_id, rows, cols and bands
#' selectors. Out-of-grid rows/cols explicitly request invalid NA padding.
#' @return Named list with a numeric four-dimensional values array (y, x,
#' wavelength, time), a logical three-dimensional valid array (y, x, time),
#' observations, band_map, geometry and provenance. Default output budget is
#' 256 MiB (option hypermodR.block_bytes).
#' @details Rows and cols are unique integer source-grid indices; out-of-grid
#'   values request NA padding marked invalid. Bands are valid positive 1-based
#'   indices. Select exactly one acquisition; sequence frames retain requested
#' order.
#'   Variable grids must be read one frame at a time. Validity requires every
#' selected
#'   band finite and any declared spatial mask TRUE. The byte check bounds
#' planned
#'   output allocation, not total resident memory; RDS sources have a separate
#'   bounded deserialization admission check.
#' @export
methods::setGeneric(
  "hm_read_block", function(x, selection = list()) {
    methods::standardGeneric("hm_read_block")
  }
)
#' @rdname hm_read_block
#' @export
methods::setMethod(
  "hm_read_block", "HMDataset", function(x, selection = list()) {
    methods::validObject(x)
    if (x@data_kind == "spectra") {
      .hm_abort("Use hm_spectra for native spectra.")
    }
    .hm_select(selection, c(
      "acquisition_id", "frame_id", "rows", "cols",
      "bands"
    ))
    obs <- x@observations
    acquisitions <- unique(obs$acquisition_id)
    a <- selection$acquisition_id
    if (is.null(a)) {
      if (length(acquisitions) !=
            1L) {
        .hm_abort("Select exactly one acquisition.")
      }
      a <- acquisitions
    }
    if (!is.character(a) ||
          length(a) !=
            1L || is.na(a) ||
          !a %in% acquisitions) {
      .hm_abort("Select exactly one known acquisition.")
    }
    obs <- obs[obs$acquisition_id == a, , drop = FALSE]
    f <- 1L
    if (methods::is(x, "HMSequence")) {
      fi <- x@frame_index
      f <- which(fi$acquisition_id == a)
      if (!is.null(selection$frame_id)) {
        ids <- selection$frame_id
        if (!is.character(ids) ||
              anyNA(ids) ||
              !length(ids) ||
              anyDuplicated(ids) ||
              any(!ids %in% fi$frame_id[f])) {
          .hm_abort("Unknown or duplicate frame_id selection.")
        }
        f <- match(ids, fi$frame_id)
      }
      obs <- obs[
        unlist(
          lapply(fi$frame_id[f], function(id) which(obs$frame_id == id)),
          use.names = FALSE
        ), ,
        drop = FALSE
      ]
    } else if (!is.null(selection$frame_id)) {
      .hm_abort("Single images have no frame selection.")
    }
    dims <- lapply(x@sources[f], .hm_source)
    if (any(
      !vapply(
        dims, identical, logical(1),
        dims[[1]]
      )
    )) {
      .hm_abort("Variable grids must be read one frame at a time.")
    }
    d <- dims[[1]]
    r <- .hm_indices(selection$rows, d[1], "rows", TRUE)
    c <- .hm_indices(selection$cols, d[2], "cols", TRUE)
    b <- .hm_indices(selection$bands, d[3], "bands")
    bytes <- as.double(length(r)) *
      length(c) *
      length(b) *
      length(f) *
      8 + as.double(length(r)) *
      length(c) *
      length(f) *
      4
    budget <- getOption("hypermodR.block_bytes", 256 * 1024^2)
    if (!is.numeric(budget) ||
          length(budget) !=
            1L || !is.finite(budget) ||
          budget <= 0 || bytes > budget) {
      .hm_abort("Selected block exceeds byte budget.", "hm_resource_error")
    }
    values <- array(
      NA_real_, c(
        length(r),
        length(c),
        length(b),
        length(f)
      ),
      dimnames = list(
        y = as.character(r),
        x = as.character(c),
        wavelength = as.character(b),
        time = as.character(f)
      )
    )
    valid <- array(
      FALSE, c(
        length(r),
        length(c),
        length(f)
      ),
      dimnames = list(
        y = as.character(r),
        x = as.character(c),
        time = as.character(f)
      )
    )
    ir <- which(r >= 1 & r <= d[1])
    ic <- which(c >= 1 & c <= d[2])
    for (j in seq_along(f)) {
      source <- x@sources[[f[j]]]
      if (!is.numeric(source) &&
            prod(as.double(source$dim)) *
              8 > 2 * bytes + 64 * 1024^2) {
        .hm_abort(
          paste0(
            "RDS source exceeds bounded deserialization allowance; ",
            "use a smaller source."
          ),
          "hm_resource_error"
        )
      }
      src <- .hm_load_source(source)
      if (length(ir) &&
            length(ic)) {
        z <- src[r[ir], c[ic], b, drop = FALSE]
        values[ir, ic, , j] <- z
        usable <- apply(
          is.finite(z),
          c(1, 2),
          all
        )
        mask <- x@provenance$spatial_masks[[f[j]]]
        if (!is.null(mask)) {
          selected_mask <- mask[r[ir], c[ic], drop = FALSE]
          usable <- usable & !is.na(selected_mask) &
            selected_mask
        }
        valid[ir, ic, j] <- usable
      }
    }
    g <- x@geometry
    g$source_window <- list(rows = r, cols = c)
    g$padding <- list(
      rows = !(r %in% seq_len(d[1])),
      cols = !(c %in% seq_len(d[2]))
    )
    g$source_dimensions <- d[1:2]
    list(
      values = values, valid = valid, observations = obs,
      band_map = .hm_band_map(x, b),
      geometry = g, provenance = x@provenance
    )
  }
)
