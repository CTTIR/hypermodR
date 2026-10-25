.hm_plain <- function(x) {
  if (typeof(x) %in%
    c(
      "externalptr", "environment", "closure", "builtin", "special",
      "weakref"
    )) {
    return(FALSE)
  }
  if (isS4(x)) {
    if (!all(
      vapply(
        methods::slotNames(x),
        function(n) .hm_plain(methods::slot(x, n)),
        logical(1)
      )
    )) {
      return(FALSE)
    }
  } else if (is.list(x) &&
               !all(vapply(x, .hm_plain, logical(1)))) {
    return(FALSE)
  }
  attrs <- attributes(x)
  if (!is.null(attrs) &&
        !all(vapply(attrs, .hm_plain, logical(1)))) {
    return(FALSE)
  }
  TRUE
}
.hm_table <- function(x, key = NULL) {
  if (!is.data.frame(x)) {
    .hm_abort("Metadata must be a data.frame.")
  }
  ids <- grep(
    "(^|_)id$|^annotation_revision$", names(x),
    value = TRUE
  )
  for (n in ids) {
    if (!is.character(x[[n]]) ||
          anyNA(x[[n]]) ||
          any(!nzchar(x[[n]]))) {
      .hm_abort("Identity columns must contain nonempty character values.")
    }
  }
  if (!is.null(key) &&
        (!key %in% names(x) ||
           anyDuplicated(x[[key]]))) {
    .hm_abort("Missing or duplicate metadata key.")
  }
  invisible(TRUE)
}
.hm_bands <- function(bands, n) {
  .hm_table(bands)
  if (nrow(bands) !=
        n || !is.numeric(bands$wavelength)) {
    .hm_abort("bands must have one numeric wavelength per native band.")
  }
  unknown <- is.na(bands$wavelength)
  if (any(!is.finite(bands$wavelength[!unknown])) ||
        any(bands$wavelength[!unknown] <= 0) ||
        anyDuplicated(bands$wavelength[!unknown])) {
    .hm_abort("Known wavelengths must be unique finite positive nm values.")
  }
  if (any(unknown) &&
    (!"availability" %in% names(bands) ||
      any(
        is.na(bands$availability[unknown]) |
          bands$availability[unknown] != "unavailable"
      ))) {
    .hm_abort("Unknown wavelengths require availability unavailable.")
  }
  if ("unit" %in% names(bands) &&
        anyNA(match(bands$unit, "nm"))) {
    .hm_abort("Wavelength unit must be nm.")
  }
  bands$unit <- rep("nm", n)
  if (!"native_index" %in% names(bands)) {
    bands$native_index <- seq_len(n)
  }
  if (anyNA(bands$native_index) ||
    anyDuplicated(bands$native_index) ||
    any(bands$native_index < 1 | bands$native_index != floor(
      bands$native_index
    ))) {
    .hm_abort("native_index must be unique positive integers.")
  }
  if (!"canonical_index" %in% names(bands)) {
    bands$canonical_index <- seq_len(n)
  }
  bands
}
.hm_source <- function(x) {
  if (is.numeric(x) &&
    length(dim(x)) %in%
      2:3 && all(
    dim(x) >
      0
  )) {
    return(dim(x))
  }
  if (is.list(x) &&
    identical(x$reader, "rds") &&
    is.character(x$locator) &&
    length(x$locator) ==
      1L && is.character(x$hash) &&
    length(x$hash) ==
      1L && is.numeric(x$dim) &&
    length(x$dim) ==
      3L && all(
    is.finite(x$dim) &
      x$dim > 0 & x$dim == floor(x$dim)
  )) {
    return(x$dim)
  }
  .hm_abort(
    paste0(
      "Source must be a numeric matrix/image or declarative ",
      "RDS image source with locator, hash and dim."
    )
  )
}
.hm_valid_dataset <- function(object) {
  tryCatch(
    {
      if (!.hm_plain(object@sources) ||
            !.hm_plain(object@provenance) ||
            !.hm_plain(object@geometry)) {
        .hm_abort(
          "Persistent objects cannot contain live handles or functions."
        )
      }
      .hm_table(object@observations, "observation_id")
      if (!nrow(object@observations)) {
        .hm_abort("At least one observation is required.")
      }
      dims <- lapply(object@sources, .hm_source)
      if (length(object@data_kind) !=
            1L || is.na(object@data_kind)) {
        .hm_abort("data_kind must be one known value.")
      }
      if (!length(dims)) {
        .hm_abort("At least one source is required.")
      }
      .hm_bands(object@bands, tail(dims[[1]], 1))
      if (any(
        vapply(
          dims, function(d) {
            tail(d, 1) !=
              nrow(object@bands)
          }, logical(1)
        )
      )) {
        .hm_abort("Source bands disagree.")
      }
      masks <- object@provenance$spatial_masks
      if (!is.null(masks)) {
        if (!is.list(masks) ||
              length(masks) !=
                length(dims)) {
          .hm_abort(
            "spatial_masks must contain one original mask per source."
          )
        }
        for (i in seq_along(masks)) {
          if (!is.null(masks[[i]]) &&
            (!is.logical(masks[[i]]) ||
              !identical(
                as.integer(dim(masks[[i]])),
                as.integer(dims[[i]][1:2])
              ))) {
            .hm_abort(paste0(
              "Spatial masks must be logical native-grid matrices; NA ",
              "is retained."
            ))
          }
        }
      }
      if (identical(object@data_kind, "spectra")) {
        if (length(dims) !=
              1L || length(dims[[1]]) !=
              2L || nrow(object@observations) !=
              dims[[1]][1] || length(object@geometry)) {
          .hm_abort(paste0(
            "Spectra require one observation-by-band matrix and no ",
            "geometry."
          ))
        }
      } else {
        if (!object@data_kind %in% c("image", "sequence") ||
          any(
            lengths(dims) !=
              3L
          )) {
          .hm_abort("Invalid spatial source shape.")
        }
        if (!all(
          c("image_id", "acquisition_id") %in%
            names(object@observations)
        )) {
          .hm_abort("Images require image_id and acquisition_id.")
        }
        for (identity in c("image_id", "acquisition_id")) {
          ids <- object@observations[[identity]]
          if (!is.character(ids) ||
                anyNA(ids) ||
                any(!nzchar(ids))) {
            .hm_abort(paste0(
              "Image and acquisition identities must be complete ",
              "character values."
            ))
          }
          if (identical(object@data_kind, "image") &&
                length(unique(ids)) !=
                  1L) {
            .hm_abort(paste0(
              "One image source requires one image_id and one ",
              "acquisition_id."
            ))
          }
        }
        if (identical(object@data_kind, "image") &&
              length(dims) !=
                1L) {
          .hm_abort("One image requires exactly one source.")
        }
        .hm_check_geometry(object@geometry)
      }
      TRUE
    },
    error = function(e) conditionMessage(e)
  )
}
