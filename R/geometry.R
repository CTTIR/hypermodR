.hm_check_geometry <- function(g) {
  if (!is.list(g) ||
    !is.numeric(g$origin) ||
    length(g$origin) !=
      2L || any(!is.finite(g$origin)) ||
    !is.matrix(g$transform) ||
    !identical(
      dim(g$transform),
      c(3L, 3L)
    ) ||
    any(!is.finite(g$transform)) ||
    abs(det(g$transform)) <
      .Machine$double.eps || any(g$transform[3, ] != c(0, 0, 1))) {
    .hm_abort(
      paste0(
        "Geometry requires finite origin (x,y) and an ",
        "invertible affine 3x3 transform."
      )
    )
  }
  invisible(TRUE)
}
#' Map image coordinates through explicit geometry
#' @param geometry Geometry with origin and homogeneous transform.
#' @param coordinates Two-column x,y matrix of source indices (1-based).
#' @param inverse Map world coordinates back to source indices.
#' @return A two-column numeric matrix.
#' @export
hm_coordinates <- function(geometry, coordinates, inverse = FALSE) {
  .hm_check_geometry(geometry)
  if (!is.matrix(coordinates) ||
        ncol(coordinates) !=
          2L || !is.numeric(coordinates) ||
        any(!is.finite(coordinates))) {
    .hm_abort("coordinates must be a finite two-column matrix.")
  }
  a <- geometry$transform
  offset <- geometry$origin - 1
  if (isTRUE(inverse)) {
    z <- t(
      solve(a) %*%
        t(cbind(coordinates, 1))
    )[,
      1:2,
      drop = FALSE
    ]
    sweep(z, 2, offset, "-")
  } else {
    t(
      a %*% t(
        cbind(
          sweep(coordinates, 2, offset, "+"),
          1
        )
      )
    )[,
      1:2,
      drop = FALSE
    ]
  }
}
