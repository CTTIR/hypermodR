.hm_nnls <- function(design, response) {
  fit <- stats::optim(
    rep(0, ncol(design)),
    function(beta) sum((response - as.vector(design %*% beta))^2),
    function(beta) {
      as.vector(2 * crossprod(
        design,
        design %*% beta - response
      ))
    },
    method = "L-BFGS-B", lower = 0, control = list(
      factr = 1e+05,
      maxit = 1000
    )
  )
  gradient <- as.vector(2 * crossprod(design, design %*% fit$par - response))
  stationarity <- ifelse(
    fit$par > 1e-08, abs(gradient),
    pmax(-gradient, 0)
  )
  tolerance <- 1e-07 * (1 + max(abs(crossprod(design, response))))
  if (max(stationarity) >
        tolerance) {
    .hm_abort(
      "Nonnegative optimization failed its KKT residual check.",
      "hm_fit_error"
    )
  }
  fit$par
}

.hm_fit_chemometric <- function(engine, x, y, control) {
  id <- engine$model_id
  p <- engine$parameters
  rank <- .hm_parameter(
    p, "rank", min(
      2L, ncol(x),
      nrow(x) -
        1L
    )
  )
  if (length(rank) !=
    1L || !is.finite(rank) ||
    rank != as.integer(rank) ||
    rank < 1L || rank > min(
    ncol(x),
    nrow(x) -
      1L
  )) {
    .hm_abort("Invalid component rank.")
  }
  engine$rank <- rank
  if (id == "pca") {
    engine$object <- stats::prcomp(x,
      center = TRUE, scale. = FALSE,
      rank. = rank
    )
  } else if (id == "ica") {
    .hm_require("fastICA")
    engine$center <- colMeans(x)
    engine$object <- fastICA::fastICA(x,
      n.comp = rank,
      maxit = .hm_parameter(p, "maxit", 500L)
    )
  } else if (id %in% c("nmf", "mcr_als")) {
    if (any(x < 0)) {
      .hm_abort("Nonnegative factorization requires nonnegative spectra.")
    }
    maxit <- .hm_parameter(p, "maxit", 100L)
    if (!is.numeric(maxit) ||
          length(maxit) !=
            1L || maxit < 1L || maxit > 10000) {
      .hm_abort("maxit must lie between 1 and 10000.")
    }
    endmembers <- .hm_parameter(
      p, "endmembers", x[seq_len(rank), ,
        drop = FALSE
      ]
    )
    if (!is.matrix(endmembers) ||
          nrow(endmembers) !=
            rank || ncol(endmembers) !=
          ncol(x) ||
          any(!is.finite(endmembers)) ||
          any(endmembers < 0)) {
      .hm_abort("Invalid endmembers.")
    }
    for (iteration in seq_len(maxit)) {
      abundance <- t(
        vapply(
          seq_len(nrow(x)),
          function(i) {
            .hm_nnls(
              t(endmembers),
              x[i, ]
            )
          }, numeric(nrow(endmembers))
        )
      )
      updated <- vapply(
        seq_len(ncol(x)),
        function(j) {
          .hm_nnls(abundance, x[, j])
        }, numeric(nrow(endmembers))
      )
      difference <- max(abs(updated - endmembers))
      endmembers <- updated
      if (difference < .hm_parameter(p, "tolerance", 1e-07)) {
        break
      }
    }
    engine$object <- list(
      endmembers = endmembers, iterations = iteration,
      converged = difference <
        .hm_parameter(p, "tolerance", 1e-07)
    )
  } else if (id == "fcls") {
    .hm_require("quadprog")
    endmembers <- p$endmembers
    if (!is.matrix(endmembers) ||
          !is.numeric(endmembers) ||
          ncol(endmembers) !=
            ncol(x) ||
          any(!is.finite(endmembers)) ||
          qr(t(endmembers))$rank !=
            nrow(endmembers)) {
      .hm_abort(paste0(
        "FCLS requires finite, independently identifiable ",
        "endmember spectra."
      ))
    }
    engine$object <- list(endmembers = endmembers)
  } else if (id == "beer_lambert") {
    return(.hm_fit_beer_lambert(engine, x, control))
  }
  engine
}

.hm_predict_chemometric <- function(engine, x) {
  id <- engine$model_id
  if (id == "pca") {
    fit <- engine$object
    return(
      sweep(
        (sweep(x, 2L, fit$center) %*%
           fit$rotation) %*% t(fit$rotation),
        2L, fit$center, "+"
      )
    )
  }
  if (id == "ica") {
    fit <- engine$object
    return(
      sweep(
        sweep(x, 2L, engine$center) %*%
          fit$K %*% fit$W %*% fit$A, 2L, engine$center, "+"
      )
    )
  }
  if (id == "beer_lambert") {
    return(.hm_predict_beer_lambert(engine, x))
  }
  endmembers <- engine$object$endmembers
  if (id == "fcls") {
    k <- nrow(endmembers)
    abundance <- t(
      vapply(
        seq_len(nrow(x)),
        function(i) {
          quadprog::solve.QP(
            2 * tcrossprod(endmembers),
            2 * endmembers %*% x[i, ], cbind(
              rep(1, k),
              diag(k)
            ),
            c(1, rep(0, k)),
            meq = 1L
          )$solution
        }, numeric(k)
      )
    )
    abundance[abs(abundance) <
                1e-12] <- 0
  } else {
    abundance <- t(
      vapply(
        seq_len(nrow(x)),
        function(i) {
          .hm_nnls(
            t(endmembers),
            x[i, ]
          )
        }, numeric(nrow(endmembers))
      )
    )
  }
  if (id == "nmf") {
    abundance %*% endmembers
  } else {
    abundance
  }
}
