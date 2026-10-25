.hm_fit_statistical <- function(engine, x, y, weights, control) {
  id <- engine$model_id
  p <- engine$parameters
  d <- .hm_engine_frame(x)
  d$outcome <- y
  formula <- stats::as.formula("outcome ~ .")
  if (id == "lm") {
    engine$object <- stats::lm(formula,
      data = d, weights = weights,
      x = TRUE, y = TRUE
    )
  } else if (id == "glm") {
    family <- if (engine$task_type == "classification") {
      stats::binomial()
    } else {
      stats::gaussian()
    }
    if (engine$task_type == "classification" && length(engine$classes) !=
          2L) {
      .hm_abort(
        "The GLM reference implements binary classification only.",
        "hm_capability_error"
      )
    }
    if (!is.null(p$family)) {
      families <- list(
        gaussian = stats::gaussian(), poisson = stats::poisson(),
        binomial = stats::binomial()
      )
      if (!p$family %in% names(families)) {
        .hm_abort("Unknown GLM family.")
      }
      family <- families[[p$family]]
      if (engine$task_type == "classification" && p$family != "binomial") {
        .hm_abort("Classification GLMs require the binomial family.")
      }
    }
    engine$object <- stats::glm(formula,
      data = d, family = family,
      weights = weights
    )
  } else if (id == "glmnet") {
    .hm_require("glmnet")
    family <- if (engine$task_type == "classification") {
      if (length(engine$classes) ==
            2L) {
        "binomial"
      } else {
        "multinomial"
      }
    } else {
      "gaussian"
    }
    lambda <- .hm_parameter(p, "lambda", 0.01)
    if (length(lambda) !=
          1L || !is.finite(lambda) ||
          lambda <= 0) {
      .hm_abort(paste0(
        "glmnet lambda must be a single positive value selected ",
        "within training."
      ))
    }
    engine$lambda <- lambda
    engine$object <- glmnet::glmnet(
      x, y,
      family = family, weights = weights, alpha = .hm_parameter(
        p, "alpha",
        1
      ),
      lambda = lambda
    )
  } else if (id == "gam") {
    .hm_require("mgcv")
    k <- .hm_parameter(p, "k", 4L)
    terms <- paste0(
      "s(band_", seq_len(ncol(x)),
      ", k = ", as.integer(k),
      ")"
    )
    formula <- stats::as.formula(paste("outcome ~", paste(terms,
                                   collapse = " + "
                                 )))
    environment(formula) <- asNamespace("mgcv")
    engine$object <- do.call(mgcv::gam, list(
      formula = formula, data = d, weights = weights,
      method = .hm_parameter(p, "method", "GCV.Cp")
    ))
  } else if (id == "mixed") {
    .hm_require("lme4")
    group <- p$group
    if (is.null(group) ||
          is.null(control$observations[[group]])) {
      .hm_abort(paste0(
        "Mixed models require a declared group column in ",
        "training observations."
      ))
    }
    d$group <- factor(control$observations[[group]])
    if (anyNA(d$group) ||
          nlevels(d$group) <
            2L) {
      .hm_abort("Mixed model grouping is invalid.")
    }
    formula <- stats::as.formula(
      paste(
        "outcome ~", paste(
          names(d)[seq_len(ncol(x))],
          collapse = " + "
        ),
        "+ (1 | group)"
      )
    )
    engine$object <- lme4::lmer(formula, data = d, weights = weights)
    engine$prediction_scope <- paste0(
      "population; random intercept excluded ",
      "for new units"
    )
  } else if (id %in% c("pls", "pcr")) {
    .hm_require("pls")
    ncomp <- .hm_parameter(
      p, "ncomp", min(
        2L, ncol(x),
        nrow(x) -
          1L
      )
    )
    if (ncomp < 1 || ncomp > min(
      ncol(x),
      nrow(x) -
        1L
    )) {
      .hm_abort("Invalid latent rank.")
    }
    engine$ncomp <- ncomp
    fun <- if (id == "pls") {
      pls::plsr
    } else {
      pls::pcr
    }
    engine$object <- fun(formula,
      data = d, ncomp = ncomp,
      validation = "none"
    )
  } else if (id == "bayes_conjugate") {
    prior_variance <- .hm_parameter(p, "prior_variance", 100)
    noise_variance <- p$noise_variance
    if (is.null(noise_variance) ||
          length(noise_variance) !=
            1L || !is.finite(noise_variance) ||
          noise_variance <= 0 || !is.finite(prior_variance) ||
          prior_variance <= 0) {
      .hm_abort(paste0(
        "Conjugate regression requires positive known noise and ",
        "prior variances."
      ))
    }
    design <- cbind(1, x)
    covariance <- solve(
      crossprod(design) / noise_variance +
        diag(1 / prior_variance, ncol(design))
    )
    engine$object <- list(
      mean = as.vector(covariance %*% crossprod(design, y) / noise_variance),
      covariance = covariance, noise_variance = noise_variance,
      uncertainty = "Gaussian posterior conditional on known noise variance"
    )
  } else if (id == "brms") {
    .hm_require("brms")
    if (!isTRUE(control$allow_sampling) ||
          is.null(p$toolchain_identity)) {
      .hm_abort(
        paste0(
          "Bayesian sampling requires explicit admission and a ",
          "toolchain identity."
        ),
        "hm_backend_unqualified"
      )
    }
    engine$object <- brms::brm(
      formula,
      data = d, family = stats::gaussian(), chains = .hm_parameter(
        p,
        "chains", 2L
      ),
      cores = 1L, iter = .hm_parameter(p, "iter", 1000L),
      seed = .hm_parameter(control, "seed", 1L),
      refresh = 0
    )
  }
  engine
}

.hm_predict_statistical <- function(engine, x, ...) {
  id <- engine$model_id
  d <- .hm_engine_frame(x)
  if (id == "bayes_conjugate") {
    return(
      as.vector(
        cbind(1, x) %*%
          engine$object$mean
      )
    )
  }
  if (id == "brms") {
    return(as.vector(colMeans(brms::posterior_epred(engine$object,
                       newdata = d
                     ))))
  }
  if (id == "mixed") {
    return(as.vector(stats::predict(engine$object,
             newdata = d,
             re.form = NA
           )))
  }
  if (id %in% c("pls", "pcr")) {
    return(
      as.vector(stats::predict(engine$object,
        newdata = d,
        ncomp = engine$ncomp
      ))
    )
  }
  if (id == "glmnet") {
    pred <- stats::predict(engine$object,
      newx = x, s = engine$lambda,
      type = "response"
    )
    if (length(dim(pred)) ==
          3L) {
      pred <- matrix(
        pred[, , 1L],
        nrow = nrow(x),
        dimnames = list(NULL, engine$classes)
      )
    }
    if (length(engine$classes) ==
          2L) {
      pred <- as.vector(pred)
    }
  } else {
    pred <- stats::predict(engine$object, newdata = d, type = "response")
  }
  if (engine$task_type == "classification") {
    return(.hm_probabilities(pred, engine$classes, nrow(x)))
  }
  as.vector(pred)
}
