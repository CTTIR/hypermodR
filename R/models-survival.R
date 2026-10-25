.hm_fit_survival <- function(engine, x, y, weights) {
  .hm_require("survival")
  if (!is.data.frame(y) ||
    !identical(
      names(y),
      c("time", "status")
    ) ||
    !is.numeric(y$time) ||
    any(!is.finite(y$time)) ||
    any(y$time <= 0) ||
    anyNA(y$status) ||
    any(!y$status %in% c(0, 1)) ||
    !any(y$status == 1)) {
    .hm_abort(paste0(
      "Survival training requires positive times and binary ",
      "event indicators."
    ))
  }
  d <- cbind(
    .hm_engine_frame(x),
    y
  )
  formula <- stats::as.formula(
    paste(
      "survival::Surv(time, status) ~", paste(
        names(d)[seq_len(ncol(x))],
        collapse = " + "
      )
    )
  )
  engine$object <- survival::coxph(formula,
    data = d, weights = weights,
    x = TRUE, y = TRUE, model = TRUE
  )
  engine$prediction_scope <- "relative hazard; baseline survival not inferred"
  engine
}
