#' Open the local spectral modelling workbench
#'
#' Import, configuration and submission are separate explicit actions. Optional
#' runtimes are never installed on launch. Each connection owns its state/jobs.
#' @param launch Run the application; FALSE returns a shiny.appobj.
#' @param host Listening interface, default loopback.
#' @param port Optional listening port.
#' @param ... Arguments passed to shiny::runApp.
#' @return A Shiny application, invisibly when launched.
#' @export
hm_app <- function(launch = TRUE, host = "127.0.0.1", port = NULL, ...) {
  if (!is.logical(launch) ||
        length(launch) !=
          1L || is.na(launch)) {
    .hm_abort("launch must be TRUE or FALSE.")
  }
  .hm_scalar_character(host, "host")
  .hm_require("shiny")
  .hm_require("bslib")
  .hm_require("callr")
  app <- shiny::shinyApp(
    .hm_app_ui(), .hm_app_server,
    onStart = function() {
      previous <- options(shiny.maxRequestSize = 50 * 1024^2)
      shiny::onStop(function() options(previous), session = NULL)
    }
  )
  if (launch) {
    shiny::runApp(app, host = host, port = port, ...)
  }
  invisible(app)
}

.hm_app_platform_notice <- function() {
  if (!identical(Sys.info()[["sysname"]], "Linux") ||
        any(!nzchar(Sys.which(c("taskset", "prlimit", "timeout"))))) {
    return(paste0(
      "Workbench fitting and runtime setup require Linux with taskset, ",
      "prlimit and timeout. This host can inspect data and prepare ",
      "configurations. Use hm_fit() directly for local R models."
    ))
  }
  NULL
}
