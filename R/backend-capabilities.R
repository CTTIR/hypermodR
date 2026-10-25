#' Inspect local runtime capabilities without starting services
#' @return List with OS, architecture, R version, Python executable path, and
#' explicit unestablished GPU qualification.
#' @export
hm_backend_capabilities <- function() {
  list(
    os = unname(Sys.info()["sysname"]),
    arch = unname(Sys.info()["machine"]),
    r_version = as.character(getRversion()),
    python = unname(Sys.which("python3")),
    gpu_qualification = "not_established", privileged_actions = FALSE
  )
}
