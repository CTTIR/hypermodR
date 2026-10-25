backend_test_profile <- function(path, model = "test-identity") {
  terms <- "Local test artifact only; unrestricted test use."
  list(
    profile_id = "test-cpu-v1", model_id = model,
    source_revision = "test-source-1",
    runtime_version = "test-runtime-1", custom_code = TRUE,
    custom_code_reviewed = TRUE,
    os = unname(Sys.info()["sysname"]),
    arch = unname(Sys.info()["machine"]),
    device = "cpu", qualification = "test_only", artifacts = list(
      list(
        path = "worker", url = paste0("file://", path),
        bytes = unname(file.info(path)$size),
        sha256 = digest::digest(file = path, algo = "sha256")
      )
    ),
    terms = list(
      list(
        reviewed = TRUE, text = terms, sha256 = digest::digest(terms,
          algo = "sha256", serialize = FALSE
        )
      )
    ),
    command = "worker", args = character()
  )
}
