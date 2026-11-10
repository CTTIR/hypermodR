test_that("Python installation ignores ambient pip configuration", {
  skip_on_cran()
  skip_if_not_installed("processx")
  skip_on_os("windows")
  python <- unname(Sys.which("python3"))
  skip_if(!nzchar(python))
  root <- tempfile("pip-configuration-")
  dir.create(root)
  root <- normalizePath(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  wheel <- file.path(root, "hm_audit_fixture-1.0-py3-none-any.whl")
  wheel_script <- c(
    "import sys,zipfile", "with zipfile.ZipFile(sys.argv[1],'w') as z:",
    "    z.writestr('hm_audit_fixture.py','value = 17\\n')",
    paste0("    z.writestr('hm_audit_fixture-1.0.dist-info/METADATA',",
           "'Metadata-Version: 2.1\\nName: hm-audit-fixture\\n",
           "Version: 1.0\\n')"),
    paste0("    z.writestr('hm_audit_fixture-1.0.dist-info/WHEEL',",
           "'Wheel-Version: 1.0\\nRoot-Is-Purelib: true\\n",
           "Tag: py3-none-any\\n')"),
    "    z.writestr('hm_audit_fixture-1.0.dist-info/RECORD','')"
  )
  processx::run(python, c("-c", paste(wheel_script, collapse = "\n"), wheel))
  worker <- file.path(root, "worker.py")
  writeLines("print('synthetic runtime')", worker)
  profile <- backend_test_profile(worker)
  artifact <- function(path) {
    list(path = basename(path), url = paste0("file://", path),
         bytes = unname(file.info(path)$size),
         sha256 = digest::digest(file = path, algo = "sha256"))
  }
  profile$artifacts[[2L]] <- artifact(wheel)
  lock <- file.path(root, "requirements.txt")
  writeLines(paste0("hm-audit-fixture==1.0 --hash=sha256:",
                    profile$artifacts[[2L]]$sha256), lock)
  profile$artifacts[[3L]] <- artifact(lock)
  profile$python <- list(
    executable = python,
    version = trimws(processx::run(python, c(
      "-c", "import platform; print(platform.python_version())"
    ))$stdout),
    lock = "requirements.txt"
  )
  redirect <- file.path(root, "outside-owned-environment")
  transport <- new.env(parent = emptyenv())
  transport$invoke <- processx::run
  testthat::local_mocked_bindings(run = function(command, args, ...) {
    if ("pip" %in% args) {
      # Represent a configured environment without touching real user files.
      writeLines(c("[global]", paste0("target = ", redirect)),
                 file.path(dirname(dirname(command)), "pip.conf"))
    }
    transport$invoke(command, args, ...)
  }, .package = "processx")
  withr::local_envvar(c(PIP_TARGET = redirect))
  backend <- hm_install_backend(hm_backend_plan(
    "test-identity", profile = profile, cache = file.path(root, "cache")
  ), TRUE, TRUE, TRUE)
  expect_false(file.exists(file.path(redirect, "hm_audit_fixture.py")))
  result <- processx::run(
    file.path(backend$path, "environment", "bin", "python"),
    c("-I", "-c", paste0(
      "import hm_audit_fixture; print(hm_audit_fixture.value)"
    ))
  )
  expect_identical(trimws(result$stdout), "17")
  expect_true(.hm_environment_ok(backend$path))
})
