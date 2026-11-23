test_that("Python installation ignores ambient pip configuration", {
  skip_on_cran()
  skip_if_not_installed("processx")
  skip_on_os("windows")
  python <- unname(Sys.which("python3"))
  skip_if(!nzchar(python))
  probe <- tempfile("copied-venv-")
  copied <- processx::run(python, c("-m", "venv", "--copies", probe),
                          error_on_status = FALSE)
  unlink(probe, recursive = TRUE)
  skip_if(copied$status != 0L,
          "This Python build cannot create copied virtual environments.")
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

test_that("an interpreter without copied environments fails informatively", {
  skip_if_not_installed("processx")
  root <- tempfile("copied-venv-refusal-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines(paste0("fixture==1.0 --hash=sha256:", strrep("a", 64)),
             file.path(root, "requirements.txt"))
  profile <- list(
    python = list(executable = "python-fixture", version = "3.12.0",
                  lock = "requirements.txt"),
    artifacts = list(list(path = "requirements.txt", sha256 = strrep("a", 64)))
  )
  testthat::local_mocked_bindings(run = function(command, args, ...) {
    if ("venv" %in% args) {
      return(list(status = 1L, stdout = "",
                  stderr = "Error: cannot create venvs without symlinks"))
    }
    list(status = 0L, stdout = "3.12.0\n", stderr = "")
  }, .package = "processx")
  expect_error(
    hypermodR:::.hm_setup_python(profile, root),
    "isolated environment with copied files",
    class = "hm_platform_error"
  )
})
