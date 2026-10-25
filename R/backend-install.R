.hm_no_symlink <- function(path) {
  p <- path.expand(path)
  while (nzchar(p) &&
           !identical(p, dirname(p))) {
    link <- Sys.readlink(p)
    if (!is.na(link) &&
          nzchar(link)) {
      .hm_abort(
        "Symlink cache or artifact paths are forbidden.",
        "hm_integrity_error"
      )
    }
    p <- dirname(p)
  }
  invisible(TRUE)
}
.hm_artifact_ok <- function(path, a) {
  file.exists(path) &&
    !dir.exists(path) &&
    identical(
      unname(file.info(path)$size),
      as.numeric(a$bytes)
    ) &&
    identical(
      digest::digest(file = path, algo = "sha256"),
      a$sha256
    )
}
#' Install declared files into an immutable user runtime directory
#' @param plan Resolved backend plan.
#' @param accept_terms Explicit acceptance bound to this plan's terms and
#' artifacts.
#' @param offline Forbid network downloads.
#' @param acknowledge_custom_code Explicit permission to run reviewed custom
#' code.
#' @return An `hm_backend` list with installation path, original plan and
#' installed_unverified state.
#' @export
hm_install_backend <- function(
  plan, accept_terms = FALSE, offline = FALSE, acknowledge_custom_code = FALSE
) {
  .hm_validate_plan(plan)
  if (!isTRUE(accept_terms)) {
    .hm_abort("Explicit terms acceptance is required.", "hm_terms_error")
  }
  if (isTRUE(plan$profile$custom_code) &&
        !isTRUE(acknowledge_custom_code)) {
    .hm_abort(
      "Explicit custom-code acknowledgement is required.",
      "hm_terms_error"
    )
  }
  .hm_no_symlink(plan$cache)
  dir.create(plan$cache,
    recursive = TRUE, showWarnings = FALSE,
    mode = "0700"
  )
  target <- file.path(plan$cache, plan$digest)
  lock <- paste0(target, ".lock")
  if (!dir.create(lock, showWarnings = FALSE, mode = "0700")) {
    .hm_abort(
      "Installation is locked; inspect ownership before manual recovery.",
      "hm_install_locked"
    )
  }
  on.exit(
    unlink(lock, recursive = TRUE),
    add = TRUE
  )
  jsonlite::write_json(
    list(
      pid = Sys.getpid(), started = as.character(Sys.time()),
      digest = plan$digest
    ),
    file.path(lock, "owner.json"),
    auto_unbox = TRUE
  )
  if (dir.exists(target)) {
    .hm_no_symlink(target)
    ok <- all(
      vapply(
        plan$profile$artifacts, function(a) {
          f <- file.path(target, a$path)
          .hm_no_symlink(f)
          .hm_artifact_ok(f, a)
        }, logical(1)
      )
    )
    if (ok && !is.null(plan$profile$python)) {
      ok <- .hm_environment_ok(target)
    }
    if (!ok) {
      .hm_abort(
        "Existing runtime failed integrity checks; it was preserved.",
        "hm_integrity_error"
      )
    }
    return(
      structure(
        list(path = target, plan = plan, state = "installed_unverified"),
        class = "hm_backend"
      )
    )
  }
  stage <- tempfile(
    paste0(
      ".stage-", substr(plan$digest, 1, 12),
      "-"
    ),
    tmpdir = plan$cache
  )
  if (!dir.create(stage, mode = "0700")) {
    .hm_abort("Cannot allocate installation staging directory.")
  }
  on.exit(
    if (dir.exists(stage)) unlink(stage, recursive = TRUE),
    add = TRUE
  )
  for (a in plan$profile$artifacts) {
    dest <- file.path(stage, a$path)
    dir.create(
      dirname(dest),
      recursive = TRUE, showWarnings = FALSE
    )
    cached <- file.path(plan$cache, "artifacts", a$sha256)
    .hm_no_symlink(cached)
    if (.hm_artifact_ok(cached, a)) {
      if (!file.copy(cached, dest)) {
        .hm_abort("Failed to stage cached artifact.")
      }
    } else if (startsWith(a$url, "file://")) {
      source <- substring(a$url, 8L)
      .hm_no_symlink(source)
      if (!file.copy(source, dest)) {
        .hm_abort("Local artifact is unavailable.")
      }
    } else {
      if (isTRUE(offline)) {
        .hm_abort("Offline artifact cache miss.", "hm_offline_error")
      }
      status <- tryCatch(
        utils::download.file(a$url, dest, mode = "wb", quiet = TRUE),
        error = function(e) 1L
      )
      if (status != 0L) {
        .hm_abort("Artifact download failed.", "hm_download_error")
      }
    }
    if (!.hm_artifact_ok(dest, a)) {
      .hm_abort("Artifact size or SHA256 mismatch.", "hm_integrity_error")
    }
  }
  Sys.chmod(
    file.path(stage, plan$profile$command),
    "0700"
  )
  if (!is.null(plan$profile$python)) {
    .hm_setup_python(plan$profile, stage)
  }
  environment_manifest <- if (!is.null(plan$profile$python)) {
    .hm_environment_inventory(file.path(stage, "environment"))
  } else {
    NULL
  }
  receipt <- list(
    environment_manifest = environment_manifest, schema_version = 1L,
    plan_digest = plan$digest,
    profile = plan$profile, accepted_terms = plan$profile$terms,
    accepted_at = as.character(Sys.time()),
    state = "installed_unverified", scientific_qualification = FALSE
  )
  jsonlite::write_json(
    receipt, file.path(stage, "receipt.json"),
    auto_unbox = TRUE, pretty = TRUE
  )
  if (!file.rename(stage, target)) {
    .hm_abort(paste0(
      "Atomic runtime activation failed; prior runtimes were ",
      "preserved."
    ))
  }
  structure(
    list(path = target, plan = plan, state = "installed_unverified"),
    class = "hm_backend"
  )
}
.hm_setup_python <- function(profile, stage) {
  .hm_require("processx")
  py <- profile$python
  if (is.null(py$executable) ||
    is.null(py$version) ||
    is.null(py$lock) ||
    !.hm_safe_relative(py$lock) ||
    !py$lock %in% vapply(
      profile$artifacts, function(a) a$path,
      ""
    )) {
    .hm_abort(paste0(
      "Python setup requires a pinned interpreter and ",
      "declared hash lock."
    ))
  }
  lock_lines <- trimws(
    readLines(
      file.path(stage, py$lock),
      warn = FALSE
    )
  )
  lock_lines <- lock_lines[nzchar(lock_lines) &
                             !startsWith(lock_lines, "#")]
  pattern <- "^[A-Za-z0-9_.-]+==[A-Za-z0-9_.+!-]+ --hash=sha256:[a-f0-9]{64}$"
  hashes <- sub(".*--hash=sha256:", "", lock_lines)
  declared <- vapply(
    profile$artifacts, function(a) a$sha256,
    ""
  )
  if (any(!grepl(pattern, lock_lines)) ||
        any(!hashes %in% declared)) {
    .hm_abort(
      paste0(
        "Python lock must contain only exact packages and ",
        "declared wheel hashes; URLs and resolver options are ",
        "forbidden."
      )
    )
  }
  version <- processx::run(
    py$executable, c(
      "-c",
      "import platform; print(platform.python_version())"
    )
  )$stdout
  if (!identical(
    trimws(version),
    py$version
  )) {
    .hm_abort("Python interpreter version mismatch.")
  }
  # --copies makes the environment independent of mutable symlink targets.
  processx::run(
    py$executable, c("-m", "venv", "--copies", file.path(
      stage,
      "environment"
    ))
  )
  binary <- file.path(
    stage, "environment", if (.Platform$OS.type == "windows") {
      "Scripts/python.exe"
    } else {
      "bin/python"
    }
  )
  processx::run(
    binary, c(
      "-m", "pip", "install", "--no-index", "--no-deps", "--require-hashes",
      "--find-links", stage, "-r", file.path(stage, py$lock)
    ),
    timeout = 3e+05, env = c(
      OMP_NUM_THREADS = "1",
      OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1"
    )
  )
}

.hm_environment_inventory <- function(path) {
  base <- normalizePath(path, mustWork = TRUE)
  items <- list.files(
    path,
    recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE,
    include.dirs = TRUE
  )
  resolved <- normalizePath(items, mustWork = TRUE)
  if (any(!startsWith(resolved, paste0(base, .Platform$file.sep)))) {
    .hm_abort(
      "Environment contains an escaping symlink.",
      "hm_integrity_error"
    )
  }
  files <- items[!dir.exists(items)]
  hashes <- vapply(
    files, function(f) digest::digest(file = f, algo = "sha256"),
    ""
  )
  names(hashes) <- substring(
    files, nchar(path) +
      2L
  )
  as.list(hashes)
}
.hm_environment_ok <- function(path) {
  receipt <- tryCatch(
    jsonlite::read_json(file.path(path, "receipt.json")),
    error = function(e) NULL
  )
  if (is.null(receipt$environment_manifest)) {
    return(FALSE)
  }
  current <- .hm_environment_inventory(file.path(path, "environment"))
  identical(current, receipt$environment_manifest)
}
