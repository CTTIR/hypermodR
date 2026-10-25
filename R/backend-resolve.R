#' Resolve official binary metadata into an exact reviewable runtime manifest
#'
#' This explicit network operation resolves metadata only. It does not download
#' model weights or install packages. Published terms and inherited distribution
#' notices must be reviewed before converting the result into an install plan.
#' @param backend Either reference_cpu or vllm_linux_cuda.
#' @param model_id Exact model identity or reference family.
#' @param python Explicit existing Python 3.11 or 3.12 executable.
#' @param uv Explicit uv resolver executable (version 0.11.26).
#' @param directory External persistent resolution directory.
#' @return An `hm_backend_resolution` containing exact artifact profile,
#' package metadata, external directory, hard gates and pending qualification.
#' @export
hm_resolve_backend <- function(
  backend = c("reference_cpu", "vllm_linux_cuda"),
  model_id, python, uv = unname(Sys.which("uv")),
  directory = tempfile("resolution-", tmpdir = tools::R_user_dir(
    "hypermodR", "cache"
  ))
) {
  backend <- match.arg(backend)
  .hm_require("processx")
  .hm_scalar_character(model_id, "model_id")
  .hm_scalar_character(python, "python")
  .hm_scalar_character(uv, "uv")
  caps <- hm_backend_capabilities()
  if (!identical(caps$os, "Linux") ||
        !identical(caps$arch, "x86_64")) {
    .hm_abort(
      paste0(
        "Automated resolver currently admits Linux x86_64 only; ",
        "other routes require separate locks."
      ),
      "hm_platform_error"
    )
  }
  version <- trimws(
    processx::run(python, c(
      "-c",
      "import platform; print(platform.python_version())"
    ))$stdout
  )
  if (!grepl("^3[.](11|12)[.]", version)) {
    .hm_abort(
      paste0(
        "Resolver requires an explicit existing Python 3.11 or ",
        "3.12 interpreter."
      ),
      "hm_platform_error"
    )
  }
  uv_version <- trimws(processx::run(uv, "--version")$stdout)
  if (!startsWith(uv_version, "uv 0.11.26 ")) {
    .hm_abort("Resolver uv version must be exactly 0.11.26.")
  }
  .hm_no_symlink(directory)
  if (!dir.create(directory,
        recursive = TRUE, showWarnings = FALSE,
        mode = "0700"
      )) {
    .hm_abort("Resolution directory must be new and exclusive.")
  }
  requirements <- if (backend == "reference_cpu") {
    c("numpy==2.2.6", "torch==2.6.0")
  } else {
    "vllm==0.11.0"
  }
  req <- list(
    directory = normalizePath(directory),
    uv = normalizePath(uv),
    python = normalizePath(python),
    requirements = as.list(requirements),
    platform = "x86_64-manylinux_2_28",
    torch_backend = if (backend == "reference_cpu") {
      "cpu"
    } else {
      NULL
    }
  )
  request_path <- file.path(directory, "resolution-input.json")
  jsonlite::write_json(req, request_path, auto_unbox = TRUE, null = "null")
  result <- processx::run(
    python, c(
      .hm_resource("backends", "resolve.py"),
      request_path
    ),
    timeout = 3e+05, env = c(
      OMP_NUM_THREADS = "1",
      OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1"
    )
  )
  resolved <- jsonlite::fromJSON(result$stdout, simplifyVector = FALSE)
  lock <- file.path(directory, "requirements.txt")
  writeLines(resolved$requirements, lock, useBytes = TRUE)
  artifacts <- resolved$artifacts
  artifacts[[length(artifacts) +
               1L]] <- list(
    path = "requirements.txt", url = paste0("file://", lock),
    bytes = unname(file.info(lock)$size),
    sha256 = digest::digest(file = lock, algo = "sha256")
  )
  for (i in seq_along(
    resolved$terms
  )) {
    resolved$terms[[i]]$sha256 <- digest::digest(
      resolved$terms[[i]]$text,
      algo = "sha256", serialize = FALSE
    )
  }
  profile <- list(
    profile_id = paste0(backend, "-", version),
    model_id = model_id,
    source_revision = if (backend == "reference_cpu") {
      "hypermodr-reference-v1"
    } else {
      "vllm-0.11.0"
    },
    runtime_version = if (
                          backend == "reference_cpu") {
      "torch-2.6.0-numpy-2.2.6"
    } else {
      "vllm-0.11.0"
    },
    os = "Linux", arch = "x86_64",
    device = if (backend == "reference_cpu") {
      "cpu"
    } else {
      "cuda"
    },
    artifacts = artifacts, terms = resolved$terms, command = "launch.py",
    args = character(), custom_code = FALSE,
    qualification = "installation_pending_smoke_and_parity",
    python = list(
      executable = normalizePath(python),
      version = version, lock = "requirements.txt"
    )
  )
  launcher <- if (backend == "reference_cpu") {
    .hm_resource("backends", "reference-launch.py")
  } else {
    .hm_resource("backends", "vllm-launch.py")
  }
  profile$artifacts[[length(profile$artifacts) +
                       1L]] <- list(
    path = "launch.py", url = paste0("file://", launcher),
    bytes = unname(file.info(launcher)$size),
    sha256 = digest::digest(file = launcher, algo = "sha256")
  )
  if (backend == "reference_cpu") {
    base <- .hm_resource("python", "hypermodr_worker")
    files <- list.files(base, recursive = TRUE, full.names = TRUE)
    files <- files[grepl("[.]py$", files)]
    for (f in files) {
      profile$artifacts[[length(profile$artifacts) +
                           1L]] <- list(
        path = paste0(
          "hypermodr_worker/", substring(
            f, nchar(base) +
              2L
          )
        ),
        url = paste0("file://", f),
        bytes = unname(file.info(f)$size),
        sha256 = digest::digest(file = f, algo = "sha256")
      )
    }
  }
  out <- list(
    profile = profile, packages = resolved$packages, backend = backend,
    directory = normalizePath(directory),
    hard_gates = c(
      "artifact-level license/notice review",
      if (backend == "vllm_linux_cuda") {
        c(
          "NVIDIA driver/GPU ABI admission",
          "exact local checkpoint manifest"
        )
      }
    ),
    qualification = "pending_optional_live_smoke_and_parity"
  )
  jsonlite::write_json(
    out, file.path(directory, "resolution.json"),
    pretty = TRUE, auto_unbox = TRUE
  )
  structure(out, class = "hm_backend_resolution")
}

#' Admit reviewed resolved metadata as an exact install plan
#' @param resolution Result of hm_resolve_backend.
#' @param reviewed_terms Full reviewed terms entries, preserving each package,
#'   version and artifact identity. Use the resolved entries after artifact
#' notice
#'   review and set each reviewed flag to TRUE.
#' @param cache External runtime cache.
#' @param hardware_verified Whether exact driver/device ABI has been checked.
#' @param checkpoint Optional exact local checkpoint artifact manifest for vLLM.
#' @param port Unprivileged loopback server port.
#' @return An `hm_backend_plan` bound to reviewed artifact terms and supplied
#' device/checkpoint admission.
#' @export
hm_admit_backend <- function(
  resolution, reviewed_terms, cache = tools::R_user_dir("hypermodR", "cache"),
  hardware_verified = FALSE, checkpoint = NULL, port = 8000L
) {
  if (!inherits(resolution, "hm_backend_resolution") ||
        !is.list(reviewed_terms) ||
        length(reviewed_terms) !=
          length(resolution$profile$terms)) {
    .hm_abort("Resolved per-artifact terms review is required.")
  }
  p <- resolution$profile
  for (i in seq_along(reviewed_terms)) {
    old <- p$terms[[i]]
    new <- reviewed_terms[[i]]
    if (!identical(old$package, new$package) ||
          !identical(old$version, new$version) ||
          !identical(old$artifact_sha256, new$artifact_sha256) ||
          !isTRUE(new$reviewed) ||
          isTRUE(new$unresolved) ||
          !is.character(new$text) ||
          !nzchar(new$text) ||
          !identical(new$sha256, digest::digest(new$text,
            algo = "sha256",
            serialize = FALSE
          ))) {
      .hm_abort(
        paste0(
          "Review requires full resolved terms while preserving ",
          "exact package, version and artifact identity."
        ),
        "hm_terms_error"
      )
    }
  }
  p$terms <- reviewed_terms
  if (resolution$backend == "vllm_linux_cuda") {
    if (!isTRUE(hardware_verified) ||
          is.null(checkpoint) ||
          !is.list(checkpoint$artifacts) ||
          !length(checkpoint$artifacts) ||
          !.hm_safe_relative(checkpoint$directory) ||
          !length(checkpoint$terms) ||
          !is.numeric(port) ||
          length(port) !=
            1L || port != as.integer(port) ||
          port < 1024 || port > 65535) {
      .hm_abort(
        paste0(
          "vLLM requires admitted device ABI, exact local ",
          "checkpoint artifacts/terms and an unprivileged port."
        )
      )
    }
    p$artifacts <- c(p$artifacts, checkpoint$artifacts)
    p$terms <- c(p$terms, checkpoint$terms)
    p$args <- c(checkpoint$directory, p$model_id, as.character(port))
    p$hardware_verified <- TRUE
  }
  hm_backend_plan(p$model_id, device = p$device, profile = p, cache = cache)
}

#' Resolve an immutable public Hugging Face checkpoint without downloading
#' weights
#' @param repository Public model repository owner/name.
#' @param revision Exact 40-character commit SHA.
#' @param directory New external metadata staging directory.
#' @return List containing repository/revision, architecture, local checkpoint
#' directory name, artifact hashes/sizes/URLs, unreviewed terms and pending
#' qualification.
#' @export
hm_resolve_checkpoint <- function(
  repository, revision, directory = tempfile("checkpoint-",
    tmpdir = tools::R_user_dir("hypermodR", "cache")
  )
) {
  .hm_require("httr2")
  if (!is.character(repository) ||
        length(repository) !=
          1L || !grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository) ||
        !.hm_safe_relative(repository) ||
        !is.character(revision) ||
        length(revision) !=
          1L || !grepl("^[a-f0-9]{40}$", revision)) {
    .hm_abort(paste0(
      "An exact public repository and immutable commit SHA ",
      "are required."
    ))
  }
  .hm_no_symlink(directory)
  if (!dir.create(directory,
        recursive = TRUE, showWarnings = FALSE,
        mode = "0700"
      )) {
    .hm_abort("Checkpoint staging directory must be new and exclusive.")
  }
  get_raw <- function(url, limit = 8 * 1024^2) {
    req <- httr2::req_timeout(
      httr2::request(url),
      30
    )
    req <- httr2::req_options(req, maxfilesize = limit)
    resp <- httr2::req_perform(req)
    out <- httr2::resp_body_raw(resp)
    if (length(out) >
          limit) {
      .hm_abort("Checkpoint metadata exceeds bounds.")
    }
    attr(out, "pagination") <- httr2::resp_header(resp, "link")
    out
  }
  base <- paste0(
    "https://huggingface.co/", repository, "/resolve/",
    revision, "/"
  )
  tree_url <- paste0(
    "https://huggingface.co/api/models/", repository, "/tree/", revision,
    "?recursive=true&expand=true&limit=50"
  )
  tree <- get_raw(tree_url)
  if (!is.null(attr(tree, "pagination")) &&
    grepl(
      "rel=\"next\"", attr(tree, "pagination"),
      fixed = TRUE
    )) {
    .hm_abort(
      paste0(
        "Checkpoint tree pagination requires a separately ",
        "audited complete manifest."
      )
    )
  }
  entries <- jsonlite::fromJSON(
    rawToChar(tree),
    simplifyVector = FALSE
  )
  # A full bounded manifest is mandatory: refuse repositories whose file
  # list is too large for this single-page resolver rather than silently
  # omitting shards.
  if (length(entries) >=
        1000L) {
    .hm_abort(
      paste0(
        "Checkpoint tree requires pagination; provide a ",
        "separately audited full manifest."
      )
    )
  }
  allowed <- function(path) {
    grepl(
      paste0(
        "([.]safetensors([.]index[.]json)?$|[.]json$|[.]model$|",
        "[.]tiktoken$|[.]txt$|^LICENSE($|[.])|^NOTICE($|[.]))"
      ),
      path
    )
  }
  files <- Filter(
    function(x) {
      identical(x$type, "file") &&
        allowed(x$path)
    }, entries
  )
  config <- Filter(
    function(x) identical(x$path, "config.json"),
    files
  )
  if (length(config) !=
        1L) {
    .hm_abort("Checkpoint config.json is missing.")
  }
  cfg <- jsonlite::fromJSON(
    rawToChar(get_raw(paste0(base, "config.json"))),
    simplifyVector = FALSE
  )
  architectures <- c(
    "Qwen2ForCausalLM", "Qwen3ForCausalLM", "MistralForCausalLM",
    "Qwen2_5_VLForConditionalGeneration",
    "Qwen3VLForConditionalGeneration", "InternVLChatModel",
    "LlavaForConditionalGeneration",
    "Mistral3ForConditionalGeneration"
  )
  if (!length(cfg$architectures) ||
    any(
      !unlist(cfg$architectures) %in%
        architectures
    ) ||
    !is.null(cfg$auto_map)) {
    .hm_abort(
      paste0(
        "Checkpoint architecture requires a separate reviewed ",
        "vLLM integration; remote custom code is forbidden."
      )
    )
  }
  artifacts <- list()
  license_text <- character()
  for (f in files) {
    if (!.hm_safe_relative(f$path)) {
      .hm_abort("Unsafe checkpoint path.")
    }
    url <- paste0(base, f$path)
    if (!is.null(f$lfs$oid)) {
      sha <- sub("^sha256:", "", f$lfs$oid)
      bytes <- f$lfs$size
      if (!grepl("^[a-f0-9]{64}$", sha)) {
        .hm_abort("Checkpoint LFS SHA256 is missing.")
      }
    } else {
      data <- get_raw(url)
      target <- file.path(directory, f$path)
      dir.create(
        dirname(target),
        recursive = TRUE, showWarnings = FALSE
      )
      writeBin(data, target)
      sha <- digest::digest(data, algo = "sha256", serialize = FALSE)
      bytes <- length(data)
      if (grepl("^LICENSE($|[.])|^NOTICE($|[.])", f$path)) {
        license_text <- c(license_text, rawToChar(data))
      }
    }
    artifacts[[length(artifacts) +
                 1L]] <- list(
      path = paste0("checkpoint/", f$path),
      url = url, sha256 = sha, bytes = bytes
    )
  }
  if (!any(
    grepl(
      "[.]safetensors$", vapply(
        artifacts, function(a) a$path,
        ""
      )
    )
  ) ||
    !length(license_text)) {
    .hm_abort(
      "Safe-tensor weights and exact license text must both be available.",
      "hm_terms_error"
    )
  }
  text <- paste(license_text, collapse = "\n\n")
  out <- list(
    repository = repository, revision = revision,
    architecture = cfg$architectures,
    directory = "checkpoint", artifacts = artifacts, terms = list(
      list(
        text = text, sha256 = digest::digest(text,
          algo = "sha256",
          serialize = FALSE
        ),
        reviewed = FALSE, source = paste0(base, "LICENSE")
      )
    ),
    qualification = "pending_platform_and_processor_parity"
  )
  jsonlite::write_json(
    out, file.path(directory, "checkpoint-manifest.json"),
    pretty = TRUE, auto_unbox = TRUE
  )
  out
}
