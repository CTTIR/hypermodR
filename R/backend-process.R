.hm_backend_device_env <- function(
  plan, resources) {
  if (!inherits(resources, "hm_resources"))
    .hm_abort(
              "Expected resource declaration.")
  if (
      identical(
                plan$profile$device, "cpu")) {
    if (
        !identical(
                   resources$device, "cpu")) {
      .hm_abort(
        "CPU backend requires CPU resource admission.",
        "hm_resource_error"
      )
    }
    return(c(CUDA_VISIBLE_DEVICES = ""))
  }
  if (
      !identical(
                 plan$profile$device,
                 "cuda") ||
        !isTRUE(
                plan$profile$hardware_verified) ||
        !grepl(
               "^cuda:(0|[1-9][0-9]*)$",
               resources$device)) {
    .hm_abort(
      paste0(
        "CUDA backend requires verified device ABI and explicit ",
        "cuda:N resource admission."
      ),
      "hm_resource_error"
    )
  }
  selected <- sub(
                  "^cuda:",
                  "", resources$device)
  inherited <- Sys.getenv(
    "CUDA_VISIBLE_DEVICES",
    unset = NA_character_
  )
  if (!is.na(inherited)) {
    visible <- trimws(
      strsplit(
               inherited,
               ",", fixed = TRUE)[[1L]]
    )
    if (!selected %in% visible) {
      .hm_abort(
        "Requested CUDA device is outside inherited visibility.",
        "hm_resource_error"
      )
    }
  }
  c(CUDA_VISIBLE_DEVICES = selected)
}

#' Start a process belonging to an installed runtime
#' @param backend Installed backend receipt.
#' @param model Expected model identity.
#' @param resources An `hm_resources` CPU declaration, or explicit cuda:N
#' for a CUDA plan with verified hardware ABI; threads must be one.
#' @param host Bind address, restricted to loopback.
#' @return Mutable owned `hm_backend_handle` environment containing process,
#' PID, owner PID, model identity and plan digest.
#' @details Starts the declared verified command through the Linux bounded
#' child launcher described in hm_resources with one numerical thread. CUDA
#' plans require explicit cuda:N selection, verified hardware ABI and a selected
#' device within inherited CUDA_VISIBLE_DEVICES when set. CUDA memory is not
#' limited by this launcher; CPU memory is per-process virtual address space,
#' not aggregate RSS. Network remains a policy declaration. Admission is not
#' live GPU qualification. Handles are valid only in their owning R session.
#' @export
hm_start_backend <- function(
  backend,
  model = backend$plan$model_id,
  resources = hm_resources(),
  host = "127.0.0.1"
) {
  .hm_require("processx")
  if (!inherits(backend, "hm_backend") ||
    !identical(
               model,
               backend$plan$model_id)) {
    .hm_abort(
              "Installed backend/model mismatch.")
  }
  if (!host %in% c("127.0.0.1", "::1") ||
    !identical(
      as.integer(resources$threads),
      1L
    )) {
    .hm_abort(
              "Loopback and one thread are required.")
  }
  .hm_validate_plan(backend$plan)
  device_env <- .hm_backend_device_env(
                                       backend$plan,
                                       resources)
  if (!identical(
    backend$path,
    file.path(
              backend$plan$cache,
              backend$plan$digest)
  )) {
    .hm_abort(
      "Runtime path differs from declared installation."
    )
  }
  .hm_no_symlink(backend$path)
  if (
      !is.null(
               backend$plan$profile$python) &&
        !.hm_environment_ok(backend$path)) {
    .hm_abort(
      "Python environment integrity changed.",
      "hm_integrity_error"
    )
  }
  for (a in backend$plan$profile$artifacts) {
    f <- file.path(backend$path, a$path)
    .hm_no_symlink(f)
    if (!.hm_artifact_ok(f, a)) {
      .hm_abort(
        "Runtime integrity changed.",
        "hm_integrity_error"
      )
    }
  }
  p <- backend$plan$profile
  command <- file.path(
                       backend$path,
                       p$command)
  args <- p$args
  if (!is.null(p$python)) {
    args <- c(command, args)
    command <- file.path(
      backend$path,
      "environment",
      if (.Platform$OS.type ==
            "windows") {
        "Scripts/python.exe"
      } else {
        "bin/python"
      }
    )
  }
  process <- .hm_resource_process(
    command, args, resources = resources,
    wd = backend$path,
    allow_cuda = identical(
                           backend$plan$profile$device,
                           "cuda"),
    env = c(
      device_env,
      PYTHONDONTWRITEBYTECODE = "1",
      OMP_NUM_THREADS = "1",
      OPENBLAS_NUM_THREADS = "1",
      MKL_NUM_THREADS = "1"
    )
  )
  handle <- new.env(parent = emptyenv())
  handle$process <- process
  handle$pid <- process$get_pid()
  handle$model <- model
  handle$plan_digest <- backend$plan$digest
  handle$owner_pid <- Sys.getpid()
  class(handle) <- "hm_backend_handle"
  handle
}
.hm_owned_handle <- function(handle) {
  if (
      !inherits(
                handle, "hm_backend_handle") ||
        !identical(
                   handle$owner_pid,
                   Sys.getpid()) ||
        !inherits(
                  handle$process, "process") ||
        !identical(
                   handle$pid,
                   handle$process$get_pid(
                   ))) {
    .hm_abort(
      "Process handle is not owned by this session."
    )
  }
  invisible(TRUE)
}
#' Inspect a managed process
#' @param handle Owned process handle.
#' @return List with PID, model, alive flag, exit status and plan digest.
#' @export
hm_backend_status <- function(handle) {
  .hm_owned_handle(handle)
  list(
    pid = handle$pid, model = handle$model,
    alive = handle$process$is_alive(
    ), exit_status = handle$process$get_exit_status(
    ),
    plan_digest = handle$plan_digest
  )
}
#' Stop only an owned runtime process
#' @param handle Owned process handle.
#' @return Final backend-status list invisibly.
#' @export
hm_stop_backend <- function(handle) {
  .hm_owned_handle(handle)
  if (handle$process$is_alive()) {
    handle$process$kill_tree()
    handle$process$wait(5000L)
    if (handle$process$is_alive()) {
      .hm_abort(
        "Cancellation is not yet acknowledged.",
        "hm_process_error"
      )
    }
  }
  invisible(hm_backend_status(handle))
}
