#' Declare bounded execution resources
#' @param cpus Positive integer logical-CPU count selected from inherited
#' allowed affinity.
#' @param threads Positive integer numerical thread budget, no larger than cpus.
#' @param memory_mb Positive finite per-process virtual address-space limit in
#' MiB for bounded child execution.
#' @param device Single device identifier. General bounded children require
#' cpu. A separately admitted CUDA backend may select an explicit cuda:N
#' device, subject to inherited visibility and verified hardware ABI.
#' @param timeout Positive finite time budget in seconds.
#' @param network Logical network-use policy declaration; network access is not
#' isolated by this launcher.
#' @return An `hm_resources` list of the validated declarations.
#' @details Bounded child routes currently require Linux taskset, prlimit and
#' timeout. They apply inherited-affinity CPU subsets, numerical thread
#' environment settings before startup, per-process RLIMIT_AS and an external
#' elapsed timeout. Memory limits concern virtual address space, not aggregate
#' descendant RSS; no additional CPU quota or network isolation is supplied.
#' CUDA selection is admitted only by the backend launcher; hm_with_resources
#' remains CPU-only. GPU memory is not limited, and admitting a device does
#' not establish live GPU or scientific qualification. This is not a security
#' sandbox.
#' @export
hm_resources <- function(
  cpus = 1L, threads = 1L, memory_mb = 1024, device = "cpu", timeout = 60,
  network = FALSE
) {
  for (v in list(cpus, threads, memory_mb, timeout)) {
    if (!is.numeric(v) ||
          length(v) !=
            1 || !is.finite(v) ||
          v <= 0) {
      .hm_abort("Resource limits must be positive finite scalars.")
    }
  }
  if (cpus != as.integer(cpus) ||
        threads != as.integer(threads) ||
        threads > cpus || !is.logical(network) ||
        length(network) !=
          1 || is.na(network)) {
    .hm_abort("Invalid thread or network budget.")
  }
  .hm_scalar_character(device, "device")
  structure(
    list(
      cpus = cpus, threads = threads, memory_mb = memory_mb, device = device,
      timeout = timeout, network = network
    ),
    class = "hm_resources"
  )
}
#' Run an R expression in a bounded child process
#' @param resources An `hm_resources` declaration.
#' @param code Unevaluated expression evaluated in a bounded child with its
#' referenced serializable caller bindings.
#' @return The serialized value of `code` returned from the child.
#' @details Evaluation occurs in a fresh child, so side effects on ordinary
#' parent R objects are not propagated. Use serializable inputs and results;
#' live handles are unsuitable. Linux child limits are described in
#' hm_resources. Nested evaluation inside an already bounded child must reuse
#' the identical outer budget. Unsupported platforms/devices fail admission
#' rather than executing without limits.
#' @export
hm_with_resources <- function(resources, code) {
  if (!inherits(resources, "hm_resources")) {
    .hm_abort("Expected resource declaration.")
  }
  if (isTRUE(getOption("hypermodR.resource.child"))) {
    outer <- getOption("hypermodR.resource.budget")
    if (!identical(resources, outer)) {
      .hm_abort(
        "Nested execution must reuse the admitted outer resource budget.",
        "hm_resource_error"
      )
    }
    return(
      eval(
        substitute(code),
        envir = parent.frame()
      )
    )
  }
  expression <- substitute(code)
  caller <- parent.frame()
  environment <- new.env(parent = baseenv())
  for (name in all.names(expression, functions = TRUE, unique = TRUE)) {
    if (exists(name, envir = caller, inherits = TRUE)) {
      assign(
        name, get(name, envir = caller, inherits = TRUE),
        envir = environment
      )
    }
  }
  worker <- .hm_resource_r_bg(
    function(expression, environment) eval(expression, environment),
    args = list(expression = expression, environment = environment),
    resources = resources
  )
  on.exit(
    if (worker$is_alive()) {
      worker$kill_tree()
      worker$wait(5000L)
    },
    add = TRUE
  )
  wait_ms <- as.integer(min(
    .Machine$integer.max, (resources$timeout + 3) * 1000
  ))
  worker$wait(wait_ms)
  if (worker$is_alive()) {
    .hm_abort("Worker cancellation was not acknowledged.", "hm_process_error")
  }
  worker$get_result()
}
