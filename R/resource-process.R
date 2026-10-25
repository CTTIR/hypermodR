# Internal child launcher. Limits are
# applied before executing the requested
# program.
.hm_resource_admit <- function(
  resources, allow_cuda = FALSE) {
  if (!inherits(resources, "hm_resources")) {
    .hm_abort(
              "Expected resource declaration.")
  }
  if (
      !identical(
                 Sys.info(
                 )[["sysname"]],
                 "Linux") ||
        any(
          !nzchar(
            Sys.which(
                      c("taskset",
                        "prlimit",
                        "timeout"))
          )
        )) {
    .hm_abort(
      paste0(
        "Hard child budgets require Linux taskset, prlimit and ",
        "timeout; this platform is unsupported."
      ),
      "hm_resource_error"
    )
  }
  if (!identical(resources$device, "cpu") &&
    !(isTRUE(allow_cuda) &&
      grepl(
            "^cuda:(0|[1-9][0-9]*)$",
            resources$device))) {
    .hm_abort(
      "Only CPU execution is supported by the bounded child route.",
      "hm_resource_error"
    )
  }
  status <- readLines(
                      "/proc/self/status",
                      warn = FALSE)
  allowed <- sub(
    "^Cpus_allowed_list:[[:space:]]*", "",
    grep(
         "^Cpus_allowed_list:",
         status, value = TRUE)
  )
  cpus <- unlist(
    lapply(
      strsplit(
               allowed,
               ",", fixed = TRUE)[[1]],
      function(x) {
        range <- as.integer(
                            strsplit(
                                     x, "-",
                                     fixed = TRUE)[[1]])
        if (length(range) ==
              1L) {
          range
        } else {
          seq.int(range[1], range[2])
        }
      }
    )
  )
  if (length(cpus) <
        resources$cpus) {
    .hm_abort(
      "CPU budget exceeds inherited affinity.",
      "hm_resource_error"
    )
  }
  list(
    affinity = paste(
      head(cpus, resources$cpus),
      collapse = ","
    ),
    memory_bytes = format(
      floor(resources$memory_mb * 1024^2),
      scientific = FALSE, trim = TRUE
    ),
    network = "not isolated; network is a policy declaration only",
    memory = paste0(
      "Linux RLIMIT_AS virtual address space per process; ",
      "inherited by descendants"
    ),
    cpu = "inherited CPU affinity subset; no additional CPU quota"
  )
}
.hm_resource_process <- function(
  command,
  args = character(), resources, wd = NULL,
  env = character(), allow_cuda = FALSE
) {
  .hm_require("processx")
  capability <- .hm_resource_admit(
                                   resources,
                                   allow_cuda = allow_cuda)
  thread_keys <- c(
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "NUMEXPR_NUM_THREADS", "BLIS_NUM_THREADS"
  )
  env <- env[!names(env) %in%
               thread_keys]
  env <- c(
    env, setNames(
      rep(
        as.character(resources$threads),
        length(thread_keys)
      ),
      thread_keys
    )
  )
  process <- processx::process$new(
    Sys.which("timeout"),
    c(
      "--signal=TERM", "--kill-after=1s",
      paste0(resources$timeout, "s"),
      Sys.which("taskset"),
      "--cpu-list", capability$affinity,
      Sys.which("prlimit"),
      paste0(
        "--as=", capability$memory_bytes,
        ":", capability$memory_bytes
      ),
      "--", command, args
    ),
    wd = wd,
    env = env, stdin = "|", stdout = "|",
    stderr = "|",
    cleanup = TRUE, cleanup_tree = TRUE
  )
  process
}
.hm_resource_owned <- function(job) {
  if (!is.environment(job) ||
    !identical(
               job$owner_pid,
               Sys.getpid()) ||
    is.null(job$process)) {
    .hm_abort(
      "Process handle is not owned by this session.",
      "hm_process_error"
    )
  }
  invisible(TRUE)
}
.hm_resource_r_bg <- function(
  func, args = list(), resources, ...) {
  .hm_resource_admit(resources)
  directory <- tempfile("hm-child-")
  dir.create(directory)
  success <- FALSE
  on.exit(
    if (
        !success) unlink(
                         directory,
                         recursive = TRUE),
    add = TRUE
  )
  payload <- file.path(
                       directory,
                       "input.rds")
  result <- file.path(
                      directory,
                      "output.rds")
  saveRDS(
    list(
      func = func,
      args = args, resources = resources,
      libpaths = .libPaths()
    ),
    payload
  )
  package <- system.file(
                         package = "hypermodR")
  if (!nzchar(package)) {
    package <- getNamespaceInfo(
      asNamespace("hypermodR"),
      "path"
    )
  }
  script <- file.path(directory, "worker.R")
  writeLines(
    c(
      "a <- commandArgs(TRUE)", paste0(
        ".libPaths(strsplit(Sys.getenv(\"HM_RESOURCE_LIBPATHS\"), ",
        ".Platform$path.sep, fixed=TRUE)[[1]])"
      ),
      "p <- Sys.getenv(\"HM_RESOURCE_PACKAGE\")",
      paste0(
        "if ",
        "(file.exists(file.path(p,\"Meta\",\"package.rds\"))) ",
        "library(hypermodR) else pkgload::load_all(p, ",
        "quiet=TRUE, export_all=FALSE)"
      ),
      "x <- readRDS(a[1])", paste0(
        "options(hypermodR.resource.child=TRUE, ",
        "hypermodR.resource.budget=x$resources)"
      ),
      paste0(
        "answer <- tryCatch(list(ok=TRUE, ",
        "value=do.call(x$func,x$args,quote=TRUE)), ",
        "error=function(e) ",
        "list(ok=FALSE,message=conditionMessage(e)))"
      ),
      "saveRDS(answer,a[2])"
    ),
    script
  )
  process <- .hm_resource_process(
    file.path(
      R.home("bin"),
      "Rscript"
    ),
    c("--vanilla", script, payload, result),
    resources, env = c(
      HM_RESOURCE_PACKAGE = package,
      HM_RESOURCE_LIBPATHS = paste(
                                   .libPaths(
                                   ), collapse = .Platform$path.sep)
    )
  )
  handle <- new.env(parent = emptyenv())
  handle$process <- process
  handle$owner_pid <- Sys.getpid()
  handle$is_alive <- function(
  ) {
    process$is_alive()
  }
  handle$wait <- function(
    timeout = -1L) {
    process$wait(timeout)
  }
  handle$kill_tree <- function() {
    .hm_resource_owned(handle)
    process$kill_tree()
  }
  handle$get_result <- function() {
    .hm_resource_owned(handle)
    if (process$is_alive()) {
      .hm_abort(
        "Worker has not exited.",
        "hm_process_error"
      )
    }
    on.exit(
      unlink(directory, recursive = TRUE),
      add = TRUE
    )
    if (!file.exists(result)) {
      .hm_abort(
        paste0(
          "Bounded worker exited without output (timeout, memory ",
          "limit, or process failure)."
        ),
        "hm_resource_error"
      )
    }
    answer <- readRDS(result)
    if (!isTRUE(answer$ok)) {
      .hm_abort(
                answer$message,
                "hm_process_error")
    }
    answer$value
  }
  reg.finalizer(
    handle, function(x) {
      if (x$process$is_alive()) {
        x$process$kill_tree()
      }
      unlink(directory, recursive = TRUE)
    }, onexit = TRUE
  )
  success <- TRUE
  handle
}
