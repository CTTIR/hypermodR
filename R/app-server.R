.hm_app_server <- function(
  input, output, session) {
  state <- hm_app_state()
  tick <- shiny::reactiveVal(0L)
  runtime <- shiny::reactiveVal(NULL)
  runtime_job <- NULL
  retained <- list()
  comparison <- shiny::reactiveVal(
                                   "Run two compatible workflows to compare.")
  refresh <- function() tick(tick() + 1L)
  safe <- function(code) {
    tryCatch(
      {
        force(code)
        state$error <- NULL
        refresh()
      }, error = function(e) {
        state$error <- .hm_app_safe_error(e)
        if (state$status == "queued") {
          .hm_app_transition(state, "failed")
        }
        refresh()
        shiny::showNotification(
          state$error, type = "error",
          duration = NULL
        )
      }
    )
  }
  admit <- function(data) {
    .hm_app_import(state, data)
    obs <- data@observations
    shiny::updateSelectInput(
      session,
      "outcome", choices = names(obs),
      selected = if (
                     "response" %in% names(
                                           obs)) {
        "response"
      } else {
        names(obs)[1]
      }
    )
    shiny::updateSelectInput(
      session,
      "groups", choices = names(obs),
      selected = if (
                     "subject_id" %in% names(
                                             obs)) {
        "subject_id"
      } else {
        names(obs)[1]
      }
    )
    shiny::updateCheckboxInput(
                               session,
                               "qc",
                               value = FALSE)
  }
  shiny::observeEvent(
    input$type, {
      catalog <- .hm_engine_catalog()
      choices <- names(
        Filter(
          function(tasks) {
            input$type %in% tasks
          }, catalog
        )
      )
      shiny::updateSelectInput(
        session,
        "model", choices = stats::setNames(
          choices, vapply(
            choices,
            function(
              id) {
              paste(
                    id, "\u00b7",
                    .hm_app_engine_status(
                                          id))
            },
            character(1)
          )
        ),
        selected = choices[1]
      )
    }
  )
  shiny::observeEvent(
                      input$example,
                      safe(
                           admit(
                                 hm_example())))
  shiny::observeEvent(
    input$import, safe(
      {
        shiny::req(input$source)
        if (
            input$source$size > 50 * 1024^2) {
          .hm_abort(
                    "Source exceeds the 50 MB import bound.")
        }
        admit(readRDS(input$source$datapath))
      }
    )
  )
  shiny::observeEvent(
    list(
      input$type,
      input$outcome, input$groups,
      input$reference,
      input$model, input$scale,
      input$seed, input$qc, input$parameters,
      input$backend_route,
      input$backend_receipt,
      input$memory,
      input$timeout, input$artifact_root
    ),
    {
      if (!is.null(state$workflow)) {
        state$revision <- state$revision +
          1L
        state$workflow <- NULL
        if (
            !state$status %in% c(
                                 "queued",
                                 "running")) {
          .hm_app_transition(
                             state,
                             "configuring")
        }
        refresh()
      }
    }, ignoreInit = TRUE
  )
  shiny::observeEvent(
    input$pool, safe(
      {
        shiny::req(state$dataset)
        admit(.hm_app_pool(state$dataset))
      }
    )
  )
  shiny::observeEvent(
    input$configure, safe(
      {
        if (!isTRUE(input$qc)) {
          .hm_abort(
                    "Review quality and geometry first.")
        }
        shiny::req(state$dataset)
        if (state$dataset@data_kind !=
              "spectra") {
          .hm_abort(
            paste0(
              "Spatial data require an explicit hm_pool or pixel ",
              "extraction recipe before modelling; import the ",
              "resulting labelled spectra."
            )
          )
        }
        task <- hm_task(
          input$outcome,
          input$type, "spectrum",
          input$groups,
          reference = list(
                           limitations = input$reference)
        )
        parameters <- .hm_app_parameters(
          if (is.null(input$parameters))
            "{}" else input$parameters
        )
        dependency <- .hm_app_engine_dependency(
                                                input$model)
        if (!is.null(dependency))
          .hm_require(dependency)
        model <- hm_model_spec(
                               input$model,
                               task,
                               parameters)
        deep <- startsWith(
                           input$model,
                           "reference:")
        backend <- NULL
        if (deep) {
          if (
              !identical(
                         input$backend_route,
                         "reference"))
            .hm_abort(
              "Select the admitted CPU reference runtime."
            )
          backend <- if (
                         inherits(
                                  runtime(
                                  ), "hm_backend")) {
            runtime()
          } else {
            if (
                is.null(input$backend_receipt))
              .hm_abort(
                paste0(
                  "Install an admitted runtime or upload its trusted ",
                  "installed receipt first."
                )
              )
            readRDS(
                    input$backend_receipt$datapath)
          }
          .hm_app_validate_backend(
                                   backend,
                                   input$model,
                                   input$memory)
        } else if (
                   !is.null(
                            input$backend_route) &&
                     !identical(
                                input$backend_route,
                                "r")) {
          .hm_abort(
            "Select the R engine runtime for this model."
          )
        }
        state$resources <- hm_resources(
          memory_mb = if (
                          is.null(
                                  input$memory))
            1024 else input$memory,
          timeout = if (
                        is.null(
                                input$timeout))
            60 else input$timeout
        )
        state$backend <- backend
        state$artifact_root <- input$artifact_root
        workflow <- hm_workflow(
          task, hm_recipe(
            if (isTRUE(input$scale)) {
              list(list(type = "scale"))
            } else {
              list()
            }
          ),
          model
        )
        split <- hm_split(
          state$dataset, task, "grouped",
          seed = input$seed, folds = 3L
        )$folds[[1L]]
        .hm_validate_fit_split(
          .hm_model_data(state$dataset),
          task, split
        )
        .hm_app_configure(state, workflow)
        state$split <- split
        state$seed <- input$seed
      }
    )
  )
  shiny::observeEvent(
    input$run, safe(
      {
        if (is.null(state$workflow) ||
          !state$status %in% c(
            "ready", "completed", "failed",
            "cancelled"
          )) {
          .hm_abort(
            "Validate a complete configuration before running."
          )
        }
        state$job_revision <- state$revision
        .hm_app_transition(state, "queued")
        state$job <- hm_submit_job(
          state$workflow, state$dataset,
          state$split, list(
            seed = state$seed,
            backend = state$backend,
            artifact_root = state$artifact_root
          ),
          resources = state$resources
        )
        .hm_app_transition(state, "running")
      }
    )
  )
  shiny::observeEvent(
    input$cancel, safe(
      {
        if (!is.null(state$job)) {
          hm_cancel_job(state$job)
          if (
              state$status %in% c(
                                  "queued",
                                  "running")) {
            .hm_app_transition(
                               state,
                               "cancelled")
          }
        }
      }
    )
  )
  shiny::observe(
    {
      shiny::invalidateLater(300, session)
      if (!is.null(state$job) &&
            state$status == "running") {
        status <- hm_job_status(state$job)
        if (status$state != "running") {
          .hm_app_transition(
                             state,
                             status$state)
          state$error <- status$error
          if (status$state == "completed") {
            state$result <- status$result
            state$result_revision <- state$job_revision
            retained[[length(retained) +
                        1L]] <<- status$result
          }
          refresh()
        }
      }
      if (!is.null(runtime_job) &&
            !runtime_job$is_alive()) {
        value <- tryCatch(
          runtime_job$get_result(
          ), error = function(
            e) {
            list(
                 error = .hm_app_safe_error(
                                            e))
          }
        )
        runtime(value)
        runtime_job <<- NULL
      }
    }
  )
  shiny::observeEvent(
    input$resolve, safe(
      {
        if (!is.null(runtime_job)) {
          .hm_abort(
                    "Wait for the current runtime operation.")
        }
        if (
            !input$model %in% names(
                                    .hm_deep_catalog(
                                    ))) {
          .hm_abort(
            paste0(
              "Select a supported spectral CPU reference model before ",
              "resolving metadata."
            )
          )
        }
        runtime(
          list(
            status = "Resolving official metadata; no installation yet."
          )
        )
        runtime_job <<- callr::r_bg(
          function(python, model) {
            hypermodR::hm_resolve_backend(
              "reference_cpu", model,
              python
            )
          }, args = list(
                         python = input$python,
                         model = input$model),
          supervise = TRUE,
          user_profile = FALSE,
          system_profile = FALSE, env = c(
            OMP_NUM_THREADS = "1",
            OPENBLAS_NUM_THREADS = "1",
            MKL_NUM_THREADS = "1"
          )
        )
      }
    )
  )
  shiny::observeEvent(
    input$install, safe(
      {
        resolution <- runtime()
        if (
            !inherits(
                      resolution,
                      "hm_backend_resolution") ||
              !isTRUE(input$terms)) {
          .hm_abort(
            paste0(
              "Resolve metadata and explicitly review every artifact ",
              "notice first."
            )
          )
        }
        if (!is.null(runtime_job)) {
          .hm_abort(
                    "Wait for the current runtime operation.")
        }
        terms <- lapply(
          resolution$profile$terms,
          function(x) {
            x$reviewed <- TRUE
            x
          }
        )
        plan <- hm_admit_backend(
                                 resolution,
                                 terms)
        runtime_job <<- callr::r_bg(
          function(plan) {
            hypermodR::hm_install_backend(
                                          plan,
                                          accept_terms = TRUE)
          }, args = list(plan = plan),
          supervise = TRUE,
          user_profile = FALSE,
          system_profile = FALSE, env = c(
            OMP_NUM_THREADS = "1",
            OPENBLAS_NUM_THREADS = "1",
            MKL_NUM_THREADS = "1"
          )
        )
        runtime(
          list(
            status = "Installing admitted CPU runtime; qualification pending."
          )
        )
      }
    )
  )
  shiny::observeEvent(
    input$compare, safe(
      {
        if (length(retained) <
              2L) {
          .hm_abort(
                    "Complete two runs on common cases first.")
        }
        pair <- tail(retained, 2L)
        comparison(
          capture.output(
            print(
              hm_compare(
                lapply(
                       pair, `[[`, "evaluation"),
                paired = TRUE,
                unit = input$groups,
                draws = 199L, seed = 42L,
                family = "development"
              )
            )
          )
        )
      }
    )
  )
  output$status <- shiny::renderText(
    {
      tick()
      paste(
        "State:",
        state$status,
        if (!is.null(state$result) &&
              !.hm_app_current(state)) {
          "\u00b7 retained result is stale"
        }, state$error
      )
    }
  )
  output$summary <- shiny::renderPrint(
    {
      tick()
      if (is.null(state$dataset)) {
        "No data imported."
      } else {
        list(
          kind = state$dataset@data_kind,
          observations = nrow(
                              state$dataset@observations),
          bands = nrow(state$dataset@bands),
          synthetic = isTRUE(
                             state$dataset@provenance$synthetic)
        )
      }
    }
  )
  output$annotation <- shiny::renderPrint(
    {
      tick()
      shiny::req(state$dataset)
      p <- state$dataset@provenance
      list(
        annotation_owner = "hyperannotatR",
        annotation_revision = p$annotation_revision,
        annotation_manifest_identity = if (
                                           !is.null(
                                                    p$annotation_manifest)) {
          .hm_hash(p$annotation_manifest)
        } else {
          "No admitted companion annotation manifest; no ROI inferred"
        }, adapter = p$adapter,
        profile = p$profile_id
      )
    }
  )
  output$geometry <- shiny::renderPrint(
    {
      tick()
      shiny::req(state$dataset)
      list(
        geometry = state$dataset@geometry,
        units = state$dataset@bands$unit,
        calibration = state$dataset@provenance$calibration
      )
    }
  )
  output$frames <- shiny::renderTable(
    {
      tick()
      if (
          methods::is(
                      state$dataset,
                      "HMSequence")) {
        head(state$dataset@frame_index, 30L)
      }
    }
  )
  output$spectra <- shiny::renderPlot(
    {
      tick()
      shiny::req(state$dataset)
      if (
          state$dataset@data_kind == "spectra") {
        d <- hm_spectra(
          state$dataset, list(
            observation_id = head(
              state$dataset@observations$observation_id,
              12L
            ),
            bands = seq_len(
              min(
                nrow(state$dataset@bands),
                128L
              )
            )
          )
        )
        graphics::matplot(
          d$band_map$wavelength, t(d$values),
          type = "l",
          lty = 1,
          xlab = "Wavelength (nm)",
          ylab = "Source intensity"
        )
      } else {
        x <- state$dataset
        selection <- list(
          acquisition_id = x@observations$acquisition_id[1],
          rows = seq_len(
            min(
              .hm_source(x@sources[[1]])[1],
              64L
            )
          ),
          cols = seq_len(
            min(
              .hm_source(x@sources[[1]])[2],
              64L
            )
          ),
          bands = 1L
        )
        if (methods::is(x, "HMSequence")) {
          selection$frame_id <- x@frame_index$frame_id[1]
        }
        block <- hm_read_block(x, selection)
        graphics::image(
          t(block$values[, , 1, 1]),
          xlab = "Source x (first 64)",
          ylab = "Source y (first 64)",
          main = "First-band preview \u00b7 first frame"
        )
      }
    }, alt = paste0(
      "Bounded source preview: first twelve spectra or the ",
      "first band of the first spatial frame."
    )
  )
  output$configuration <- shiny::renderPrint(
    {
      tick()
      if (is.null(state$workflow)) {
        "Configuration incomplete."
      } else {
        list(
          task = state$workflow$task,
          model = state$workflow$model$model_id,
          training = length(
                            state$split$train),
          heldout = length(state$split$test),
          seed = state$seed
        )
      }
    }
  )
  output$metrics <- shiny::renderTable(
    {
      tick()
      if (!is.null(state$result)) {
        data.frame(
          metric = names(
                         state$result$evaluation$metrics),
          value = unname(
                         state$result$evaluation$metrics)
        )
      }
    }
  )
  output$catalogue <- shiny::renderTable(
    {
      records <- hm_models(task = input$type)
      do.call(
        rbind, lapply(
          records, function(record) {
            id <- record$model_id
            data.frame(
              model = id,
              status = if (id %in%
                names(
                      .hm_engine_catalog(
                      ))) {
                .hm_app_engine_status(id)
              } else {
                paste0(
                  "Blocked here: separate native/tensor/source admission ",
                  "required"
                )
              }, qualified = FALSE
            )
          }
        )
      )
    }
  )
  output$result_plot <- shiny::renderPlot(
    {
      tick()
      shiny::req(state$result)
      evaluation <- state$result$evaluation
      prediction <- evaluation$predictions
      observed <- evaluation$reference[[prediction$task$outcome]]
      if (
          prediction$task$type == "regression") {
        graphics::plot(
          observed, prediction$values,
          xlab = "Observed held-out reference",
          ylab = "Predicted held-out value",
          main = "Held-out development predictions",
          pch = 19
        )
        graphics::abline(0, 1, lty = 2)
      } else {
        graphics::mosaicplot(
          table(
            Observed = observed,
            Predicted = prediction$values
          ),
          main = "Held-out development classification",
          color = TRUE
        )
      }
    }, alt = paste0(
      "Held-out observed versus predicted values from the ",
      "same evaluation used for the metric table; ",
      "classification uses a confusion mosaic."
    )
  )
  output$comparison <- shiny::renderText(
                                         comparison(
                                         ))
  output$runtime <- shiny::renderPrint(
    {
      r <- runtime()
      if (
          inherits(
                   r, "hm_backend_resolution")) {
        list(
          backend = r$backend,
          packages = r$packages,
          terms = r$profile$terms,
          qualification = r$qualification
        )
      } else {
        r
      }
    }
  )
  output$export_action <- shiny::renderUI(
    {
      tick()
      if (.hm_app_current(state)) {
        shiny::downloadButton(
                              "download",
                              "Download verified run bundle")
      } else {
        shiny::p(
          "Complete the current configuration before exporting a run."
        )
      }
    }
  )
  output$download <- shiny::downloadHandler(
    filename = function(
    ) {
      "hypermodR-run.zip"
    },
    content = function(file) {
      .hm_require("zip")
      if (!.hm_app_current(state)) {
        .hm_abort(
          "Only the current completed result can be exported."
        )
      }
      directory <- tempfile("export-")
      on.exit(
        unlink(directory, recursive = TRUE),
        add = TRUE
      )
      hm_export_result(
        state$result,
        directory,
        include_private = isTRUE(
                                 input$private)
      )
      zip::zipr(
        file,
        list.files(
                   directory,
                   full.names = TRUE),
        root = directory
      )
    }
  )
  session$onSessionEnded(
    function() {
      if (!is.null(state$job)) {
        hm_cancel_job(state$job)
      }
      if (!is.null(runtime_job) &&
            runtime_job$is_alive()) {
        runtime_job$kill_tree()
      }
    }
  )
}

.hm_app_safe_error <- function(error) {
  message <- conditionMessage(error)
  if (grepl(
    paste0(
      "token|password|secret|credential|https?://|authoriz",
      "ation|api.key"
    ),
    message, ignore.case = TRUE
  )) {
    return(
      paste0(
        "Operation failed. Inspect the private local runtime ",
        "evidence; sensitive details are withheld."
      )
    )
  }
  substr(message, 1L, 500L)
}
