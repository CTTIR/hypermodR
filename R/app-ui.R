.hm_app_ui <- function() {
  bslib::page_navbar(
    window_title = "hypermodR",
    title = shiny::tagList(
      .cttir_brand_logo(), "hypermodR \u00b7 Spectral workbench"
    ),
    lang = "en",
    theme = bslib::bs_theme(
      version = 5, bootswatch = "flatly",
      primary = "#185b88",
      secondary = "#52646b",
      base_font = "system-ui",
      `web-font-path` = FALSE
    ),
    fillable = FALSE,
    header = shiny::tagList(
      shiny::tags$head(
        shiny::tags$style(
          paste0(
            ".bslib-page-navbar>.container-fluid{padding-top:.6r",
            "em}.card{margin-bottom:.8rem}.card-body{padding:1re",
            "m}.navbar{min-height:3.2rem}.btn-primary{--bs-btn-c",
            "olor:#fff;--bs-btn-hover-color:#fff;--bs-btn-active",
            "-color:#fff}.navbar .nav-link.active{color:#fff;",
            "border-bottom:2px solid #fff}.btn-default{",
            "--bs-btn-bg:#52646b;--bs-btn-color:#fff;",
            "--bs-btn-hover-bg:#405057;--bs-btn-hover-color:#fff}",
            ".navbar-brand{max-width:calc(100vw - 6rem);",
            "white-space:normal}"
          )
        )
      ),
      bslib::input_dark_mode(id = "dark"),
      shiny::div(
        role = "status",
        `aria-live` = "polite",
        shiny::textOutput("status")
      )
    ),
    bslib::nav_panel(
      "1 \u00b7 Import",
      bslib::layout_columns(
        bslib::card(
          bslib::card_header(
                             "Begin with a validated source"),
          shiny::p(
            paste0(
              "Open an explicitly synthetic grouped example or import ",
              "a trusted HMDataset / HMSequence RDS (maximum 50 MB)."
            )
          ),
          shiny::actionButton(
            "example",
            "Open synthetic example",
            class = "btn-primary"
          ),
          shiny::fileInput(
            "source", "Trusted dataset RDS",
            accept = ".rds"
          ),
          shiny::actionButton(
                              "import",
                              "Import selected source")
        ),
        bslib::card(
          bslib::card_header(
                             "Source summary"),
          shiny::verbatimTextOutput(
                                    "summary")
        )
      )
    ),
    bslib::nav_panel(
      "2 \u00b7 Quality & geometry",
      bslib::card(
        shiny::p(
          paste0(
            "Preview: first 12 spectra and first 128 bands only. No ",
            "calibration, pooling or resampling is applied."
          )
        ),
        shiny::plotOutput("spectra"),
        shiny::verbatimTextOutput(
                                  "geometry"),
        shiny::tableOutput("frames"),
        shiny::checkboxInput(
          "qc",
          "I reviewed units, calibration, finite bands and geometry",
          FALSE
        )
      )
    ),
    bslib::nav_panel(
      "3 \u00b7 Regions & task", bslib::card(
        shiny::p(
          paste0(
            "Model the admitted observation table, or explicitly ",
            "mean-pool each image/frame below. Eligible pixels and ",
            "uniform image references are validated. ROI masks and ",
            "annotation interchange are prepared with the package ",
            "API before import."
          )
        ),
        shiny::verbatimTextOutput(
                                  "annotation"),
        shiny::actionButton(
          "pool",
          "Explicitly mean-pool each complete image/frame"
        ),
        shiny::selectInput(
          "type",
          "Endpoint type",
          c("regression",
            "classification")
        ),
        shiny::selectInput(
          "outcome", "Reference column",
          character()
        ),
        shiny::selectInput(
          "groups",
          "Independent grouping column",
          character()
        ),
        shiny::textInput(
          "reference",
          "Reference limitations / development role",
          "Development only; reference credibility is not established"
        )
      )
    ),
    bslib::nav_panel(
      "4 \u00b7 Model & runtime",
      bslib::card(
        shiny::selectInput(
          "model",
          "Compatible spectral model",
          character()
        ),
        shiny::textAreaInput(
          "parameters",
          "Engine parameters (JSON object)",
          "{}", rows = 3
        ),
        shiny::selectInput(
          "backend_route",
          "Execution runtime",
          c(
            `R engines` = "r",
            `Admitted installed CPU reference` = "reference"
          )
        ),
        shiny::fileInput(
          "backend_receipt",
          "Trusted installed hm_backend receipt RDS",
          accept = ".rds"
        ),
        shiny::numericInput(
          "memory", paste0(
            "Per-process memory limit (MiB; references require at ",
            "least 4096)"
          ),
          1024, min = 256
        ),
        shiny::numericInput(
          "timeout",
          "Elapsed time limit (seconds)",
          60, min = 1
        ),
        shiny::textInput(
          "artifact_root",
          "Persistent learned-artifact run directory",
          file.path(
            tools::R_user_dir(
                              "hypermodR",
                              "cache"),
            "workbench-runs"
          )
        ),
        shiny::tableOutput("catalogue"),
        shiny::p(
          paste0(
            "All candidates are scientifically unqualified. Native ",
            "image and temporal routes require their separate ",
            "tensor APIs; they cannot fit this spectral table."
          )
        ),
        shiny::checkboxInput(
          "scale",
          "Fit centering/scaling on training data only",
          FALSE
        ),
        shiny::numericInput(
          "seed", "Reproducibility seed",
          42, min = 0
        ),
        shiny::p(
          paste0(
            "Native spectral inputs, local CPU, one numerical ",
            "thread. Technical execution does not establish ",
            "clinical usefulness."
          )
        ),
        shiny::actionButton(
          "configure",
          "Validate and reserve configuration"
        ),
        shiny::hr(), shiny::p(
          paste0(
            "Optional runtime setup: explicit metadata resolution, ",
            "terms review, then admitted installation. These ",
            "operations run in a separate process."
          )
        ),
        shiny::textInput(
          "python",
          "Existing Python 3.11 / 3.12 executable",
          "/usr/bin/python3"
        ),
        shiny::actionButton(
          "resolve",
          "Resolve CPU reference metadata (network)"
        ),
        shiny::verbatimTextOutput("runtime"),
        shiny::checkboxInput(
          "terms",
          "I reviewed every resolved artifact license and notice",
          FALSE
        ),
        shiny::actionButton(
          "install",
          "Admit reviewed CPU plan and install"
        )
      )
    ),
    bslib::nav_panel(
      "5 \u00b7 Run", bslib::card(
        shiny::p(
          paste0(
            "Explicit first fold of a three-fold grouped split; no ",
            paste0(
              "tuning. Runtime limits are explicitly declared in ",
              "Model & runtime."
            )
          )
        ),
        shiny::verbatimTextOutput(
                                  "configuration"),
        shiny::actionButton(
          "run", "Run configured workflow",
          class = "btn-primary"
        ),
        shiny::actionButton(
                            "cancel",
                            "Cancel owned job")
      )
    ),
    bslib::nav_panel(
      "6 \u00b7 Compare", bslib::card(
        shiny::p(
          paste0(
            "Held-out development metrics. Retained results remain ",
            "identified after edits; stale results cannot be ",
            "exported as current."
          )
        ),
        shiny::tableOutput("metrics"),
        shiny::plotOutput(
                          "result_plot",
                          height = "300px"),
        shiny::actionButton(
          "compare",
          "Compare two retained runs on common cases"
        ),
        shiny::verbatimTextOutput(
                                  "comparison")
      )
    ),
    bslib::nav_panel(
      "7 \u00b7 Export", bslib::card(
        shiny::p(
          paste0(
            "Configuration, aggregate metrics, identity and ",
            "reproduction code. Supply the original admitted ",
            "dataset to reproduce. Private objects are opt-in."
          )
        ),
        shiny::checkboxInput(
          "private",
          "Include private fitted objects and observation identities",
          FALSE
        ),
        shiny::uiOutput("export_action")
      )
    )
  )
}
