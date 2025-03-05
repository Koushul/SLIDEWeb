library(shiny)
library(bslib)
library(shinyjs)
library(yaml)
library(SLIDE)
library(DT)
library(plotly)
library(rmarkdown)

ui <- page_navbar(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  useShinyjs(),
  title = tags$a(
    "SLIDE Analysis",
    href = "https://www.nature.com/articles/s41592-024-02175-z#Abs1",
    target = "_blank",
    style = "color: inherit; text-decoration: none; hover: {text-decoration: underline;}"
  ),
  header = tags$head(
    tags$style(HTML("
      .toggle-switch {
        position: relative;
        display: inline-block;
        width: 46px;
        height: 24px;
      }
      .toggle-switch input {
        opacity: 0;
        width: 0;
        height: 0;
      }
      .toggle-slider {
        position: absolute;
        cursor: pointer;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background-color: #4CAF50;
        transition: .3s;
        border-radius: 24px;
        box-shadow: inset 0 1px 2px rgba(0,0,0,0.1);
      }
      .toggle-slider:before {
        position: absolute;
        content: '';
        height: 18px;
        width: 18px;
        left: 3px;
        bottom: 3px;
        background-color: white;
        transition: .3s;
        border-radius: 50%;
        box-shadow: 0 1px 2px rgba(0,0,0,0.15);
      }
      input:checked + .toggle-slider {
        background-color: #FA8072;
      }
      input:checked + .toggle-slider:before {
        transform: translateX(22px);
      }
      .toggle-label {
        margin: 0 8px;
        font-weight: 400;
        color: #555;
        font-size: 0.9em;
      }
      .toggle-container {
        display: flex;
        align-items: center;
        justify-content: center;
        margin-bottom: 12px;
        padding: 8px 0;
      }
      
      /* Button hover animations */
      .btn {
        transition: all 0.3s ease !important;
      }
      .btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      
      /* Save Configuration button */
      .btn-save-config {
        background-color: #607D8B !important;
        border: none !important;
        color: white !important;
      }
      .btn-save-config:hover {
        background-color: #546E7A !important;
      }
      
      /* Run SLIDE button */
      .btn-success:hover {
        filter: brightness(0.85);
      }
      
      /* Run SLIDEcv button */
      .btn-slidecv {
        background-color: #FA8072 !important;
        border: none !important;
        color: white !important;
        opacity: 0.5;
        cursor: not-allowed;
      }
      .btn-slidecv.enabled {
        opacity: 1;
        cursor: pointer;
      }
      .btn-slidecv.enabled:hover {
        filter: brightness(0.85);
      }
      
      /* Load Results button */
      .btn-info:hover {
        filter: brightness(0.85);
      }
    "))
  ),
  nav_panel("", icon = icon("home"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        div(class = "toggle-container",
          span(class = "toggle-label", "Path"),
          tags$label(class = "toggle-switch",
            tags$input(type = "checkbox", id = "input_type"),
            tags$span(class = "toggle-slider")
          ),
          span(class = "toggle-label", "Upload")
        ),
        conditionalPanel(
          condition = "input.input_type === true",
          fileInput("x_file", "Select X data file (CSV)", accept = ".csv")
        ),
        conditionalPanel(
          condition = "input.input_type === false",
          textInput("x_path", "X data file path (CSV)", value = "")
        ),
        uiOutput("x_shape_info"),
        conditionalPanel(
          condition = "input.input_type === true",
          fileInput("y_file", "Select Y data file (CSV)", accept = ".csv")
        ),
        conditionalPanel(
          condition = "input.input_type === false",
          textInput("y_path", "Y data file path (CSV)", value = "")
        ),
        textInput("out_path", "Output Directory", value = "slide_results"),
        hr(),
        accordion(
          open=FALSE,
          accordion_panel(
            "Parameters",
            open = FALSE,
            multiple = FALSE,
            div(
              "Delta values (comma-separated)",
              tags$small(class = "text-muted d-block mb-2", "Controls the number of latent factors. Higher values = fewer factors. Default: 0.01,0.1"),
              textInput("delta", NULL, value = "0.01")
            ),
            div(
              "Lambda values (comma-separated)",
              tags$small(class = "text-muted d-block mb-2", "Controls latent factor sparsity. Higher values = fewer features per factor. Default: 0.5,1"),
              textInput("lambda", NULL, value = "0.5")
            ),
            div(
              "Spec",
              tags$small(class = "text-muted d-block mb-2", "Controls number of significant latent factors. Aim for 5-12 LFs. Default: 0.1"),
              numericInput("spec", NULL, value = 0.1, min = 0, max = 1, step = 0.01)
            ),
            div(
              "Y Factor",
              tags$small(class = "text-muted d-block mb-2", "Set TRUE if Y is binary, FALSE otherwise"),
              checkboxInput("y_factor", NULL, value = TRUE)
            ),
            div(
              "Y Levels (comma-separated)",
              tags$small(class = "text-muted d-block mb-2", "For binary Y only: specify level order (e.g. 0,1). Leave empty for continuous/ordinal Y"),
              textInput("y_levels", NULL, value = "0,1")
            ),
            div(
              "Evaluation Type",
              tags$small(class = "text-muted d-block mb-2", "Use 'correlation' for continuous Y, 'auc' for binary Y"),
              selectInput("eval_type", NULL, choices = c("auc", "correlation"))
            ),
            div(
              "SLIDE Iterations",
              tags$small(class = "text-muted d-block mb-2", "Higher iterations = more stable performance. Default: 1000"),
              numericInput("SLIDE_iter", NULL, value = 1000, min = 100)
            ),
            div(
              "Top Features",
              tags$small(class = "text-muted d-block mb-2", "Number of top features to plot from each latent factor"),
              numericInput("SLIDE_top_feats", NULL, value = 10, min = 1)
            ),
            div(
              "Sample CV Iterations",
              tags$small(class = "text-muted d-block mb-2", "Number of iterations for cross-validation approximation. Default: 500"),
              numericInput("sampleCV_iter", NULL, value = 500, min = 100)
            ),
            div(
              "Sample CV K",
              tags$small(class = "text-muted d-block mb-2", "Number of folds for cross-validation. Default: 4"),
              numericInput("sampleCV_K", NULL, value = 4, min = 2)
            ),
            div(
              "Do Interactions",
              tags$small(class = "text-muted d-block mb-2", "Set FALSE to disable interacting latent factors"),
              checkboxInput("do_interacts", NULL, value = TRUE)
            ),
            div(
              class = "d-flex justify-content-end mt-3",
              downloadButton("download_yaml", "Download YAML", class = "btn-default")
            )
          )
        ),
        hr(),
        actionButton("save_yaml", "Save Configuration", 
          class = "btn-primary btn-save-config w-100"
        ),
        div(
          style = "margin-top: 10px;",
          actionButton("run_slide", "Run SLIDE", class = "btn-success w-100")
        ),
        div(
          style = "margin-top: 10px;",
          actionButton("run_slidecv", "Run SLIDEcv", 
            class = "btn-slidecv w-100"
          )
        ),
        tags$div(id = "status_message", class = "mt-2 text-muted small"),
        fileInput("config_upload", "Or Upload YAML Config", accept = ".yaml")
      ),
      div(
        class = "container-fluid p-3",
        style = "height: calc(100vh - 56px); overflow-y: auto;",
        card(
          card_header("Load Results"),
          textInput("results_path", "Results Directory Path", value = ""),
          helpText("Enter the path to an existing SLIDE results directory"),
          actionButton("load_results", "Load Results", class = "btn-info")
        ),
        uiOutput("results_cards"),
        card(
          full_screen = TRUE,
          card_header("SLIDE Pipeline Overview"),
          card_body(
            p("The SLIDE pipeline consists of two main steps:"),
            tags$ol(
              tags$li("optimizeSLIDE: Calculate and select latent factors (LFs) for multiple input parameter combinations."),
              tags$li("Review the output and choose optimal parameters (delta, lambda and f_size)."),
              tags$li("SLIDEcv: Perform rigorous k-fold cross-validation with chosen parameters.")
            ),
            h4("Input Data Requirements:"),
            tags$ul(
              tags$li("Two CSV files are required:"),
              tags$li(HTML("<strong>X file:</strong> Data matrix in sample by feature format (e.g., cell by gene for single cell transcriptomics, or region by protein for spatial proteomics)")),
              tags$li(HTML("<strong>Y file:</strong> Response vector containing outcomes (e.g., disease severity, spatial regions, clonal expansion)")),
              tags$li("Both files must have row names and column names"),
              tags$li("For ordinal responses (more than two classes), ensure there are ordinal relationships between y values")
            )
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Analysis Steps"),
          card_body(
            accordion(
              open=FALSE,
              accordion_panel(
                "Step 1: Parameter Tuning",
                open = FALSE,
                p("Run optimizeSLIDE, which will:"),
                tags$ul(
                  tags$li("Calculate and select latent factors (LFs) for multiple input parameter combinations"),
                  tags$li("Generate a summary table showing performance for each parameter combination"),
                  tags$li("Create visualizations and correlation networks for the latent factors")
                )
              ),
              accordion_panel(
                "Step 2: Understanding Outputs",
                open = FALSE,
                p("After optimizeSLIDE completes, analyze the results in this order:"),
                tags$ol(
                  tags$li(
                    strong("Evaluate Latent Factor Performance (ControlPerformancePlot.png)"),
                    tags$ul(
                      tags$li("Red line: True performance of significant latent factors"),
                      tags$li("Blue density: Performance of knockoff marginal latent factors"),
                      tags$li("Green density: Performance of true marginals with knockoff interactions")
                    )
                  ),
                  tags$li(
                    strong("Examine Top Features (plotSigGenes_marginals.png)"),
                    tags$ul(
                      tags$li("Shows top features in marginal significant latent factors"),
                      tags$li("Top 10: Features by latent factor loading"),
                      tags$li("Bottom 10: Features by univariate correlation/AUC with response")
                    )
                  )
                )
              ),
              accordion_panel(
                "Step 3: Run Cross-validation",
                open = FALSE,
                p("With your chosen parameters, run rigorous k-fold cross-validation:"),
                tags$div(
                  class = "well",
                  tags$pre(
                    tags$code(
                      "SLIDE::SLIDEcv(yaml_path='path/to/yaml_params.yaml', nrep=20, k=10)"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  config <- reactiveVal(NULL)
  results <- reactiveValues(
    summary_table = NULL,
    current_output_path = NULL
  )
  
  # Add reactive value to track analysis state
  analysis_running <- reactiveVal(FALSE)
  
  # Add reactive value to track if analysis should be canceled
  should_cancel <- reactiveVal(FALSE)
  
  # Add reactive value to store the current R process
  current_process <- reactiveVal(NULL)
  
  # Observer to update button appearance based on state
  observe({
    if (analysis_running()) {
      updateActionButton(session, "run_slide",
                        label = "Cancel")
    } else {
      updateActionButton(session, "run_slide",
                        label = "Run SLIDE")
    }
  })
  
  # Function to clean up after analysis stops
  cleanup_analysis <- function() {
    analysis_running(FALSE)
    should_cancel(FALSE)
    current_process(NULL)
  }
  
  output$x_shape_info <- renderUI({
    input_type <- if (isTRUE(input$input_type)) "upload" else "path"
    if (input_type == "upload") {
      req(input$x_file)
      file_path <- input$x_file$datapath
    } else {
      req(input$x_path)
      if (!file.exists(input$x_path)) {
        return(tags$small(
          class = "text-danger d-block mb-2",
          "File not found at specified path"
        ))
      }
      file_path <- input$x_path
    }
    
    tryCatch({
      x <- as.matrix(utils::read.csv(file_path, row.names = 1, check.names = F))
      tags$small(
        class = "text-muted d-block mb-2",
        sprintf("Shape: %d samples × %d features", nrow(x), ncol(x))
      )
    }, error = function(e) {
      tags$small(
        class = "text-danger d-block mb-2",
        "Error reading file. Please ensure it has row names and column names."
      )
    })
  })
  
  create_yaml_config <- reactive({
    input_type <- if (isTRUE(input$input_type)) "upload" else "path"
    
    # Get X file path
    if (input_type == "upload") {
      req(input$x_file)
      x_path <- normalizePath(input$x_file$datapath, winslash = "/", mustWork = TRUE)
    } else {
      req(input$x_path)
      if (!file.exists(input$x_path)) {
        showNotification("X data file not found at specified path", type = "error")
        return(NULL)
      }
      x_path <- normalizePath(input$x_path, winslash = "/", mustWork = TRUE)
    }
    
    # Get Y file path
    if (input_type == "upload") {
      req(input$y_file)
      y_path <- normalizePath(input$y_file$datapath, winslash = "/", mustWork = TRUE)
    } else {
      req(input$y_path)
      if (!file.exists(input$y_path)) {
        showNotification("Y data file not found at specified path", type = "error")
        return(NULL)
      }
      y_path <- normalizePath(input$y_path, winslash = "/", mustWork = TRUE)
    }
    
    config_list <- list(
      x_path = x_path,
      y_path = y_path,
      out_path = input$out_path,
      delta = as.numeric(strsplit(input$delta, ",")[[1]]),
      lambda = as.numeric(strsplit(input$lambda, ",")[[1]]),
      spec = input$spec,
      y_factor = input$y_factor,
      y_levels = as.numeric(strsplit(input$y_levels, ",")[[1]]),
      eval_type = input$eval_type,
      SLIDE_iter = input$SLIDE_iter,
      SLIDE_top_feats = input$SLIDE_top_feats,
      sampleCV_iter = input$sampleCV_iter,
      sampleCV_K = input$sampleCV_K,
      do_interacts = input$do_interacts
    )
    
    return(config_list)
  })
  
  observeEvent(input$config_upload, {
    req(input$config_upload)
    
    tryCatch({
      uploaded_config <- yaml::read_yaml(input$config_upload$datapath)
      
      if (!file.exists(uploaded_config$x_path)) {
        stop("X data file not found: ", uploaded_config$x_path)
      }
      if (!file.exists(uploaded_config$y_path)) {
        stop("Y data file not found: ", uploaded_config$y_path)
      }
      
      uploaded_config$x_path <- normalizePath(uploaded_config$x_path, winslash = "/", mustWork = TRUE)
      uploaded_config$y_path <- normalizePath(uploaded_config$y_path, winslash = "/", mustWork = TRUE)
      
      if (!dir.exists(uploaded_config$out_path)) {
        dir.create(uploaded_config$out_path, recursive = TRUE, showWarnings = FALSE)
      }
      uploaded_config$out_path <- normalizePath(uploaded_config$out_path, winslash = "/", mustWork = TRUE)
      
      config(uploaded_config)
      showNotification(sprintf("Loaded configuration from: %s", input$config_upload$name), type = "message")
    }, error = function(e) {
      showNotification(sprintf("Error loading configuration: %s", e$message), type = "error")
    })
  })
  
  observeEvent(input$save_yaml, {
    tryCatch({
      yaml_config <- isolate(create_yaml_config())
      yaml_file <- file.path(isolate(input$out_path), "yaml_params.yaml")
      dir.create(dirname(yaml_file), showWarnings = FALSE, recursive = TRUE)
      yaml::write_yaml(yaml_config, yaml_file)
      config(yaml_config)
      showNotification("Configuration saved successfully!", type = "message")
    }, error = function(e) {
      showNotification(sprintf("Error saving configuration: %s", e$message), type = "error")
    })
  })
  
  output$current_config <- renderPrint({
    req(config())
    yaml::as.yaml(config())
  })
  
  output$download_yaml <- downloadHandler(
    filename = function() {
      "yaml_params.yaml"
    },
    content = function(file) {
      tryCatch({
        yaml_config <- isolate(create_yaml_config())
        yaml::write_yaml(yaml_config, file)
      }, error = function(e) {
        showNotification(sprintf("Error downloading configuration: %s", e$message), type = "error")
      })
    }
  )
  
  observeEvent(input$run_slide, {
    if (analysis_running()) {
      # If analysis is running, cancel it
      should_cancel(TRUE)
      
      # Get the current process
      proc <- current_process()
      if (!is.null(proc)) {
        # Try to terminate the process gracefully
        tools::pskill(proc$get_pid(), signal = 15)  # SIGTERM
        Sys.sleep(0.5)  # Give it a moment to terminate
        
        # Force kill if still running
        if (proc$is_alive()) {
          tools::pskill(proc$get_pid(), signal = 9)  # SIGKILL
        }
      }
      
      html("status_message", "Analysis canceled.")
      cleanup_analysis()
      
    } else {
      # If analysis is not running, start new analysis
      should_cancel(FALSE)
      analysis_running(TRUE)
      req(config())
      
      shinyjs::addClass(selector = "#status_message", class = "status-running")
      html("status_message", "Analysis in progress...")
      
      withProgress(message = 'Running SLIDE analysis...', value = 0, {
        tryCatch({
          # Create a custom optimizeSLIDE function with progress updates
          optimizeSLIDE_with_progress <- function(input_params, sink_file = F) {
            # Initial setup progress
            incProgress(0.1, detail = "Setting up parameters...")
            
            if (dir.exists(input_params$out_path)){
              updateProgressText("Populating outputs to existing directory...")
            } else{
              updateProgressText("Creating output directory...")
              dir.create(file.path(input_params$out_path), showWarnings = F, recursive = T)
            }

            # Check for early cancellation
            if (should_cancel()) {
              cleanup_analysis()
              return(NULL)
            }

            # Load and process data
            incProgress(0.2, detail = "Loading data matrices...")
            x <- as.matrix(utils::read.csv(input_params$x_path, row.names = 1, check.names = F))
            colnames(x) = stringr::str_replace_all(colnames(x), pattern = " ", replacement = "_")
            y <- as.matrix(utils::read.csv(input_params$y_path, row.names = 1))
            x_std <- scale(x, T, T)

            # Initialize parameters
            delta = if(is.null(input_params$delta)) c(0.01, 0.1) else input_params$delta
            lambda = if(is.null(input_params$lambda)) c(0.5, 1.0) else input_params$lambda
            alpha_level = if(is.null(input_params$alpha)) 0.05 else input_params$alpha
            thresh_fdr = if(is.null(input_params$thresh_fdr)) 0.2 else input_params$thresh_fdr
            spec = if(is.null(input_params$spec)) 0.1 else input_params$spec
            do_interacts = if(is.null(input_params$do_interacts)) TRUE else input_params$do_interacts
            SLIDE_iter = if(is.null(input_params$SLIDE_iter)) 1000 else input_params$SLIDE_iter
            SLIDE_top_feats = if(is.null(input_params$SLIDE_top_feats)) 10 else input_params$SLIDE_top_feats
            sampleCV_iter = if(is.null(input_params$sampleCV_iter)) 500 else input_params$sampleCV_iter
            
            # Initialize summary table
            summary_table <- as.data.frame(matrix(NA, nrow = length(delta) * length(lambda), ncol = 7))
            colnames(summary_table) <- c('delta', 'lambda', 'f_size', 'Num_of_LFs', 'Num_of_Sig_LFs', 'Num_of_Interactors', 'sampleCV_Performance')

            total_iterations <- length(delta) * length(lambda)
            current_iteration <- 0

            for (d in delta) {
              for (l in lambda) {
                # Check for cancellation at each iteration
                if (should_cancel()) {
                  cleanup_analysis()
                  return(NULL)
                }
                
                current_iteration <- current_iteration + 1
                progress_pct <- 0.2 + (0.8 * current_iteration / total_iterations)
                
                incProgress(progress_pct / total_iterations, 
                           detail = sprintf("Processing delta=%.3f, lambda=%.3f (%d/%d)", 
                                          d, l, current_iteration, total_iterations))

                loop_outpath = paste0(input_params$out_path, '/', d, '_', l, '_', 'out/')
                dir.create(file.path(loop_outpath), showWarnings = F, recursive = T)

                # Save run-specific YAML for this iteration
                run_yaml <- input_params
                run_yaml$delta <- d
                run_yaml$lambda <- l
                run_yaml$out_path <- loop_outpath
                yaml::write_yaml(run_yaml, paste0(loop_outpath, "yaml_params.yaml"))

                updateProgressText(sprintf("Getting latent factors for delta=%.3f, lambda=%.3f...", d, l))
                
                if (input_params$y_factor) {
                  y_temp <- SLIDE::toCont(y, input_params$y_levels)
                  saveRDS(y_temp, file = paste0(input_params$out_path, "/binary_y_mapping.rds"))
                  orig_y <- as.matrix(y_temp$cat_y)
                  y <- as.matrix(y_temp$cont_y)
                  row.names(y) <- row.names(y_temp$cat_y)
                }

                all_latent_factors <- SLIDE::getLatentFactors(
                  x = x,
                  x_std = x_std,
                  y = y,
                  sigma = NULL,
                  delta = d,
                  lambda = l,
                  rep_cv = input_params$rep_cv,
                  alpha_level = alpha_level,
                  thresh_fdr = thresh_fdr,
                  out_path = loop_outpath
                )

                saveRDS(all_latent_factors, paste0(loop_outpath, 'AllLatentFactors.rds'))
                
                updateProgressText("Calculating Z matrix...")
                z_matrix <- SLIDE::calcZMatrix(x_std, all_latent_factors, x_path = NULL, 
                                             lf_path = NULL, loop_outpath)

                updateProgressText("Running SLIDE analysis...")
                SLIDE_res <- SLIDE::runSLIDE(y, y_path = NULL, z_path = NULL, z_matrix, 
                                           all_latent_factors, lf_path = NULL, niter = SLIDE_iter, 
                                           spec = spec, do_interacts = do_interacts)
                
                saveRDS(SLIDE_res, paste0(loop_outpath, 'SLIDE_LFs.rds'))

                if(length(SLIDE_res$SLIDE_res$marginal_vars) != 0) {
                  updateProgressText("Processing top features...")
                  SLIDE_res <- SLIDE::getTopFeatures(x, y, all_latent_factors, loop_outpath, 
                                                   SLIDE_res, num_top_feats = SLIDE_top_feats, 
                                                   condition = input_params$eval_type)
                  
                  saveRDS(SLIDE_res, paste0(loop_outpath, 'SLIDE_LFs.rds'))
                  SLIDE::plotSigGenes(SLIDE_res, plot_interaction = do_interacts, out_path = loop_outpath)

                  if(length(SLIDE_res$SLIDE_res$marginal_vars) != 0) {
                    updateProgressText("Calculating control performance...")
                    SLIDE::calcControlPerformance(z_matrix = z_matrix, y, do_interacts, 
                                                SLIDE_res, condition = input_params$eval_type, 
                                                loop_outpath)
                  }

                  updateProgressText("Calculating sample CV performance...")
                  performance = SLIDE::sampleCV(y, z_matrix, SLIDE_res, 
                                             sampleCV_K = input_params$sampleCV_K,
                                             condition = input_params$eval_type, 
                                             sampleCV_iter = sampleCV_iter, 
                                             logistic = FALSE, 
                                             out_path = loop_outpath)

                  if (do_interacts == TRUE) {
                    interactors = c(SLIDE_res$interaction$p1, SLIDE_res$interaction$p2)
                    interactors = interactors[!(interactors %in% SLIDE_res$marginal_vals)]
                    interactors = unique(interactors)
                    loop_summary = c(d, l, SLIDE_res$SLIDE_param['f_size'], 
                                   all_latent_factors$K, length(SLIDE_res$marginal_vals), 
                                   length(interactors), performance)
                  } else {
                    loop_summary = c(d, l, SLIDE_res$SLIDE_param['f_size'], 
                                   all_latent_factors$K, length(SLIDE_res$marginal_vals), 
                                   'NA', performance)
                  }
                } else {
                  loop_summary = c(d, l, SLIDE_res$SLIDE_param['f_size'], 
                                 all_latent_factors$K, "NA", "NA", "NA")
                }
                
                summary_table[current_iteration, ] = loop_summary
                updateProgressText(sprintf("Completed iteration %d/%d", current_iteration, total_iterations))
              }
            }

            write.csv(summary_table, paste0(input_params$out_path, "/summary_table.csv"))
            
            # Store results in reactive values
            results$summary_table <- summary_table
            results$current_output_path <- input_params$out_path
            
            return(summary_table)
          }

          # Add updateProgressText function
          updateProgressText <- function(text) {
            html("status_message", paste0("Analysis in progress...<br>", text))
          }

          # Run the analysis with progress updates
          slide_results <- optimizeSLIDE_with_progress(isolate(config()))
          gc()
          
          cleanup_analysis()
          shinyjs::removeClass(selector = "#status_message", class = "status-running")
          shinyjs::addClass(selector = "#status_message", class = "status-complete")
          
          if (!should_cancel()) {
            html("status_message", "Analysis completed successfully!")
            showNotification("SLIDE analysis completed successfully!", type = "message")
          }
          
        }, error = function(e) {
          cleanup_analysis()
          html("status_message", sprintf("Error in SLIDE analysis: %s", e$message))
          showNotification(sprintf("Error in SLIDE analysis: %s", e$message), type = "error")
        })
      })
    }
  })

  # Modify the summary table output to include row selection
  output$summary_table <- renderDT({
    req(results$summary_table)
    datatable(results$summary_table,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              selection = 'single',
              extensions = 'Buttons',
              rownames = FALSE) %>%
      formatRound(columns = c('delta', 'lambda', 'sampleCV_Performance'), digits = 3)
  })

  # Add observer for selected row and output for folder contents
  output$selected_folder_contents <- renderUI({
    req(results$current_output_path, results$summary_table, input$summary_table_rows_selected)
    
    # Get selected row data
    selected_row <- results$summary_table[input$summary_table_rows_selected, ]
    
    # Create folder name based on delta and lambda values
    folder_name <- paste0(selected_row$delta, "_", selected_row$lambda, "_out/")
    folder_path <- file.path(results$current_output_path, folder_name)
    
    # Check if folder exists
    if (!dir.exists(folder_path)) {
      return(h4(sprintf("No folder found for delta=%.3f, lambda=%.3f", 
                       selected_row$delta, selected_row$lambda)))
    }
    
    # Normalize path after confirming it exists
    folder_path <- normalizePath(folder_path, winslash = "/", mustWork = TRUE)
    
    # Register the directory for static file serving
    resource_name <- paste0("plots_", gsub("[^a-zA-Z0-9]", "_", folder_name))
    shiny::addResourcePath(resource_name, folder_path)
    
    # Define the plots we want to display
    plot_files <- c(
      "ControlPerformancePlot.png",
      "plotInteractions.png",
      "plotSigGenes_marginals.png"
    )
    
    # Check which plots exist
    existing_plots <- plot_files[file.exists(file.path(folder_path, plot_files))]
    
    if (length(existing_plots) == 0) {
      return(h4("No visualization plots found in the selected analysis folder"))
    }
    
    div(
      h4(sprintf("Analysis Results for delta=%.3f, lambda=%.3f", 
                 selected_row$delta, selected_row$lambda),
         class = "mb-3"),
      
      # Display plots
      if (length(existing_plots) > 0) {
        div(
          class = "d-flex flex-row justify-content-between align-items-start gap-3",
          style = "width: 100%;",
          lapply(existing_plots, function(plot_file) {
            image_url <- paste0("/", resource_name, "/", plot_file)
            div(
              class = "text-center",
              style = "flex: 1; min-width: 0;",
              h5(tools::file_path_sans_ext(plot_file), class = "mb-2"),
              tags$img(
                src = image_url,
                style = "width: 100%; max-height: 400px; object-fit: contain;",
                class = "border rounded"
              ),
              onclick = sprintf("window.open('%s', '_blank')", image_url)
            )
          })
        )
      }
    )
  })

  # Update the SLIDEcv observer to use the correct YAML path
  observeEvent(input$run_slidecv, {
    req(results$current_output_path, results$summary_table, input$summary_table_rows_selected)
    
    # Get selected row data and folder path
    selected_row <- results$summary_table[input$summary_table_rows_selected, ]
    
    # Create folder name based on delta and lambda values
    folder_name <- paste0(selected_row$delta, "_", selected_row$lambda, "_out")
    yaml_file <- file.path(results$current_output_path, folder_name, "yaml_params.yaml")
    
    if (!file.exists(yaml_file)) {
      showNotification("YAML configuration file not found in results directory", type = "error")
      return()
    }
    
    # Run SLIDEcv in a separate R process
    withProgress(message = 'Running SLIDEcv...', value = 0, {
      tryCatch({
        # Create R script for running SLIDEcv
        script_content <- sprintf('
          library(SLIDE)
          result <- SLIDE::SLIDEcv(yaml_path = "%s", nrep = 20, k = 5)
          saveRDS(result, file = "%s")
        ', yaml_file, file.path(results$current_output_path, "slidecv_results.rds"))
        
        temp_script <- tempfile(fileext = ".R")
        writeLines(script_content, temp_script)
        
        # Run the script in a separate R process
        system2("Rscript", temp_script, wait = TRUE)
        
        # Check if results file was created
        results_file <- file.path(results$current_output_path, "slidecv_results.rds")
        if (file.exists(results_file)) {
          showNotification("SLIDEcv analysis completed successfully!", type = "message")
          
          # Display results
          cv_results <- readRDS(results_file)
          showModal(modalDialog(
            title = "SLIDEcv Results",
            renderPrint({
              cat("Cross-validation Results:\n")
              print(cv_results)
            }),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        } else {
          showNotification("SLIDEcv analysis failed to generate results", type = "error")
        }
      }, error = function(e) {
        showNotification(sprintf("Error in SLIDEcv analysis: %s", e$message), type = "error")
      })
    })
  })

  # Add the load results observer
  observeEvent(input$load_results, {
    req(input$results_path)
    
    tryCatch({
      # Validate directory exists
      if (!dir.exists(input$results_path)) {
        stop("Directory does not exist")
      }
      
      # Check for summary table
      summary_file <- file.path(input$results_path, "summary_table.csv")
      if (!file.exists(summary_file)) {
        stop("No summary_table.csv found in the specified directory")
      }
      
      # Load summary table
      summary_table <- read.csv(summary_file)
      
      # Update reactive values
      results$summary_table <- summary_table
      results$current_output_path <- normalizePath(input$results_path, winslash = "/", mustWork = TRUE)
      
      # Show success message
      showNotification("Results loaded successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(sprintf("Error loading results: %s", e$message), type = "error")
    })
  })

  output$preprocessing_content <- renderUI({
    # Create a temporary HTML file from the Rmd
    temp_html <- tempfile(fileext = ".html")
    
    # Get the path to the vignette file
    vignette_path <- system.file("vignettes", "Preprocessing-and-Filtering.Rmd", package = "SLIDE")
    if (file.exists(vignette_path)) {
      rmarkdown::render(vignette_path,
                       output_file = temp_html,
                       quiet = TRUE)
      
      # Read the HTML content
      html_content <- readLines(temp_html)
      HTML(paste(html_content, collapse = "\n"))
    } else {
      HTML("<div class='alert alert-warning'>Preprocessing guide not found. Please make sure the SLIDE package is properly installed.</div>")
    }
  })

  output$results_cards <- renderUI({
    if (is.null(results$summary_table)) {
      tagList(
        card(
          class = "opacity-50",
          card_header("Summary Table"),
          div(class = "card-body text-center text-muted",
              "Load results to view summary table")
        ),
        card(
          class = "opacity-50",
          card_header("Plots"),
          div(class = "card-body text-center text-muted",
              "Select a row from the summary table to view analysis files")
        )
      )
    } else {
      tagList(
        card(
          card_header("Summary Table"),
          DTOutput("summary_table")
        ),
        card(
          card_header("Plots"),
          uiOutput("selected_folder_contents")
        )
      )
    }
  })

  # Add observer to enable/disable SLIDEcv button based on row selection
  observe({
    if (!is.null(input$summary_table_rows_selected)) {
      shinyjs::addClass("run_slidecv", "enabled")
    } else {
      shinyjs::removeClass("run_slidecv", "enabled")
    }
  })
}

shinyApp(ui, server)
