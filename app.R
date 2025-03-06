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
      /* Remove toggle switch styles */
      
      /* Path input field styling */
      .path-input input {
        width: 100%;
        font-family: monospace;
      }
      
      /* Input group styling */
      .input-group {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 8px;
      }
      
      .input-group .form-control {
        flex: 1;
      }
      
      .input-group .btn-upload {
        background-color: #4CAF50;
        color: white;
        border: none;
        padding: 6px 12px;
        border-radius: 4px;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .input-group .btn-upload:hover {
        background-color: #45a049;
        transform: translateY(-2px);
      }
      
      .input-group .btn-upload i {
        margin-right: 4px;
      }
      
      /* Validation icons */
      .validation-icon {
        display: inline-block;
        margin-left: 4px;
        vertical-align: middle;
      }
      .validation-icon.valid {
        color: #28a745;
      }
      .validation-icon.invalid {
        color: #dc3545;
      }
      
      /* Path input container */
      .path-input-container {
        position: relative;
        width: 100%;
      }

      /* Label with validation icon */
      .label-with-validation {
        display: flex;
        align-items: center;
        gap: 4px;
        margin-bottom: 4px;
      }
      
      /* Shape info text */
      .shape-info {
        margin-top: 2px;
        margin-bottom: 8px;
      }
      
      /* Adjust spacing for file inputs */
      .shiny-input-container {
        margin-bottom: 8px;
      }
      
      /* Path input with truncation */
      .path-input-truncate {
        position: relative;
        width: 100%;
      }
      
      .path-input-truncate input {
        width: 100%;
        font-family: monospace;
      }
      
      .path-input-truncate.overflow::after {
        content: attr(data-path);
        visibility: visible;
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        direction: rtl;
        padding: 6px 12px;
        pointer-events: none;
        background: transparent;
      }
      
      .path-input-truncate.overflow input:focus,
      .path-input-truncate.overflow input:hover {
        position: relative;
        z-index: 1;
      }
      
      /* Wrapped path input styling */
      .path-input-wrap input {
        width: 100%;
        text-overflow: ellipsis;
        overflow: hidden;
        white-space: nowrap;
      }
      
      .path-input-wrap input:focus,
      .path-input-wrap input:hover {
        white-space: pre-wrap;
        word-wrap: break-word;
        height: auto;
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

      /* Disabled button styles */
      .btn-disabled {
        opacity: 0.5 !important;
        cursor: not-allowed !important;
        pointer-events: none !important;
      }

      /* Progress bar animation */
      @keyframes progress-bar-stripes {
        from { background-position: 1rem 0; }
        to { background-position: 0 0; }
      }

      .progress-bar {
        background-image: linear-gradient(45deg, 
          rgba(255, 255, 255, 0.15) 25%, 
          transparent 25%, 
          transparent 50%, 
          rgba(255, 255, 255, 0.15) 50%, 
          rgba(255, 255, 255, 0.15) 75%, 
          transparent 75%, 
          transparent);
        background-size: 1rem 1rem;
      }

      .progress.active .progress-bar {
        animation: progress-bar-stripes 1s linear infinite;
      }

      /* Add active class to progress bar container when running */
      .shiny-progress.active .progress {
        height: 0.5rem;
        background-color: #e9ecef;
        border-radius: 0.25rem;
        margin: 0.5rem 0;
      }

      .shiny-progress.active .progress .progress-bar {
        height: 100%;
        border-radius: 0.25rem;
        transition: width 0.6s ease;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        function getFileName(path) {
          if (!path) return '';
          const parts = path.split('/');
          return parts[parts.length - 1] || path;
        }

        // Add functions to manage button states
        window.enableButton = function(buttonId) {
          $('#' + buttonId).removeClass('btn-disabled');
        }

        window.disableButton = function(buttonId) {
          $('#' + buttonId).addClass('btn-disabled');
        }

        // Add functions to manage progress bar animation
        window.startProgressAnimation = function() {
          $('.shiny-progress').addClass('active');
          $('.progress').addClass('active');
        }

        window.stopProgressAnimation = function() {
          $('.shiny-progress').removeClass('active');
          $('.progress').removeClass('active');
        }

        // Observe Shiny progress events
        $(document).on('shiny:progressing', function(event) {
          startProgressAnimation();
        });

        $(document).on('shiny:idle', function(event) {
          stopProgressAnimation();
        });

        window.disableAllRunButtons = function() {
          $('#run_slide, #run_slidecv').addClass('btn-disabled');
        }

        window.enableAllRunButtons = function() {
          $('#run_slide, #run_slidecv').removeClass('btn-disabled');
        }

        function updatePathDisplay() {
          $('.path-input input, .path-input-truncate input').each(function() {
            const $input = $(this);
            const $container = $input.parent();
            const fullPath = $input.val();
            
            if (this.scrollWidth > this.clientWidth) {
              $container.addClass('overflow');
              if (!$input.is(':focus')) {
                const fileName = getFileName(fullPath);
                $input.attr('data-full-path', fullPath);
                $input.val(fileName);
              }
            } else {
              $container.removeClass('overflow');
            }
            
            // Store full path for hover
            $input.attr('title', fullPath);
          });
        }

        // Update on input change
        $(document).on('input', '.path-input input, .path-input-truncate input', function() {
          const $input = $(this);
          $input.attr('data-full-path', $input.val());
          setTimeout(updatePathDisplay, 0);
        });
        
        // Show full path on focus
        $(document).on('focus', '.path-input input, .path-input-truncate input', function() {
          const $input = $(this);
          const fullPath = $input.attr('data-full-path');
          if (fullPath) {
            $input.val(fullPath);
          }
        });

        // Show filename on blur
        $(document).on('blur', '.path-input input, .path-input-truncate input', function() {
          const $input = $(this);
          const fullPath = $input.attr('data-full-path');
          if (fullPath && this.scrollWidth > this.clientWidth) {
            $input.val(getFileName(fullPath));
          }
        });
        
        // Initial check
        setTimeout(updatePathDisplay, 100);
        
        // Update on window resize
        $(window).on('resize', updatePathDisplay);
      });
    "))
  ),
  nav_panel("", icon = icon("home"),
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        div(class = "input-group",
          div(style = "flex: 1;",
            tags$div(class = "path-input-truncate",
              div(class = "path-input-container",
                div(class = "label-with-validation",
                  "X data file path (CSV)",
                  tags$i(id = "x_path_icon", class = "fas validation-icon")
                ),
                textInput("x_path", NULL, value = "")
              )
            )
          ),
          fileInput("x_file", NULL, accept = ".csv", 
            buttonLabel = list(icon("upload"), "Upload"),
            width = "auto"
          )
        ),
        div(class = "shape-info",
          uiOutput("x_shape_info")
        ),
        div(class = "input-group",
          div(style = "flex: 1;",
            tags$div(class = "path-input-truncate",
              div(class = "path-input-container",
                div(class = "label-with-validation",
                  "Y data file path (CSV)",
                  tags$i(id = "y_path_icon", class = "fas validation-icon")
                ),
                textInput("y_path", NULL, value = "")
              )
            )
          ),
          fileInput("y_file", NULL, accept = ".csv", 
            buttonLabel = list(icon("upload"), "Upload"),
            width = "auto"
          )
        ),
        tags$div(class = "path-input-truncate",
          textInput("out_path", "Output Directory", value = "slide_results")
        ),
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
              "Threshold FDR",
              tags$small(class = "text-muted d-block mb-2", "False discovery rate threshold for feature selection. Default: 0.2"),
              numericInput("thresh_fdr", NULL, value = 0.2, min = 0, max = 1, step = 0.01)
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
        div(
          style = "margin-top: 10px;",
          actionButton("run_slide", "Run SLIDE", class = "btn-success w-100 btn-disabled")
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
      # Replace the existing results section with the dynamic one
      uiOutput("results_section")
    )
  )
)

server <- function(input, output, session) {
  # Initialize all reactive values at the start of server
  config <- reactiveVal(NULL)
  results <- reactiveValues(
    summary_table = NULL,
    current_output_path = NULL
  )
  
  # Add reactive values for directory navigation
  current_dir <- reactiveVal(NULL)
  dir_contents <- reactiveVal(NULL)
  
  # Add reactive value to track analysis state
  analysis_running <- reactiveVal(FALSE)
  
  # Add reactive value to track if analysis should be canceled
  should_cancel <- reactiveVal(FALSE)
  
  # Add reactive value to store the current R process
  current_process <- reactiveVal(NULL)
  
  # Add reactive values for file validation
  x_file_valid <- reactiveVal(FALSE)
  y_file_valid <- reactiveVal(FALSE)

  # Initialize JavaScript functions
  js <- list(
    enableButton = function(buttonId) {
      shinyjs::removeClass(buttonId, "btn-disabled")
    },
    disableButton = function(buttonId) {
      shinyjs::addClass(buttonId, "btn-disabled")
    },
    disableAllRunButtons = function() {
      shinyjs::addClass("run_slide", "btn-disabled")
      shinyjs::addClass("run_slidecv", "btn-disabled")
    },
    enableAllRunButtons = function() {
      shinyjs::removeClass("run_slide", "btn-disabled")
      shinyjs::removeClass("run_slidecv", "btn-disabled")
    }
  )
  
  # Observer to update button states based on file validation and analysis state
  observe({
    if (analysis_running()) {
      updateActionButton(session, "run_slide", label = "Cancel")
      js$disableAllRunButtons()
    } else {
      updateActionButton(session, "run_slide", label = "Run SLIDE")
      if (x_file_valid() && y_file_valid()) {
        js$enableButton("run_slide")
      } else {
        js$disableButton("run_slide")
      }
    }
  })
  
  # Function to clean up after analysis stops
  cleanup_analysis <- function() {
    analysis_running(FALSE)
    should_cancel(FALSE)
    current_process(NULL)
    
    # Re-enable buttons based on validation state
    if (x_file_valid() && y_file_valid()) {
      js$enableButton("run_slide")
    } else {
      js$disableButton("run_slide")
    }
    
    # Enable SLIDEcv button if results are available
    if (!is.null(results$summary_table) && !is.null(input$summary_table_rows_selected)) {
      js$enableButton("run_slidecv")
    } else {
      js$disableButton("run_slidecv")
    }
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
      thresh_fdr = input$thresh_fdr,
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
      js$disableAllRunButtons()  # Disable buttons while analysis is running
      
      # First save the configuration
      tryCatch({
        yaml_config <- isolate(create_yaml_config())
        if (is.null(yaml_config)) {
          showNotification("Please check your input parameters", type = "error")
          cleanup_analysis()
          return()
        }
        
        yaml_file <- file.path(isolate(input$out_path), "yaml_params.yaml")
        dir.create(dirname(yaml_file), showWarnings = FALSE, recursive = TRUE)
        yaml::write_yaml(yaml_config, yaml_file)
        config(yaml_config)
        
        shinyjs::addClass(selector = "#status_message", class = "status-running")
        html("status_message", "Analysis in progress...")
        
        withProgress(message = 'Running SLIDE analysis...', value = 0, {
          tryCatch({
            # Create a custom optimizeSLIDE function with progress updates
            optimizeSLIDE_with_progress <- function(input_params, sink_file = F) {
              # Initial setup progress
              
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
              total_iterations <- length(delta) * length(lambda)
              current_iteration <- 0
              summary_table <- as.data.frame(matrix(NA, nrow = total_iterations, ncol = 7))
              colnames(summary_table) <- c('delta', 'lambda', 'f_size', 'Num_of_LFs', 'Num_of_Sig_LFs', 'Num_of_Interactors', 'sampleCV_Performance')

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
                             detail = sprintf("🏃‍♂️️ delta=%.3f, lambda=%.3f (%d/%d)", 
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
                  SLIDE_res <- tryCatch({
                    SLIDE::runSLIDE(y, y_path = NULL, z_path = NULL, z_matrix, 
                                  all_latent_factors, lf_path = NULL, niter = SLIDE_iter, 
                                  spec = spec, do_interacts = do_interacts)
                  }, error = function(e) {
                    updateProgressText("Error running SLIDE analysis, skipping...")
                    return(NULL)
                  })
                  
                  if (is.null(SLIDE_res)) {
                    next
                  }
                  
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
                      performance = SLIDE::sampleCV(y, z_matrix, SLIDE_res, 
                                               sampleCV_K = input_params$sampleCV_K,
                                               condition = input_params$eval_type, 
                                               sampleCV_iter = sampleCV_iter, 
                                               logistic = FALSE, 
                                               out_path = loop_outpath)

                      SLIDE::calcControlPerformance(z_matrix = z_matrix, y, do_interacts, 
                                                  SLIDE_res, condition = input_params$eval_type, 
                                                  loop_outpath)

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
      }, error = function(e) {
        cleanup_analysis()
        showNotification(sprintf("Error saving configuration: %s", e$message), type = "error")
      })
    }
  })

  # Modify the summary table output to include row selection
  output$summary_table <- renderDT({
    req(results$summary_table)
    datatable(results$summary_table,
              options = list(
                pageLength = 50,
                scrollX = FALSE,
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
              h5(
                sprintf(
                  "%s", 
                  case_when(
                    plot_file == "ControlPerformancePlot.png" ~ sprintf("Performance: %.3f", selected_row$sampleCV_Performance),
                    plot_file == "plotInteractions.png" ~ "Interactions",
                    plot_file == "plotSigGenes_marginals.png" ~ sprintf("Features (Total LFs: %s)", selected_row$Num_of_LFs),
                    TRUE ~ tools::file_path_sans_ext(plot_file)
                  ),
                  selected_row$sampleCV_Performance
                ), 
                class = "mb-2"
              ),
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

  # Function to list directory contents
  get_dir_contents <- function(path) {
    if (!dir.exists(path)) return(NULL)
    
    # Get subdirectories
    dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
    dirs <- dirs[dirs != ""]  # Remove empty strings
    
    # Create data frame with directory info
    data.frame(
      name = dirs,
      type = "folder",
      has_results = sapply(file.path(path, dirs), function(d) {
        file.exists(file.path(d, "summary_table.csv"))
      }),
      path = file.path(path, dirs),
      stringsAsFactors = FALSE
    )
  }

  # Observer for results path changes
  observeEvent(input$results_path, {
    req(input$results_path)
    if (input$results_path == "") return()
    
    path <- normalizePath(input$results_path, winslash = "/")
    current_dir(path)
    
    # Update directory contents
    dir_contents(get_dir_contents(path))
    
    # Check for summary table
    summary_file <- file.path(path, "summary_table.csv")
    if (file.exists(summary_file)) {
      tryCatch({
        summary_table <- read.csv(summary_file)
        results$summary_table <- summary_table
        results$current_output_path <- path
        showNotification("Results loaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(sprintf("Error loading results: %s", e$message), type = "error")
      })
    }
  })

  # Directory browser output
  output$dir_browser <- renderDT({
    req(dir_contents())
    contents <- dir_contents()
    
    # Add parent directory if not at root
    current_path <- current_dir()
    parent_path <- dirname(current_path)
    
    if (current_path != parent_path) {
      parent_row <- data.frame(
        name = "..",
        type = "parent",
        has_results = FALSE,
        path = parent_path,
        stringsAsFactors = FALSE
      )
      contents <- rbind(parent_row, contents)
    }
    
    # Create the interactive table
    datatable(
      contents,
      selection = 'single',
      options = list(
        dom = 't',  # Only show table, no search/pagination
        pageLength = -1,  # Show all rows
        ordering = FALSE,  # Disable sorting
        scrollY = "calc(100% - 1px)",  # Make table body scrollable
        scroller = TRUE,  # Enable virtual scrolling
        columnDefs = list(
          list(
            targets = 0,  # First column (name)
            render = JS("
              function(data, type, row) {
                if (row[1] === 'folder') {
                  return '<div class=\"d-flex align-items-center\"><i class=\"fas fa-folder me-2 text-warning\"></i>' + 
                         '<span style=\"flex-grow: 1;\">' + data + '</span></div>';
                } else {
                  return '<div class=\"d-flex align-items-center\"><i class=\"fas fa-level-up-alt me-2 text-secondary\"></i>' + 
                         '<span style=\"flex-grow: 1;\">' + data + '</span></div>';
                }
              }
            ")
          ),
          list(targets = c(1, 3), visible = FALSE)  # Hide type and path columns
        )
      ),
      colnames = c("Name", "Type", "Has Results", "Path"),
      rownames = FALSE,
      escape = FALSE,
      class = 'compact hover'  # Add compact and hover classes
    ) %>%
      formatStyle(
        'has_results',
        target = 'row',
        backgroundColor = styleEqual(c(TRUE), c('#d4edda'))
      ) %>%
      formatStyle(
        'name',
        cursor = 'pointer'
      )
  })

  # Add observer to update input field when directory is selected
  observeEvent(input$dir_browser_rows_selected, {
    req(input$dir_browser_rows_selected)
    contents <- dir_contents()
    
    # Add parent directory row if needed
    current_path <- current_dir()
    parent_path <- dirname(current_path)
    if (current_path != parent_path) {
      parent_row <- data.frame(
        name = "..",
        type = "parent",
        has_results = FALSE,
        path = parent_path,
        stringsAsFactors = FALSE
      )
      contents <- rbind(parent_row, contents)
    }
    
    # Get selected path and update input
    selected_path <- contents$path[input$dir_browser_rows_selected]
    updateTextInput(session, "results_path", value = selected_path)
  })

  output$results_cards <- renderUI({
    if (is.null(results$summary_table)) {
      return(NULL)  # Don't show anything if no results loaded
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

  # Move the UI definition inside the server function
  output$load_results_ui <- renderUI({
    div(
      class = "row",
      div(
        class = "col-12",
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              "Directory Browser",
              if (!is.null(current_dir())) {
                tags$small(
                  class = "text-muted",
                  tags$code(
                    style = "word-break: break-all;",
                    current_dir()
                  )
                )
              }
            )
          ),
          div(
            class = "card-body p-3",
            div(
              class = "mb-3",
              style = "display: flex; gap: 10px; align-items: center;",
              div(
                style = "flex-grow: 1; min-width: 0;",
                tags$div(
                  class = "path-input-truncate",
                  style = "width: 100%;",
                  textInput("results_path", "Enter or browse to a directory containing SLIDE results", 
                          value = current_dir(),
                          placeholder = "Enter or browse to a directory containing SLIDE results",
                          width = '100%')
                )
              )
            ),
            div(
              style = "height: 250px; overflow-y: auto; border: 1px solid #dee2e6; border-radius: 4px;",
              DTOutput("dir_browser")
            )
          )
        )
      )
    )
  })

  output$results_section <- renderUI({
    div(
      class = "container-fluid p-3",
      style = "height: calc(100vh - 56px); overflow-y: auto;",
      uiOutput("load_results_ui"),
      uiOutput("results_cards")
    )
  })

  # Observe file validity and update button states
  observe({
    if (analysis_running()) {
      js$disableAllRunButtons()
    } else {
      if (x_file_valid() && y_file_valid()) {
        js$enableButton('run_slide')
      } else {
        js$disableButton('run_slide')
      }
    }
  })

  # Update X file validation status
  observe({
    # Validate X path
    if (!is.null(input$x_path) && input$x_path != "") {
      tryCatch({
        if (file.exists(input$x_path)) {
          x <- as.matrix(utils::read.csv(input$x_path, row.names = 1, check.names = F))
          # If we get here, file is valid
          shinyjs::removeClass(id = "x_path_icon", class = "fa-times invalid")
          shinyjs::addClass(id = "x_path_icon", class = "fa-check valid")
          x_file_valid(TRUE)
        } else {
          shinyjs::removeClass(id = "x_path_icon", class = "fa-check valid")
          shinyjs::addClass(id = "x_path_icon", class = "fa-times invalid")
          x_file_valid(FALSE)
        }
      }, error = function(e) {
        shinyjs::removeClass(id = "x_path_icon", class = "fa-check valid")
        shinyjs::addClass(id = "x_path_icon", class = "fa-times invalid")
        x_file_valid(FALSE)
      })
    } else if (!is.null(input$x_file)) {
      tryCatch({
        x <- as.matrix(utils::read.csv(input$x_file$datapath, row.names = 1, check.names = F))
        shinyjs::removeClass(id = "x_path_icon", class = "fa-times invalid")
        shinyjs::addClass(id = "x_path_icon", class = "fa-check valid")
        x_file_valid(TRUE)
      }, error = function(e) {
        shinyjs::removeClass(id = "x_path_icon", class = "fa-check valid")
        shinyjs::addClass(id = "x_path_icon", class = "fa-times invalid")
        x_file_valid(FALSE)
      })
    } else {
      shinyjs::removeClass(id = "x_path_icon", class = "fa-check valid")
      shinyjs::removeClass(id = "x_path_icon", class = "fa-times invalid")
      x_file_valid(FALSE)
    }
  })
  
  # Update Y file validation status
  observe({
    # Validate Y path
    if (!is.null(input$y_path) && input$y_path != "") {
      tryCatch({
        if (file.exists(input$y_path)) {
          y <- as.matrix(utils::read.csv(input$y_path, row.names = 1))
          # If we get here, file is valid
          shinyjs::removeClass(id = "y_path_icon", class = "fa-times invalid")
          shinyjs::addClass(id = "y_path_icon", class = "fa-check valid")
          y_file_valid(TRUE)
        } else {
          shinyjs::removeClass(id = "y_path_icon", class = "fa-check valid")
          shinyjs::addClass(id = "y_path_icon", class = "fa-times invalid")
          y_file_valid(FALSE)
        }
      }, error = function(e) {
        shinyjs::removeClass(id = "y_path_icon", class = "fa-check valid")
        shinyjs::addClass(id = "y_path_icon", class = "fa-times invalid")
        y_file_valid(FALSE)
      })
    } else if (!is.null(input$y_file)) {
      tryCatch({
        y <- as.matrix(utils::read.csv(input$y_file$datapath, row.names = 1))
        shinyjs::removeClass(id = "y_path_icon", class = "fa-times invalid")
        shinyjs::addClass(id = "y_path_icon", class = "fa-check valid")
        y_file_valid(TRUE)
      }, error = function(e) {
        shinyjs::removeClass(id = "y_path_icon", class = "fa-check valid")
        shinyjs::addClass(id = "y_path_icon", class = "fa-times invalid")
        y_file_valid(FALSE)
      })
    } else {
      shinyjs::removeClass(id = "y_path_icon", class = "fa-check valid")
      shinyjs::removeClass(id = "y_path_icon", class = "fa-times invalid")
      y_file_valid(FALSE)
    }
  })

  # Update x_path when file is uploaded
  observeEvent(input$x_file, {
    req(input$x_file)
    updateTextInput(session, "x_path", value = input$x_file$datapath)
  })

  # Update y_path when file is uploaded
  observeEvent(input$y_file, {
    req(input$y_file)
    updateTextInput(session, "y_path", value = input$y_file$datapath)
  })
}

shinyApp(ui, server)
