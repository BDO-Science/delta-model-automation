
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(gt)

app_version <- "1.1.0"

run_page <- function(prefix, title, theme_class) {
  
  is_current <- identical(prefix, "current")
  status_name <- "warning"
  
  flow_input <- function(input_id, label, value) {
    if (is_current) {
      textInput(
        input_id,
        label,
        format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
      )
    } else {
      numericInput(input_id, label, value)
    }
  }
  
  flow_info <- function(
    text = "Default value is based on the latest available 7-day average flow.",
    note_output_id = NULL
  ) {
    if (!is_current) {
      return(NULL)
    }
    
    tags$div(
      class = "flow-info-hover",
      tags$span(
        class = "flow-info-trigger",
        tabindex = "0",
        role = "button",
        `aria-label` = "Input information",
        icon("info-circle")
      ),
      tags$div(
        class = "flow-info-popover",
        tags$div(class = "flow-info-static", text),
        if (!is.null(note_output_id)) {
          tagList(
            tags$hr(),
            uiOutput(note_output_id)
          )
        }
      )
    )
  }
  
  info_heading <- function(title, text, heading_class = NULL) {
    tags$div(
      class = "section-heading-with-info",
      tags$h4(
        class = heading_class,
        title
      ),
      if (is_current) {
        flow_info(text)
      } else {
        tags$div(
          class = "flow-info-hover",
          tags$span(
            class = "flow-info-trigger",
            tabindex = "0",
            role = "button",
            `aria-label` = paste(title, "information"),
            icon("info-circle")
          ),
          tags$div(
            class = "flow-info-popover",
            tags$div(class = "flow-info-static", text)
          )
        )
      }
    )
  }
  
  weekly_value_card <- function(label, output_id, info_text) {
    div(
      class = "exploration-input-card weekly-static-input-card",
      tags$div(
        class = "flow-info-hover",
        tags$span(
          class = "flow-info-trigger",
          tabindex = "0",
          role = "button",
          `aria-label` = paste(label, "information"),
          icon("info-circle")
        ),
        tags$div(
          class = "flow-info-popover",
          tags$div(class = "flow-info-static", info_text)
        )
      ),
      tags$label(label),
      tags$div(
        class = "weekly-static-value form-control",
        textOutput(output_id, inline = TRUE)
      )
    )
  }
  
  tabItem(
    tabName = paste0("run_", prefix),
    
    div(
      class = theme_class,
      
      if (is_current) {
        tagList(
          conditionalPanel(
            condition = sprintf("input.%s_model_tabs == 'PTM Emulator and Event Horizon'", prefix),
            tags$div(
              class = "exploration-workflow-guide",
              icon("info-circle"),
              tags$span(
                tags$b("How to use this page: "),
                "Run using current Delta condition flows or test your own. Review or edit the flow inputs below, select the risk threshold, and click Run Models. Model processing may take a few minutes. Results will appear below in the map, charts, and tables."
              )
            )
          ),
          conditionalPanel(
            condition = sprintf("input.%s_model_tabs == 'ECO-PTM Emulator'", prefix),
            tags$div(
              class = "exploration-workflow-guide",
              icon("info-circle"),
              tags$span(
                tags$b("How to use this page: "),
                "Run using current Delta condition flows or test your own. Review or edit the inputs below and click Run Models. Model processing may take a few minutes. ECO-PTM results will appear in the results table below."
              )
            )
          )
        )
      },
      
      if (!is_current) {
        tagList(
          conditionalPanel(
            condition = sprintf("input.%s_model_tabs == 'PTM Emulator and Event Horizon'", prefix),
            tags$div(
              class = "exploration-workflow-guide",
              icon("info-circle"),
              tags$span(
                tags$b("How to use this page: "),
                "Run using historical Delta condition flows or historical OMRI scenario flows. Select the archive week and flow source, review the read-only 7-day average inputs, select the risk threshold, and click Run Models. Model processing may take a few minutes. Results will appear below in the maps, bar charts, tables, and Event Horizon plot."
              )
            )
          ),
          conditionalPanel(
            condition = sprintf("input.%s_model_tabs == 'ECO-PTM Emulator'", prefix),
            tags$div(
              class = "exploration-workflow-guide",
              icon("info-circle"),
              tags$span(
                tags$b("How to use this page: "),
                "Run using historical Delta condition flows or historical OMRI scenario flows. Select the archive week and flow source, review the read-only 7-day average inputs, and click Run Models. Model processing may take a few minutes. ECO-PTM results will appear in the results table below."
              )
            )
          )
        )
      },
      
      fluidRow(
        box(
          width = 12,
          title = "Input Methods",
          status = status_name,
          solidHeader = TRUE,
          
          if (is_current) {
            tagList(
              radioButtons(
                paste0(prefix, "_input_method"),
                "Input Method:",
                choices = c(
                  "Enter a Single Set of Values" = "single",
                  "Upload CSV or Excel File (coming soon)" = "upload"
                ),
                selected = "single",
                inline = TRUE
              ),
              conditionalPanel(
                condition = sprintf("input.%s_input_method == 'upload'", prefix),
                tags$div(
                  class = "alert alert-info",
                  "The upload-file workflow will be implemented in a later phase."
                )
              )
            )
          } else {
            tagList(
              tags$div(
                class = "weekly-input-method-row",
                tags$div(
                  class = "weekly-input-method-primary",
                  radioButtons(
                    paste0(prefix, "_input_method"),
                    NULL,
                    choices = c(
                      "Read from weekly assessment archive folder" = "folder"
                    ),
                    selected = "folder",
                    inline = TRUE
                  )
                ),
                tags$div(
                  class = "weekly-input-method-source",
                  radioButtons(
                    "forecast_flow_source",
                    NULL,
                    choices = c(
                      "Use Observed Flow" = "observed",
                      "Use Forecast OMRI Scenario Flows" = "forecast"
                    ),
                    selected = "observed",
                    inline = TRUE
                  )
                )
              ),
              tags$div(
                class = "archive-control-panel weekly-archive-selection",
                uiOutput(paste0(prefix, "_archive_date_ui")),
                conditionalPanel(
                  condition = "input.forecast_flow_source == 'forecast'",
                  uiOutput(paste0(prefix, "_archive_scenario_ui"))
                )
              )
            )
          }
        )
      ),
      
      conditionalPanel(
        condition = if (is_current) {
          sprintf(
            "input.%s_input_method == 'single'",
            prefix
          )
        } else {
          sprintf(
            "input.%s_input_method == 'folder'",
            prefix
          )
        },
        
        tabsetPanel(
          id = paste0(prefix, "_model_tabs"),
          
          # ==========================================================
          # PTM
          # ==========================================================
          tabPanel(
            "PTM Emulator and Event Horizon",
            
            fluidRow(
              class = "emulator-top-row",
              
              box(
                width = 12,
                title = "Inputs",
                status = status_name,
                solidHeader = TRUE,
                conditionalPanel(
                  condition = sprintf("input.%s_input_method == 'single'", prefix),
                  
                  if (is_current) {
                    tagList(
                      div(
                        class = "exploration-action-row",
                        div(
                          class = "exploration-scenario-field",
                          textInput(
                            paste0(prefix, "_ptm_name"),
                            "Scenario Name (User Defined):",
                            "EXPLOR_01"
                          )
                        ),
                        div(
                          class = "exploration-action-buttons",
                          actionButton(
                            paste0("run_", prefix, "_ptm"),
                            "Run Models",
                            icon = icon("play"),
                            class = "btn-success model-run-button"
                          ),
                          downloadButton(
                            paste0("download_", prefix, "_ptm"),
                            "Download Results (CSV)",
                            class = "btn-primary"
                          )
                        )
                      ),
                      
                      div(
                        class = "exploration-input-grid exploration-ptm-grid",
                        
                        div(
                          class = "exploration-input-card",
                          flow_info(
                            note_output_id = paste0(prefix, "_ptm_exp_note")
                          ),
                          flow_input(
                            paste0(prefix, "_ptm_exp"),
                            "EXP: Combined CVP and SWP Export (cfs):",
                            6000
                          )
                        ),
                        
                        div(
                          class = "exploration-input-card",
                          flow_info(
                            note_output_id = paste0(prefix, "_ptm_ver_note")
                          ),
                          flow_input(
                            paste0(prefix, "_ptm_ver"),
                            "VER: San Joaquin River Flow at Vernalis Flow (cfs):",
                            3000
                          )
                        ),
                        
                        div(
                          class = "exploration-input-card",
                          flow_info(
                            "Default value is based on the latest available 7-day average flow. PTM emulator input only: SAC is used by the PTM emulators and is not used by the Event Horizon emulator.",
                            paste0(prefix, "_ptm_sac_note")
                          ),
                          flow_input(
                            paste0(prefix, "_ptm_sac"),
                            "SAC: Sacramento River Flow at Freeport Flow (cfs):",
                            18000
                          )
                        ),
                        
                        div(
                          class = "exploration-input-card",
                          flow_info(
                            "Default value is based on the latest available 7-day average flow. EAST is the sum of Mokelumne, Calaveras, and Cosumnes River flows.",
                            paste0(prefix, "_ptm_east_note")
                          ),
                          flow_input(
                            paste0(prefix, "_ptm_east"),
                            "EAST: East-Side River Flow (cfs):",
                            1500
                          )
                        ),
                        
                        div(
                          class = "exploration-input-card",
                          flow_info(
                            "Default value is based on the latest available 7-day average flow. XGEO is the sum of Sacramento River above Delta Cross Channel and Sacramento River below Georgiana Slough.",
                            paste0(prefix, "_ptm_xgeo_note")
                          ),
                          flow_input(
                            paste0(prefix, "_ptm_xgeo"),
                            "XGEO: Interior Delta Flow (cfs):",
                            3000
                          )
                        ),
                        
                        div(
                          class = "exploration-input-card exploration-threshold-card",
                          tags$div(class = "input-card-spacer"),
                          selectInput(
                            paste0(prefix, "_ptm_threshold"),
                            "Entrainment Risk/Event Horizon Risk Threshold (%):",
                            choices = seq(15, 80, by = 5),
                            selected = 25
                          )
                        )
                      )
                    )
                  } else {
                    textInput(
                      paste0(prefix, "_ptm_name"),
                      "Scenario Name (User Defined):",
                      "Forecast Conditions Run 1"
                    )
                  }
                ),
                
                conditionalPanel(
                  condition = sprintf("input.%s_input_method == 'folder'", prefix),
                  
                  if (!is_current) {
                    tagList(
                      div(
                        class = "weekly-inputs-info-row",
                        tags$div(
                          class = "flow-info-hover",
                          tags$span(
                            class = "flow-info-trigger",
                            tabindex = "0",
                            role = "button",
                            `aria-label` = "Weekly Assessment input information",
                            icon("info-circle")
                          ),
                          tags$div(
                            class = "flow-info-popover",
                            tags$div(
                              class = "flow-info-static",
                              "Inputs are read-only. Values remain blank until an archive week is selected. The 7-day average flow from the selected archive week and flow source is used for all models."
                            )
                          )
                        )
                      ),
                      
                      div(
                        class = "exploration-action-row weekly-action-row",
                        div(
                          class = "exploration-scenario-field",
                          textInput(
                            paste0(prefix, "_ptm_archive_name"),
                            "Scenario Name:",
                            ""
                          )
                        ),
                        div(
                          class = "exploration-action-buttons",
                          actionButton(
                            paste0("run_", prefix, "_ptm"),
                            "Run Models",
                            icon = icon("play"),
                            class = "btn-success model-run-button"
                          ),
                          downloadButton(
                            paste0("download_", prefix, "_ptm"),
                            "Download Results (CSV)",
                            class = "btn-primary"
                          )
                        )
                      ),
                      
                      div(
                        class = "exploration-input-grid exploration-ptm-grid weekly-input-grid",
                        weekly_value_card(
                          "EXP: Combined CVP and SWP Export (cfs):",
                          "forecast_ptm_exp_value",
                          "Read-only 7-day average EXP calculated from the selected archive week and flow source."
                        ),
                        weekly_value_card(
                          "VER: San Joaquin River Flow at Vernalis (cfs):",
                          "forecast_ptm_ver_value",
                          "Read-only 7-day average VER calculated from the selected archive week and flow source."
                        ),
                        weekly_value_card(
                          "SAC: Sacramento River Flow at Freeport (cfs):",
                          "forecast_ptm_sac_value",
                          "Read-only 7-day average SAC calculated from the selected archive week and flow source. SAC is used by the PTM emulators and is not used by the Event Horizon emulator."
                        ),
                        weekly_value_card(
                          "EAST: East-Side River Flow (cfs):",
                          "forecast_ptm_east_value",
                          "Read-only 7-day average EAST calculated from the selected archive week and flow source. EAST is the sum of Mokelumne, Calaveras, and Cosumnes River flows."
                        ),
                        weekly_value_card(
                          "XGEO: Interior Delta Flow (cfs):",
                          "forecast_ptm_xgeo_value",
                          "Read-only 7-day average XGEO calculated from the selected archive week and flow source."
                        ),
                        div(
                          class = "exploration-input-card exploration-threshold-card",
                          tags$div(class = "input-card-spacer"),
                          selectInput(
                            paste0(prefix, "_ptm_threshold"),
                            "Entrainment Risk/Event Horizon Risk Threshold (%):",
                            choices = seq(15, 80, by = 5),
                            selected = 25
                          )
                        )
                      )
                    )
                  } else {
                    tagList(
                      textInput(
                        paste0(prefix, "_ptm_archive_name"),
                        "Scenario Name (User Defined):",
                        "Observed Conditions Run 1"
                      ),
                      uiOutput(paste0(prefix, "_ptm_archive_summary"))
                    )
                  }
                )
              ),
              
              box(
                width = 12,
                title = NULL,
                status = status_name,
                solidHeader = TRUE,
                class = "model-results-box",
                
                tags$div(
                  class = "model-results-loading",
                  tags$div(
                    class = "model-results-loading-card",
                    icon("spinner", class = "fa-spin"),
                    tags$span(" Running models and loading results...")
                  )
                ),
                
                tabsetPanel(
                  id = paste0(prefix, "_ptm_result_tabs"),
                  
                  tabPanel(
                    "Event Horizon and 7-Day Entrainment",
                    
                    uiOutput(
                      paste0(prefix, "_eh_summary")
                    ),
                    
                    info_heading(
                      "7-Day Entrainment Risk Map",
                      if (is_current) {
                        "The map contains PTM emulator results from the 7-day observed conditions input values. Results include PTM emulator estimated entrainment at each node, a contour polygon that interpolates a boundary between nodes based on the user defined entrainment risk threshold, and the entrainment event horizon. Click a node for details."
                      } else {
                        "The map contains PTM emulator results from the 7-day forecast conditions input values. Results include PTM emulator estimated entrainment at each node, a contour polygon that interpolates a boundary between nodes based on the user defined entrainment risk threshold, and the entrainment event horizon. Click a node for details."
                      }
                    ),
                    
                    conditionalPanel(
                      condition = if (is_current) {
                        sprintf("input.%s_input_method == 'folder'", prefix)
                      } else {
                        sprintf(
                          "input.%s_input_method == 'folder' && input.forecast_flow_source == 'observed'",
                          prefix
                        )
                      },
                      tags$div(
                        class = "section-heading-with-info map-window-heading",
                        tags$h4("Map Window End Date"),
                        tags$div(
                          class = "flow-info-hover",
                          tags$span(
                            class = "flow-info-trigger",
                            tabindex = "0",
                            role = "button",
                            `aria-label` = "Map Window End Date information",
                            icon("info-circle")
                          ),
                          tags$div(
                            class = "flow-info-popover",
                            tags$div(
                              class = "flow-info-static",
                              "For observed-flow weekly assessments, measured archive data are evaluated using seven rolling 7-day average windows. Each slider date is the end date of one rolling 7-day window. Move the slider to update the map and Event Horizon result for that window."
                            )
                          )
                        )
                      ),
                      uiOutput(paste0(prefix, "_ptm7_map_date_ui"))
                    ),
                    
                    leafletOutput(
                      paste0(prefix, "_ptm7_map"),
                      height = 500
                    ),
                    
                    downloadButton(
                      paste0("download_", prefix, "_ptm7_map"),
                      "Download Map (PNG)",
                      class = "btn-primary map-download"
                    ),
                    
                    br(), br(),
                    
                    conditionalPanel(
                      condition = sprintf(
                        "input.%s_input_method == 'folder' && '%s' == 'current'",
                        prefix,
                        prefix
                      ),
                      
                      tags$h4(
                        "7-Day Rolling Entrainment Prediction"
                      ),
                      
                      uiOutput(paste0(prefix, "_ptm7_timeseries_nodes_ui")),
                      
                      tags$p(
                        class = "figure-note",
                        paste(
                          "Predictions are shown for seven rolling 7-day",
                          "windows ending on the last seven measured dates."
                        )
                      ),
                      
                      plotlyOutput(
                        paste0(prefix, "_ptm7_timeseries"),
                        height = 600
                      )
                    )
                  ),
                  
                  tabPanel(
                    "30-Day Entrainment",
                    
                    conditionalPanel(
                      condition = "false",
                      
                      tags$div(
                        class = "alert alert-info",
                        paste(
                          "The archive-folder forecast workflow runs only the",
                          "PTM Emulator 7-day model, based on the seven-day forecast average."
                        )
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "true",                      info_heading(
                        "30-Day Entrainment Risk Map",
                        if (is_current) {
                          "The map contains PTM 30-Day emulator results using 7-day observed conditions input values. Results include PTM emulator estimated entrainment at each node and a contour polygon that interpolates a boundary between nodes based on the user defined entrainment risk threshold."
                        } else {
                          "The map contains PTM 30-Day emulator results using 7-day forecast conditions input values. Results include PTM emulator estimated entrainment at each node and a contour polygon that interpolates a boundary between nodes based on the user defined entrainment risk threshold."
                        }
                      ),
                      
                      leafletOutput(
                        paste0(prefix, "_ptm30_map"),
                        height = 500
                      ),
                      
                      downloadButton(
                        paste0("download_", prefix, "_ptm30_map"),
                        "Download Map (PNG)",
                        class = "btn-primary map-download"
                      ),
                      
                      br(), br(),                      info_heading(
                        "30-Day Estimated Entrainment Bar-chart by Locations (DSM2 Node)",
                        "Values are percent entrainment."
                      ),
                      
                      plotlyOutput(
                        paste0(prefix, "_ptm30_plot"),
                        height = 900
                      ),
                      
                      br(),
                      
                      tags$h4("30-Day Estimated Entrainment Percentage by Location (DSM2 Node)"),
                      
                      div(
                        class = "wide-table-scroll",
                        tableOutput(paste0(prefix, "_ptm30_table"))
                      )
                    )
                  )
                )
              )
            ),
            
            conditionalPanel(
              condition = sprintf(
                "input.%s_ptm_result_tabs == 'Event Horizon and 7-Day Entrainment'",
                prefix
              ),
              
              fluidRow(
                box(
                  width = 12,
                  status = status_name,
                  solidHeader = FALSE,
                  class = "combined-emulator-plots-box",
                  
                  fluidRow(
                    column(
                      width = 6,                      info_heading(
                        "7-Day Estimated Entrainment Bar-chart by Locations (DSM2 Node)",
                        "Values are percent entrainment.",
                        heading_class = "emulator-figure-title"
                      ),
                      
                      plotlyOutput(
                        paste0(prefix, "_ptm7_plot"),
                        height = 620
                      )
                    ),
                    
                    column(
                      width = 6,                      info_heading(
                        "Event Horizon: River Miles from Clifton Court Forebay",
                        "Historical 7-day average flows (2023-2026) were used to generate the event horizons that shown in the background of the scatter plot; the selected result is highlighted in red.",
                        heading_class = "emulator-figure-title"
                      ),
                      plotlyOutput(
                        paste0(prefix, "_eh_scatter"),
                        height = 620
                      )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  title = "7-Day Estimated Entrainment Percentage by Location (DSM2 Node)",
                  status = status_name,
                  solidHeader = TRUE,
                  
                  tableOutput(
                    paste0(prefix, "_ptm7_table")
                  )
                )
              )
            )
          ),
          
          # ==========================================================
          # ECO-PTM
          # ==========================================================
          tabPanel(
            "ECO-PTM Emulator",
            
            conditionalPanel(
              condition = "false",
              tags$div(
                class = "alert alert-info",
                paste(
                  "ECO-PTM Emulator is not run for archive-folder forecast conditions.",
                  "Use Observed Conditions with Read from Archive Folder."
                )
              )
            ),
            
            conditionalPanel(
              condition = "true",
              
              fluidRow(
                class = "emulator-top-row",
                
                box(
                  width = 12,
                  title = "Inputs",
                  status = status_name,
                  solidHeader = TRUE,
                  
                  
                  conditionalPanel(
                    condition = sprintf(
                      "input.%s_input_method == 'single'",
                      prefix
                    ),
                    
                    if (is_current) {
                      tagList(
                        div(
                          class = "exploration-action-row",
                          div(
                            class = "exploration-scenario-field",
                            textInput(
                              paste0(prefix, "_eco_name"),
                              "Scenario Name (User Defined):",
                              "EXPLOR_01"
                            )
                          ),
                          div(
                            class = "exploration-action-buttons",
                            actionButton(
                              paste0("run_", prefix, "_eco"),
                              "Run Models",
                              icon = icon("play"),
                              class = "btn-success model-run-button"
                            ),
                            downloadButton(
                              paste0("download_", prefix, "_eco"),
                              "Download Results (CSV)",
                              class = "btn-primary"
                            )
                          )
                        ),
                        
                        div(
                          class = "exploration-input-grid exploration-eco-grid",
                          
                          div(
                            class = "exploration-input-card",
                            flow_info(
                              note_output_id = paste0(prefix, "_eco_sac_note")
                            ),
                            flow_input(
                              paste0(prefix, "_eco_sac"),
                              "SAC: Sacramento River Flow at Freeport Flow (cfs):",
                              18000
                            )
                          ),
                          
                          div(
                            class = "exploration-input-card",
                            flow_info(
                              "Default value is based on the latest available 7-day average flow. YOL is the sum of Sacramento Weir and Fremont Weir. Sacramento Weir flow is assumed to be 0 cfs; Fremont Weir uses CDEC FRE flow data.",
                              paste0(prefix, "_eco_yol_note")
                            ),
                            flow_input(
                              paste0(prefix, "_eco_yol"),
                              "YOL: Yolo Bypass Flow (cfs):",
                              1000
                            )
                          ),
                          
                          div(
                            class = "exploration-input-card",
                            flow_info(
                              "MOKE uses the latest available 7-day average flow calculated from MOKE_daily.csv.",
                              paste0(prefix, "_eco_moke_note")
                            ),
                            flow_input(
                              paste0(prefix, "_eco_moke"),
                              "MOKE: Mokelumne Flow (cfs):",
                              800
                            )
                          ),
                          
                          div(
                            class = "exploration-input-card exploration-dcc-card",
                            tags$div(class = "input-card-spacer"),
                            radioButtons(
                              paste0(prefix, "_eco_dcc"),
                              "Delta Cross Channel Status:",
                              choices = c("Closed" = 0, "Open" = 1),
                              selected = 0,
                              inline = TRUE
                            )
                          )
                        )
                      )
                    } else {
                      tagList(
                        textInput(
                          paste0(prefix, "_eco_name"),
                          "Scenario Name (User Defined):",
                          "Forecast Conditions Run 1"
                        ),
                        numericInput(
                          paste0(prefix, "_eco_sac"),
                          "SAC: Sacramento River Flow at Freeport Flow (cfs):",
                          18000
                        ),
                        numericInput(
                          paste0(prefix, "_eco_yol"),
                          "YOL: Yolo Bypass Flow (cfs):",
                          1000
                        ),
                        numericInput(
                          paste0(prefix, "_eco_moke"),
                          "MOKE: Mokelumne Flow (cfs):",
                          800
                        ),
                        radioButtons(
                          paste0(prefix, "_eco_dcc"),
                          "Delta Cross Channel Status:",
                          choices = c("Closed" = 0, "Open" = 1),
                          selected = 0,
                          inline = TRUE
                        )
                      )
                    }
                  ),
                  
                  conditionalPanel(
                    condition = sprintf(
                      "input.%s_input_method == 'folder'",
                      prefix
                    ),
                    
                    if (!is_current) {
                      tagList(
                        div(
                          class = "weekly-inputs-info-row",
                          tags$div(
                            class = "flow-info-hover",
                            tags$span(
                              class = "flow-info-trigger",
                              tabindex = "0",
                              role = "button",
                              `aria-label` = "Weekly Assessment input information",
                              icon("info-circle")
                            ),
                            tags$div(
                              class = "flow-info-popover",
                              tags$div(
                                class = "flow-info-static",
                                "Inputs are read-only. Values remain blank until an archive week is selected. The 7-day average flow from the selected archive week and flow source is used for all models."
                              )
                            )
                          )
                        ),
                        
                        div(
                          class = "exploration-action-row weekly-action-row",
                          div(
                            class = "exploration-scenario-field",
                            textInput(
                              paste0(prefix, "_eco_archive_name"),
                              "Scenario Name:",
                              ""
                            )
                          ),
                          div(
                            class = "exploration-action-buttons",
                            actionButton(
                              paste0("run_", prefix, "_eco"),
                              "Run Models",
                              icon = icon("play"),
                              class = "btn-success model-run-button"
                            ),
                            downloadButton(
                              paste0("download_", prefix, "_eco"),
                              "Download Results (CSV)",
                              class = "btn-primary"
                            )
                          )
                        ),
                        
                        div(
                          class = "exploration-input-grid exploration-eco-grid weekly-input-grid",
                          weekly_value_card(
                            "SAC: Sacramento River Flow at Freeport (cfs):",
                            "forecast_eco_sac_value",
                            "Read-only 7-day average SAC calculated from the selected archive week and flow source."
                          ),
                          weekly_value_card(
                            "YOL: Yolo Bypass Flow (cfs):",
                            "forecast_eco_yol_value",
                            "Read-only 7-day average YOL calculated from the selected archive week and flow source."
                          ),
                          weekly_value_card(
                            "MOKE: Mokelumne Flow (cfs):",
                            "forecast_eco_moke_value",
                            "Read-only 7-day average MOKE calculated from the selected archive week and flow source."
                          ),
                          weekly_value_card(
                            "Delta Cross Channel Status:",
                            "forecast_eco_dcc_value",
                            "Read-only DCC status from the selected archive week. DCC is a status variable rather than a flow."
                          )
                        )
                      )
                    } else {
                      tagList(
                        textInput(
                          paste0(prefix, "_eco_archive_name"),
                          "Scenario Name (User Defined):",
                          "Observed Conditions Run 1"
                        ),
                        tags$p(
                          class = "figure-note",
                          paste(
                            "The model inputs are calculated from the latest",
                            "7 measured days. DCC is read from the archive file",
                            "when available; otherwise it defaults to closed (0)."
                          )
                        )
                      )
                    }
                  )
                ),
                
                box(
                  width = 12,
                  title = "ECO-PTM Emulator Results",
                  status = status_name,
                  solidHeader = TRUE,
                  class = "model-results-box",
                  
                  tags$div(
                    class = "model-results-loading",
                    tags$div(
                      class = "model-results-loading-card",
                      icon("spinner", class = "fa-spin"),
                      tags$span(" Running models and loading results...")
                    )
                  ),
                  
                  div(
                    class = "wide-table-scroll",
                    tableOutput(paste0(prefix, "_eco_table"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

ui <- dashboardPage(
  dashboardHeader(
    titleWidth = 300,
    title = tags$div(
      style = "display:flex;align-items:center;",
      tags$img(src = "logo.png", height = "30px", style = "margin-right:10px;"),
      tags$span("*DRAFT Entrainment Dashboard", style = "font-family:Segoe UI Semibold;font-size:16px;")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Exploration", tabName = "run_current", icon = icon("water")),
      menuItem("Weekly Assessments", tabName = "run_forecast", icon = icon("cloud-sun")),
      menuItem("Scenario Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Data Access", tabName = "data", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Keep figure and table headings visually prominent throughout the app. */
        .tab-content h4,
        .box-title,
        .figure-title,
        .table-title,
        .emulator-figure-title{
          font-weight:700 !important;
        }

        .main-header .logo{width:300px!important;height:60px!important;line-height:60px!important;background:white;color:#0a7e8c;border-bottom:3px solid #0a7e8c}
        .main-header .navbar{min-height:60px;background:white;border-bottom:3px solid #0a7e8c}
        .main-sidebar{width:300px!important;background:#fbfeff;border-right:1px solid #d8edf1}
        .content-wrapper,.right-side{margin-left:300px!important;background:#f9fbfc;padding:18px}
        .sidebar-menu>li>a{color:#0a6270;font-size:14px;font-weight:600}
        .sidebar-menu li:hover>a,.sidebar-menu>li.active>a{background:#eef9fb!important;color:#075f6d!important;border-left:4px solid #0a7e8c}
        .box{border-radius:8px;box-shadow:0 2px 8px rgba(0,0,0,.06);border-top:4px solid #0a7e8c}
        .current-theme{background:#fffaf2;padding:6px;border-radius:8px}
        .current-theme .box{border-top-color:#e69f00}
        .forecast-theme{background:#fffaf2;padding:6px;border-radius:8px}
        .forecast-theme .box{border-top-color:#e69f00}
        .forecast-theme .box.box-warning > .box-header{
          background:#f39c12 !important;
          color:#ffffff !important;
        }
        .forecast-theme .box.box-warning{
          border-top-color:#e69f00 !important;
        }
        .forecast-single-warning{
          margin-top:10px;
          margin-bottom:0;
        }
        .observed-flow-note{
          margin-top:-10px;
          margin-bottom:12px;
          padding:7px 9px;
          background:#f7f9fb;
          border-left:3px solid #5b7894;
          color:#3d4c59;
          font-size:12px;
          line-height:1.45;
        }
        .observed-flow-note a{
          font-weight:600;
        }
        /* Keep input/results panels compact and let each panel fit its own content. */
        .emulator-top-row{
          display:flex;
          flex-wrap:wrap;
          align-items:flex-start;
        }
        .emulator-top-row > [class*='col-']{
          display:block;
        }
        .emulator-top-row > [class*='col-'] > .box{
          width:100%;
          height:auto;
          margin-bottom:15px;
        }
        @media (max-width:991px){
          .emulator-top-row{
            display:block;
          }
          .emulator-top-row > [class*='col-']{
            display:block;
          }
        }

        body{font-family:Segoe UI;color:#333}


        .figure-note {
          font-size: 14px;
          color: #4d5b61;
          margin-bottom: 10px;
        }
        .map-download {
          margin-top: 10px;
          margin-bottom: 5px;
        }
        .archive-control-panel {
          padding: 12px 14px;
          margin-top: 10px;
          background: #f4fbfc;
          border: 1px solid #b8dce2;
          border-radius: 6px;
        }

        .archive-control-panel h4 {
          margin-top: 0;
          color: #075f6d;
          font-weight: 700;
        }

        .wide-table-scroll {
          width: 100%;
          overflow-x: auto;
          white-space: nowrap;
        }
        .wide-table-scroll table {
          width: max-content !important;
          min-width: 100%;
          font-size: 14px;
        }
        .leaflet-tooltip.node-permanent-label {
          background: rgba(255,255,255,0.88);
          border: 1px solid #0a7e8c;
          color: #075f6d;
          font-size: 13px;
          font-weight: 700;
          padding: 2px 5px;
          box-shadow: none;
        }
        /* Event Horizon summary */
        .event-horizon-summary {
        margin: 10px 0 16px 0;
        padding: 14px 18px;
        background: #fff7f7;
        border: 1px solid #e1b5b5;
        border-left: 5px solid #b2182b;
        border-radius: 6px;
        }

        .event-horizon-summary-title {
        margin-bottom: 5px;
        color: #333333;
        font-size: 17px;
        font-weight: 700;
        }

        .event-horizon-summary-text {
        color: #333333;
        font-size: 15px;
        line-height: 1.55;
        }

        .event-horizon-highlight {
        color: #8b1e1e;
        font-size: 16px;
        font-weight: 800;
        }
        /* Permanent entrainment labels above map nodes */
        .entrainment-permanent-label {
        padding: 2px 5px !important;
        background: rgba(255, 255, 255, 0.92) !important;
        border: 1px solid #555555 !important;
        border-radius: 10px !important;
        box-shadow: none !important;
        color: #222222 !important;
        font-size: 12px !important;
        font-weight: 700 !important;
        }
        .combined-emulator-plots-box {
        margin-top: 14px;
          }

        .emulator-figure-title {
        margin: 4px 0 12px 0;
        color: #333333;
        font-size: 17px;
        font-weight: 700;
        }

        /* Exploration page: compact, accessible horizontal input layout. */
        .current-theme .exploration-intro {
          margin-bottom: 16px;
        }

        .current-theme .nav-tabs {
          margin-bottom: 14px;
        }

        .current-theme .exploration-action-row {
          display: flex;
          align-items: flex-end;
          justify-content: space-between;
          gap: 18px;
          padding: 2px 0 16px 0;
          margin-bottom: 16px;
          border-bottom: 1px solid #e3eaee;
        }

        .current-theme .exploration-scenario-field {
          flex: 1 1 420px;
          max-width: 620px;
        }

        .current-theme .exploration-scenario-field .form-group {
          margin-bottom: 0;
        }

        .current-theme .exploration-action-buttons {
          display: flex;
          align-items: center;
          justify-content: flex-end;
          gap: 10px;
          flex: 0 0 auto;
          padding-bottom: 1px;
        }

        .current-theme .exploration-action-buttons .btn {
          min-width: 150px;
          min-height: 36px;
          border-radius: 5px;
          font-weight: 600;
        }

        .current-theme .exploration-input-grid {
          display: grid;
          gap: 14px;
          width: 100%;
          align-items: stretch;
        }

        .current-theme .exploration-ptm-grid {
          grid-template-columns: repeat(6, minmax(0, 1fr));
        }

        .current-theme .exploration-eco-grid {
          grid-template-columns: repeat(4, minmax(0, 1fr));
        }

        .current-theme .exploration-input-card {
          position: relative;
          min-width: 0;
          padding: 12px 12px 10px 12px;
          background: #ffffff;
          border: 1px solid #d9e5e9;
          border-radius: 7px;
          box-shadow: 0 1px 3px rgba(0,0,0,.035);
        }

        .current-theme .exploration-input-card:hover {
          border-color: #aacbd2;
          box-shadow: 0 2px 7px rgba(0,0,0,.055);
        }

        .current-theme .exploration-input-card .form-group {
          margin-bottom: 0;
        }

        .current-theme .exploration-input-card label.control-label {
          display: block;
          min-height: 38px;
          margin-bottom: 6px;
          color: #34444c;
          font-size: 12.5px;
          line-height: 1.3;
          font-weight: 600;
        }

        .current-theme .exploration-input-card input.form-control,
        .current-theme .exploration-input-card select.form-control {
          width: 100%;
          height: 36px;
          border-radius: 4px;
        }

        .current-theme .exploration-input-card .radio {
          margin-top: 7px;
        }

        .current-theme .input-card-spacer {
          height: 22px;
        }

        /* Accessible hover/focus information control.
           Live date/flow notes render inside the popover instead of below inputs. */
        .current-theme .flow-info-hover {
          position: relative;
          display: flex;
          justify-content: flex-end;
          min-height: 22px;
          margin-bottom: 2px;
          z-index: 25;
        }

        .current-theme .flow-info-trigger {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 22px;
          height: 22px;
          color: #287d93;
          background: #eef8fa;
          border: 1px solid #b9dbe2;
          border-radius: 50%;
          font-size: 13px;
          cursor: help;
          outline: none;
        }

        .current-theme .flow-info-trigger:hover,
        .current-theme .flow-info-trigger:focus {
          color: #075f6d;
          background: #e2f3f6;
          border-color: #82bcc7;
          box-shadow: 0 0 0 2px rgba(10,126,140,.12);
        }

        .current-theme .flow-info-popover {
          display: none;
          position: absolute;
          top: 27px;
          right: 0;
          width: 330px;
          max-width: min(330px, calc(100vw - 60px));
          padding: 12px 14px;
          background: #ffffff;
          color: #33454d;
          border: 1px solid #c9d9de;
          border-radius: 7px;
          box-shadow: 0 5px 18px rgba(0,0,0,.14);
          font-size: 12.5px;
          line-height: 1.45;
          text-align: left;
          white-space: normal;
        }

        .current-theme .flow-info-hover:hover .flow-info-popover,
        .current-theme .flow-info-hover:focus-within .flow-info-popover {
          display: block;
        }

        .current-theme .flow-info-popover hr {
          margin: 9px 0;
          border-top: 1px solid #e4ecef;
        }

        .current-theme .flow-info-popover .observed-flow-note {
          margin: 0;
          padding: 0;
          background: transparent;
          border: 0;
          color: #40525a;
          font-size: 12.5px;
          line-height: 1.45;
        }

        .current-theme .flow-info-popover .observed-flow-note a {
          font-weight: 600;
        }

        /* Keep information popovers fully visible, including the leftmost EXP card. */
        .current-theme .exploration-input-grid,
        .current-theme .exploration-input-card,
        .current-theme .box-body {
          overflow: visible;
        }

        .current-theme .exploration-input-card:hover,
        .current-theme .exploration-input-card:focus-within {
          z-index: 60;
        }

        .current-theme .exploration-input-card:first-child .flow-info-popover {
          left: 0;
          right: auto;
        }

        /* Keep section information icons immediately beside their headings. */
        .current-theme .section-heading-with-info {
          display: flex;
          align-items: center;
          justify-content: flex-start;
          gap: 7px;
          margin: 4px 0 10px 0;
        }

        .current-theme .section-heading-with-info h4 {
          margin: 0;
        }

        .current-theme .section-heading-with-info .flow-info-hover {
          display: inline-flex;
          flex: 0 0 auto;
          min-height: 0;
          margin: 0;
          z-index: 70;
        }

        .current-theme .section-heading-with-info .flow-info-popover {
          left: 0;
          right: auto;
        }

        /* Compact workflow guidance for Exploration. */
        .current-theme .exploration-workflow-guide {
          display: flex;
          align-items: flex-start;
          gap: 9px;
          margin: 0 0 14px 0;
          padding: 10px 12px;
          background: #f4fafb;
          border: 1px solid #cfe3e7;
          border-left: 4px solid #0a7e8c;
          border-radius: 6px;
          color: #344a52;
          font-size: 13px;
          line-height: 1.45;
        }

        .current-theme .exploration-workflow-guide > i {
          margin-top: 2px;
          color: #0a7e8c;
        }

                /* Full-width input and result sections on Exploration keep the page linear. */
        .current-theme .emulator-top-row {
          display: block;
        }

        .current-theme .emulator-top-row > [class*='col-'] {
          display: block;
          width: 100%;
          float: none;
        }

        .current-theme .emulator-top-row > [class*='col-'] > .box {
          width: 100%;
          height: auto;
          margin-bottom: 16px;
        }

        @media (max-width: 1350px) {
          .current-theme .exploration-ptm-grid {
            grid-template-columns: repeat(3, minmax(0, 1fr));
          }
        }

        @media (max-width: 1050px) {
          .current-theme .exploration-eco-grid {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }

          .current-theme .exploration-action-row {
            align-items: stretch;
            flex-direction: column;
          }

          .current-theme .exploration-scenario-field {
            max-width: none;
          }

          .current-theme .exploration-action-buttons {
            justify-content: flex-start;
          }
        }

        @media (max-width: 760px) {
          .current-theme .exploration-ptm-grid,
          .current-theme .exploration-eco-grid {
            grid-template-columns: 1fr;
          }

          .current-theme .exploration-action-buttons {
            flex-direction: column;
            align-items: stretch;
          }

          .current-theme .exploration-action-buttons .btn {
            width: 100%;
          }

          .current-theme .flow-info-popover {
            right: auto;
            left: 0;
          }
        }

        /* Loading state stays inside the results panel. */
        .model-results-box .box-body {
          position: relative;
          min-height: 220px;
        }
        .model-results-loading {
          display: none;
          position: absolute;
          z-index: 2000;
          inset: 0;
          align-items: center;
          justify-content: center;
          background: rgba(255,255,255,0.88);
          border-radius: 4px;
        }
        .model-run-requested .model-results-loading {
          display: flex;
        }
        .model-results-loading-card {
          padding: 16px 20px;
          background: #ffffff;
          border: 1px solid #d9e4ea;
          border-radius: 7px;
          box-shadow: 0 2px 10px rgba(0,0,0,.08);
          color: #31515f;
          font-size: 15px;
          font-weight: 600;
        }
    
        .weekly-input-method-row{
          display:flex;
          align-items:flex-start;
          gap:32px;
          flex-wrap:wrap;
        }
        .weekly-input-method-primary,
        .weekly-input-method-source{
          flex:1 1 360px;
          min-width:280px;
        }
        .weekly-archive-selection{
          margin-top:8px;
          display:flex;
          gap:24px;
          align-items:flex-end;
          flex-wrap:wrap;
        }
        .weekly-archive-selection .form-group{
          min-width:280px;
          margin-bottom:0;
        }
        .weekly-static-value{
          background:#f7fafb !important;
          cursor:default;
          min-height:34px;
          display:flex;
          align-items:center;
          font-weight:600;
        }
        .weekly-action-row{
          margin-bottom:18px;
        }
      
        /* Weekly Assessments: mirror Exploration's readable visual system. */
        .forecast-theme .exploration-workflow-guide {
          display:flex;
          align-items:flex-start;
          gap:9px;
          margin:0 0 14px 0;
          padding:10px 12px;
          background:#f4fafb;
          border:1px solid #cfe3e7;
          border-left:4px solid #0a7e8c;
          border-radius:6px;
          color:#344a52;
          font-size:13px;
          line-height:1.45;
        }
        .forecast-theme .exploration-workflow-guide > i {
          margin-top:2px;
          color:#0a7e8c;
        }
        .forecast-theme .exploration-action-row {
          display:flex;
          gap:16px;
          align-items:flex-end;
          justify-content:space-between;
          flex-wrap:wrap;
        }
        .forecast-theme .exploration-scenario-field {
          flex:1 1 360px;
          max-width:520px;
        }
        .forecast-theme .exploration-action-buttons {
          display:flex;
          gap:10px;
          align-items:center;
          flex-wrap:wrap;
        }
        .forecast-theme .weekly-input-grid {
          display:grid;
          gap:12px;
          align-items:stretch;
          margin-top:12px;
        }
        .forecast-theme .exploration-ptm-grid.weekly-input-grid {
          grid-template-columns:repeat(3, minmax(0, 1fr));
        }
        .forecast-theme .exploration-eco-grid.weekly-input-grid {
          grid-template-columns:repeat(4, minmax(0, 1fr));
        }
        .forecast-theme .exploration-input-card {
          position:relative;
          min-width:0;
          padding:14px 14px 12px;
          background:#ffffff;
          border:1px solid #e1e7eb;
          border-radius:7px;
          box-shadow:0 1px 3px rgba(0,0,0,.04);
        }
        .forecast-theme .exploration-input-card label {
          display:block;
          font-weight:600;
          margin-bottom:7px;
          line-height:1.35;
        }
        .forecast-theme .flow-info-hover {
          position:relative;
          display:inline-flex;
          z-index:80;
        }
        .forecast-theme .flow-info-trigger {
          display:inline-flex;
          align-items:center;
          justify-content:center;
          width:24px;
          height:24px;
          color:#0a7e8c;
          cursor:help;
          border-radius:50%;
          outline:none;
        }
        .forecast-theme .flow-info-popover {
          display:none;
          position:absolute;
          z-index:3000;
          top:28px;
          left:0;
          width:320px;
          max-width:min(320px, 78vw);
          padding:10px 12px;
          background:#ffffff;
          border:1px solid #cfd9df;
          border-radius:6px;
          box-shadow:0 4px 14px rgba(0,0,0,.12);
          color:#344a52;
          font-size:12px;
          line-height:1.45;
        }
        .forecast-theme .flow-info-hover:hover .flow-info-popover,
        .forecast-theme .flow-info-hover:focus-within .flow-info-popover {
          display:block;
        }
        .forecast-theme .section-heading-with-info {
          display:flex;
          align-items:center;
          justify-content:flex-start;
          gap:7px;
          margin:4px 0 10px 0;
        }
        .forecast-theme .section-heading-with-info h4 {
          margin:0;
        }
        .forecast-theme .section-heading-with-info .flow-info-hover {
          flex:0 0 auto;
          margin:0;
        }
        .forecast-theme .weekly-inputs-info-row {
          display:flex;
          justify-content:flex-start;
          margin:0 0 8px 0;
        }
        .map-window-heading {
          margin-top:12px !important;
          margin-bottom:2px !important;
        }
        .map-window-heading h4 {
          font-size:14px;
          font-weight:700;
        }
        @media (max-width:1100px) {
          .forecast-theme .exploration-ptm-grid.weekly-input-grid {
            grid-template-columns:repeat(2, minmax(0, 1fr));
          }
          .forecast-theme .exploration-eco-grid.weekly-input-grid {
            grid-template-columns:repeat(2, minmax(0, 1fr));
          }
        }
        @media (max-width:760px) {
          .forecast-theme .exploration-ptm-grid.weekly-input-grid,
          .forecast-theme .exploration-eco-grid.weekly-input-grid {
            grid-template-columns:1fr;
          }
          .forecast-theme .exploration-action-buttons {
            width:100%;
          }
        }
        .compare-input-grid {display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:10px 14px;align-items:start;margin-bottom:14px;}
        .compare-input-grid .form-group {margin-bottom:6px;}
        .comparison-controls-row {
          display:grid;
          grid-template-columns:minmax(220px,1.05fr) minmax(180px,.75fr) minmax(360px,2fr) auto;
          gap:14px 18px;
          align-items:end;
          width:100%;
        }
        .omri-controls-row {
          grid-template-columns:minmax(260px,1.25fr) minmax(220px,1fr) minmax(190px,.75fr) auto auto;
        }
        .comparison-control-item {min-width:0;}
        .comparison-runs-item .checkbox-inline {
          margin-left:0;
          margin-right:14px;
          margin-bottom:6px;
        }
        .comparison-download-item,
        .comparison-action-item {
          display:flex;
          align-items:flex-end;
          padding-bottom:15px;
        }
        .comparison-download-item .btn,
        .comparison-action-item .btn {white-space:normal;}
        .comparison-section-heading {
          justify-content:flex-start !important;
          gap:7px !important;
          margin-bottom:10px !important;
        }
        .comparison-section-heading h4 {margin:0 !important;}

        /* Scenario Comparison info icons: keep descriptive text hidden until hover/focus. */
        .content-wrapper .flow-info-hover,
        .tab-content .flow-info-hover {
          position:relative;
          display:inline-flex;
          align-items:center;
          z-index:200;
        }

        .content-wrapper .flow-info-trigger,
        .tab-content .flow-info-trigger {
          display:inline-flex;
          align-items:center;
          justify-content:center;
          width:22px;
          height:22px;
          color:#0a7e8c;
          cursor:help;
          border-radius:50%;
          outline:none;
          line-height:1;
        }

        .content-wrapper .flow-info-popover,
        .tab-content .flow-info-popover {
          display:none !important;
          position:absolute;
          z-index:5000;
          top:26px;
          left:0;
          width:340px;
          max-width:min(340px, 80vw);
          padding:10px 12px;
          background:#ffffff;
          border:1px solid #cfd9df;
          border-radius:6px;
          box-shadow:0 5px 16px rgba(0,0,0,.16);
          color:#344a52;
          font-size:12px;
          font-weight:400;
          line-height:1.45;
          text-align:left;
          white-space:normal;
        }

        .content-wrapper .flow-info-hover:hover .flow-info-popover,
        .content-wrapper .flow-info-hover:focus-within .flow-info-popover,
        .tab-content .flow-info-hover:hover .flow-info-popover,
        .tab-content .flow-info-hover:focus-within .flow-info-popover {
          display:block !important;
        }

        .comparison-section-heading,
        .section-heading-with-info {
          display:flex;
          align-items:center;
          justify-content:flex-start;
          gap:7px;
        }

        .comparison-section-heading .flow-info-hover,
        .section-heading-with-info .flow-info-hover {
          flex:0 0 auto;
          margin-left:0 !important;
        }
        .comparison-global-guide {
          margin:0 0 14px 0 !important;
        }
        .comparison-section-heading {
          padding:4px 0 2px 0;
        }
        .comparison-section-heading .flow-info-hover {
          margin-left:0 !important;
          flex:0 0 auto;
        }
        @media (max-width:1200px) {
          .comparison-controls-row,
          .omri-controls-row {
            grid-template-columns:repeat(2,minmax(0,1fr));
          }
          .comparison-runs-item {grid-column:1 / -1;}
        }
        @media (max-width:760px) {
          .comparison-controls-row,
          .omri-controls-row {grid-template-columns:1fr;}
          .comparison-runs-item {grid-column:auto;}
          .comparison-download-item,
          .comparison-action-item {padding-bottom:0;}
        }
        @media (max-width:900px) {.compare-input-grid {grid-template-columns:1fr;}}
")),
      tags$script(HTML("
      function resizeLeafletMaps() {
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 250);
    
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 750);
      }
    
      $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
        resizeLeafletMaps();
      });
    
      $(document).on('shown.bs.tab', 'a[data-toggle=\"pill\"]', function() {
        resizeLeafletMaps();
      });
    
      $(document).on('click', '.sidebar-menu a', function() {
        resizeLeafletMaps();
      });

      var modelRunWaiting = false;
      var modelRunBecameBusy = false;

      $(function() {
        $('body').tooltip({
          selector: '.flow-info-icon',
          container: 'body'
        });
      });

      $(document).on('click', '.model-run-button', function() {
        modelRunWaiting = true;
        modelRunBecameBusy = false;
        $('body').addClass('model-run-requested');
      });

      $(document).on('shiny:busy', function() {
        if (modelRunWaiting) {
          modelRunBecameBusy = true;
          $('body').addClass('model-run-requested');
        }
      });

      $(document).on('shiny:idle', function() {
        if (modelRunWaiting && modelRunBecameBusy) {
          setTimeout(function() {
            $('body').removeClass('model-run-requested');
            modelRunWaiting = false;
            modelRunBecameBusy = false;
          }, 150);
        }
      });
    "))
    ),
    
    tabItems(
      # -----------------------------
      # About
      # -----------------------------
      tabItem(
        tabName = "about",
        div(style = "width: 100%;height: calc(100vh - 100px);overflow-x: auto;overflow-y: auto;box-sizing: border-box;",
            div(style = "width: 1100px;min-width: 1100px;margin: 0 auto;box-sizing: border-box;",
                fluidRow(
                  box(
                    width = 12,
                    height = 300,
                    solidHeader = FALSE,
                    div(
                      style = "display: flex; align-items: center;",
                      tags$img(src = "logo.png", height = "160px"),
                      
                      div(
                        style = "margin-left: 30px;margin-right:50px;",
                        
                        h1("*DRAFT Entrainment Dashboard"),
                        h4("Version:",
                           tags$code(style = "margin-left:20px",
                                     paste0("  ", app_version, "  ")
                           ),
                           tags$a(style = "font-style:italic;margin-left:20px;",
                                  "[Release Notes]",
                                  href = "#release-notes"
                           )
                        ),
                        h4("Date Last Updated:", weight = "bold", 
                           tags$b("2026-12-01")
                        ),
                        
                        
                        tags$hr(),
                        
                        
                        h5(
                          style = "text-align: justify;",
                          tags$b("Data Refresh Schedule: "),
                          paste(
                            "Default flow values for observed conditions will be retrieved daily",
                            "through APIs from public sources and updated based on the trailing",
                            "7-day average flow. Data used by the Weekly Assessments page will",
                            "be updated weekly during the OMR entrainment management season.",
                            "For additional information on data sources, please go to the",
                            "Data Access page."
                          )
                        )
                      )
                    )
                  ),
                  fluidRow(
                    box(
                      width = 12,
                      height = 2000,
                      div(style = "margin-left: 60px;margin-right:120px",
                          h2("Overview"),
                          h5(
                            style = "text-align: justify;",
                            
                            "This ShinyApp makes forecast and/or presents hindcast results of particle entrainment and hydraulic footprint metrics within the Sacramento-San Joaquin Delta based on either 7-day or 30-day average flows. The observed conditions simulations and predictions are used for estimating entrainment and hydraulic footprint metrics based on actual conditions, with the option for users to modify flow variables to user-defined values. The forecast conditions simulations are used for estimating entrainment and hydraulic footprint metrics based on previously forecasted conditions during the OMR entrainment management season. The scenario comparison allows users to compare particle entrainment and hydraulic footprint metrics for different scenarios generated using either observed conditions or forecast conditions.",
                            
                            tags$br(),
                            tags$br(),
                            
                            "This interactive application is designed based on machine learning models developed by the Contra Costa Water District's (CCWD) ",
                            
                            tags$a(
                              "hydraulic footprint project",
                              href = "https://github.com/cchang-ccwater/CCWD_Hydraulic_Footprints",
                              target = "_blank",
                              rel = "noopener noreferrer"
                            ),
                            
                            ". Further details on the model development, original training datasets, and comparative evaluation of model results are available in the CCWD report ",
                            tags$sup(
                              tags$a(
                                "(1)",
                                href = "#ref-1"
                              )
                            ),
                            "."
                          ),
                          
                          h3("Author & Contact Information"),
                          gt_output("about_info_table"),
                          h3("Technical Guidelines:"),
                          div(style = "margin-left: 60px;",
                              h4("Visual Identity Compliance:"),
                              h5(style = "text-align: justify;", "The application framework is built using the ", 
                                 tags$code("shiny"), 
                                 "and ", 
                                 tags$code("shinydashboard"),
                                 "packages. Figures are generated using the", 
                                 tags$code("ggplot"),
                                 "and ", 
                                 tags$code("viridis"),
                                 "packages. Additional packages including",
                                 tags$code("leaflet"),
                                 ", ", 
                                 tags$code("sf"),
                                 ", ",                        
                                 tags$code("lwgeom"),  
                                 ", and ", 
                                 tags$code("dplyr"),                                 
                                 ", are used for interactive mapping, spatial data processing, and geometric calculations. Most interface text uses font sizes between 13px and 16px to ensure good readability under normal viewing conditions. The application primarily uses the Segoe UI font family, with,",
                                 tags$b("Regular", style = "font-family: Segoe UI"),
                                 tags$b(","),
                                 tags$b("Semibold", style = "font-family: Segoe UI Semibold"),
                                 tags$b(","),
                                 tags$b("Italic", style = "font-style: italic"),
                                 "styles applied where appropriate. Additional accessibility features have been implemented or are planned for future releases to further improve accessibility:"
                              ),
                              h5(style = "margin-left: 60px; text-align: justify;","	- Adjustable text size: A text size adjustment option is available, allowing users to change the font size from small to large to improve readability. The default text size is Medium."),
                              p(style = "text-align: center;",
                                tags$span(style = "font-size: 0.83em;margin-right:20px;","Small"),
                                tags$span(style = "font-size: 1.17em;margin-right:20px;","Medium"),
                                tags$span(style = "font-size: 2.00em;","Large")
                              ),
                              h5(style = "margin-left: 60px; text-align: justify;","  - Colorblind-friendly color palette: A color palette option is available, allowing users to switch between different plot color schemes. By default, the application uses the Viridis color palette, which is designed to be perceptually uniform and accessible for users with color vision deficiency. Users may also switch to a high-contrast color palette to enhance visibility."),
                              h4("Browser Compatibility:"),
                              h5(
                                "This app can work on Edge, Chrome, Safari, and Firefox, as tested through version ",
                                tags$code(paste0("v", app_version)),
                                "."
                              ),
                              h4("Performance Standards: "),
                              h5(style = "text-align: justify;","The application provides reasonable load times under normal operating conditions. The ECO-PTM Emulator page typically loads in less than 1 second; the PTM Emulator page in approximately 2-3 seconds; and the Event Horizon page in approximately 7-9 seconds because it loads Leaflet maps, geo-spatial files, and multiple plots. Standard weekly prediction tasks are generally completed almost immediately, while large prediction requests involving long time series and many input features, e.g., 190k records, may require substantially more processing and rendering time."),
                              h5(style = "text-align: justify;","Concurrent-user capacity depends on the deployment environment, including available CPU, memory, and the number of Shiny worker processes. The application is expected to support multiple users performing normal navigation, data exploration, and standard predictions, although several simultaneous computationally intensive prediction requests may increase response times. Final concurrent-user capacity should therefore be confirmed through load testing in the production environment."),
                              h4("Mobile Responsiveness:"),
                              h5("This application is usable also on mobile devices.")
                              
                          ),
                          h3(
                            id = "reference",
                            "References"
                          ),
                          
                          div(
                            style = "margin-left: 60px;",
                            
                            h5(
                              id = "ref-1",
                              style = "text-align: justify; line-height: 1.6;",
                              
                              "Contra Costa Water District. (2026). ",
                              
                              tags$em(
                                paste(
                                  "Intuitive Quantitative Metrics for Rapid Assessment",
                                  "of Entrainment Risks and Salmonid Responses in The Delta."
                                )
                              ),
                              
                              " ",
                              
                              tags$a(
                                "[View PDF]",
                                href = "20260512_CCWD_Entrainment_TM_Final.pdf",
                                target = "_blank",
                                rel = "noopener noreferrer"
                              )
                            )
                          ),
                          h3(
                            id = "release-notes",
                            "Release Notes:"
                          ),
                          
                          div(
                            style = "margin-left: 60px;",
                            
                            tags$div(
                              style = paste(
                                "margin-bottom: 24px;",
                                "padding-left: 12px;",
                                "border-left: 4px solid #3c8dbc;"
                              ),
                              
                              h4(
                                style = paste(
                                  "margin-bottom: 8px;",
                                  "font-weight: 600;"
                                ),
                                
                                "Version ",
                                
                                tags$code(
                                  "1.1.0",
                                  style = paste(
                                    "font-weight: 400;",
                                    "margin-left: 10px;"
                                  )
                                ),
                                
                                tags$small(
                                  style = paste(
                                    "margin-left: 10px;",
                                    "color: #777777;",
                                    "font-weight: normal;"
                                  ),
                                  "December 1, 2026"
                                )
                              ),
                              
                              h5(
                                style = paste(
                                  "line-height: 1.6;",
                                  "margin-top: 6px;",
                                  "margin-bottom: 6px;",
                                  "margin-left: 30px;",
                                  "font-weight: normal;"
                                ),
                                tags$b("-Added: "),
                                "First installment of Shiny app"
                              ),
                              
                              h5(
                                style = paste(
                                  "line-height: 1.6;",
                                  "margin-top: 6px;",
                                  "margin-bottom: 6px;",
                                  "margin-left: 30px;",
                                  "font-weight: normal;"
                                ),
                                tags$b("-Improved: "),
                                "NA"
                              ),
                              
                              h5(
                                style = paste(
                                  "line-height: 1.6;",
                                  "margin-top: 6px;",
                                  "margin-bottom: 6px;",
                                  "margin-left: 30px;",
                                  "font-weight: normal;"
                                ),
                                tags$b("-Fixed: "),
                                "NA"
                              )
                            )
                          )
                      )
                    )
                  )
                )
            )
        )
      ),
      
      run_page("current", "Observed Conditions", "current-theme"),
      run_page("forecast", "Forecast Conditions", "forecast-theme"),
      
      tabItem(
        tabName = "comparison",
        
        tags$div(
          class = "exploration-workflow-guide comparison-global-guide",
          icon("info-circle"),
          tags$span(
            tags$b("How to use this page: "),
            "Select a model and two or more compatible saved runs from this Shiny session. Review the comparison chart, spatial map, and detailed table below. PTM and Event Horizon runs support spatial comparison; ECO-PTM results are compared in the chart and table."
          )
        ),
        
        tabsetPanel(
          id = "comparison_sections",
          
          tabPanel(
            "Library of runs",
            
            fluidRow(
              box(
                width = 12,
                title = "Comparison Controls",
                status = "primary",
                solidHeader = TRUE,
                div(
                  class = "comparison-controls-row",
                  div(
                    class = "comparison-control-item",
                    selectInput(
                      "comparison_model",
                      "Model:",
                      choices = c(
                        "PTM Emulator 7-Day Entrainment",
                        "PTM Emulator 30-Day Entrainment",
                        "ECO-PTM Emulator Survival",
                        "ECO-PTM Emulator Interior Routing",
                        "Event Horizon"
                      )
                    )
                  ),
                  div(
                    class = "comparison-control-item",
                    conditionalPanel(
                      condition = "input.comparison_model == 'PTM Emulator 7-Day Entrainment' || input.comparison_model == 'PTM Emulator 30-Day Entrainment'",
                      selectInput(
                        "comparison_map_threshold",
                        "Map Risk Threshold (%):",
                        choices = seq(15, 80, by = 5),
                        selected = 25
                      )
                    )
                  ),
                  div(
                    class = "comparison-control-item comparison-runs-item",
                    uiOutput("comparison_run_selector")
                  ),
                  div(
                    class = "comparison-control-item comparison-download-item",
                    downloadButton(
                      "download_comparison",
                      "Download Comparison (CSV)",
                      class = "btn-primary"
                    )
                  )
                )
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                status = "primary",
                solidHeader = FALSE,
                tags$div(
                  class = "section-heading-with-info comparison-section-heading",
                  tags$h4("Scenario Comparison Chart"),
                  tags$div(
                    class = "flow-info-hover",
                    tags$span(
                      class = "flow-info-trigger",
                      tabindex = "0",
                      role = "button",
                      `aria-label` = "Scenario Comparison Chart information",
                      icon("info-circle")
                    ),
                    tags$div(
                      class = "flow-info-popover",
                      tags$div(
                        class = "flow-info-static",
                        "Compare compatible model runs saved during the current Shiny session. For PTM Emulator models, the chart compares predicted entrainment by DSM2 node. For Event Horizon, the chart compares predicted river-mile distance across selected scenarios."
                      )
                    )
                  )
                ),
                plotlyOutput("comparison_plot", height = 700)
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                status = "primary",
                solidHeader = FALSE,
                tags$div(
                  class = "section-heading-with-info comparison-section-heading",
                  tags$h4("Scenario Spatial Comparison Map"),
                  tags$div(
                    class = "flow-info-hover",
                    tags$span(
                      class = "flow-info-trigger",
                      tabindex = "0",
                      role = "button",
                      `aria-label` = "Scenario Spatial Comparison Map information",
                      icon("info-circle")
                    ),
                    tags$div(
                      class = "flow-info-popover",
                      tags$div(
                        class = "flow-info-static",
                        "This map compares spatial outputs for the selected scenarios. For PTM Emulator comparisons, high- and low-risk zones are overlaid for each selected scenario. For Event Horizon comparisons, the predicted channel-path reaches are overlaid in different colors."
                      )
                    )
                  )
                ),
                leafletOutput("comparison_spatial_map", height = 650)
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                status = "primary",
                solidHeader = FALSE,
                tags$div(
                  class = "section-heading-with-info comparison-section-heading",
                  tags$h4("Detailed Scenario Comparison Results"),
                  tags$div(
                    class = "flow-info-hover",
                    tags$span(
                      class = "flow-info-trigger",
                      tabindex = "0",
                      role = "button",
                      `aria-label` = "Detailed Scenario Comparison Results information",
                      icon("info-circle")
                    ),
                    tags$div(
                      class = "flow-info-popover",
                      tags$div(
                        class = "flow-info-static",
                        "Detailed tabular results for the selected compatible runs. Use this table to inspect node-level PTM predictions, Event Horizon distances, model inputs, archive metadata, and saved-run identifiers."
                      )
                    )
                  )
                ),
                div(
                  class = "wide-table-scroll",
                  tableOutput("comparison_table")
                )
              )
            )
          ),
          
          tabPanel(
            "OMRI Comparison",
            
            fluidRow(
              box(
                width = 12,
                title = "OMRI Comparison Controls",
                status = "primary",
                solidHeader = TRUE,
                div(
                  class = "comparison-controls-row omri-controls-row",
                  div(
                    class = "comparison-control-item",
                    uiOutput("omri_archive_dates_ui")
                  ),
                  div(
                    class = "comparison-control-item",
                    selectInput(
                      "omri_comparison_model",
                      "Forecast Model:",
                      choices = c(
                        "PTM Emulator 7-Day Entrainment",
                        "Event Horizon"
                      ),
                      selected = "PTM Emulator 7-Day Entrainment"
                    )
                  ),
                  div(
                    class = "comparison-control-item",
                    conditionalPanel(
                      condition = "input.omri_comparison_model == 'PTM Emulator 7-Day Entrainment'",
                      selectInput(
                        "omri_comparison_map_threshold",
                        "Map Risk Threshold (%):",
                        choices = seq(15, 80, by = 5),
                        selected = 25
                      )
                    ),
                    conditionalPanel(
                      condition = "input.omri_comparison_model == 'Event Horizon'",
                      selectInput(
                        "omri_comparison_risk",
                        "Event Horizon Risk Level (%):",
                        choices = seq(15, 80, by = 5),
                        selected = 25
                      )
                    )
                  ),
                  div(
                    class = "comparison-control-item comparison-action-item",
                    actionButton(
                      "build_omri_comparison",
                      "Run All Available OMRI Scenarios",
                      icon = icon("play"),
                      class = "btn-success"
                    )
                  ),
                  div(
                    class = "comparison-control-item comparison-download-item",
                    downloadButton(
                      "download_omri_comparison",
                      "Download OMRI Comparison (CSV)",
                      class = "btn-primary"
                    )
                  )
                )
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                status = "primary",
                solidHeader = FALSE,
                tags$div(
                  class = "section-heading-with-info comparison-section-heading",
                  tags$h4("OMRI Forecast Scenario Comparison Chart"),
                  tags$div(
                    class = "flow-info-hover",
                    tags$span(
                      class = "flow-info-trigger",
                      tabindex = "0",
                      role = "button",
                      `aria-label` = "OMRI Forecast Scenario Comparison Chart information",
                      icon("info-circle")
                    ),
                    tags$div(
                      class = "flow-info-popover",
                      tags$div(
                        class = "flow-info-static",
                        "For every selected archive date, the app reads each available OMRI CSV, calculates the seven-day forecast average, carries forward the latest measured XGEO, and compares the forecast emulator results."
                      )
                    )
                  )
                ),
                uiOutput("omri_comparison_status"),
                plotlyOutput("omri_comparison_plot", height = 750)
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                status = "primary",
                solidHeader = FALSE,
                tags$div(
                  class = "section-heading-with-info comparison-section-heading",
                  tags$h4("OMRI Forecast Scenario Spatial Comparison Map"),
                  tags$div(
                    class = "flow-info-hover",
                    tags$span(
                      class = "flow-info-trigger",
                      tabindex = "0",
                      role = "button",
                      `aria-label` = "OMRI Forecast Scenario Spatial Comparison Map information",
                      icon("info-circle")
                    ),
                    tags$div(
                      class = "flow-info-popover",
                      tags$div(
                        class = "flow-info-static",
                        "This map compares spatial outputs for all available OMRI forecast scenarios from the selected archive date(s). For PTM Emulator comparisons, high- and low-risk zones are overlaid by OMRI scenario. For Event Horizon comparisons, the predicted channel-path reaches are overlaid in different colors."
                      )
                    )
                  )
                ),
                leafletOutput("omri_comparison_spatial_map", height = 650)
              )
            ),
            
            fluidRow(
              box(
                width = 12,
                status = "primary",
                solidHeader = FALSE,
                tags$div(
                  class = "section-heading-with-info comparison-section-heading",
                  tags$h4("Detailed OMRI Forecast Scenario Results"),
                  tags$div(
                    class = "flow-info-hover",
                    tags$span(
                      class = "flow-info-trigger",
                      tabindex = "0",
                      role = "button",
                      `aria-label` = "Detailed OMRI Forecast Scenario Results information",
                      icon("info-circle")
                    ),
                    tags$div(
                      class = "flow-info-popover",
                      tags$div(
                        class = "flow-info-static",
                        "Detailed tabular results for the selected archive date(s) and OMRI scenario(s)."
                      )
                    )
                  )
                ),
                div(
                  class = "wide-table-scroll",
                  tableOutput("omri_comparison_table")
                )
              )
            )
          ),
          
          tabPanel(
            "Compare 2 Scenarios",
            fluidRow(
              box(
                width = 6, title = "Scenario 1", status = "warning", solidHeader = TRUE,
                tags$div(
                  class = "section-heading-with-info",
                  tags$h4("Inputs"),
                  tags$div(
                    class = "flow-info-hover",
                    tags$span(class = "flow-info-trigger", tabindex = "0", role = "button", `aria-label` = "Scenario 1 input information", icon("info-circle")),
                    tags$div(class = "flow-info-popover", tags$div(class = "flow-info-static", "Scenario 1 is initialized with the same latest observed 7-day average flows used by the Exploration page. Edit any input to create a user-defined scenario, then click Run Scenario 1."))
                  )
                ),
                textInput(
                  "compare_s1_name",
                  "Scenario Name:",
                  "COMPARE_01"
                ),
                div(class = "compare-input-grid",
                    textInput("compare_s1_exp", "EXP: Combined CVP and SWP Export (cfs):", "6,000"),
                    textInput("compare_s1_ver", "VER: San Joaquin River Flow at Vernalis (cfs):", "3,000"),
                    textInput("compare_s1_sac", "SAC: Sacramento River Flow at Freeport (cfs):", "18,000"),
                    textInput("compare_s1_east", "EAST: East-Side River Flow (cfs):", "1,500"),
                    textInput("compare_s1_xgeo", "XGEO: Interior Delta Flow (cfs):", "3,000"),
                    textInput("compare_s1_yol", "YOL: Yolo Bypass Flow (cfs):", "1,000"),
                    textInput("compare_s1_moke", "MOKE: Mokelumne Flow (cfs):", "800"),
                    radioButtons("compare_s1_dcc", "Delta Cross Channel Status:", choices = c("Closed" = 0, "Open" = 1), selected = 0, inline = TRUE),
                    selectInput("compare_s1_risk", "Entrainment / Event Horizon Risk Threshold (%):", choices = seq(15,80,by=5), selected = 25)
                ),
                actionButton("run_compare_s1", "Run Scenario 1", icon = icon("play"), class = "btn-success", width = "100%")
              ),
              box(
                width = 6, title = "Scenario 2", status = "warning", solidHeader = TRUE,
                tags$div(
                  class = "section-heading-with-info",
                  tags$h4("Inputs"),
                  tags$div(
                    class = "flow-info-hover",
                    tags$span(class = "flow-info-trigger", tabindex = "0", role = "button", `aria-label` = "Scenario 2 input information", icon("info-circle")),
                    tags$div(class = "flow-info-popover", tags$div(class = "flow-info-static", "Scenario 2 starts from the same current-condition baseline as Scenario 1. Change only the variables you want to test, then click Run Scenario 2."))
                  )
                ),
                textInput(
                  "compare_s2_name",
                  "Scenario Name:",
                  "COMPARE_02"
                ),
                div(class = "compare-input-grid",
                    textInput("compare_s2_exp", "EXP: Combined CVP and SWP Export (cfs):", "6,000"),
                    textInput("compare_s2_ver", "VER: San Joaquin River Flow at Vernalis (cfs):", "3,000"),
                    textInput("compare_s2_sac", "SAC: Sacramento River Flow at Freeport (cfs):", "18,000"),
                    textInput("compare_s2_east", "EAST: East-Side River Flow (cfs):", "1,500"),
                    textInput("compare_s2_xgeo", "XGEO: Interior Delta Flow (cfs):", "3,000"),
                    textInput("compare_s2_yol", "YOL: Yolo Bypass Flow (cfs):", "1,000"),
                    textInput("compare_s2_moke", "MOKE: Mokelumne Flow (cfs):", "800"),
                    radioButtons("compare_s2_dcc", "Delta Cross Channel Status:", choices = c("Closed" = 0, "Open" = 1), selected = 0, inline = TRUE),
                    selectInput("compare_s2_risk", "Entrainment / Event Horizon Risk Threshold (%):", choices = seq(15,80,by=5), selected = 25)
                ),
                actionButton("run_compare_s2", "Run Scenario 2", icon = icon("play"), class = "btn-success", width = "100%")
              )
            ),
            fluidRow(
              box(width=12, title="Comparison Controls", status="primary", solidHeader=TRUE,
                  tags$div(class="section-heading-with-info", tags$h4("Select Output"),
                           tags$div(class="flow-info-hover", tags$span(class="flow-info-trigger", tabindex="0", role="button", `aria-label`="Comparison output information", icon("info-circle")),
                                    tags$div(class="flow-info-popover", tags$div(class="flow-info-static", "After both scenarios have been run, choose which emulator output to compare. PTM and Event Horizon outputs include a spatial comparison map; ECO-PTM outputs are compared in the chart and table."))
                           )
                  ),
                  selectInput("compare_two_model", "Model:", choices=c("PTM Emulator 7-Day Entrainment","PTM Emulator 30-Day Entrainment","ECO-PTM Emulator Survival","ECO-PTM Emulator Interior Routing","Event Horizon"), selected="PTM Emulator 7-Day Entrainment"),
                  conditionalPanel(condition="input.compare_two_model == 'PTM Emulator 7-Day Entrainment' || input.compare_two_model == 'PTM Emulator 30-Day Entrainment'",
                                   selectInput("compare_two_map_threshold", "Map Risk Threshold (%):", choices=seq(15,80,by=5), selected=25)
                  )
              )
            ),
            fluidRow(box(width=12,status="primary",solidHeader=FALSE,
                         tags$div(class="section-heading-with-info", tags$h4("Scenario Comparison Chart"),
                                  tags$div(class="flow-info-hover", tags$span(class="flow-info-trigger", tabindex="0", role="button", `aria-label`="Comparison chart information", icon("info-circle")),
                                           tags$div(class="flow-info-popover", tags$div(class="flow-info-static", "The chart compares Scenario 1 and Scenario 2 for the selected emulator output."))
                                  )
                         ), plotlyOutput("compare_two_plot",height=700))),
            fluidRow(box(width=12,status="primary",solidHeader=FALSE,
                         tags$div(class="section-heading-with-info", tags$h4("Scenario Spatial Comparison Map"),
                                  tags$div(class="flow-info-hover", tags$span(class="flow-info-trigger", tabindex="0", role="button", `aria-label`="Spatial comparison information", icon("info-circle")),
                                           tags$div(class="flow-info-popover", tags$div(class="flow-info-static", "For PTM outputs, the map compares the two scenarios using the selected risk threshold. For Event Horizon, the predicted channel-path reaches are compared. ECO-PTM outputs do not have map geometry."))
                                  )
                         ), leafletOutput("compare_two_spatial_map",height=650))),
            fluidRow(box(width=12,status="primary",solidHeader=FALSE,
                         tags$div(class="section-heading-with-info", tags$h4("Detailed Scenario Comparison Results"),
                                  tags$div(class="flow-info-hover", tags$span(class="flow-info-trigger", tabindex="0", role="button", `aria-label`="Comparison table information", icon("info-circle")),
                                           tags$div(class="flow-info-popover", tags$div(class="flow-info-static", "The table provides the detailed emulator outputs and input values for Scenario 1 and Scenario 2."))
                                  )
                         ), div(class="wide-table-scroll", tableOutput("compare_two_table"))))
          )
        )
      ),
      
      # -----------------------------
      # Data Access
      # -----------------------------
      tabItem(
        tabName = "data",
        div(style = "width: 100%;height: calc(100vh - 100px);overflow-x: auto;overflow-y: auto;box-sizing: border-box;",
            div(style = "width: 1800px;min-width: 1800px;margin: 0 auto;box-sizing: border-box;",        
                fluidRow(
                  box(
                    style = "margin-left: 20px;",
                    width = 6,
                    h1("Data Access", style = "margin-left: 10px;"),
                    solidHeader = FALSE,
                    h3("Quick Overview", style = "margin-left: 10px;"),
                    style = "text-align: Justify;margin-left: 10px;margin-right:60px;",
                    p(style = "margin-left: 10px;", "This ShinyApp makes forecast and/or presents hindcast results on the particle entrainment within the Sacramento-San Joaquin Delta. The real-time simulations and predictions are used for providing quick assessment and help with the potential effects of CVP and SWP alternative operations on listed species. This interactive application is designed based on the machine learning models that were originally developed for the Contra Costa Water District (CCWD)'s hydraulic footprint project."),
                    h5("Here are three types of models:", style = "margin-left: 10px;"),
                    div(style = "margin-left:60px",
                        h5("- DSM2 ECO-PTM emulator models",
                           tags$a(" (part 1)", href = "#intro-eco-ptm")
                        ),
                        h5("- DSM2 PTM emulator models",
                           tags$a(" (part 2)", href = "#intro-ptm")
                        ),
                        h5("- Model for the Entrainment Event Horizon",
                           tags$a(" (part 3)", href = "#intro-event-horizon")
                        )
                    ),
                    h3("Data Availability", style = "margin-left: 10px;"),
                    
                    p(style = "margin-left: 10px;",
                      "Data and tools supporting the PTM Emulator dashboard are provided below."
                    ),
                    
                    tags$hr(),
                    
                    # ---------------------
                    # GitHub
                    # ---------------------
                    div(
                      style = "
          padding:15px;
          border:1px solid #d9d9d9;
          border-radius:6px;
          margin-bottom:15px;
          background:white;
        ",
                      
                      tags$h4("PTM Emulator GitHub Repository"),
                      
                      p(
                        paste(
                          "Access the DSM2 weekly modeling workflow, PTM and ECO-PTM",
                          "post-processing tools, SacPAS reporting code, model documentation,",
                          "and the Emulator/Event Horizon Shiny application."
                        )
                      ),
                      
                      tags$a(
                        class = "btn btn-success",
                        href = "https://github.com/BDO-Science/delta-model-automation",
                        target = "_blank",
                        rel = "noopener noreferrer",
                        title = paste(
                          "Open the BDO-Science delta-model-automation",
                          "repository in a new browser tab"
                        ),
                        icon("github"),
                        " View on GitHub"
                      )
                    ),
                    
                    # ---------------------
                    # SacPAS
                    # ---------------------
                    div(
                      style = "
          padding:15px;
          border:1px solid #d9d9d9;
          border-radius:6px;
          background:white;
        ",
                      
                      tags$h4("SacPAS Weekly Assessment"),
                      
                      p(
                        "Weekly Sacramento River Winter-Run assessment forecasts and supporting evaluation products."
                      ),
                      
                      tags$a(
                        class = "btn btn-primary",
                        href = "https://can01.safelinks.protection.outlook.com/?url=https%3A%2F%2Fcbr.washington.edu%2Fsacramento%2Fassessments%2Ftest%2Fforecast_sacpas.html&data=05%7C02%7CLaura.Manuel%40stantec.com%7C7d690048e1804ee4cc7408deccc5ac84%7C413c6f2c219a469297d3f2b4d80281e7%7C0%7C0%7C639173346569845423%7CUnknown%7CTWFpbGZsb3d8eyJFbXB0eU1hcGkiOnRydWUsIlYiOiIwLjAuMDAwMCIsIlAiOiJXaW4zMiIsIkFOIjoiTWFpbCIsIldUIjoyfQ%3D%3D%7C0%7C%7C%7C&sdata=OIZCfSzN%2BkO8UrHo3QV4D4Aike6mQpP7LvkmfgoCKa0%3D&reserved=0",
                        target = "_blank",
                        icon("external-link-alt"),
                        " Open SacPAS Assessment"
                      )
                    )
                  ),
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-eco-ptm","P1 DSM2 ECO-PTM emulator models",style = "margin-left:10px;"),
                      gt::gt_output("ecoptm_inputs_table"),
                      tags$hr(),
                      fluidRow(
                        column(width = 7,
                               box(title = "Model Visualization Tool",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   leaflet::leafletOutput("eco_ptm_map", width = "100%", height = "650px")
                               )
                        ),
                        column(width = 5,
                               box(title = "Model Parameters",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   div(style = "height:650px;overflow-y:auto;",gt::gt_output("ecoptm_parameters_table"))
                               )
                        )
                      )
                  ),
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-ptm","P2 DSM2 PTM emulator models",style = "margin-left:10px;"),
                      gt::gt_output("ptm_inputs_table"),
                      tags$hr(),
                      fluidRow(
                        column(width = 7,
                               box(title = "DSM2 Nodes and Model Input Stations",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   tabsetPanel(id = "ptm_map_tabs",type = "tabs",
                                               tabPanel(title = "7-Day",leaflet::leafletOutput("node_station_map_7day",width = "100%",height = "620px")),
                                               tabPanel(title = "30-Day",leaflet::leafletOutput("node_station_map_30day",width = "100%",height = "620px"))
                                   )
                               )
                        ),
                        column(width = 5,
                               box(title = "Model Parameters",width = 12,height = "720px",status = "primary",solidHeader = TRUE,
                                   div(style = "height:650px;overflow-y:auto;",
                                       gt::gt_output("ptm_parameters_table")
                                   )
                               )
                        )
                      )
                  ),        
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-event-horizon","P3 Model for the Entrainment Event Horizon",style = "margin-left:10px;"),
                      gt::gt_output("horizon_inputs_table"),
                      tags$hr(),
                      fluidRow(
                        column(width = 7,
                               box(title = "Model Input Locations",width = 12,height = "740px",status = "primary",solidHeader = TRUE,
                                   leaflet::leafletOutput("horizon_map",width = "100%",height = "670px")
                               )
                        ),
                        
                        column(width = 5,
                               box(title = "Inputs Data Range",width = 12,height = "320px",status = "primary",solidHeader = TRUE,
                                   div(style = "height:350px;overflow-y:auto;",
                                       gt::gt_output("horizon_datarange_table")
                                   )
                               ),
                               
                               box(title = "Model Parameters",width = 12,height = "400px",status = "primary",solidHeader = TRUE,
                                   div(style = "height:350px;overflow-y:auto;",
                                       gt::gt_output("horizon_parameter_table")
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
)

