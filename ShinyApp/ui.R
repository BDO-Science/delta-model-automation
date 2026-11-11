
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(gt)
source("ccwd_qgcd_excel.R", local=TRUE)
CCWD_QGCD_DEFAULT <- ccwd_qgcd_default_or_na()
APP_NAME <- "Entrainment Dashboard"
APP_VERSION <- "1.1.0"

# Scenario Comparison uses the same 2026 LightGBM feature inputs as Exploration.
compare_ccwd_card <- function(id, label, value, description=NULL, kind="text") {
  control <- if (identical(kind,"dcc")) {
    radioButtons(id, label, choices=c("Closed"=0,"Open"=1), selected=1, inline=TRUE)
  } else if (identical(kind,"risk")) {
    selectInput(id, label, choices=seq(15,80,by=5), selected=25)
  } else {
    textInput(id, label, value=value)
  }
  div(class="exploration-input-card compare-ccwd-card",
    div(class="flow-info-hover",
      tags$span(class="flow-info-trigger", tabindex="0", role="button",
                `aria-label`=paste(label,"information"), icon("info-circle")),
      div(class="flow-info-popover", div(class="flow-info-static",
        if (is.null(description)) "Editable scenario input using the same feature definition as Exploration." else description))),
    control)
}

run_page <- function(prefix, title, theme_class) {
  
  is_current <- identical(prefix, "current")
  status_name <- "warning"
  # QGCD client-facing source is the DAYFLOW-based daily reference series.
  current_qgcd <- CCWD_QGCD_DEFAULT


  flow_input <- function(input_id, label, value) {
    if (is_current) {
      textInput(
        input_id,
        label,
        if (is.finite(value)) format(round(value, 0), big.mark = ",", scientific = FALSE, trim = TRUE) else "Unavailable"
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
              ),
              uiOutput("forecast_weekly_gate_override_ui")
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
                            "Default value is based on the latest available 7-day average flow. SAC is used in both new PTM and new Event Horizon models.",
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
                            tagList(
                              tags$div(class = "ccwd-help-title", "Gross Delta Depletion (QGCD)"),
                            tags$div(class = "ccwd-help-lead", "Source: DAYFLOW-based daily gross channel depletion reference series. Daily reference values are in cfs; each is converted to a monthly-equivalent TAF/month."),
                            tags$div(class = "ccwd-help-section", tags$strong("How the default is selected"),
                              tags$p("For each of the latest 7 complete days, daily cfs × 86,400 / 43,560 × number of days in that calendar month / 1,000. The seven daily monthly-equivalent values are averaged. The model uses the editable input shown here.")),
                            tags$div(class = "ccwd-help-section", tags$strong("Current reference"),
                              tags$div(class = "ccwd-help-stat", paste0(if (is.finite(current_qgcd)) format(round(current_qgcd), big.mark=",", trim=TRUE) else "Unavailable", " TAF/month; source: DAYFLOW-based daily reference"))),
                            tags$div(class = "ccwd-help-foot", "The displayed value is the seven-day mean of daily monthly-equivalent reference values, not a fixed monthly lookup. Editable for scenarios.")
                            )
                          ),
                          flow_input(paste0(prefix, "_ptm_qgcd"),
                                     "QGCD: Gross Delta Depletion (TAF/month):", current_qgcd)
                        ),

                        div(
                          class = "exploration-input-card",
                          flow_info(
                            tagList(
                              tags$div(class = "ccwd-help-title", "Gross Delta Accretion (QPREC)"),
                            tags$div(class = "ccwd-help-lead", "Seven-day mean of daily monthly-equivalent QPREC derived from measured SCKC1 precipitation and DAYFLOW five-day runoff distribution."),
                            tags$div(class = "ccwd-help-section", tags$strong("Calculation in 3 steps"),
                              tags$ol(
                                tags$li("For every day, average rainfall on that day and the preceding four days. Each rainfall event contributes over five days."),
                                tags$li("Convert each daily distributed rainfall depth to TAF/day using 682,230 acres."),
                                tags$li("Multiply each daily TAF/day rate by the number of calendar days in that month; average the seven values for the model input.")
                              )),
                            tags$div(class = "ccwd-help-foot", "SCKC1 daily precipitation is obtained from the IEM CA_DCP daily-summary service (station, day, precip_in, network). Zero rainfall is a valid value; missing dates are not treated as zero. This is a monthly-equivalent estimate, not an observed calendar-month total.")
                            ),
                            "current_ptm_qprec_auto_note"
                          ),
                          textInput(paste0(prefix, "_ptm_qprec"),
                                    "QPREC: Gross Delta Accretion (TAF/month):", value = "Retrieving...")
                        ),

                        div(
                          class = "exploration-input-card",
                          flow_info(
                            tagList(
                              tags$div(class = "ccwd-help-title", "Historical DCC Flow"),
                            tags$div(class = "ccwd-help-lead", "Measured Delta Cross Channel discharge from CDEC station DLC, hourly FLOW series (sensor 20). Used by the PTM 30-day model."),
                            tags$div(class = "ccwd-help-section", tags$strong("How the default is calculated"),
                              tags$ol(
                                tags$li("Group available hourly FLOW measurements by day and calculate a daily mean (cfs)."),
                                tags$li("Average the available daily means over the latest historical seven-day window."),
                                tags$li("When DCC gates are selected Closed, force flow to 0 cfs, regardless of measured values.")
                              )),
                            tags$div(class = "ccwd-help-foot", "Coverage and partial days are reported below. Gate status is user-selected, not retrieved from historical operation records. The CalSim3 forecast regression is not used here.")
                            ),
                            "current_ptm_dcc_auto_note"
                          ),
                          textInput(paste0(prefix, "_ptm_dcc_flow"),
                                    "DCC: Observed / Estimated Flow (cfs):", value = "")
                        ),

                        div(
                          class = "exploration-input-card",
                          flow_info("Historical gate status is user-selected. Closed: 0 cfs. Open: CDEC measured DCC flow if available; otherwise estimated using 0.1846 × measured Freeport SAC + 306.52 cfs. Confirm actual historical gate status before scientific use."),
                          radioButtons(paste0(prefix, "_ptm_dcc_status"),
                                       "DCC Gate Status (User Selected):",
                                       choices = c("Closed" = 0, "Open" = 1),
                                       selected = 1, inline = TRUE)
                        ),
                        
                        div(
                          class = "exploration-input-card exploration-threshold-card",
                          if (is_current) {
                            flow_info("Choose the entrainment risk threshold (15–80%). The selected percentage is used for the risk-map contour and the Event Horizon distance prediction.")
                          } else {
                            tags$div(class = "flow-info-hover",
                              tags$span(class = "flow-info-trigger", tabindex = "0", role = "button",
                                `aria-label` = "Entrainment risk and Event Horizon threshold information", icon("info-circle")),
                              tags$div(class = "flow-info-popover",
                                tags$div(class = "flow-info-static",
                                  "Choose the entrainment risk threshold (15–80%). It controls the risk-map contour and the Event Horizon distance prediction.")))
                          },
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
                          "QGCD: Gross Delta Depletion (TAF/month):",
                          "forecast_ptm_qgcd_value",
                          "Source: DAYFLOW-based daily gross channel depletion reference. Each daily cfs value is converted to monthly-equivalent TAF/month using the calendar-month length; the seven daily values are averaged across assessment dates, including cross-month windows."
                        ),
                        weekly_value_card(
                          "QPREC: Gross Delta Accretion (TAF/month):",
                          "forecast_ptm_qprec_value",
                          tagList("Measured windows use SCKC1 precipitation. Forecast windows combine earlier SCKC1 observations with historical CNRFC QPF issued on or before the archive cutoff. Precipitation is distributed over five days using 682,230 acres; each daily result is scaled by calendar-month length and averaged over seven days. CNRFC missing tail days are carried forward and flagged. No zero-filling.",
                                  tags$hr(),uiOutput("forecast_ptm_qprec_source_note"))
                        ),
                        weekly_value_card(
                          "DCC: Flow (cfs):",
                          "forecast_ptm_dcc_flow_value",
                          tagList("Closed gates: 0 cfs. Open measured dates: observed CDEC DLC flow when available; missing days use 0.1846 × measured Freeport SAC + 306.52 cfs. Open forecast dates: regression with forecast SAC.", tags$hr(), uiOutput("forecast_ptm_dcc_source_note"))
                        ),
                        weekly_value_card(
                          "DCC Gate Status:",
                          "forecast_ptm_dcc_status_value",
                          "Forecast uses the first forecast day's recorded gate status for the entire week (0 closed, 1 open). Observed uses the last measured day's status. Missing/invalid status is not silently assumed closed."
                        ),
                        div(
                          class = "exploration-input-card exploration-threshold-card",
                          if (is_current) {
                            flow_info("Choose the entrainment risk threshold (15–80%). The selected percentage is used for the risk-map contour and the Event Horizon distance prediction.")
                          } else {
                            tags$div(class = "flow-info-hover",
                              tags$span(class = "flow-info-trigger", tabindex = "0", role = "button",
                                `aria-label` = "Entrainment risk and Event Horizon threshold information", icon("info-circle")),
                              tags$div(class = "flow-info-popover",
                                tags$div(class = "flow-info-static",
                                  "Choose the entrainment risk threshold (15–80%). It controls the risk-map contour and the Event Horizon distance prediction.")))
                          },
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
                              "YOL = CDEC FRE daily flow (sensor 41) + Sacramento Weir (assumed 0 cfs). Missing Fremont observations are temporarily assumed 0 for model execution testing only; confirm with the team.",
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
                              selected = 1,
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
                        textInput(
                          paste0(prefix, "_eco_sac"),
                          "SAC: Sacramento River Flow at Freeport Flow (cfs):",
                          "18,000"
                        ),
                        textInput(
                          paste0(prefix, "_eco_yol"),
                          "YOL: Yolo Bypass Flow (cfs):",
                          "1,000"
                        ),
                        textInput(
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
                            "Forecast: the first forecast day determines Open (1) or Closed (0) for the whole seven-day week. Observed: use the last measured day's status. ECO-PTM requires a binary status, not an average."
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

        /* Data Access: supplementary hydrologic methodology, matching existing teal design. */
        .ccwd-data-methodology {padding:8px 12px 18px; font-size:14px; line-height:1.6; color:#36474f;}
        .ccwd-data-methodology > h3 {margin:5px 0 11px; color:#174f5b; font-size:21px;}
        .ccwd-data-methodology > p {margin-bottom:16px; max-width:1250px;}
        .ccwd-data-methodology .nav-tabs {border-bottom:2px solid #cce5e9; margin-bottom:20px;}
        .ccwd-data-methodology .nav-tabs > li > a {font-weight:600; color:#147b89;}
        .ccwd-data-methodology .nav-tabs > li.active > a {border-top:3px solid #0a7e8c; color:#125664;}
        .ccwd-data-methodology .tab-pane {max-width:1380px; padding:2px 8px 10px;}
        .ccwd-data-methodology .tab-pane h4 {margin:22px 0 9px; color:#176271; font-weight:700; font-size:16px;}
        .ccwd-data-methodology .tab-pane p,.ccwd-data-methodology .tab-pane li {font-size:13.5px; line-height:1.65;}
        .ccwd-data-methodology .tab-pane ol,.ccwd-data-methodology .tab-pane ul {padding-left:24px;}
        .ccwd-data-methodology .tab-pane li {margin-bottom:6px;}
        .ccwd-data-methodology .ccwd-method-equation {padding:12px 15px; background:#f2f8fa; border-left:3px solid #0a7e8c; font-weight:600; overflow-wrap:anywhere;}
        .ccwd-data-methodology .ccwd-method-caution {padding:11px 14px; border-left:3px solid #c39441; background:#fffaf1; color:#685632; margin-top:16px;}
        .ccwd-data-methodology .ccwd-method-example-table-wrap {max-width:100%; overflow-x:auto; margin:12px 0 5px; border:1px solid #d6e6ea; border-radius:6px;}
        .ccwd-data-methodology .ccwd-method-example-table {width:100%; min-width:680px; margin:0; font-size:13px; white-space:normal;}
        .ccwd-data-methodology .ccwd-method-example-table th {background:#e8f4f6; color:#185965; border-color:#d4e4e8; vertical-align:middle;}
        .ccwd-data-methodology .ccwd-method-example-table td {vertical-align:middle; border-color:#e0e9ed;}
        .ccwd-data-methodology .ccwd-method-example-total {background:#edf7f8; color:#174e5b;}
        .ccwd-data-methodology .ccwd-method-example-caption {font-size:12px !important; color:#63747c; margin:8px 0 15px;}
        .ccwd-data-methodology .ccwd-method-links a {color:#087784; font-weight:600;}
        .ccwd-data-methodology .ccwd-method-links .fa {font-size:10px; margin-left:3px;}
        .data-access-parameter-table {
          width: 100%;
          box-sizing: border-box;
          padding-right: 3px;
        }
        
        .data-access-parameter-table .gt_table {
          min-width: 640px;
        }
        
        .data-access-parameter-table .gt_sourcenotes {
          white-space: normal !important;
          overflow-wrap: anywhere;
        }
        
        .data-access-parameter-table .gt_row {
          white-space: normal;
        }
        /* Data Access: fluid page width, scrolling limited to wide model input tables. */
        .data-access-layout {
          width:100%; min-width:0; max-width:100%;
          box-sizing:border-box;
        }
        .data-access-content {
          width:100%; max-width:1640px; min-width:0;
          margin:0 auto; box-sizing:border-box;
        }
        .data-access-layout .row, .data-access-layout [class*='col-'] {
          min-width:0;
        }
        .data-access-layout .box {
          margin-left:0 !important;
          max-width:100%;
        }
        .data-access-layout .box-body {
          min-width:0;
        }
        .data-access-table-section {
          min-width:0; width:100%; margin:8px 0 20px;
        }
        .data-access-table-hint {
          display:flex; align-items:center; gap:7px;
          font-size:12px; color:#486674; margin:0 0 7px;
        }
        .data-access-table-hint .fa { color:#087d8d; }
        .data-access-table-scroll {
          display:block; width:100%; max-width:100%; min-width:0;
          overflow-x:auto; overflow-y:hidden;
          border:1px solid #d7e4e8; border-radius:5px;
          scrollbar-width:auto;
          scrollbar-color:#8cb6c0 #edf5f7;
          -webkit-overflow-scrolling:touch;
        }
        .data-access-table-scroll:focus-visible {
          outline:3px solid #2c96aa; outline-offset:3px;
        }
        .data-access-table-scroll::-webkit-scrollbar { height:13px; }
        .data-access-table-scroll::-webkit-scrollbar-track { background:#edf5f7; }
        .data-access-table-scroll::-webkit-scrollbar-thumb {
          background:#8cb6c0; border-radius:7px; border:2px solid #edf5f7;
        }
        .data-access-table-scroll .gt_table {
          width:max-content !important;
          min-width:1380px !important;
          max-width:none !important;
        }
        .data-access-table-scroll .gt_table th,
        .data-access-table-scroll .gt_table td {
          overflow-wrap:break-word;
          vertical-align:top;
        }
        @media (max-width:850px) {
          .data-access-content { max-width:100%; }
          .data-access-table-scroll .gt_table { min-width:1300px !important; }
        }

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
        .about-inline-reference {
          color: #087784;
          font-weight: 600;
          text-decoration: none;
          white-space: nowrap;
        }
        
        .about-inline-reference:hover,
        .about-inline-reference:focus {
          color: #055b65;
          text-decoration: underline;
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
          display: grid;
          grid-template-columns: repeat(9, minmax(150px, 1fr));
          overflow-x: auto;
          padding-bottom: 8px;
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

        /* Read-only copies of input help popovers are mounted under body by JS.
           The originals remain bound for Shiny's dynamic help text. */
        .ccwd-viewport-popover {
          display: block !important;
          position: fixed !important;
          z-index: 2147483000 !important;
          background: #fff;
          color: #33454d;
          padding: 12px 14px;
          border: 1px solid #c9d9de;
          border-radius: 7px;
          box-shadow: 0 7px 24px rgba(0,0,0,.19);
          font-size: 12.5px;
          line-height: 1.45;
          white-space: normal;
          max-height: calc(100vh - 30px);
          overflow-y: auto;
          overflow-wrap: anywhere;
          pointer-events: none;
        }
        .ccwd-viewport-popover a { pointer-events: auto; }
        .ccwd-viewport-popover hr {margin: 12px 0;border-top: 1px solid #e4ecef;}
        .ccwd-viewport-popover .ccwd-help-title {font-weight: 750;font-size: 15px;line-height:1.35;color:#153d55;margin-bottom:7px;}
        .ccwd-viewport-popover .ccwd-help-lead {font-size:12.5px;line-height:1.55;color:#435662;margin-bottom:10px;}
        .ccwd-viewport-popover .ccwd-help-section {border-top:1px solid #e8eef1;padding-top:10px;margin-top:10px;}
        .ccwd-viewport-popover .ccwd-help-section strong {display:block;color:#254f63;font-size:12px;margin-bottom:6px;}
        .ccwd-viewport-popover .ccwd-help-section p {margin:0;}
        .ccwd-viewport-popover .ccwd-help-section ol {margin:4px 0 0;padding-left:20px;}
        .ccwd-viewport-popover .ccwd-help-section li {margin:0 0 7px;line-height:1.5;}
        .ccwd-viewport-popover .ccwd-help-stat {padding:8px 10px;background:#f0f6f9;border-radius:5px;color:#174960;font-weight:700;}
        .ccwd-viewport-popover .ccwd-help-detail {display:grid;grid-template-columns:108px minmax(0,1fr);gap:5px 10px;padding:8px 0;line-height:1.5;}
        .ccwd-viewport-popover .ccwd-help-key {font-size:11px;color:#6b7a84;}
        .ccwd-viewport-popover .ccwd-help-value {font-weight:600;color:#263d4b;overflow-wrap:anywhere;}
        .ccwd-viewport-popover .ccwd-help-foot {margin-top:10px;color:#667883;font-size:11.5px;line-height:1.5;}
        .ccwd-viewport-popover .ccwd-help-status {font-size:12px;background:#fff6e7;padding:9px 10px;border-radius:5px;color:#7b5619;}
        .ccwd-viewport-popover .ccwd-help-source {margin-top:8px;font-size:11px;overflow-wrap:anywhere;}


        /* Keep information popovers fully visible, including the leftmost EXP card. */
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

        /* The nine PTM inputs stay on one horizontally scrollable row. */

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
          .current-theme .exploration-eco-grid {
            grid-template-columns: repeat(4, minmax(150px,1fr));
            overflow-x: auto;
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
        /* Six-point alignment update: identical control baselines and single-row grids. */
        .current-theme .exploration-ptm-grid,
        .forecast-theme .exploration-ptm-grid.weekly-input-grid {
          display:grid;
          grid-template-columns:repeat(9, minmax(174px, 1fr));
          overflow-x:auto;
          overflow-y:visible;
          gap:12px;
          align-items:stretch;
          padding-bottom:10px;
        }
        .current-theme .exploration-input-card,
        .forecast-theme .exploration-input-card {
          display:flex;
          flex-direction:column;
          min-width:0;
          height:100%;
        }
        .current-theme .exploration-input-card > .form-group,
        .forecast-theme .exploration-input-card > .form-group {
          display:flex;
          flex-direction:column;
          flex:1 1 auto;
          margin:0;
        }
        .current-theme .exploration-input-card label.control-label,
        .forecast-theme .exploration-input-card label.control-label,
        .forecast-theme .weekly-static-input-card > label {
          min-height:54px;
          display:flex;
          align-items:flex-end;
          margin-bottom:8px;
        }
        .current-theme .exploration-input-card .form-group > input.form-control,
        .current-theme .exploration-input-card .form-group > select.form-control,
        .forecast-theme .exploration-input-card .form-group > select.form-control,
        .forecast-theme .weekly-static-input-card .weekly-static-value {
          width:100%;
          min-height:36px;
          height:36px;
          margin-top:auto;
          box-sizing:border-box;
        }
        .forecast-theme .weekly-static-input-card .weekly-static-value {flex:0 0 36px;}
        .current-theme .exploration-threshold-card .flow-info-hover,
        .forecast-theme .exploration-threshold-card .flow-info-hover {min-height:24px;}
        .current-theme .exploration-eco-grid,
        .forecast-theme .exploration-eco-grid.weekly-input-grid {
          display:grid;
          grid-template-columns:repeat(4, minmax(170px, 1fr));
          gap:12px;
          overflow-x:auto;
        }
        /* Do not let older responsive rules wrap Weekly Assessment inputs. */
        @media (max-width:1100px) {
          .forecast-theme .exploration-ptm-grid.weekly-input-grid {grid-template-columns:repeat(9, minmax(174px, 1fr));}
          .forecast-theme .exploration-eco-grid.weekly-input-grid {grid-template-columns:repeat(4, minmax(170px, 1fr));}
        }
        @media (max-width:760px) {
          .forecast-theme .exploration-ptm-grid.weekly-input-grid {grid-template-columns:repeat(9, minmax(174px, 1fr));}
          .forecast-theme .exploration-eco-grid.weekly-input-grid {grid-template-columns:repeat(4, minmax(170px, 1fr));}
        }
        /* Scenario Comparison guidance: match the Exploration/Weekly panels. */
        .comparison-global-guide {
          display:flex;
          align-items:flex-start;
          gap:9px;
          padding:10px 12px;
          background:#f4fafb;
          border:1px solid #cfe3e7;
          border-left:4px solid #0a7e8c;
          border-radius:6px;
          color:#344a52;
          font-size:13px;
          line-height:1.45;
          margin:0 0 14px 0 !important;
        }
        .comparison-global-guide > i {margin-top:2px;color:#0a7e8c;}

        .comparison-section-heading {
          padding:4px 0 2px 0;
        }
        .comparison-section-heading .flow-info-hover {
          margin-left:0 !important;
          flex:0 0 auto;
        }
        .about-foundation-section {
          margin: 8px 0 24px 0;
          padding: 18px 22px;
          background: #f7fbfc;
          border: 1px solid #cfe2e6;
          border-left: 5px solid #0a7e8c;
          border-radius: 7px;
        }
        
        .about-foundation-section h4 {
          margin-top: 18px;
          margin-bottom: 7px;
          color: #075f6d;
          font-size: 17px;
          font-weight: 700;
        }
        
        .about-foundation-section h5 {
          margin-top: 7px;
          margin-bottom: 8px;
          color: #33454d;
          font-weight: 400;
        }
        
        .about-capability-list {
          margin: 7px 0 16px 24px;
          padding-left: 18px;
          color: #33454d;
          font-size: 14px;
          line-height: 1.6;
        }
        
        .about-capability-list li {
          margin-bottom: 5px;
        }
        
        .about-model-note {
          margin-top: 16px;
          padding: 12px 14px;
          background: #fffaf0;
          border: 1px solid #ead5a8;
          border-left: 4px solid #d89b21;
          border-radius: 5px;
          color: #4b4436;
          font-size: 13.5px;
          line-height: 1.55;
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

        /* 2026 improvements: uniform input alignment and one-row desktop layout. */
        .current-theme .exploration-ptm-grid,
        .forecast-theme .exploration-ptm-grid.weekly-input-grid {
          grid-template-columns:repeat(9,minmax(0,1fr));
          gap:7px;
          overflow-x:visible;
          align-items:stretch;
        }
        .current-theme .exploration-input-card,
        .forecast-theme .exploration-input-card {padding:10px 8px;}
        .current-theme .exploration-input-card label.control-label,
        .forecast-theme .exploration-input-card label.control-label,
        .forecast-theme .weekly-static-input-card > label {
          align-items:flex-start;
          min-height:86px;
          font-size:11.5px;
          line-height:1.3;
          margin-top:0;
          margin-bottom:6px;
        }
        .current-theme .exploration-input-card > .form-group,
        .forecast-theme .exploration-input-card > .form-group {justify-content:flex-end;}
        .current-theme .exploration-input-card .form-group > input.form-control,
        .forecast-theme .exploration-input-card .form-group > input.form-control,
        .forecast-theme .exploration-input-card .form-group > select.form-control {
          width:100%;height:36px;min-height:36px;margin-top:auto;
        }
        /* Only display the viewport-positioned tooltip copy; never show the
           hidden in-card popover simultaneously. */
        .content-wrapper .flow-info-hover > .flow-info-popover,
        .tab-content .flow-info-hover > .flow-info-popover,
        .current-theme .flow-info-hover > .flow-info-popover,
        .forecast-theme .flow-info-hover > .flow-info-popover {display:none !important;}
        .compare-new-model-grid {grid-template-columns:repeat(3,minmax(0,1fr));gap:9px;margin:12px 0;}
        .compare-new-model-grid .compare-ccwd-card {display:flex;flex-direction:column;min-width:0;padding:10px;}
        .compare-new-model-grid .compare-ccwd-card .form-group {display:flex;flex-direction:column;flex:1;min-width:0;margin:0;}
        .compare-new-model-grid .compare-ccwd-card label.control-label {min-height:66px;align-items:flex-start;display:flex;line-height:1.35;font-size:12px;margin:0 0 6px;}
        .compare-new-model-grid .compare-ccwd-card .form-control {height:36px;min-height:36px;margin-top:auto;width:100%;}
        .compare-new-model-grid .compare-ccwd-card .flow-info-hover {align-self:flex-end;min-height:22px;}
        .compare-new-model-grid .compare-ccwd-card .radio-inline {font-size:12px;}
        @media (max-width:1200px) {
          .current-theme .exploration-ptm-grid,
          .forecast-theme .exploration-ptm-grid.weekly-input-grid {grid-template-columns:repeat(9,minmax(112px,1fr));overflow-x:auto;}
          .compare-new-model-grid {grid-template-columns:repeat(2,minmax(0,1fr));}
        }
        @media (max-width:900px) {.compare-new-model-grid {grid-template-columns:1fr;}}

        /* Use only the positioned, formatted tooltip: never reveal its original. */
        body .flow-info-hover > .flow-info-popover,
        body .flow-info-hover:hover > .flow-info-popover,
        body .flow-info-hover:focus-within > .flow-info-popover {
          display:none !important;visibility:hidden !important;
        }
        body > .ccwd-viewport-popover {
          display:block !important;visibility:visible !important;
          z-index:10000 !important;
        }
        /* Keep the radio control in the same bottom input row as text boxes. */
        .current-theme .exploration-input-card .shiny-input-radiogroup > .shiny-options-group,
        .forecast-theme .exploration-input-card .shiny-input-radiogroup > .shiny-options-group {
          display:flex;align-items:center;justify-content:flex-start;
          gap:8px;min-height:36px;height:36px;
          margin-top:auto;white-space:nowrap;
        }
        .current-theme .exploration-input-card .shiny-input-radiogroup > .shiny-options-group .radio-inline,
        .forecast-theme .exploration-input-card .shiny-input-radiogroup > .shiny-options-group .radio-inline {
          margin:0;padding-top:0;line-height:20px;font-size:11.5px;
        }
        .current-theme .exploration-input-card .shiny-input-radiogroup > .shiny-options-group .radio-inline + .radio-inline {
          margin-left:0;
        }
        .current-theme .exploration-input-card .shiny-input-radiogroup > label.control-label {
          min-height:86px;align-items:flex-start;margin-top:0;margin-bottom:6px;
        }
        /* Modern two-column compare forms within the side-by-side scenario panels. */
        .compare-scenario-panel {border-radius:7px;overflow:visible;}
        .compare-scenario-panel > .box-body {position:relative;padding:16px 16px 18px;}
        .compare-fields-heading {display:flex;justify-content:space-between;gap:8px;
          align-items:center;margin:13px 0 10px;padding:9px 0 8px;
          font-size:11px;font-weight:700;letter-spacing:.08em;color:#47717d;
          border-bottom:1px solid #dce8ec;}
        .compare-fields-heading > span {font-size:10px;letter-spacing:normal;
          color:#778c96;font-weight:400;}
        .compare-new-model-grid {display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:11px;}
        .compare-new-model-grid .compare-ccwd-card {
          background:#fbfdfe;border:1px solid #dce6eb;border-radius:7px;
          padding:11px 11px 12px;min-height:154px;
          box-shadow:0 1px 2px rgba(24,66,81,.035);
        }
        .compare-new-model-grid .compare-ccwd-card:focus-within {
          border-color:#56a6b4;box-shadow:0 0 0 2px rgba(50,147,165,.1);
        }
        .compare-new-model-grid .compare-ccwd-card label.control-label {
          font-size:12px;line-height:1.35;min-height:53px;margin-bottom:8px;
        }
        .compare-new-model-grid .compare-ccwd-card .shiny-options-group {
          min-height:36px;display:flex;align-items:center;gap:10px;
          margin-top:auto;white-space:nowrap;
        }
        .compare-new-model-grid .compare-ccwd-card .radio-inline,
        .compare-new-model-grid .compare-ccwd-card .radio-inline + .radio-inline {
          margin:0;font-size:12px;
        }
        .compare-new-model-grid .compare-ccwd-card .flow-info-hover {min-height:22px;margin-bottom:5px;}
        .compare-scenario-panel .compare-run-action {border-radius:5px;height:42px;font-weight:600;}
        .compare-loading-cover {display:none;position:absolute;inset:0;z-index:1000;
          background:rgba(250,253,254,.83);align-items:center;justify-content:center;
          border-radius:6px;}
        .compare-scenario-panel.is-comparing > .box-body > .compare-loading-cover {display:none;}
        .compare-loading-pill {padding:15px 18px;background:white;border:1px solid #c7dce2;
          border-radius:8px;box-shadow:0 3px 15px rgba(0,0,0,.1);color:#245a68;
          font-weight:600;font-size:14px;}
        .compare-loading-pill .fa {margin-right:8px;}
        .compare-status {margin-top:10px;padding:10px 12px;border:1px solid #dbe3ea;
          border-radius:6px;background:#f7fafc;color:#35465a;font-size:13px;
          line-height:1.45;min-height:42px;}
        .compare-status[data-state='running'] {background:#eef7fb;border-color:#aed8e7;color:#164b65;}
        .compare-status[data-state='success'] {background:#eff9f2;border-color:#b9dfc2;color:#206335;}
        .compare-status[data-state='error'] {background:#fff2f2;border-color:#ebadad;color:#962b2b;}
        .compare-status[data-state='slow'] {background:#fff9e9;border-color:#edd49b;color:#765211;}
        .compare-status .fa {margin-right:7px;}
        .compare-scenario-panel.is-comparing .compare-run-action {opacity:.65;pointer-events:none;}

        .comparison-map-container {position:relative;min-height:650px;}
        .comparison-map-progress {display:none;position:absolute;inset:0;z-index:1500;
          align-items:center;justify-content:center;background:rgba(249,252,254,.86);
          border:1px solid #dce4ea;border-radius:5px;}
        .comparison-map-progress.is-active {display:flex;}
        .comparison-map-progress-content {background:white;border:1px solid #c8d8e1;
          padding:22px 26px;border-radius:8px;box-shadow:0 4px 18px rgba(24,47,62,.09);
          max-width:380px;text-align:center;color:#244a5e;font-size:14px;}
        .comparison-map-progress-content .fa {font-size:25px;display:block;margin:0 auto 11px;color:#247b96;}
        .comparison-map-progress-content strong {display:block;margin-bottom:8px;}
        .comparison-map-progress-detail {font-size:12px;line-height:1.5;color:#526875;}
        .comparison-map-message {font-size:12px;color:#576b7b;margin:8px 0 12px;min-height:18px;}
        .comparison-map-message[data-state='error'] {color:#9d2929;}
        @media(max-width:1160px) {.compare-new-model-grid {grid-template-columns:1fr;}}

          /* About page: consistent typography, spacing, and responsive layout. */
          .about-page-scroll {width:100%;height:calc(100vh - 100px);overflow:auto;box-sizing:border-box;}
          .about-page {width:min(1120px,100%);margin:0 auto;padding:0 8px 32px;box-sizing:border-box;color:#30434b;}
          .about-page .box {border-top-color:#0a7e8c;}
          .about-page h1,.about-page h2,.about-page h3,.about-page h4,.about-page h5,.about-page p,.about-page li {font-family:Segoe UI,Arial,sans-serif;}
          .about-page h1 {margin:0 0 10px;font-size:30px;line-height:1.2;font-weight:700;color:#163f49;}
          .about-page h2 {margin:8px 0 14px;font-size:24px;line-height:1.3;font-weight:700;color:#075f6d;}
          .about-page h3 {margin:28px 0 12px;font-size:19px;line-height:1.35;font-weight:700;color:#174f5b;}
          .about-page h4 {margin:20px 0 8px;font-size:16px;line-height:1.4;font-weight:700;color:#2b5059;}
          .about-page h5,.about-page p,.about-page li {font-size:14px;line-height:1.65;font-weight:400;color:#334a53;}
          .about-page h5 {margin:7px 0 12px;}
          .about-page code {font-size:.92em;color:#8b2342;background:#f8edf1;border:1px solid #edd7df;border-radius:4px;padding:2px 6px;}
          .about-hero {display:grid;grid-template-columns:180px minmax(0,1fr);align-items:center;gap:28px;min-height:250px;padding:24px 30px;}
          .about-hero-logo {display:block;width:160px;max-width:100%;height:auto;margin:auto;}
          .about-hero-copy {min-width:0;}
          .about-content {padding:8px 34px 28px;}
          .about-section-content {margin-left:24px;padding-left:18px;border-left:2px solid #dce9ec;}
          .about-foundation-section {margin-top:12px;}
          .about-capability-list {margin-left:22px;padding-left:18px;}
          .about-page .gt_table {font-family:Segoe UI,Arial,sans-serif!important;font-size:14px!important;}
          .about-page a {color:#087784;font-weight:600;}
          .about-page a:hover,.about-page a:focus {color:#055b65;text-decoration:underline;}
          @media(max-width:760px) {
            .about-page {padding:0 2px 24px;}
            .about-hero {grid-template-columns:1fr;text-align:center;padding:22px 16px;}
            .about-content {padding:6px 14px 22px;}
            .about-section-content {margin-left:0;padding-left:12px;}
            .about-page h1 {font-size:26px;}
            .about-page h2 {font-size:21px;}
          }
")),
      tags$script(src = "ccwd-ui.js")
    ),
    
    tabItems(
      # -----------------------------
      # About
      # -----------------------------3
      tabItem(
        tabName = "about",
        div(class = "about-page-scroll",
            div(class = "about-page",
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
                                     paste0("  ", APP_VERSION, "  ")
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
                            "be updated weekly during the OMR entrainment management season (December through June).",
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
                      div(style = "margin-left: 60px;margin-right:120px",
                          h2("Overview"),
                          div(
                            class = "about-overview-text",
                            
                            p(
                              "This interactive application uses hydrodynamic model emulators to ",
                              "develop quick and intuitive outputs that support real-time and ",
                              "large-scale assessments of the potential effects of Central Valley ",
                              "Project (CVP) and State Water Project (SWP) export operations on ",
                              "listed species in the Sacramento-San Joaquin Delta. The hydrodynamic ",
                              "model emulators are machine-learning models trained on DSM2 PTM and ",
                              "ECO-PTM outputs. The models produce end-of-7-day and end-of-30-day ",
                              "percent entrainment estimates for surface-oriented particles entering ",
                              "the CVP and SWP export facilities, hydraulic-footprint metrics, and ",
                              "salmon survival and routing estimates."
                            ),
                            
                            p(
                              "Users can interact with the application through the following tabs:"
                            ),
                            
                            tags$ul(
                              class = "about-workflow-list",
                              
                              tags$li(
                                tags$strong("Exploration. "),
                                "Produces output metrics using current Delta conditions or ",
                                "user-defined inputs."
                              ),
                              
                              tags$li(
                                tags$strong("Weekly Assessments. "),
                                "Produces output metrics using forecasted, previously forecasted, ",
                                "or previously observed Delta conditions during the OMR entrainment ",
                                "management season."
                              ),
                              
                              tags$li(
                                tags$strong("Scenario Comparison. "),
                                "Compares output metrics across two or more scenarios from the ",
                                "Exploration and Weekly Assessments tabs, or from user-defined ",
                                "scenarios."
                              )
                            ),
                            
                            p(
                              "This interactive application is based on machine-learning models ",
                              "developed through the Contra Costa Water District's (CCWD) hydraulic ",
                              "footprint project. Further details on model development, original ",
                              "training datasets, and comparative evaluation of model results are ",
                              "available in the CCWD report and supporting model repository  ",
                              
                              tags$a(
                                "(1)",
                                href = "https://github.com/cchang-ccwater/CCWD_Hydraulic_Footprints",
                                target = "_blank",
                                rel = "noopener noreferrer",
                                title = "Open the CCWD Hydraulic Footprints model repository in a new browser tab"
                              ),
                              
                              "."
                            )
                          ),
                          h3(
                            "Purpose and Scientific Foundation"
                          ),
                          
                          div(
                            class = "about-foundation-section",
                            
                            h5(
                              style = "text-align: justify; line-height: 1.65;",
                              
                              paste(
                                "The Entrainment Dashboard is an interactive decision-support",
                                "and exploratory analysis application for assessing how Delta",
                                "flow conditions and Central Valley Project and State Water",
                                "Project operations may influence particle entrainment, salmonid",
                                "routing and survival, and the spatial extent of entrainment risk.",
                                "The application is intended to make technically rigorous model",
                                "information easier to explore during current-condition",
                                "assessments, weekly planning, and comparisons of alternative",
                                "operational scenarios."
                              )
                            ),
                            
                            h4(
                              "Scientific foundation"
                            ),
                            
                            h5(
                              style = "text-align: justify; line-height: 1.65;",
                              
                              paste(
                                "The application is founded on machine-learning emulators",
                                "developed from a large dataset of CalSim and Delta Simulation",
                                "Model II simulations. The underlying study included 191,100",
                                "Particle Tracking Module simulations and 2,772 Ecological",
                                "Particle Tracking Module simulations representing a broad range",
                                "of historical hydrology and alternative Old and Middle River",
                                "management conditions. Boosted regression-tree models were",
                                "trained and evaluated using five-fold cross-validation and",
                                "parsimonious predictor-selection procedures."
                              ),
                              
                              " ",
                              
                              tags$a(
                                "(Contra Costa Water District 2026)",
                                href = "20260512_CCWD_Entrainment_TM_Final.pdf",
                                target = "_blank",
                                rel = "noopener noreferrer",
                                class = "about-inline-reference",
                                title = paste(
                                  "Open Intuitive Quantitative Metrics for Rapid Assessment",
                                  "of Entrainment Risks and Salmonid Responses in The Delta"
                                )
                              ),
                              
                              tags$br(),
                              tags$br(),
                              
                              paste(
                                "The emulators are not intended to replace DSM2. They rapidly",
                                "estimate selected DSM2-derived responses without requiring the",
                                "complete model setup, simulation, and post-processing workflow",
                                "for every scenario."
                              )
                            ),
                            
                            h4(
                              "PTM entrainment emulators"
                            ),
                            
                            h5(
                              style = "text-align: justify; line-height: 1.65;",
                              
                              paste(
                                "The PTM emulators estimate the percentage of particles",
                                "entrained at the combined CVP and SWP export facilities after",
                                "7 or 30 days for supported release locations. The 7-day",
                                "predictions support the weekly time scale of short-term",
                                "operational assessment, while the 30-day predictions provide",
                                "a longer-term or quasi-steady-state view of transport and",
                                "entrainment. Predictions account for location and readily",
                                "available hydrologic and operational inputs, including combined",
                                "exports, Vernalis flow, Sacramento River flow, eastside",
                                "tributary flow, and interior Delta flow."
                              )
                            ),
                            
                            h4(
                              "ECO-PTM salmonid-response emulators"
                            ),
                            
                            h5(
                              style = "text-align: justify; line-height: 1.65;",
                              
                              paste(
                                "The ECO-PTM emulators estimate combined through-Delta survival",
                                "from Freeport toward Chipps Island and the probability of",
                                "routing into the interior Delta through Georgiana Slough or",
                                "the Delta Cross Channel. These outputs provide a rapid",
                                "representation of modeled relationships among Delta inflows,",
                                "channel routing, gate operations, and Sacramento-origin",
                                "juvenile salmonid migration outcomes."
                              )
                            ),
                            
                            h4(
                              "Entrainment Event Horizon"
                            ),
                            
                            h5(
                              style = "text-align: justify; line-height: 1.65;",
                              
                              paste(
                                "The Entrainment Event Horizon converts a spatially complex",
                                "pattern of entrainment into an intuitive distance-based metric.",
                                "For a selected entrainment-risk threshold, the Event Horizon",
                                "represents the shortest channel distance from Clifton Court",
                                "Forebay to the upstream boundary of the corresponding high-risk",
                                "zone along the Old and Middle River corridor. A larger Event",
                                "Horizon indicates that the selected risk level extends farther",
                                "upstream, while a smaller Event Horizon indicates that the risk",
                                "is more geographically constrained near the export facilities."
                              )
                            ),
                            
                            h4(
                              "What users can do"
                            ),
                            
                            tags$ul(
                              class = "about-capability-list",
                              
                              tags$li(
                                "Evaluate current and archived weekly Delta conditions."
                              ),
                              
                              tags$li(
                                paste(
                                  "Test user-defined combinations of exports, river inflows,",
                                  "eastside flows, and interior Delta flow."
                                )
                              ),
                              
                              tags$li(
                                paste(
                                  "Compare observed-flow and forecast OMRI alternatives from",
                                  "weekly assessment archives."
                                )
                              ),
                              
                              tags$li(
                                paste(
                                  "Review node-level 7-day and 30-day entrainment predictions",
                                  "and interpolated spatial patterns."
                                )
                              ),
                              
                              tags$li(
                                paste(
                                  "Evaluate the predicted Event Horizon at risk thresholds from",
                                  "15% through 80%."
                                )
                              ),
                              
                              tags$li(
                                paste(
                                  "Compare saved scenarios and download model results and",
                                  "map figures."
                                )
                              )
                            ),
                            
                            tags$div(
                              class = "about-model-note",
                              
                              tags$b(
                                "Important interpretation note: "
                              ),
                              
                              paste(
                                "Dashboard values are machine-learning predictions of simulated",
                                "DSM2 PTM and ECO-PTM responses. They are not direct observations,",
                                "real-time fish forecasts, or substitutes for complete",
                                "hydrodynamic, biological, or regulatory analyses. Results should",
                                "be interpreted with field observations, species information,",
                                "model limitations, and other available technical evidence."
                              )
                            )
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
                                tags$code(paste0("v", APP_VERSION)),
                                "."
                              ),
                              h4("Performance Standards: "),
                              h5(style = "text-align: justify;","The machine-learning emulators and event horizon models complete predictions in 0-2 seconds,",
                                 "while the accompanying data preparation and output processing can take ~30 seconds. ",
                                 "Operations that retrieve external data, evaluate multiple archived ",
                                 "scenarios, interpolate spatial results, or generate maps may contribute ",
                                 "to longer load times."),
                              h5(style = "text-align: justify;","Run times vary with the selected workflow, external-source availability, ",
                                 "server load, and the number of scenarios evaluated."),
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
            "Use Library of runs to compare saved scenarios, OMRI Comparison to compare archive forecasts, or Compare 2 Scenarios to run two user-defined sets of inputs. Run or select scenarios first; charts and tables appear independently of the map. To create a spatial comparison, click Generate Spatial Map in that section. Map interpolation may take longer than model predictions; a loading indicator shows its progress and any reported error. PTM and Event Horizon support maps; ECO-PTM results use charts and tables."
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
                uiOutput("comparison_library_status"),
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
                actionButton("generate_library_map", "Generate Spatial Map", icon=icon("map"), class="btn-primary comparison-map-action"),
                tags$div(id="library_map_message", class="comparison-map-message", role="status", `aria-live`="polite", "Select Generate Spatial Map to build the map."),
                tags$p(class="figure-note", "Spatial interpolation can take time. Charts and tables do not depend on the map."),
                tags$div(class="comparison-map-container",
                  leafletOutput("comparison_spatial_map", height = 650),
                  tags$div(id="library_map_progress", class="comparison-map-progress", role="status", `aria-live`="polite",
                           tags$div(class="comparison-map-progress-content",
                                    icon("spinner", class="fa-spin"),
                                    tags$strong("Generating spatial map"),
                                    tags$div(class="comparison-map-progress-detail", "Preparing map...")))),
                downloadButton(
                  "download_comparison_map",
                  "Download Comparison Map (PNG)",
                  class = "btn-primary map-download"
                ),
                tags$p(
                  class = "figure-note",
                  paste(
                    "Scenario-colored shading is a generalized interpolation of",
                    "entrainment emulator outputs between supported DSM2 nodes.",
                    "Opacity represents predicted entrainment percentage;",
                    "spatial transitions are approximate."
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
                tags$div(id="omri_run_status_live", class="compare-status", `data-state`="idle",
                         "Ready. Click Run All Available OMRI Scenarios to begin."),
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
                actionButton("generate_omri_map", "Generate Spatial Map", icon=icon("map"), class="btn-primary comparison-map-action"),
                tags$div(id="omri_map_message", class="comparison-map-message", role="status", `aria-live`="polite", "Select Generate Spatial Map to build the map."),
                tags$p(class="figure-note", "The map runs separately from forecast prediction, chart, and table generation."),
                tags$div(class="comparison-map-container",
                  leafletOutput("omri_comparison_spatial_map", height = 650),
                  tags$div(id="omri_map_progress", class="comparison-map-progress", role="status", `aria-live`="polite",
                           tags$div(class="comparison-map-progress-content",
                                    icon("spinner", class="fa-spin"),
                                    tags$strong("Generating spatial map"),
                                    tags$div(class="comparison-map-progress-detail", "Preparing map...")))),
                downloadButton(
                  "download_omri_comparison_map",
                  "Download OMRI Comparison Map (PNG)",
                  class = "btn-primary map-download"
                ),
                tags$p(
                  class = "figure-note",
                  paste(
                    "Scenario-colored shading is a generalized interpolation of",
                    "entrainment emulator outputs between supported DSM2 nodes.",
                    "Opacity represents predicted entrainment percentage;",
                    "spatial transitions are approximate."
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
                width = 6, title = "Scenario 1", status = "warning", solidHeader = TRUE, class="compare-scenario-panel",
                tags$div(class="compare-loading-cover", tags$div(class="compare-loading-pill", icon("spinner", class="fa-spin"), " Running scenario 1 models...")), 
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
                tags$div(class="compare-fields-heading", "MODEL INPUTS", tags$span("Hover over an info icon for details")),
                div(class = "compare-input-grid compare-new-model-grid",
                    compare_ccwd_card("compare_s1_exp", "EXP: Combined CVP and SWP Export (cfs):", "6,000", "Latest observed 7-day average combined export; editable."),
                    compare_ccwd_card("compare_s1_ver", "VER: San Joaquin River at Vernalis (cfs):", "3,000", "Latest observed 7-day average Vernalis flow."),
                    compare_ccwd_card("compare_s1_sac", "SAC: Sacramento River at Freeport (cfs):", "18,000", "Latest observed 7-day average Freeport flow."),
                    compare_ccwd_card("compare_s1_east", "EAST: East-Side River Flow (cfs):", "1,500", "Mokelumne + Calaveras + Cosumnes flows; observed 7-day average."),
                    compare_ccwd_card("compare_s1_qgcd", "QGCD: Gross Delta Depletion (TAF/month):", if (is.finite(CCWD_QGCD_DEFAULT)) format(round(CCWD_QGCD_DEFAULT), big.mark=",", trim=TRUE) else "Unavailable", "Source: DAYFLOW-based daily gross channel depletion reference. Daily cfs is converted to monthly-equivalent TAF/month and averaged over seven days; the same default as Exploration."),
                    compare_ccwd_card("compare_s1_qprec", "QPREC: Gross Delta Accretion (TAF/month):", "", "SCKC1 (Stockton Fire Station #4) IEM daily precip_in. For day d: average rainfall d−4 through d, multiply by 682,230 acres / 12,000 to obtain TAF/day, then multiply by calendar days in month. Average seven daily monthly-equivalent values. An isolated missing observation may be estimated from adjacent measured SCKC1 days and explicitly flagged; missing values are never automatically treated as zero."),
                    compare_ccwd_card("compare_s1_dcc_flow", "DCC: Measured/Scenario Flow (cfs):", "", "Closed gates: 0 cfs. Open gates: use measured CDEC DCC flow when available; if missing, estimate from SAC as 0.1846 × SAC + 306.52 cfs."),
                    compare_ccwd_card("compare_s1_dcc", "DCC Gate Status:", NULL, "0 = Closed, 1 = Open. In comparison scenarios the user chooses status.", kind="dcc"),
                    compare_ccwd_card("compare_s1_risk", "Entrainment / Event Horizon Threshold (%):", NULL, "Threshold for PTM map and Event Horizon model.", kind="risk"),
                    compare_ccwd_card("compare_s1_yol", "YOL: Yolo Bypass Flow (cfs):", "1,000", "SACWEIR plus FREWEIR, 7-day average for ECO-PTM."),
                    compare_ccwd_card("compare_s1_moke", "MOKE: Mokelumne Flow (cfs):", "800", "7-day mean MOKE flow, for ECO-PTM.")
                ),
                actionButton("run_compare_s1", "Run Scenario 1", icon = icon("play"), class = "btn-success compare-run-action", width = "100%"),
                tags$div(id="compare_s1_status", class="compare-status", `data-state`="idle",
                         icon("info-circle"), "Ready. Default inputs are available; live source updates may continue loading.")
              ),
              box(
                width = 6, title = "Scenario 2", status = "warning", solidHeader = TRUE, class="compare-scenario-panel",
                tags$div(class="compare-loading-cover", tags$div(class="compare-loading-pill", icon("spinner", class="fa-spin"), " Running scenario 2 models...")), 
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
                tags$div(class="compare-fields-heading", "MODEL INPUTS", tags$span("Hover over an info icon for details")),
                div(class = "compare-input-grid compare-new-model-grid",
                    compare_ccwd_card("compare_s2_exp", "EXP: Combined CVP and SWP Export (cfs):", "6,000", "Latest observed 7-day average combined export; editable."),
                    compare_ccwd_card("compare_s2_ver", "VER: San Joaquin River at Vernalis (cfs):", "3,000", "Latest observed 7-day average Vernalis flow."),
                    compare_ccwd_card("compare_s2_sac", "SAC: Sacramento River at Freeport (cfs):", "18,000", "Latest observed 7-day average Freeport flow."),
                    compare_ccwd_card("compare_s2_east", "EAST: East-Side River Flow (cfs):", "1,500", "Mokelumne + Calaveras + Cosumnes flows; observed 7-day average."),
                    compare_ccwd_card("compare_s2_qgcd", "QGCD: Gross Delta Depletion (TAF/month):", if (is.finite(CCWD_QGCD_DEFAULT)) format(round(CCWD_QGCD_DEFAULT), big.mark=",", trim=TRUE) else "Unavailable", "Source: DAYFLOW-based daily gross channel depletion reference. Daily cfs is converted to monthly-equivalent TAF/month and averaged over seven days; the same default as Exploration."),
                    compare_ccwd_card("compare_s2_qprec", "QPREC: Gross Delta Accretion (TAF/month):", "", "SCKC1 (Stockton Fire Station #4) IEM daily precip_in. For day d: average rainfall d−4 through d, multiply by 682,230 acres / 12,000 to obtain TAF/day, then multiply by calendar days in month. Average seven daily monthly-equivalent values. An isolated missing observation may be estimated from adjacent measured SCKC1 days and explicitly flagged; missing values are never automatically treated as zero."),
                    compare_ccwd_card("compare_s2_dcc_flow", "DCC: Measured/Scenario Flow (cfs):", "", "Closed gates: 0 cfs. Open gates: use measured CDEC DCC flow when available; if missing, estimate from SAC as 0.1846 × SAC + 306.52 cfs."),
                    compare_ccwd_card("compare_s2_dcc", "DCC Gate Status:", NULL, "0 = Closed, 1 = Open. In comparison scenarios the user chooses status.", kind="dcc"),
                    compare_ccwd_card("compare_s2_risk", "Entrainment / Event Horizon Threshold (%):", NULL, "Threshold for PTM map and Event Horizon model.", kind="risk"),
                    compare_ccwd_card("compare_s2_yol", "YOL: Yolo Bypass Flow (cfs):", "1,000", "SACWEIR plus FREWEIR, 7-day average for ECO-PTM."),
                    compare_ccwd_card("compare_s2_moke", "MOKE: Mokelumne Flow (cfs):", "800", "7-day mean MOKE flow, for ECO-PTM.")
                ),
                actionButton("run_compare_s2", "Run Scenario 2", icon = icon("play"), class = "btn-success compare-run-action", width = "100%"),
                tags$div(id="compare_s2_status", class="compare-status", `data-state`="idle",
                         icon("info-circle"), "Ready. Adjust inputs and click Run Scenario 2 when ready.")
              )
            ),
            fluidRow(
              box(width=12, title="Comparison Controls", status="primary", solidHeader=TRUE,
                  tags$div(class="section-heading-with-info", tags$h4("Select Output"),
                           tags$div(class="flow-info-hover", tags$span(class="flow-info-trigger", tabindex="0", role="button", `aria-label`="Comparison output information", icon("info-circle")),
                                    tags$div(class="flow-info-popover", tags$div(class="flow-info-static", "After both scenarios have been run, choose which emulator output to compare. PTM and Event Horizon outputs include a spatial comparison map; ECO-PTM outputs are compared in the chart and table."))
                           )
                  ),
                  uiOutput("compare_two_readiness"),
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
                         ), actionButton("generate_compare_two_map", "Generate Spatial Map", icon=icon("map"), class="btn-primary comparison-map-action"),
                tags$div(id="compare_two_map_message", class="comparison-map-message", role="status", `aria-live`="polite", "Select Generate Spatial Map to build the map."),
                         tags$p(class="figure-note", "Model results appear in the chart and table before spatial interpolation is requested."),
                         tags$div(class="comparison-map-container",
                  leafletOutput("compare_two_spatial_map", height = 650),
                  tags$div(id="compare_two_map_progress", class="comparison-map-progress", role="status", `aria-live`="polite",
                           tags$div(class="comparison-map-progress-content",
                                    icon("spinner", class="fa-spin"),
                                    tags$strong("Generating spatial map"),
                                    tags$div(class="comparison-map-progress-detail", "Preparing map...")))),
              downloadButton(
                "download_compare_two_map",
                "Download Two-Scenario Map (PNG)",
                class = "btn-primary map-download"
              ),
              tags$p(
                class = "figure-note",
                paste(
                  "Scenario-colored shading is a generalized interpolation of",
                  "entrainment emulator outputs between supported DSM2 nodes.",
                  "Opacity represents predicted entrainment percentage;",
                  "spatial transitions are approximate."
                )
              ))),
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
        div(class = "data-access-layout",
            div(class = "data-access-content",        
                fluidRow(
                    box(
                      style = "margin-left:20px;",
                      width = 12,
                      solidHeader = FALSE,
                      
                      div(
                        class = "data-access-intro",
                        
                        h1("Data Access"),
                        
                        h3("Quick Overview"),
                        
                        p(
                          "The Entrainment Dashboard uses five updated LightGBM emulators ",
                          "to estimate particle entrainment, salmonid response, and Event ",
                          "Horizon distance in the Sacramento-San Joaquin Delta. Input ",
                          "definitions remain consistent across the application, but input ",
                          "source and timing depend on the selected workflow."
                        ),
                        
                        tags$ul(
                          tags$li(
                            tags$strong("Exploration: "),
                            "starts with editable current-condition defaults derived from ",
                            "observed data, reference data, or documented calculations."
                          ),
                          
                          tags$li(
                            tags$strong("Weekly Assessments - Measured: "),
                            "uses archived measured values for each 7-day assessment window."
                          ),
                          
                          tags$li(
                            tags$strong("Weekly Assessments - Forecast OMRI: "),
                            "uses the selected OMRI scenario's first seven forecast dates, ",
                            "together with documented reference-series, QPF, regression, ",
                            "or persistence methods where required."
                          )
                        ),
                        
                        div(
                          class = "data-access-note",
                          
                          tags$strong("How to read the tables: "),
                          
                          "The tables distinguish observed measurements, archived forecasts, ",
                          "reference-series values, calculated values, regression estimates, ",
                          "persistence estimates, and explicit user assumptions. Model outputs ",
                          "are machine-learning predictions of simulated DSM2 PTM or ECO-PTM ",
                          "responses; they are not direct observations."
                        ),
                        
                        h3("Model Input and Source Tables"),
                        
                        p(
                          "The updated tables below document the inputs required by each ",
                          "emulator and explain how each input is obtained for Exploration, ",
                          "Weekly Measured assessments, and Weekly OMRI Forecast assessments."
                        ),
                        
                        div(
                          class = "data-access-model-links",
                          
                          tags$a(
                            href = "#intro-eco-ptm",
                            icon("fish"),
                            " ECO-PTM models"
                          ),
                          
                          tags$a(
                            href = "#intro-ptm",
                            icon("project-diagram"),
                            " PTM models"
                          ),
                          
                          tags$a(
                            href = "#intro-event-horizon",
                            icon("ruler-horizontal"),
                            " Entrainment Event Horizon"
                          )
                        ),
                        
                        tags$hr(),
                        
                        h3("Data and Supporting Tools"),
                        
                        p(
                          "The following resources provide the model-automation code, ",
                          "post-processing tools, weekly assessment products, and supporting ",
                          "technical materials."
                        ),
                        
                        fluidRow(
                          column(
                            width = 6,
                            
                            div(
                              class = "data-access-resource-stack",
                              
                              # ------------------------------------------------------------------------
                              # CCWD machine-learning model repository
                              # ------------------------------------------------------------------------
                              div(
                                class = "data-access-resource-card",
                                
                                tags$h4(
                                  icon("github"),
                                  " CCWD Hydraulic Footprint Model Repository"
                                ),
                                
                                p(
                                  "Access the public set of machine-learning models developed through ",
                                  "the Contra Costa Water District's hydraulic footprint project, ",
                                  "including PTM, ECO-PTM emulator, and event horizon models and supporting Delta ",
                                  "location resources."
                                ),
                                
                                tags$a(
                                  class = "btn btn-success",
                                  href = "https://github.com/cchang-ccwater/CCWD_Hydraulic_Footprints/tree/main",
                                  target = "_blank",
                                  rel = "noopener noreferrer",
                                  title = paste(
                                    "Open the CCWD Hydraulic Footprints model repository",
                                    "in a new browser tab"
                                  ),
                                  icon("github"),
                                  " View Model Repository"
                                )
                              ),
                              
                              # ------------------------------------------------------------------------
                              # DSM2 model-automation and Shiny application repository
                              # ------------------------------------------------------------------------
                              div(
                                class = "data-access-resource-card",
                                
                                tags$h4(
                                  icon("github"),
                                  " Model Automation Repository"
                                ),
                                
                                p(
                                  "Access the DSM2 weekly modeling workflow, PTM and ECO-PTM ",
                                  "post-processing tools, SacPAS reporting code, model documentation, ",
                                  "and the Emulator/Event Horizon Shiny application."
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
                                  " View Application Repository"
                                )
                              )
                            )
                          ),
                          
                          column(
                            width = 6,
                            
                            div(
                              class = "data-access-resource-card",
                              
                              tags$h4(
                                icon("chart-line"),
                                " SacPAS Weekly Assessment"
                              ),
                              
                              p(
                                "Open the weekly Sacramento River Winter-Run assessment ",
                                "forecast and supporting model-evaluation products."
                              ),
                              
                              tags$a(
                                class = "btn btn-primary",
                                href = paste0(
                                  "https://can01.safelinks.protection.outlook.com/?",
                                  "url=https%3A%2F%2Fcbr.washington.edu%2Fsacramento%2F",
                                  "assessments%2Ftest%2Fforecast_sacpas.html&",
                                  "data=05%7C02%7CLaura.Manuel%40stantec.com%7C",
                                  "7d690048e1804ee4cc7408deccc5ac84%7C",
                                  "413c6f2c219a469297d3f2b4d80281e7%7C0%7C0%7C",
                                  "639173346569845423%7CUnknown%7C",
                                  "TWFpbGZsb3d8eyJFbXB0eU1hcGkiOnRydWUsIlYiOiIwLjAuMDAwMCIs",
                                  "IlAiOiJXaW4zMiIsIkFOIjoiTWFpbCIsIldUIjoyfQ%3D%3D%7C0%7C",
                                  "%7C%7C&sdata=OIZCfSzN%2BkO8UrHo3QV4D4Aike6mQpP7LvkmfgoCKa0%3D&",
                                  "reserved=0"
                                ),
                                target = "_blank",
                                rel = "noopener noreferrer",
                                title = "Open the SacPAS weekly assessment in a new browser tab",
                                icon("external-link-alt"),
                                " Open SacPAS Assessment"
                              )
                            )
                          )
                        )
                      )
                   
                   
                  ),
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-eco-ptm","P1 DSM2 ECO-PTM emulator models",style = "margin-left:10px;"),
                      div(class = "data-access-table-section",
                          tags$p(class = "data-access-table-hint", icon("arrows-alt-h"),
                                 "Scroll within this table to view all seven columns, including Source / provenance."),
                          div(class = "data-access-table-scroll", tabindex = "0", role = "region",
                              `aria-label` = "ECO-PTM model inputs: horizontally scrollable table",
                              gt::gt_output("ecoptm_inputs_table"))
                      ),
                      tags$hr(),
                      fluidRow(
                        column(
                          width = 7,
                          
                          box(
                            title = tagList(
                              icon("map-marker-alt"),
                              " ECO-PTM Emulator Model Input Stations"
                            ),
                            width = 12,
                            height = "720px",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            leaflet::leafletOutput(
                              "eco_ptm_map",
                              width = "100%",
                              height = "650px"
                            )
                          )
                        ),
                        
                        column(
                          width = 5,
                          
                          box(
                            title = "Model Parameters",
                            width = 12,
                            height = "720px",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            div(
                              class = "data-access-parameter-table",
                              style = paste0(
                                "height:650px;",
                                "overflow-y:auto;",
                                "overflow-x:auto;"
                              ),
                              
                              gt::gt_output(
                                "ecoptm_parameters_table"
                              )
                            )
                          )
                        )
                      )
                  ),
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-ptm","P2 DSM2 PTM emulator models",style = "margin-left:10px;"),
                      div(class = "data-access-table-section",
                          tags$p(class = "data-access-table-hint", icon("arrows-alt-h"),
                                 "Scroll within this table to view all seven columns, including Source / provenance."),
                          div(class = "data-access-table-scroll", tabindex = "0", role = "region",
                              `aria-label` = "PTM model inputs: horizontally scrollable table",
                              gt::gt_output("ptm_inputs_table"))
                      ),
                      tags$hr(),
                      fluidRow(
                        column(
                          width = 7,
                          
                          box(
                            title = tagList(
                              icon("map-marked-alt"),
                              " PTM Emulator Release Nodes and Model Input Stations"
                            ),
                            width = 12,
                            height = "720px",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            tabsetPanel(
                              id = "ptm_map_tabs",
                              type = "tabs",
                              
                              tabPanel(
                                title = "7-Day",
                                leaflet::leafletOutput(
                                  "node_station_map_7day",
                                  width = "100%",
                                  height = "620px"
                                )
                              ),
                              
                              tabPanel(
                                title = "30-Day",
                                leaflet::leafletOutput(
                                  "node_station_map_30day",
                                  width = "100%",
                                  height = "620px"
                                )
                              )
                            )
                          )
                        ),
                        
                        column(
                          width = 5,
                          
                          box(
                            title = "Model Parameters",
                            width = 12,
                            height = "720px",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            div(
                              class = "data-access-parameter-table",
                              style = paste0(
                                "height:650px;",
                                "overflow-y:auto;",
                                "overflow-x:auto;"
                              ),
                              
                              gt::gt_output(
                                "ptm_parameters_table"
                              )
                            )
                          )
                        )
                      )
                  ),        
                  box(style = "margin-left:20px;",width = 12,solidHeader = FALSE,
                      h3(id = "intro-event-horizon","P3 Model for the Entrainment Event Horizon",style = "margin-left:10px;"),
                      div(class = "data-access-table-section",
                          tags$p(class = "data-access-table-hint", icon("arrows-alt-h"),
                                 "Scroll within this table to view all seven columns, including Source / provenance."),
                          div(class = "data-access-table-scroll", tabindex = "0", role = "region",
                              `aria-label` = "Event Horizon model inputs: horizontally scrollable table",
                              gt::gt_output("horizon_inputs_table"))
                      ),
                      tags$hr(),
                      fluidRow(
                        column(
                          width = 7,
                          
                          box(
                            title = tagList(
                              icon("route"),
                              " Entrainment Event Horizon Model Input Stations"
                            ),
                            width = 12,
                            height = "720px",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            leaflet::leafletOutput(
                              "horizon_map",
                              width = "100%",
                              height = "650px"
                            )
                          )
                        ),
                        
                        column(
                          width = 5,
                          
                          box(
                            title = "Model Parameters",
                            width = 12,
                            height = "720px",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            div(
                              class = "data-access-parameter-table",
                              style = paste0(
                                "height:650px;",
                                "overflow-y:auto;",
                                "overflow-x:auto;"
                              ),
                              
                              gt::gt_output(
                                "horizon_parameter_table"
                              )
                            )
                          )
                        )
                      )
                  ),
                  # Client-facing methodology supplement; no model or server calculations are changed.
                  box(
                    style = "margin-left:20px;",
                    width = 12,
                    solidHeader = FALSE,
                    div(
                      class = "ccwd-data-methodology",
                      tags$h3(id = "ccwd-input-methodology", "Additional Data Sources and Calculation Methods"),
                      tags$p(
                        "The model input tables above identify the required variables and their sources. ",
                        "The sections below explain the preparation of three derived inputs: Gross Delta Accretion (QPREC), ",
                        "gross channel depletion (QGCD), and Delta Cross Channel flow (DCC). ",
                        "Observed measurements, forecasts, and estimates are distinguished throughout."
                      ),
                      tabsetPanel(
                        id = "data_methodology_tabs",
                        type = "tabs",
                        tabPanel(
                          title = "QPREC | Gross Delta Accretion (TAF/month)",
                          tags$h4("Observed precipitation"),
                          tags$p(
                            "Source: Stockton Fire Station No. 4 (SCKC1), the precipitation station ",
                            "identified in DWR DAYFLOW documentation. Daily observations are obtained through ",
                            "the Iowa Environmental Mesonet (IEM). Exploration uses the latest complete observed ",
                            "calculation window, while Weekly Measured uses observations corresponding to the ",
                            "selected archive dates. A recorded zero is valid; a missing observation is not assumed zero."
                          ),
                          tags$p(class = "ccwd-method-links",
                            tags$a(href = "https://mesonet.agron.iastate.edu/sites/site.php?network=CA_DCP&station=SCKC1", target = "_blank", rel = "noopener noreferrer", "SCKC1 observations", icon("external-link-alt")),
                            "  |  ",
                            tags$a(href = "https://data.ca.gov/@california-department-of-water-resources/dayflow/r/e8410426-9cdc-4ef4-8699-83836b0ed73f", target = "_blank", rel = "noopener noreferrer", "DWR DAYFLOW documentation", icon("external-link-alt"))
                          ),
                          tags$h4("Forecast precipitation for Weekly OMRI scenarios"),
                          tags$p(
                            "Archived California-Nevada River Forecast Center (CNRFC) Quantitative Precipitation ",
                            "Forecast (QPF) grids provide six-hour forecast precipitation. The script selects ",
                            "the appropriate historical forecast issuance, extracts precipitation at the grid ",
                            "cell nearest SCKC1, and aggregates the six-hour intervals to California-local ",
                            "daily totals. These forecast totals are combined with preceding SCKC1 observations ",
                            "for the five-day runoff calculation. All OMRI scenarios for an assessment use the ",
                            "same forecast precipitation series."
                          ),
                          tags$p(class = "ccwd-method-links",
                            tags$a(href = "https://www.cnrfc.noaa.gov/arc_search.php", target = "_blank", rel = "noopener noreferrer", "CNRFC forecast archive", icon("external-link-alt")),
                            "  |  ",
                            tags$a(href = "https://www.cnrfc.noaa.gov/netcdfInfo.php", target = "_blank", rel = "noopener noreferrer", "NetCDF grid documentation", icon("external-link-alt"))
                          ),
                          tags$h4("Gross Delta Accretion (QPREC): five-day distribution and monthly-equivalent conversion"),
                          tags$p(
                            "QPREC is expressed in TAF/month for model input. The method distributes each day's " ,
                            "measured or forecast rainfall evenly over five days (the rainfall day plus four following days). " ,
                            "When rainfall occurs on consecutive days, the distributed contributions overlap and are added."
                          ),
                          tags$h4("Worked example: rainfall on two consecutive days"),
                          tags$p(
                            "Suppose Day 1 receives 10 inches of rainfall and Day 2 receives 7 inches, with no additional " ,
                            "rainfall on Days 3 through 6. The Day 1 event contributes 10 / 5 = 2.0 inches " ,
                            "per day on Days 1-5. The Day 2 event contributes 7 / 5 = 1.4 inches " ,
                            "per day on Days 2-6. On Day 2, both contributions are included: 2.0 + 1.4 = 3.4 inches."
                          ),
                          div(class = "ccwd-method-example-table-wrap",
                            tags$table(class = "table table-bordered table-striped ccwd-method-example-table",
                              tags$thead(tags$tr(
                                tags$th("Day"),
                                tags$th("Measured rainfall (in)"),
                                tags$th("From Day 1 event (in)"),
                                tags$th("From Day 2 event (in)"),
                                tags$th("Distributed daily depth (in)")
                              )),
                              tags$tbody(
                                tags$tr(tags$td("1"), tags$td("10"), tags$td("2.0"), tags$td("0"),   tags$td(tags$strong("2.0"))),
                                tags$tr(tags$td("2"), tags$td("7"),  tags$td("2.0"), tags$td("1.4"), tags$td(tags$strong("3.4"))),
                                tags$tr(tags$td("3"), tags$td("0"),  tags$td("2.0"), tags$td("1.4"), tags$td(tags$strong("3.4"))),
                                tags$tr(tags$td("4"), tags$td("0"),  tags$td("2.0"), tags$td("1.4"), tags$td(tags$strong("3.4"))),
                                tags$tr(tags$td("5"), tags$td("0"),  tags$td("2.0"), tags$td("1.4"), tags$td(tags$strong("3.4"))),
                                tags$tr(tags$td("6"), tags$td("0"),  tags$td("0"),   tags$td("1.4"), tags$td(tags$strong("1.4"))),
                                tags$tr(class = "ccwd-method-example-total",
                                  tags$td(tags$strong("Total")), tags$td(tags$strong("17")),
                                  tags$td(tags$strong("10.0")), tags$td(tags$strong("7.0")),
                                  tags$td(tags$strong("17.0")))
                              )
                            )
                          ),
                          tags$p(class = "ccwd-method-example-caption",
                            "Illustrative rainfall events (not actual station observations). The distributed depth represents " ,
                            "each day's rainfall contribution to the runoff calculation, not newly measured rainfall. " ,
                            "The total distributed depth remains 17 inches, equal to the original rainfall total."
                          ),
                          tags$h4("Converting the distributed depth to the QPREC model input"),
                          tags$ol(
                            tags$li("For each assessment date, add contributions from rainfall on that date and the four preceding dates. Equivalently: distributed depth = (today's rainfall + rainfall on previous four days) / 5."),
                            tags$li("Convert the daily distributed depth to volume over the assumed Delta area: QPREC (TAF/day) = distributed depth (inches) x 682,230 acres / 12,000."),
                            tags$li("Convert the result to a monthly-equivalent rate: daily QPREC (TAF/month) = QPREC (TAF/day) x calendar days in the month of that date."),
                            tags$li("For a seven-day model input window, average the seven daily monthly-equivalent QPREC values. For a window spanning two months, use each day's own month length.")
                          ),
                          tags$p(class = "ccwd-method-equation",
                            tags$strong("Day 2 worked calculation: "),
                            "Distributed depth = (10 + 7) / 5 = 3.4 in; ",
                            "QPREC = 3.4 x 682,230 / 12,000 = 193.30 TAF/day; ",
                            "if Day 2 occurs in a 30-day month, monthly-equivalent QPREC = 193.30 x 30 = approximately 5,799 TAF/month."
                          ),
                          tags$p(class = "ccwd-method-example-caption",
                            "The monthly-equivalent value is a daily runoff rate expressed on a monthly scale; " ,
                            "it is not an observed full-month runoff volume."
                          ),
                          tags$p(
                            class = "ccwd-method-caution",
                            tags$strong("Forecast and missing-data assumptions: "),
                            "When archived forecast coverage is shorter than seven complete days, the last complete ",
                            "forecast day's rainfall is carried forward to the remaining days and identified as ",
                            "an assumption. If a valid CNRFC forecast cannot be retrieved, the application may use ",
                            "a labeled persistence estimate based on the latest observed SCKC1-derived daily runoff. ",
                            "Limited isolated gaps in observed rainfall may be interpolated and identified. ",
                            "These monthly-equivalent inputs are application estimates, not published DAYFLOW ",
                            "calendar-month totals."
                          )
                        ),
                        tabPanel(
                          title = "QGCD | Channel Depletion",
                          tags$h4("Gross Delta channel depletion"),
                          tags$p(
                            "Source: DAYFLOW-based daily gross channel depletion reference values. ",
                            "The underlying daily series is expressed in cubic feet per second (cfs) and varies by ",
                            "calendar day. QGCD is supplied separately from QPREC; this input is gross channel ",
                            "depletion rather than net channel depletion."
                          ),
                          tags$h4("Calculation"),
                          tags$p(class = "ccwd-method-equation",
                            "Daily monthly-equivalent QGCD (TAF/month) = daily QGCD (cfs) x 86,400 / 43,560 x days in month / 1,000"
                          ),
                          tags$p(
                            "Exploration uses the latest seven complete calendar days. Weekly Measured and Weekly ",
                            "Forecast OMRI use the reference values corresponding to their respective seven dates. ",
                            "For each window, the seven daily monthly-equivalent values are averaged; for ",
                            "cross-month windows, each date uses its own month's length. The reference is ",
                            "independent of the selected OMRI export scenario."
                          ),
                          tags$p(class = "ccwd-method-links",
                            tags$a(href = "https://data.ca.gov/@california-department-of-water-resources/dayflow/r/e8410426-9cdc-4ef4-8699-83836b0ed73f", target = "_blank", rel = "noopener noreferrer", "DWR DAYFLOW reference", icon("external-link-alt"))
                          ),
                          tags$p(class = "ccwd-method-caution",
                            "The resulting TAF/month quantity is a daily rate converted to a monthly-equivalent ",
                            "value, not a directly observed full-month total."
                          )
                        ),
                        tabPanel(
                          title = "DCC | Delta Cross Channel Flow",
                          tags$h4("Gate status and flow are separate inputs"),
                          tags$p(
                            "DCC gate status is a binary indicator (0 = closed; 1 = open). Exploration allows ",
                            "users to select the gate status. For Weekly Measured, the archived status on the ",
                            "last measured day of each assessment window is used; for Weekly Forecast OMRI, the ",
                            "first forecast day's archived status is applied across the seven-day window."
                          ),
                          tags$h4("Measured and estimated DCC flow (cfs)"),
                          tags$ul(
                            tags$li(tags$strong("Gates closed: "), "DCC flow is always forced to 0 cfs."),
                            tags$li(tags$strong("Gates open - observed dates: "),
                              "The app retrieves measured hourly FLOW data from CDEC station DLC (sensor 20), ",
                              "aggregates valid hourly measurements into daily mean cfs, and calculates the ",
                              "assessment-window mean. Where an observed daily DCC flow is unavailable, ",
                              "a regression estimate is substituted using measured Sacramento River flow at Freeport (SAC) for that date."),
                            tags$li(tags$strong("Gates open - forecast dates: "),
                              "DCC flow is estimated using the same regression with scenario-specific forecast SAC ",
                              "at Freeport. Daily estimates are averaged for the seven-day input.")
                          ),
                          tags$p(class = "ccwd-method-equation",
                            "DCC flow (cfs) = 0.1846 x SAC at Freeport (cfs) + 306.52"
                          ),
                          tags$p(class = "ccwd-method-links",
                            tags$a(href = "https://cdec.water.ca.gov/dynamicapp/QueryF?s=DLC", target = "_blank", rel = "noopener noreferrer", "CDEC DLC flow station", icon("external-link-alt")),
                            "  |  ",
                            tags$a(href = "https://cdec.water.ca.gov/", target = "_blank", rel = "noopener noreferrer", "CDEC data portal", icon("external-link-alt"))
                          ),
                          tags$p(class = "ccwd-method-caution",
                            "Regression values are estimates, not measured DCC discharge. ",
                            "The PTM 30-day emulator uses DCC flow (cfs); ECO-PTM uses gate status (0/1)."
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

