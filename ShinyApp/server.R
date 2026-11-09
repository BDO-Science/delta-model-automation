
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(viridis)
library(sf)
library(leaflet)
library(lwgeom)
library(gstat)
library(sp)
library(plotly)
library(gt)
library(lightgbm)
library(xgboost)
library(metR)
library(mgcv)
library (ps)
source("ccwd_qgcd_excel.R", local=TRUE)
APP_NAME <- "Entrainment Dashboard"
APP_VERSION <- "1.1.0"


MODEL_DIR <- "STN_EMULATOR/models"
OUTPUT_DIR <- "STN_EMULATOR/Output"
SHAPE_DIR <- "STN_EMULATOR/shapefiles"
# ------------------------------------------------------------------
# Public GitHub archive configuration
# ------------------------------------------------------------------

GITHUB_OWNER <- "BDO-Science"
GITHUB_REPO <- "delta-model-automation"
GITHUB_BRANCH <- "main"
GITHUB_ARCHIVE_PATH <- "DSM2/Model/DataExternal"

GITHUB_API_BASE <- paste0(
  "https://api.github.com/repos/",
  GITHUB_OWNER,
  "/",
  GITHUB_REPO,
  "/contents"
)

# Remote files are downloaded here before being parsed.
# The cache lasts for the lifetime of the running R process.
GITHUB_ARCHIVE_CACHE_DIR <- file.path(
  tempdir(),
  "delta-model-automation",
  "DataExternal"
)

dir.create(
  GITHUB_ARCHIVE_CACHE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)
DSM2_NODE_SHP <- file.path( SHAPE_DIR,  "i12_DSM2_Grid_V2025-05-28_Hist_nodes.shp")
DSM2_CHANNEL_SHP <- file.path(  SHAPE_DIR,  "i12_DSM2_Grid_V2025-05-28_Hist_channels_centerlines.shp")
DSM2_CHANNEL_DEF <- file.path(  "STN_EMULATOR",  "channel_std_delta_grid_NAVD_20121214.txt")
DSM2_PATH_FILE <- file.path(  "STN_EMULATOR",  "Region_Location_Node_Path.csv")
source("Server_Data_Access.R")

# ------------------------------------------------------------------
# Updated CCWD public-release models for the Exploration tab.
# Files are already present directly in STN_EMULATOR/models/.
# ------------------------------------------------------------------
NEW_CCWD_MODEL_FILES <- c(
  PTM7 = "PTM7_lightgbm.txt",
  PTM30 = "PTM30_lightgbm.txt",
  ECO_survival = "ECOPTM_survival_lightgbm.txt",
  ECO_interior = "ECOPTM_interior_lightgbm.txt",
  EH = "EH_lightgbm.txt"
)
NEW_CCWD_MODEL_DIR <- MODEL_DIR
NEW_CCWD_MODEL_PATHS <- file.path(NEW_CCWD_MODEL_DIR, NEW_CCWD_MODEL_FILES)
names(NEW_CCWD_MODEL_PATHS) <- names(NEW_CCWD_MODEL_FILES)
missing_new_models <- NEW_CCWD_MODEL_PATHS[!file.exists(NEW_CCWD_MODEL_PATHS)]
if (length(missing_new_models)) {
  stop(
    "Missing new CCWD LightGBM model file(s): ",
    paste(basename(missing_new_models), collapse = ", "),
    ". Expected in ", normalizePath(MODEL_DIR, mustWork = FALSE),
    ". Open the app from the ShinyApp directory."
  )
}
message("New Exploration LightGBM models found: ",
        paste(basename(NEW_CCWD_MODEL_PATHS), collapse = ", "))

# Old archive prediction functions below still use their original input
# definitions. We deliberately do NOT pass the revised model files to those
# legacy functions: their feature schemas differ. The archive migration is
# separate work. Missing legacy files are not a startup error or warning.
load_legacy_lightgbm <- function(filename) {
  path <- file.path(MODEL_DIR, filename)
  if (!file.exists(path)) return(NULL)
  lightgbm::lgb.load(path)
}
ptm7_model <- load_legacy_lightgbm("PTM_Entrainment7d_lightgbm.txt")
ptm30_model <- load_legacy_lightgbm("PTM_Entrainment30d_lightgbm.txt")
eco_survival_model <- load_legacy_lightgbm("ECOPTM_survival_lightgbm.txt")
eco_interior_model <- load_legacy_lightgbm("ECOPTM_interior_lightgbm.txt")
event_horizon_model <- NULL
get_event_horizon_model <- local({
  model <- NULL
  function() {
    if (is.null(model)) {
      legacy_path <- file.path(MODEL_DIR, "xgb_event_horizon.json")
      if (!file.exists(legacy_path)) stop(
        "Historical/legacy Event Horizon model unavailable. ",
        "The Exploration tab uses EH_lightgbm.txt. ",
        "The historical archive workflow has not yet been migrated."
      )
      model <<- xgboost::xgb.load(legacy_path)
    }
    model
  }
})
bound_percent <- function(x) pmax(0, pmin(100, x))

read_lgb_features <- function(model_file) {
  header <- readLines(model_file, n = 60, warn = FALSE)
  feature_line <- grep("^feature_names=", header, value = TRUE)
  if (length(feature_line) != 1) {
    stop("Could not read LightGBM feature names from: ", model_file)
  }
  strsplit(sub("^feature_names=", "", feature_line), "\\s+")[[1]]
}

fmt_int <- function(x) {
  format(
    round(x),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

fmt_one <- function(x) {
  format(
    round(x, 1),
    nsmall = 1,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

format_comparison_table <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }

  if ("Risk_Level_Percent" %in% names(df)) {
    df$Risk_Level_Percent <- fmt_int(
      df$Risk_Level_Percent
    )
  }

  input_columns <- intersect(
    names(df),
    c(
      "EXP",
      "VER",
      "SAC",
      "EAST",
      "XGEO",
      "YOL",
      "YOLO",
      "MOKE",
      "MOK"
    )
  )

  if (length(input_columns) > 0) {
    df[input_columns] <- lapply(
      df[input_columns],
      fmt_int
    )
  }

  prediction_columns <- intersect(
    names(df),
    c(
      "Prediction_Raw",
      "Prediction_Final"
    )
  )

  if (length(prediction_columns) > 0) {
    is_event_horizon <- if ("Model" %in% names(df)) {
      grepl(
        "Event Horizon",
        as.character(df$Model),
        ignore.case = TRUE
      )
    } else {
      rep(FALSE, nrow(df))
    }

    for (column_name in prediction_columns) {
      values <- suppressWarnings(
        as.numeric(
          df[[column_name]]
        )
      )

      df[[column_name]] <- ifelse(
        is.na(values),
        as.character(df[[column_name]]),
        ifelse(
          is_event_horizon,
          fmt_one(values),
          fmt_int(values)
        )
      )
    }
  }

  df
}
parse_dsm2_channel_definition <- function(path) {
  
  lines <- readLines(path, warn = FALSE)
  
  channel_start <- which(trimws(lines) == "CHANNEL")[1]
  
  if (is.na(channel_start)) {
    stop("Could not find CHANNEL block in: ", path)
  }
  
  header_line <- channel_start + 1
  
  end_candidates <- which(
    seq_along(lines) > header_line &
      trimws(lines) == "END"
  )
  
  if (length(end_candidates) == 0) {
    stop("Could not find END after CHANNEL block in: ", path)
  }
  
  channel_end <- end_candidates[1]
  
  channel_lines <- lines[(header_line + 1):(channel_end - 1)]
  
  channel_lines <- channel_lines[
    nzchar(trimws(channel_lines)) &
      !grepl("^\\s*#", channel_lines)
  ]
  
  channel_table <- read.table(
    text = paste(channel_lines, collapse = "\n"),
    header = FALSE,
    col.names = c(
      "CHAN_NO",
      "LENGTH_FT",
      "MANNING",
      "DISPERSION",
      "UPNODE",
      "DOWNNODE"
    ),
    stringsAsFactors = FALSE
  )
  
  channel_table |>
    dplyr::mutate(
      CHAN_NO = as.integer(CHAN_NO),
      LENGTH_FT = as.numeric(LENGTH_FT),
      UPNODE = as.integer(UPNODE),
      DOWNNODE = as.integer(DOWNNODE)
    )
}
find_first_existing_column <- function(data, candidates, label) {
  
  normalized_names <- toupper(gsub("[^A-Za-z0-9]", "", names(data)))
  normalized_candidates <- toupper(gsub("[^A-Za-z0-9]", "", candidates))
  
  match_index <- match(normalized_candidates, normalized_names)
  match_index <- match_index[!is.na(match_index)]
  
  if (length(match_index) == 0) {
    stop(
      "Could not identify ",
      label,
      " column. Available columns are: ",
      paste(names(data), collapse = ", ")
    )
  }
  
  names(data)[match_index[1]]
}
# Legacy ECO feature sets are only needed by archived / non-Exploration code.
# Do not fail application startup when only the new models are installed.
legacy_feature_names <- function(filename) {
  path <- file.path(MODEL_DIR, filename)
  if (!file.exists(path)) return(character())
  read_lgb_features(path)
}
eco_survival_features <- legacy_feature_names("ECOPTM_survival_lightgbm.txt")
eco_interior_features <- legacy_feature_names("ECOPTM_interior_lightgbm.txt")


# ------------------------------------------------------------------
# Observed-condition trailing 7-day flow retrieval
# ------------------------------------------------------------------

cdec_daily_link <- function(station_id, end_date = Sys.Date()) {
  paste0(
    "https://cdec.water.ca.gov/dynamicapp/QueryDaily?s=",
    station_id,
    "&end=",
    format(as.Date(end_date), "%Y-%m-%d")
  )
}

# Historical DCC measurement reference supplied by the project team:
# CDEC plotter sensor_no=33744. Plot-series IDs are NOT the same as the
# SensorNums argument of CDEC's JSONDataServlet.
# DLC / sensor 20 is an API mapping to VERIFY against CDEC metadata.
# If CDEC metadata show a different station/sensor or reporting duration,
# update these configuration values before scientific production use.
DCC_CDEC_PLOT_ID <- 33744L
DCC_CDEC_STATION <- "DLC"
DCC_CDEC_SENSOR <- 20L
DCC_CDEC_PLOT_URL <- paste0(
  "https://cdec.water.ca.gov/cdecplotter/JspPlotServlet?",
  "sensor_no=33744&end=&geom=small&interval=2"
)

observed_source_links <- list(
  CLC = cdec_daily_link("CLC"),
  TRP = cdec_daily_link("TRP"),
  VNS = cdec_daily_link("VNS"),
  FPT = cdec_daily_link("FPT"),
  FRE = cdec_daily_link("FRE"),
  NHG = cdec_daily_link("NHG"),
  MOK = "https://www.ebmud.com/water/about-your-water/water-supply/water-supply-reports/daily-water-supply-report",
  COS = "https://waterdata.usgs.gov/monitoring-location/USGS-11335000/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true",
  XGEO_A = "https://waterdata.usgs.gov/monitoring-location/USGS-11447890/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true",
  XGEO_C = "https://waterdata.usgs.gov/monitoring-location/USGS-11447905/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true",
  DCC = DCC_CDEC_PLOT_URL
)

download_observed_source_file <- function(
  url,
  fileext = ".txt",
  attempts = 3,
  pause_seconds = 1
) {
  destination <- tempfile(fileext = fileext)
  old_timeout <- getOption("timeout")
  previous_user_agent <- getOption("HTTPUserAgent")

  on.exit(
    {
      options(
        timeout = old_timeout,
        HTTPUserAgent = previous_user_agent
      )

      if (file.exists(destination)) {
        unlink(destination)
      }
    },
    add = TRUE
  )

  options(
    timeout = max(120, old_timeout),
    HTTPUserAgent = paste(
      "Mozilla/5.0",
      "(Windows NT 10.0; Win64; x64)",
      "AppleWebKit/537.36",
      "Chrome/126.0 Safari/537.36"
    )
  )

  last_error <- NULL

  for (attempt in seq_len(attempts)) {
    if (file.exists(destination)) {
      unlink(destination)
    }

    success <- tryCatch(
      {
        suppressWarnings(
          utils::download.file(
            url = url,
            destfile = destination,
            mode = "wb",
            method = "libcurl",
            quiet = TRUE
          )
        )

        file.exists(destination) &&
          isTRUE(file.info(destination)$size > 0)
      },
      error = function(error_condition) {
        last_error <<- conditionMessage(error_condition)
        FALSE
      }
    )

    if (isTRUE(success)) {
      return(readLines(destination, warn = FALSE))
    }

    if (attempt < attempts) {
      Sys.sleep(pause_seconds * attempt)
    }
  }

  stop(
    paste0(
      "The source could not be reached after ",
      attempts,
      " attempts",
      if (!is.null(last_error)) paste0(": ", last_error) else "."
    )
  )
}

parse_observed_dates <- function(values) {
  text_values <- trimws(as.character(values))

  parsed <- suppressWarnings(
    as.Date(
      substr(text_values, 1, 10),
      format = "%Y-%m-%d"
    )
  )

  missing <- is.na(parsed)

  if (any(missing)) {
    parsed[missing] <- suppressWarnings(
      as.Date(
        substr(text_values[missing], 1, 10),
        format = "%m/%d/%Y"
      )
    )
  }

  parsed
}

decode_cdec_html_text <- function(value) {
  value <- gsub("(?is)<br\\s*/?>", " ", value, perl = TRUE)
  value <- gsub("(?is)<[^>]+>", " ", value, perl = TRUE)
  value <- gsub("&nbsp;|&#160;", " ", value, ignore.case = TRUE)
  value <- gsub("&amp;", "&", value, ignore.case = TRUE)
  value <- gsub("&lt;", "<", value, ignore.case = TRUE)
  value <- gsub("&gt;", ">", value, ignore.case = TRUE)
  value <- gsub("&quot;", "\"", value, ignore.case = TRUE)
  value <- gsub("&#39;|&apos;", "'", value, ignore.case = TRUE)
  trimws(gsub("\\s+", " ", value))
}


extract_cdec_query_daily_rows <- function(html_lines) {
  html_text <- paste(html_lines, collapse = "\n")

  row_matches <- regmatches(
    html_text,
    gregexpr(
      "(?is)<tr\\b[^>]*>.*?</tr>",
      html_text,
      perl = TRUE
    )
  )[[1]]

  if (length(row_matches) == 0) {
    stop("The CDEC daily page did not contain a readable data table.")
  }

  rows <- lapply(
    row_matches,
    function(row_text) {
      cells <- regmatches(
        row_text,
        gregexpr(
          "(?is)<t[dh]\\b[^>]*>.*?</t[dh]>",
          row_text,
          perl = TRUE
        )
      )[[1]]

      if (length(cells) == 0) {
        return(character())
      }

      vapply(
        cells,
        decode_cdec_html_text,
        character(1)
      )
    }
  )

  rows[lengths(rows) > 0]
}


parse_cdec_numeric_value <- function(value) {
  cleaned <- trimws(as.character(value)[1])

  if (
    length(cleaned) == 0 ||
    is.na(cleaned) ||
    cleaned %in% c("", "---", "--", "M", "m", "BRT")
  ) {
    return(NA_real_)
  }

  cleaned <- gsub(",", "", cleaned, fixed = TRUE)

  match_position <- regexpr(
    "-?[0-9]+(?:\\.[0-9]+)?",
    cleaned,
    perl = TRUE
  )

  if (length(match_position) == 0 || match_position[1] < 0) {
    return(NA_real_)
  }

  numeric_text <- regmatches(
    cleaned,
    match_position
  )

  suppressWarnings(as.numeric(numeric_text)[1])
}


read_cdec_daily_values <- function(
  station_id,
  sensor_number,
  end_date = Sys.Date(),
  lookback_days = 30
) {
  station_id <- toupper(trimws(station_id))
  end_date <- as.Date(end_date)
  start_date <- end_date - lookback_days

  query_url <- paste0(
    "https://cdec.water.ca.gov/dynamicapp/req/JSONDataServlet?",
    "Stations=", utils::URLencode(station_id, reserved = TRUE),
    "&SensorNums=", utils::URLencode(as.character(sensor_number), reserved = TRUE),
    "&dur_code=D",
    "&Start=", format(start_date, "%Y-%m-%d"),
    "&End=", format(end_date, "%Y-%m-%d")
  )

  response_text <- paste(
    download_observed_source_file(
      query_url,
      fileext = ".json"
    ),
    collapse = "\n"
  )

  if (!nzchar(trimws(response_text))) {
    stop(
      paste0(
        "CDEC returned an empty response for ",
        station_id,
        " sensor ",
        sensor_number,
        "."
      )
    )
  }

  cdec_data <- tryCatch(
    jsonlite::fromJSON(
      response_text,
      simplifyDataFrame = TRUE
    ),
    error = function(error_condition) {
      stop(
        paste0(
          "The CDEC JSON response could not be parsed for ",
          station_id,
          " sensor ",
          sensor_number,
          ": ",
          conditionMessage(error_condition)
        )
      )
    }
  )

  if (is.null(cdec_data) || length(cdec_data) == 0) {
    stop(
      paste0(
        "CDEC returned no daily records for ",
        station_id,
        " sensor ",
        sensor_number,
        "."
      )
    )
  }

  cdec_data <- as.data.frame(
    cdec_data,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  normalized_names <- tolower(
    gsub(
      "[^a-z0-9]",
      "",
      names(cdec_data)
    )
  )

  date_candidates <- c(
    "obsdate",
    "date",
    "datetime"
  )

  value_candidates <- c(
    "value",
    "obsvalue"
  )

  date_index <- match(
    date_candidates,
    normalized_names,
    nomatch = 0
  )

  date_index <- date_index[date_index > 0][1]

  value_index <- match(
    value_candidates,
    normalized_names,
    nomatch = 0
  )

  value_index <- value_index[value_index > 0][1]

  if (
    length(date_index) == 0 ||
    is.na(date_index) ||
    length(value_index) == 0 ||
    is.na(value_index)
  ) {
    stop(
      paste0(
        "The CDEC response for ",
        station_id,
        " sensor ",
        sensor_number,
        " did not contain recognizable date and value fields. ",
        "Returned fields: ",
        paste(names(cdec_data), collapse = ", "),
        "."
      )
    )
  }

  result <- data.frame(
    Date = parse_observed_dates(
      cdec_data[[date_index]]
    ),
    Value = suppressWarnings(
      as.numeric(
        gsub(
          ",",
          "",
          as.character(cdec_data[[value_index]]),
          fixed = TRUE
        )
      )
    ),
    stringsAsFactors = FALSE
  )

  # All CDEC series used here are flows in cfs and should not be
  # negative. CDEC can return negative missing-value sentinels such as
  # -9999; exclude those before calculating the trailing average.
  result <- result[
    !is.na(result$Date) &
      is.finite(result$Value) &
      result$Value >= 0 &
      result$Date >= start_date &
      result$Date <= end_date,
    ,
    drop = FALSE
  ]

  result <- result[
    !duplicated(result$Date, fromLast = TRUE),
    ,
    drop = FALSE
  ]

  result <- result[
    order(result$Date),
    ,
    drop = FALSE
  ]

  if (nrow(result) == 0) {
    stop(
      paste0(
        "CDEC returned a response for ",
        station_id,
        " sensor ",
        sensor_number,
        ", but no nonnegative daily flow values were found between ",
        format(start_date, "%m/%d/%Y"),
        " and ",
        format(end_date, "%m/%d/%Y"),
        "."
      )
    )
  }

  attr(result, "source_url") <- query_url
  attr(result, "station_id") <- station_id
  attr(result, "sensor_number") <- sensor_number

  result
}

# ------------------------------------------------------------------
# Exploration ONLY: measured, tidally filtered hourly FLOW at CDEC DLC.
# Hourly FLOW is sensor 20 (CDEC sensor type, not plot-series ID 33744).
# Use observed daily means, retaining signed discharge and coverage counts.
# No CalSim3 regression is used in Exploration.
# ------------------------------------------------------------------
read_historical_dcc_hourly <- function(end_date = Sys.Date() - 1L,
                                       lookback_days = 35L) {
  end_date <- as.Date(end_date)
  start_date <- end_date - as.integer(lookback_days) + 1L
  base <- "https://cdec.water.ca.gov/dynamicapp/req/"
  args <- paste0("Stations=DLC&SensorNums=20&dur_code=H&Start=",
                 format(start_date, "%Y-%m-%d"), "&End=",
                 format(end_date + 1L, "%Y-%m-%d"))
  urls <- c(paste0(base, "CSVDataServlet?", args),
            paste0(base, "JSONDataServlet?", args))
  failures <- character()
  for (k in seq_along(urls)) {
    dat <- tryCatch({
      response <- download_observed_source_file(urls[k],
                       fileext=if (k == 1L) ".csv" else ".json",
                       attempts=2L)
      if (k == 1L) {
        x <- utils::read.csv(text=paste(response,collapse="\n"),
                             check.names=FALSE, stringsAsFactors=FALSE)
      } else {
        x <- jsonlite::fromJSON(paste(response, collapse="\n"),
                                flatten=TRUE)
        x <- as.data.frame(x, check.names=FALSE, stringsAsFactors=FALSE)
      }
      if (!nrow(x)) stop("CDEC returned zero hourly rows")
      nms <- gsub("[^a-z0-9]", "", tolower(names(x)))
      # CDEC current CSV returns DATE TIME and OBS DATE; use observation
      # timestamp when present. Normalize case before stripping punctuation.
      datetime_col <- match("datetime", nms)
      if (is.na(datetime_col)) datetime_col <- match("obsdate", nms)
      if (is.na(datetime_col)) datetime_col <- match("date", nms)
      value_col <- match("value", nms)
      if (is.na(value_col)) value_col <- match("obsvalue", nms)
      if (is.na(datetime_col) || is.na(value_col)) {
        stop("Unrecognized CDEC hourly columns: ", paste(names(x),collapse=", "))
      }
      # CDEC exports observation datetimes in Pacific Standard Time (PST).
      stamps <- as.character(x[[datetime_col]])
      stamps <- gsub("T", " ", stamps, fixed=TRUE)
      stamps <- sub("\\.[0-9]+$", "", stamps)
      stamps <- sub("(Z|[+-][0-9]{2}:?[0-9]{2})$", "", stamps)
      tm <- as.POSIXct(stamps, format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT+8")
      missing <- is.na(tm)
      if (any(missing)) tm[missing] <- as.POSIXct(stamps[missing],
                     format="%m/%d/%Y %H:%M:%S", tz="Etc/GMT+8")
      missing <- is.na(tm)
      if (any(missing)) tm[missing] <- as.POSIXct(stamps[missing],
                     format="%m/%d/%Y %H:%M", tz="Etc/GMT+8")
      missing <- is.na(tm)
      if (any(missing)) tm[missing] <- as.POSIXct(stamps[missing],
                     format="%Y-%m-%d %H:%M", tz="Etc/GMT+8")
      vals <- suppressWarnings(as.numeric(gsub(",", "",
                                   as.character(x[[value_col]]),fixed=TRUE)))
      day <- as.Date(tm, tz="Etc/GMT+8")
      keep <- !is.na(tm) & is.finite(vals) & vals > -9000 &
              day >= start_date & day <= end_date
      y <- data.frame(Date=day[keep], Timestamp=tm[keep],
                      Flow_cfs=vals[keep])
      y <- y[!duplicated(y$Timestamp,fromLast=TRUE),,drop=FALSE]
      y <- y[order(y$Timestamp),,drop=FALSE]
      if (!nrow(y)) stop("No usable FLOW values in hourly CDEC response")
      attr(y,"source_url") <- urls[k]
      y
    }, error=function(e) e)
    if (!inherits(dat,"error")) return(dat)
    failures <- c(failures, paste0(if(k==1L) "CSV" else "JSON", ": ",
                                    conditionMessage(dat)))
  }
  stop("Hourly FLOW retrieval failed for CDEC DLC sensor 20: ",
       paste(failures,collapse="; "))
}

# Prefer observed CDEC DLC hourly FLOW; if the hourly service fails, try the
# *observed* daily flow product at the SAME CDEC station/sensor (not a forecast
# regression).  Record which source supplied the value.
read_historical_dcc_daily_csv <- function(end_date, lookback_days) {
  end_date <- as.Date(end_date)
  start_date <- end_date-as.integer(lookback_days)+1L
  errors <- character()
  # 41: native mean-daily FLOW; 20 daily is accepted only if published.
  for (sensor in c(41L,20L)) {
    url <- paste0('https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?',
                  'Stations=DLC&SensorNums=',sensor,'&dur_code=D&Start=',start_date,
                  '&End=',end_date+1L)
    result <- tryCatch({
      lines <- download_observed_source_file(url,fileext='.csv',attempts=2L)
      x <- utils::read.csv(text=paste(lines,collapse='\n'),
                           check.names=FALSE,stringsAsFactors=FALSE)
      nms <- gsub('[^a-z0-9]','',tolower(names(x)))
      date_col <- match('obsdate',nms)
      if (is.na(date_col)) date_col <- match('datetime',nms)
      if (is.na(date_col)) date_col <- match('date',nms)
      value_col <- match('value',nms)
      if (is.na(date_col) || is.na(value_col))
        stop('Missing date/value columns: ',paste(names(x),collapse=', '))
      dates <- as.Date(substr(trimws(as.character(x[[date_col]])),1L,10L))
      if (all(is.na(dates))) dates <- as.Date(substr(trimws(as.character(x[[date_col]])),1L,10L),format='%m/%d/%Y')
      vals <- suppressWarnings(as.numeric(gsub(',','',as.character(x[[value_col]]),fixed=TRUE)))
      ok <- !is.na(dates) & is.finite(vals) & vals > -9000 &
              dates>=start_date & dates<=end_date
      if (!any(ok)) stop('No valid observed daily flow values')
      result <- data.frame(Date=dates[ok],Value=vals[ok],Hours=24L)
      result <- result[!duplicated(result$Date,fromLast=TRUE),,drop=FALSE]
      result <- result[order(result$Date),,drop=FALSE]
      attr(result,'source_url') <- url
      attr(result,'source_note') <- paste0('CDEC DLC sensor ',sensor,' observed daily mean flow')
      result
    },error=function(e)e)
    if (!inherits(result,'error')) return(result)
    errors <- c(errors,paste0('sensor ',sensor,': ',conditionMessage(result)))
  }
  stop('CDEC DLC observed daily FLOW unavailable: ',paste(errors,collapse='; '),call.=FALSE)
}

read_historical_dcc_daily <- function(end_date=Sys.Date()-1L,
                                      lookback_days=35L) {
  hourly <- tryCatch(read_historical_dcc_hourly(end_date,lookback_days),error=function(e)e)
  if (!inherits(hourly,'error')) {
    spl <- split(hourly$Flow_cfs,hourly$Date)
    out <- data.frame(Date=as.Date(names(spl)),
                      Value=vapply(spl,mean,numeric(1)),
                      Hours=vapply(spl,length,integer(1)))
    out <- out[order(out$Date),,drop=FALSE]
    attr(out,'source_url') <- attr(hourly,'source_url')
    attr(out,'source_note') <- 'CDEC DLC sensor 20 observed hourly flow, daily means'
    expected <- seq(as.Date(end_date)-as.integer(lookback_days)+1L,as.Date(end_date),by='day')
    if (all(expected %in% out$Date)) return(out)
    daily <- tryCatch(read_historical_dcc_daily_csv(end_date,lookback_days),error=function(e)e)
    if (!inherits(daily,'error')) {
      extra <- daily[!daily$Date %in% out$Date,,drop=FALSE]
      if (nrow(extra)) out <- rbind(out,extra)
      out <- out[order(out$Date),,drop=FALSE]
      attr(out,'source_note') <- 'CDEC DLC sensor 20 hourly flow with observed daily mean gap-fill'
    }
    return(out)
  }
  daily <- tryCatch(read_historical_dcc_daily_csv(end_date,lookback_days),error=function(e)e)
  if (!inherits(daily,'error')) return(daily)
  stop('CDEC DLC measured flow unavailable. Hourly: ',conditionMessage(hourly),
       '; daily observed fallback: ',conditionMessage(daily),call.=FALSE)
}

read_historical_dcc_7day <- function(end_date=Sys.Date()-1L) {
  end_date <- as.Date(end_date)
  dat <- read_historical_dcc_daily(end_date, lookback_days=35L)
  # Prefer the most recent 7-day calendar window with valid observations.
  # Gaps do not erase available records; incomplete days are flagged.
  recent <- dat[dat$Date >= end_date-6L & dat$Date <= end_date,,drop=FALSE]
  if (!nrow(recent)) {
    # CDEC can lag. Find the latest 7-day window containing observations.
    last <- max(dat$Date)
    recent <- dat[dat$Date >= last-6L & dat$Date <= last,,drop=FALSE]
  }
  if (!nrow(recent)) stop("No observed DLC hourly FLOW in the past 35 days")
  target <- seq(max(recent$Date)-6L,max(recent$Date),by="day")
  # The observed-day mean is based on available hourly measurements.
  # A day with fewer than 24 hours remains marked incomplete.
  list(value=mean(recent$Value), dates=recent$Date,
       days_available=nrow(recent), hours=sum(recent$Hours),
       incomplete_dates=recent$Date[recent$Hours < 24L],
       missing_dates=setdiff(target,recent$Date),
       source_url=attr(dat,"source_url"), plot_url=DCC_CDEC_PLOT_URL,
       source="CDEC DLC sensor 20 hourly FLOW; mean of observed daily means")
}

read_cdec_query_daily_mflow <- function(
  station_id,
  end_date = Sys.Date(),
  lookback_days = 45
) {
  station_id <- toupper(trimws(station_id))
  end_date <- as.Date(end_date)
  start_date <- end_date - lookback_days

  query_url <- paste0(
    "https://cdec.water.ca.gov/dynamicapp/QueryDaily?s=",
    utils::URLencode(station_id, reserved = TRUE),
    "&end=",
    format(end_date, "%Y-%m-%d")
  )

  rows <- extract_cdec_query_daily_rows(
    download_observed_source_file(
      query_url,
      fileext = ".html"
    )
  )

  header_index <- which(
    vapply(
      rows,
      function(row) any(grepl("M\\s*FLOW", row, ignore.case = TRUE, perl = TRUE)),
      logical(1)
    )
  )[1]

  if (is.na(header_index)) {
    stop(paste0("Could not find the M FLOW column on the CDEC ", station_id, " daily page."))
  }

  header_row <- rows[[header_index]]
  flow_column <- which(grepl("M\\s*FLOW", header_row, ignore.case = TRUE, perl = TRUE))[1]

  parsed_rows <- lapply(
    rows[(header_index + 1):length(rows)],
    function(row) {
      if (length(row) < flow_column || length(row) < 1) {
        return(NULL)
      }

      date_value <- parse_observed_dates(row[1])
      flow_value <- parse_cdec_numeric_value(row[flow_column])

      if (is.na(date_value) || !is.finite(flow_value) || flow_value < 0) {
        return(NULL)
      }

      data.frame(
        Date = date_value,
        Value = flow_value,
        stringsAsFactors = FALSE
      )
    }
  )

  parsed_rows <- parsed_rows[!vapply(parsed_rows, is.null, logical(1))]

  if (length(parsed_rows) == 0) {
    stop(paste0("No usable M FLOW values were found on the CDEC ", station_id, " daily page."))
  }

  result <- dplyr::bind_rows(parsed_rows)
  result <- result[
    result$Date >= start_date &
      result$Date <= end_date,
    ,
    drop = FALSE
  ]
  result <- result[!duplicated(result$Date, fromLast = TRUE), , drop = FALSE]
  result <- result[order(result$Date), , drop = FALSE]

  if (nrow(result) == 0) {
    stop(paste0("No usable CDEC ", station_id, " M FLOW values were found in the requested date range."))
  }

  attr(result, "source_url") <- query_url
  attr(result, "station_id") <- station_id
  attr(result, "sensor_number") <- 41
  result
}

read_fremont_weir_daily_values <- function(
  end_date = Sys.Date(),
  lookback_days = 45
) {
  # Read Fremont Weir directly from the CDEC QueryDaily page and
  # explicitly extract the M FLOW (CFS) column. This avoids relying
  # on a hard-coded sensor number for FRE.
  read_cdec_query_daily_mflow(
    station_id = "FRE",
    end_date = end_date,
    lookback_days = lookback_days
  )
}

read_moke_daily_values <- function(
  file_path = file.path("data", "MOKE_daily.csv")
) {
  if (!file.exists(file_path)) {
    stop(
      paste0(
        "MOKE daily file was not found: ",
        file_path,
        "."
      )
    )
  }

  raw_data <- utils::read.csv(
    file_path,
    skip = 4,
    header = FALSE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (ncol(raw_data) < 2) {
    stop(
      "MOKE daily file does not contain the expected date and flow columns."
    )
  }

  result <- data.frame(
    Date = parse_observed_dates(
      as.character(raw_data[[1]])
    ),
    Value = suppressWarnings(
      as.numeric(
        gsub(
          ",",
          "",
          as.character(raw_data[[2]]),
          fixed = TRUE
        )
      )
    ),
    stringsAsFactors = FALSE
  )

  result <- result[
    !is.na(result$Date) &
      is.finite(result$Value),
    ,
    drop = FALSE
  ]

  result <- result[
    !duplicated(result$Date, fromLast = TRUE),
    ,
    drop = FALSE
  ]

  result <- result[
    order(result$Date),
    ,
    drop = FALSE
  ]

  if (nrow(result) == 0) {
    stop("No usable daily MOKE values were found in MOKE_daily.csv.")
  }

  result
}


read_usgs_daily_values <- function(
  site_number,
  parameter_code,
  end_date = Sys.Date(),
  lookback_days = 45,
  use_latest_available = FALSE
) {
  start_date <- if (isTRUE(use_latest_available)) {
    as.Date("1900-01-01")
  } else {
    as.Date(end_date) - lookback_days
  }

  query_url <- paste0(
    "https://waterservices.usgs.gov/nwis/dv/",
    "?format=rdb",
    "&sites=", site_number,
    "&startDT=", format(start_date, "%Y-%m-%d"),
    "&endDT=", format(as.Date(end_date), "%Y-%m-%d"),
    "&parameterCd=", parameter_code,
    "&statCd=00003",
    "&siteStatus=all"
  )

  text_lines <- download_observed_source_file(query_url)

  data_lines <- text_lines[
    !grepl("^#", text_lines) &
      nzchar(trimws(text_lines))
  ]

  if (length(data_lines) < 3) {
    stop(
      paste0(
        "USGS site ",
        site_number,
        " returned no usable daily records."
      )
    )
  }

  usgs_data <- utils::read.delim(
    text = paste(data_lines, collapse = "\n"),
    header = TRUE,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "Ice", "Eqp", "Ssn", "Rat", "Dis")
  )

  if (
    nrow(usgs_data) > 0 &&
    "agency_cd" %in% names(usgs_data) &&
    grepl(
      "^[0-9]+[a-zA-Z]$",
      as.character(usgs_data$agency_cd[1])
    )
  ) {
    usgs_data <- usgs_data[-1, , drop = FALSE]
  }

  date_index <- which(
    toupper(names(usgs_data)) %in% c("DATETIME", "DATE")
  )[1]

  parameter_columns <- which(
    grepl(
      paste0("_", parameter_code, "_"),
      names(usgs_data),
      fixed = TRUE
    ) &
      !grepl("_CD$", names(usgs_data))
  )

  if (is.na(date_index) || length(parameter_columns) == 0) {
    stop(
      paste0(
        "USGS response for site ",
        site_number,
        " did not contain the requested parameter ",
        parameter_code,
        "."
      )
    )
  }

  # Prefer the daily mean statistic column when several columns are returned.
  mean_columns <- parameter_columns[
    grepl("_00003$", names(usgs_data)[parameter_columns])
  ]

  value_index <- if (length(mean_columns) > 0) {
    mean_columns[1]
  } else {
    parameter_columns[1]
  }

  result <- data.frame(
    Date = parse_observed_dates(
      usgs_data[[date_index]]
    ),
    Value = suppressWarnings(
      as.numeric(
        gsub(",", "", usgs_data[[value_index]])
      )
    )
  )

  result <- result[
    !is.na(result$Date) &
      is.finite(result$Value),
    ,
    drop = FALSE
  ]

  result <- result[
    order(result$Date),
    ,
    drop = FALSE
  ]

  if (nrow(result) == 0) {
    stop(
      paste0(
        "No usable daily USGS values were returned for site ",
        site_number,
        "."
      )
    )
  }

  result
}

trailing_seven_summary <- function(data) {
  data <- data[
    !is.na(data$Date) &
      is.finite(data$Value) &
      data$Value > -9000,
    ,
    drop = FALSE
  ]

  data <- data[
    order(data$Date),
    ,
    drop = FALSE
  ]

  if (nrow(data) < 7) {
    stop("Fewer than seven usable daily values were available.")
  }

  selected <- utils::tail(data, 7)

  list(
    value = mean(selected$Value, na.rm = TRUE),
    start_date = min(selected$Date),
    end_date = max(selected$Date),
    dates = selected$Date,
    values = selected$Value
  )
}


trailing_thirty_summary <- function(data) {
  data <- data[
    !is.na(data$Date) &
      is.finite(data$Value) &
      data$Value > -9000,
    ,
    drop = FALSE
  ]

  data <- data[
    order(data$Date),
    ,
    drop = FALSE
  ]

  if (nrow(data) < 30) {
    stop("Fewer than thirty usable daily values were available.")
  }

  selected <- utils::tail(data, 30)

  list(
    value = mean(selected$Value, na.rm = TRUE),
    start_date = min(selected$Date),
    end_date = max(selected$Date),
    dates = selected$Date,
    values = selected$Value
  )
}

safe_observed_summary <- function(
  expression,
  attempts = 2,
  pause_seconds = 1
) {
  expression_code <- substitute(expression)
  evaluation_environment <- parent.frame()
  last_error <- NULL

  for (attempt in seq_len(attempts)) {
    result <- tryCatch(
      {
        value <- eval(expression_code, envir = evaluation_environment)
        list(ok = TRUE, data = value, message = NULL)
      },
      error = function(error_condition) {
        last_error <<- conditionMessage(error_condition)
        NULL
      }
    )

    if (!is.null(result)) {
      return(result)
    }

    if (attempt < attempts) {
      Sys.sleep(pause_seconds * attempt)
    }
  }

  list(
    ok = FALSE,
    data = NULL,
    message = paste0(
      "The live source was temporarily unavailable after ",
      attempts,
      " attempts. The existing input value is being used."
    ),
    technical_message = last_error
  )
}

format_observed_value <- function(value) {
  if (!is.finite(value)) {
    return("Unavailable")
  }

  format(
    round(value, 0),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

parse_flow_input <- function(value) {
  suppressWarnings(
    as.numeric(
      gsub(",", "", as.character(value), fixed = TRUE)
    )
  )
}


# =======================================================================
# EXPLORATION ONLY - DAYFLOW-style five-day precipitation contribution,
# aggregated over a rolling 30-day window (a PROPOSED TAF/30-day model input).
# Daily effective rainfall on d = (P[d] + P[d-1] + ... + P[d-4]) / 5.
# Each rainfall event therefore contributes equally on its observation day
# and the following four dates. 30-day volume (TAF) = sum(effective daily
# inches on 30 consecutive dates) * 679000 / 12000.
# The four days before the 30-day window are required for correct carry-in.
# Do not interpret missing daily precipitation as zero or silently annualize.
# IMPORTANT: This rolling window has not been verified as the training-time
# QPREC definition used by Ching-Fu; it is explicitly a rolling estimate.
# =======================================================================
ccwd_qprec_five_day_rolling30 <- function(daily, ending_on,
                                          delta_area_acres = 679000) {
  if (!is.data.frame(daily) ||
      !all(c("date", "precip_in") %in% names(daily))) {
    stop("Daily precipitation input needs date and precip_in columns")
  }
  if (anyDuplicated(daily$date)) stop("Duplicate dates in precipitation series")
  if (any(is.finite(daily$precip_in) & daily$precip_in < 0)) {
    stop("Negative daily precipitation values require source review")
  }
  ending_on <- as.Date(ending_on)
  start_on <- ending_on - 29L
  required_start <- start_on - 4L
  required_dates <- seq(required_start, ending_on, by = "day")
  date_to_value <- setNames(daily$precip_in, as.character(daily$date))
  observed <- unname(date_to_value[as.character(required_dates)])
  missing_dates <- required_dates[!is.finite(observed)]
  complete <- length(observed) == 34L && all(is.finite(observed))
  daily_dates <- seq(start_on, ending_on, by = "day")
  effective_daily <- if (complete) {
    vapply(daily_dates, function(d) {
      dates5 <- seq(d - 4L, d, by = "day")
      sum(unname(date_to_value[as.character(dates5)])) / 5
    }, numeric(1))
  } else rep(NA_real_, 30L)
  raw_window <- unname(date_to_value[as.character(daily_dates)])
  total_inches <- if (complete) sum(effective_daily) else NA_real_
  list(
    start_date = start_on, end_date = ending_on,
    required_start_date = required_start,
    expected_input_days = 34L,
    observed_input_days = sum(is.finite(observed)),
    missing_dates = missing_dates,
    complete = complete,
    raw_precip_inches_30d = if (all(is.finite(raw_window)))
      sum(raw_window) else NA_real_,
    distributed_precip_inches_30d = total_inches,
    accretion_taf_30d = total_inches * delta_area_acres / 12000
  )
}

ccwd_fetch_noaa_daily_precip <- function(station, first_day, last_day) {
  url <- paste0(
    "https://www.ncei.noaa.gov/access/services/data/v1",
    "?dataset=daily-summaries&stations=", station,
    "&dataTypes=PRCP&startDate=", format(first_day, "%Y-%m-%d"),
    "&endDate=", format(last_day, "%Y-%m-%d"),
    "&units=standard&format=csv")
  response <- download_observed_source_file(url, fileext = ".csv")
  if (!length(response) || !any(grepl("DATE", response, fixed = TRUE))) {
    stop("NOAA returned no daily precipitation table for ", station)
  }
  p <- utils::read.csv(text = paste(response, collapse = "\n"),
                       stringsAsFactors = FALSE, check.names = FALSE,
                       na.strings = c("", "NA", "M"))
  if (!all(c("DATE", "PRCP") %in% names(p))) {
    stop("NOAA response missing DATE or PRCP for ", station)
  }
  if ("STATION" %in% names(p) &&
      any(!is.na(p$STATION) & p$STATION != station)) {
    stop("NOAA returned a different precipitation station")
  }
  p$date <- as.Date(as.character(p$DATE))
  # NOAA marks trace precipitation with T; treat this as zero measurable
  # inches, while keeping genuinely missing records as NA.
  raw <- trimws(as.character(p$PRCP))
  p$precip_in <- suppressWarnings(as.numeric(raw))
  p$precip_in[!is.na(raw) & toupper(raw) == "T"] <- 0
  p <- p[!is.na(p$date) & p$date >= first_day &
           p$date <= last_day, c("date", "precip_in"), drop = FALSE]
  if (!nrow(p)) stop("No NOAA precipitation observations for ", station)
  if (anyDuplicated(p$date)) stop("Duplicate daily records for ", station)
  list(daily = p, source_url = url)
}

fetch_ccwd_noaa_rolling30_accretion <- function(reference_date = Sys.Date(),
                                                months_back = 18L,
                                                delta_area_acres = 679000) {
  reference_date <- as.Date(reference_date)
  last_day <- reference_date - 1L  # exclude potentially incomplete current day
  first_day <- last_day - as.integer(ceiling(months_back * 31))
  stations <- c("US1CASJ0004", "USW00023237")
  names(stations) <- c("STOCKTON 5.8 NNE", "STOCKTON AIRPORT")
  candidates <- list()
  failures <- character(0)
  for (station_name in names(stations)) {
    station <- unname(stations[[station_name]])
    item <- tryCatch({
      raw <- ccwd_fetch_noaa_daily_precip(station, first_day, last_day)
      valid <- raw$daily$date[is.finite(raw$daily$precip_in)]
      if (!length(valid)) stop("no valid daily precipitation measurements")
      newest <- max(valid)
      # Prefer most recent COMPLETE rolling window, not necessarily latest
      # observation if its preceding 33 days have gaps. Never fill gaps as zero.
      ends <- seq(newest, max(min(valid) + 33L, first_day + 33L), by = "-1 day")
      chosen <- NULL
      most_recent <- NULL
      for (ending_on in ends) {
        candidate <- ccwd_qprec_five_day_rolling30(raw$daily,
                            ending_on, delta_area_acres)
        if (is.null(most_recent)) most_recent <- candidate
        if (isTRUE(candidate$complete)) {
          chosen <- candidate
          break
        }
      }
      if (is.null(chosen)) {
        missing_sample <- if (length(most_recent$missing_dates))
          paste(head(format(most_recent$missing_dates, "%Y-%m-%d"), 8L),
                collapse = ", ") else "not determined"
        stop("no complete 30-day window; latest observation ",
             format(newest, "%Y-%m-%d"), "; ",
             most_recent$observed_input_days, "/34 required observations",
             "; missing dates include: ", missing_sample)
      }
      list(latest = chosen, station = station,
           station_name = station_name, source_url = raw$source_url,
           latest_observation_date = newest,
           requested_through_date = last_day)
    }, error = function(e) e)
    if (inherits(item, "error")) {
      failures <- c(failures, paste0(station_name, ": ", conditionMessage(item)))
    } else {
      candidates[[station_name]] <- item
    }
  }
  if (!length(candidates)) stop(
    "No complete rolling 30-day precipitation data available. ",
    paste(failures, collapse = "; "),
    ". Missing observations were not assumed zero.")
  # Choose the station with the most recent COMPLETE 30-day period;
  # first station wins a tie (Stockton 5.8 NNE).
  dates <- vapply(candidates, function(x) as.numeric(x$latest$end_date), numeric(1))
  winner <- candidates[[order(dates, decreasing = TRUE)[1L]]]
  winner$delta_area_acres <- delta_area_acres
  winner$method <- "DAYFLOW five-day distribution; rolling 30-day TAF"
  winner$source_is_proxy <- TRUE
  winner$failures <- failures
  winner
}

source("ccwd_qprec_sources.R", local=TRUE)

server <- function(input, output, session) {
  ccwd_display_whole <- function(value) {
    if (is.null(value) || !length(value) || !is.finite(value[1])) return("")
    format(round(as.numeric(value[1]), 0), big.mark=",", scientific=FALSE, trim=TRUE)
  }
  ccwd_info_line <- function(key, value) {
    tagList(tags$div(class="ccwd-help-key", key), tags$div(class="ccwd-help-value", value))
  }

  # Exploration measured DCC cache; separate from Weekly Assessment.
  ccwd_dcc_measured <- reactiveVal(NULL)
  ccwd_dcc_note <- reactiveVal("Retrieving historical DCC flow from CDEC...")
  output$current_ptm_dcc_auto_note <- renderUI({
    dat <- ccwd_dcc_measured()
    if (is.null(dat)) return(tags$div(class="ccwd-help-status", ccwd_dcc_note()))
    tags$div(class="ccwd-help-section",
      tags$strong("Latest historical data used"),
      tags$div(class="ccwd-help-detail",
        ccwd_info_line("Measured mean", paste0(ccwd_display_whole(dat$value), " cfs")),
        ccwd_info_line("Observed days", paste0(dat$days_available, " of 7")),
        ccwd_info_line("Hourly readings", as.character(dat$hours)),
        ccwd_info_line("Partial days", as.character(length(dat$incomplete_dates))),
        ccwd_info_line("Missing days", as.character(length(dat$missing_dates)))
      ),
      tags$div(class="ccwd-help-source", "CDEC DLC hourly FLOW: ",
        tags$a(href=dat$source_url, target="_blank", rel="noopener noreferrer", "Open measurement source")),
      tags$div(class="ccwd-help-foot", "Closed gate: 0 cfs. Open gate: observed CDEC DCC flow where available; otherwise estimate from measured SAC at Freeport (0.1846 × SAC + 306.52 cfs).")
    )
  })
  # Defer external requests until the browser has rendered its controls.
  # This prevents slow HTTP calls from holding the initial comparison form hostage.
  observeEvent(input$ccwd_load_dcc, {
    req(isTRUE(input$ccwd_load_dcc))
    result <- tryCatch(read_historical_dcc_7day(), error=function(e) e)
    if (inherits(result,"error")) {
      ccwd_dcc_measured(NULL)
      ccwd_dcc_note(paste0("Historical DCC data unavailable: ",
        conditionMessage(result),
        ". When Open, the app estimates missing DCC flow from measured SAC at Freeport: 0.1846 × SAC + 306.52 cfs."))
    } else {
      ccwd_dcc_measured(result)
      ccwd_dcc_note(paste0(
        "CDEC DLC hourly FLOW → daily means → historical mean: ",
        round(result$value, 2), " cfs (", result$days_available,
        " observed days, ", result$hours, " hourly values; ",
        length(result$incomplete_dates), " partial days, ",
        length(result$missing_dates), " missing days). ",
        "Gate status is user-selected, not verified historical operations. ",
        "Source: ", result$source_url))
    }
  }, once=TRUE)
  observeEvent(list(ccwd_dcc_measured(), input$current_ptm_dcc_status, input$current_ptm_sac), {
    gate <- suppressWarnings(as.numeric(input$current_ptm_dcc_status))
    measured <- ccwd_dcc_measured()
    if (identical(gate, 0)) {
      updateTextInput(session,"current_ptm_dcc_flow",value="0")
    } else if (identical(gate, 1) && !is.null(measured)) {
      updateTextInput(session,"current_ptm_dcc_flow",
                      value=ccwd_display_whole(measured$value))
    } else if (identical(gate, 1)) {
      sac <- parse_flow_input(input$current_ptm_sac)
      if (length(sac)==1L && is.finite(sac))
        updateTextInput(session,"current_ptm_dcc_flow",
                        value=ccwd_display_whole(0.1846*sac+306.52))
    }
  }, ignoreInit=FALSE)
  # Exploration uses latest complete SCKC1 observations, not NOAA proxy.
  ccwd_qprec_note <- reactiveVal("Retrieving SCKC1 daily precipitation summaries...")
  ccwd_qprec_detail <- reactiveVal(NULL)
  output$current_ptm_qprec_auto_note <- renderUI({
    d <- ccwd_qprec_detail()
    if (is.null(d)) return(tags$div(class="ccwd-help-status",ccwd_qprec_note()))
    tags$div(class="ccwd-help-section",
      tags$strong("SCKC1 DAYFLOW-style QPREC estimate"),
      ccwd_info_line("Station","Stockton Fire Station No. 4 (SCKC1)"),
      ccwd_info_line("Last complete date",as.character(d$through)),
      ccwd_info_line("Runoff area","682,230 acres"),
      ccwd_info_line("Conversion","Five-day runoff × calendar days in month"),
      ccwd_info_line("7-day model input",paste0(ccwd_display_whole(d$value)," TAF/month")),
      if (length(d$estimated_dates)) tags$div(class="ccwd-help-foot",
        paste0("Estimated SCKC1 rainfall for missing day(s): ",
          paste(as.character(d$estimated_dates),collapse=", "),
          ". Linear interpolation between adjacent actual observations; not a measured zero.")),
      tags$a(href=d$source_url,target="_blank","SCKC1 daily precipitation summary"),
      tags$div(class="ccwd-help-foot","Monthly-equivalent estimate, not the DAYFLOW observed calendar-month total."))
  })
  # Start Exploration QPREC from the server after the initial UI flush.
  # A previous release relied solely on a one-time browser shiny:connected
  # event, which can be missed and leave the input permanently blank.
  ccwd_qprec_started <- FALSE
  ccwd_initialize_exploration_qprec <- function() {
    if (isTRUE(ccwd_qprec_started)) return(invisible(NULL))
    ccwd_qprec_started <<- TRUE
    ccwd_qprec_note("Checking SCKC1 daily precipitation observations...")
    result <- tryCatch({
      # Use the latest *complete* local-date window, not blindly yesterday;
      # station reporting can lag a few days. Eleven dates are necessary
      # for a seven-day average with four preceding rainfall days.
      last <- Sys.Date() - 1L
      rain <- ccwd_sckc1_daily(last - 59L, last)
      complete <- rain$date[is.finite(rain$precip_in)]
      if (!length(complete)) stop("SCKC1 returned no valid precipitation days")
      endings <- rev(complete)
      chosen <- NULL
      # R's `for (day in Date_vector)` can strip the Date class. Indexing
      # with `[` preserves dates and prevents invalid seven-day windows.
      for (i in seq_along(endings)) {
        end_day <- endings[i]
        dates <- seq(end_day - 6L, end_day, by="day")
        candidate <- tryCatch(ccwd_qprec_from_daily(rain, dates),error=function(e) NULL)
        if (!is.null(candidate)) {
          age_days <- as.integer(last - end_day)
          if (age_days > 14L) {
            stop(paste0("Most recent complete seven-day SCKC1 window ends ",
              as.character(end_day), " (", age_days,
              " days before the requested date). Refusing to label stale observations as current."))
          }
          chosen <- list(value=mean(candidate$qprec_taf_month),
                         through=end_day,source_url=attr(rain,"source_url"),
                         estimated_dates=as.Date(rain$date[which(rain$estimated %in% TRUE &
                           rain$date >= min(dates)-4L & rain$date <= max(dates))]),
                         age_days=age_days)
          break
        }
      }
      if (is.null(chosen)) stop(paste0(
        "No complete SCKC1 seven-day precipitation window with the four-day carry-in ",
        "was found in the past 60 days. Missing precipitation is never set to zero."))
      chosen
    }, error=function(e)e)
    if (inherits(result,"error")) {
      ccwd_qprec_detail(NULL)
      ccwd_qprec_note(paste0("QPREC source error: ",conditionMessage(result),
        " Check the SCKC1 daily source or the RStudio Console."))
      # Never leave a blank text box: it is indistinguishable from a UI bug.
      updateTextInput(session,"current_ptm_qprec",value="Unavailable")
      message("[CCWD Exploration QPREC] ",conditionMessage(result))
    } else {
      ccwd_qprec_detail(result)
      ccwd_qprec_note("SCKC1 QPREC calculated")
      updateTextInput(session,"current_ptm_qprec",value=ccwd_display_whole(result$value))
      message("[CCWD Exploration QPREC] ",as.character(result$through),
        ": ",result$value," TAF/month (seven-day mean, SCKC1)")
    }
    invisible(NULL)
  }
  # Browser event is still supported, but is no longer the sole trigger.
  observeEvent(input$ccwd_load_noaa, {
    if (isTRUE(input$ccwd_load_noaa)) ccwd_initialize_exploration_qprec()
  })
  session$onFlushed(function() {
    ccwd_initialize_exploration_qprec()
  }, once=TRUE)



  app_name_value <- if (exists("APP_NAME", inherits = TRUE)) APP_NAME else "Entrainment Dashboard"
  app_version_value <- if (exists("APP_VERSION", inherits = TRUE)) APP_VERSION else "1.1.0"

  add_export_metadata <- function(data) {
    data %>%
      dplyr::mutate(
        Application_Name = app_name_value,
        Application_Version = app_version_value,
        Application_Version_Label = paste(app_name_value, "version", app_version_value),
        Export_Date = format(Sys.Date(), "%Y-%m-%d"),
        Export_Timestamp_UTC = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      ) %>%
      dplyr::relocate(
        Application_Name, Application_Version, Application_Version_Label,
        Export_Date, Export_Timestamp_UTC
      )
  }

  prepare_ptm_export <- function(data) {
    data <- data %>%
      dplyr::mutate(
        Entrainment_Model_Period = dplyr::case_when(
          grepl("7-Day", Model, fixed = TRUE) ~ "7-Day",
          grepl("30-Day", Model, fixed = TRUE) ~ "30-Day",
          TRUE ~ NA_character_
        ),
        Entrainment_7_Day_Percent = dplyr::if_else(
          Entrainment_Model_Period == "7-Day", as.numeric(Prediction_Final), NA_real_
        ),
        Entrainment_30_Day_Percent = dplyr::if_else(
          Entrainment_Model_Period == "30-Day", as.numeric(Prediction_Final), NA_real_
        )
      )
    add_export_metadata(data)
  }

  map_export_caption <- function(include_interpolation = TRUE) {
    lines <- c(paste(app_name_value, "version", app_version_value))
    if (isTRUE(include_interpolation)) {
      lines <- c(
        lines,
        paste(
          "Interpolated risk shading is a generalized spatial representation of entrainment",
          "emulator outputs between supported DSM2 nodes. Boundaries and transitions are",
          "approximate and should not be interpreted as precise risk-zone limits."
        )
      )
    }
    c(lines, paste0("Exported ", format(Sys.time(), "%B %d, %Y at %I:%M %p"), ".")) %>%
      paste(collapse = "\n")
  }


  observed_flow_state <- reactiveVal(
    list(
      status = "loading",
      message = "Retrieving observed trailing 7-day average flows.",
      updated_at = NULL
    )
  )

  observed_note <- function(
    label,
    source_items = NULL,
    special_note = NULL
  ) {
    state <- observed_flow_state()

    if (!identical(state$status, "ready")) {
      return(
        tags$div(
          class = "observed-flow-note",
          if (identical(state$status, "loading")) {
            "Retrieving the latest observed 7-day flow data."
          } else {
            "Live source refresh is temporarily unavailable. The existing box value is being used."
          }
        )
      )
    }

    item <- state[[label]]

    if (is.null(item) || !isTRUE(item$ok)) {
      return(
        tags$div(
          class = "observed-flow-note",
          "Live source refresh is temporarily unavailable. The existing box value is being used."
        )
      )
    }

    latest_date <- if (!is.null(item$latest_date)) {
      item$latest_date
    } else if (!is.null(item$dates) && length(item$dates) > 0) {
      max(item$dates, na.rm = TRUE)
    } else {
      item$end_date
    }

    latest_value <- if (!is.null(item$latest_value)) {
      item$latest_value
    } else if (!is.null(item$values) && length(item$values) > 0) {
      tail(item$values, 1)
    } else {
      item$value
    }

    tags$div(
      class = "observed-flow-note",
      paste0(
        "Latest available data date ",
        format(latest_date, "%m/%d/%Y"),
        ": flow = ",
        format_observed_value(latest_value),
        " cfs"
      ),
      tags$br(),
      paste0(
        "7-day average flow = ",
        format_observed_value(item$value),
        " cfs"
      )
    )
  }

  output$current_ptm_exp_note <- renderUI({
    observed_note(
      "EXP",
      list(
        list(
          text = "CLC INFLOW CFS (sensor 76)",
          url = observed_source_links$CLC
        ),
        list(
          text = "TRP DC PUMP CFS (sensor 70)",
          url = observed_source_links$TRP
        )
      )
    )
  })

  output$current_ptm_ver_note <- renderUI({
    observed_note(
      "VER",
      list(
        list(
          text = "VNS M FLOW CFS (sensor 41)",
          url = observed_source_links$VNS
        )
      )
    )
  })

  output$current_ptm_sac_note <- renderUI({
    observed_note(
      "SAC",
      list(
        list(
          text = "FPT FLOW CFS (sensor 20)",
          url = observed_source_links$FPT
        )
      )
    )
  })

  output$current_ptm_east_note <- renderUI({
    state <- observed_flow_state()
    special_note <- NULL

    if (
      identical(state$status, "ready") &&
      !is.null(state$EAST) &&
      isTRUE(state$EAST$ok)
    ) {
      special_note <- paste0(
        "EAST = MOK + CAL + COS. MOK uses the trailing 7-day average of ",
        "EBMUD Mokelumne River below WID daily flow; it is combined with the ",
        "current CAL and COS trailing averages."
      )
    }

    observed_note(
      "EAST",
      list(
        list(
          text = "MOK: EBMUD Mokelumne River below WID",
          url = observed_source_links$MOK
        ),
        list(
          text = "CAL: NHG OUTFLOW CFS (sensor 23)",
          url = observed_source_links$NHG
        ),
        list(
          text = "COS (USGS 11335000 discharge)",
          url = observed_source_links$COS
        )
      ),
      special_note = special_note
    )
  })

  output$current_ptm_xgeo_note <- renderUI({
    observed_note(
      "XGEO",
      list(
        list(
          text = "XGEO_A (USGS 11447890)",
          url = observed_source_links$XGEO_A
        ),
        list(
          text = "XGEO_C (USGS 11447905)",
          url = observed_source_links$XGEO_C
        )
      ),
      special_note = "XGEO is calculated as XGEO_A minus XGEO_C."
    )
  })

  output$current_eco_sac_note <- renderUI({
    observed_note("SAC")
  })

  output$current_eco_yol_note <- renderUI({
    observed_note("YOL")
  })

  output$current_eco_moke_note <- renderUI({
    observed_note("MOK")
  })

  observeEvent(
    list(input$current_input_method, input$ccwd_load_observed),
    {
      req(isTRUE(input$ccwd_load_observed))
      if (!identical(input$current_input_method, "single")) {
        return(invisible(NULL))
      }

      observed_flow_state(
        list(
          status = "loading",
          message = "Retrieving observed trailing 7-day average flows.",
          updated_at = Sys.time()
        )
      )

      clc <- safe_observed_summary(
        trailing_seven_summary(
          read_cdec_daily_values("CLC", 76)
        )
      )

      trp <- safe_observed_summary(
        trailing_seven_summary(
          read_cdec_daily_values("TRP", 70)
        )
      )

      vns <- safe_observed_summary(
        trailing_seven_summary(
          read_cdec_daily_values("VNS", 41)
        )
      )

      fpt <- safe_observed_summary(
        trailing_seven_summary(
          read_cdec_daily_values("FPT", 20)
        )
      )

      fre <- safe_observed_summary(
        trailing_seven_summary(
          read_fremont_weir_daily_values()
        )
      )

      nhg <- safe_observed_summary(
        trailing_seven_summary(
          read_cdec_daily_values("NHG", 23)
        )
      )

      moke_daily <- tryCatch(
        read_moke_daily_values(),
        error = function(error_condition) {
          error_condition
        }
      )

      mok <- if (inherits(moke_daily, "error")) {
        list(
          ok = FALSE,
          data = NULL,
          message = conditionMessage(moke_daily)
        )
      } else {
        safe_observed_summary(
          trailing_seven_summary(
            moke_daily
          )
        )
      }

      cos <- safe_observed_summary(
        trailing_seven_summary(
          read_usgs_daily_values(
            site_number = "11335000",
            parameter_code = "00060"
          )
        )
      )

      xgeo_a <- safe_observed_summary(
        trailing_seven_summary(
          read_usgs_daily_values(
            site_number = "11447890",
            parameter_code = "72137"
          )
        )
      )

      xgeo_c <- safe_observed_summary(
        trailing_seven_summary(
          read_usgs_daily_values(
            site_number = "11447905",
            parameter_code = "72137"
          )
        )
      )

      exp_result <- if (isTRUE(clc$ok) && isTRUE(trp$ok)) {
        list(
          ok = TRUE,
          value = clc$data$value + trp$data$value,
          start_date = max(clc$data$start_date, trp$data$start_date),
          end_date = min(clc$data$end_date, trp$data$end_date),
          latest_date = min(clc$data$end_date, trp$data$end_date),
          latest_value = tail(clc$data$values, 1) + tail(trp$data$values, 1),
          message = NULL
        )
      } else {
        list(
          ok = FALSE,
          message = paste(
            c(
              if (!isTRUE(clc$ok)) paste0("CLC: ", clc$message) else NULL,
              if (!isTRUE(trp$ok)) paste0("TRP: ", trp$message) else NULL
            ),
            collapse = " "
          )
        )
      }

      ver_result <- if (isTRUE(vns$ok)) {
        c(list(ok = TRUE), vns$data)
      } else {
        list(ok = FALSE, message = vns$message)
      }

      sac_result <- if (isTRUE(fpt$ok)) {
        c(list(ok = TRUE), fpt$data)
      } else {
        list(ok = FALSE, message = fpt$message)
      }

      yol_result <- if (isTRUE(fre$ok)) {
        list(
          ok = TRUE,
          value = 0 + fre$data$value,
          start_date = fre$data$start_date,
          end_date = fre$data$end_date,
          latest_date = max(fre$data$dates, na.rm = TRUE),
          latest_value = 0 + tail(fre$data$values, 1),
          dates = fre$data$dates,
          values = 0 + fre$data$values,
          message = NULL
        )
      } else {
        # TEST SCENARIO ONLY: missing FRE sensor 41 -> YOL = 0 cfs.
        # Not an observed zero; unresolved CDEC data-quality issue.
        list(ok = TRUE, value = 0, latest_value = 0,
             latest_date = Sys.Date(), start_date = Sys.Date(),
             end_date = Sys.Date(), message =
               "TEST ONLY: FRE missing; assumed 0 cfs (Sacramento Weir also 0).")
      }

      east_result <- if (
        isTRUE(mok$ok) &&
        isTRUE(nhg$ok) &&
        isTRUE(cos$ok)
      ) {
        list(
          ok = TRUE,
          value = mok$data$value + nhg$data$value + cos$data$value,
          start_date = min(
            mok$data$start_date,
            nhg$data$start_date,
            cos$data$start_date
          ),
          end_date = max(
            mok$data$end_date,
            nhg$data$end_date,
            cos$data$end_date
          ),
          # Display the latest MOKE date from MOKE_daily.csv for EAST,
          # because MOKE is the locally maintained component used in EAST.
          latest_date = max(moke_daily$Date, na.rm = TRUE),
          latest_value = tail(mok$data$values, 1) + tail(nhg$data$values, 1) + tail(cos$data$values, 1),
          message = NULL
        )
      } else {
        list(
          ok = FALSE,
          message = paste(
            c(
              if (!isTRUE(mok$ok)) paste0("MOK: ", mok$message) else NULL,
              if (!isTRUE(nhg$ok)) paste0("CAL: ", nhg$message) else NULL,
              if (!isTRUE(cos$ok)) paste0("COS: ", cos$message) else NULL
            ),
            collapse = " "
          )
        )
      }

      xgeo_result <- if (
        isTRUE(xgeo_a$ok) &&
        isTRUE(xgeo_c$ok)
      ) {
        list(
          ok = TRUE,
          value = xgeo_a$data$value - xgeo_c$data$value,
          start_date = max(
            xgeo_a$data$start_date,
            xgeo_c$data$start_date
          ),
          end_date = min(
            xgeo_a$data$end_date,
            xgeo_c$data$end_date
          ),
          latest_date = min(
            xgeo_a$data$end_date,
            xgeo_c$data$end_date
          ),
          latest_value = tail(xgeo_a$data$values, 1) - tail(xgeo_c$data$values, 1),
          message = NULL
        )
      } else {
        list(
          ok = FALSE,
          message = paste(
            c(
              if (!isTRUE(xgeo_a$ok)) {
                paste0("XGEO_A: ", xgeo_a$message)
              } else {
                NULL
              },
              if (!isTRUE(xgeo_c$ok)) {
                paste0("XGEO_C: ", xgeo_c$message)
              } else {
                NULL
              }
            ),
            collapse = " "
          )
        )
      }

      moke_state <- if (isTRUE(mok$ok) && !inherits(moke_daily, "error")) {
        latest_moke_date <- max(moke_daily$Date, na.rm = TRUE)
        latest_moke_index <- which(moke_daily$Date == latest_moke_date)[1]
        c(
          list(ok = TRUE),
          mok$data,
          list(
            latest_date = latest_moke_date,
            latest_value = moke_daily$Value[latest_moke_index]
          )
        )
      } else {
        list(ok = FALSE)
      }

      state <- list(
        status = "ready",
        updated_at = Sys.time(),
        EXP = exp_result,
        VER = ver_result,
        SAC = sac_result,
        YOL = yol_result,
        EAST = east_result,
        XGEO = xgeo_result,
        MOK = moke_state
      )

      observed_flow_state(state)

      if (isTRUE(exp_result$ok)) {
        updateTextInput(
          session,
          "current_ptm_exp",
          value = format_observed_value(exp_result$value)
        )
      }

      if (isTRUE(ver_result$ok)) {
        updateTextInput(
          session,
          "current_ptm_ver",
          value = format_observed_value(ver_result$value)
        )
      }

      if (isTRUE(sac_result$ok)) {
        updateTextInput(
          session,
          "current_ptm_sac",
          value = format_observed_value(sac_result$value)
        )
      }

      if (isTRUE(east_result$ok)) {
        updateTextInput(
          session,
          "current_ptm_east",
          value = format_observed_value(east_result$value)
        )
      }

      if (isTRUE(xgeo_result$ok)) {
        updateTextInput(
          session,
          "current_ptm_xgeo",
          value = format_observed_value(xgeo_result$value)
        )
      }

      if (isTRUE(sac_result$ok)) {
        updateTextInput(
          session,
          "current_eco_sac",
          value = format_observed_value(sac_result$value)
        )
      }

      if (isTRUE(yol_result$ok)) {
        updateTextInput(
          session,
          "current_eco_yol",
          value = format_observed_value(yol_result$value)
        )
      }

      if (isTRUE(mok$ok)) {
        updateTextInput(
          session,
          "current_eco_moke",
          value = format_observed_value(mok$data$value)
        )
      }

      failed_labels <- c(
        if (!isTRUE(exp_result$ok)) "EXP" else NULL,
        if (!isTRUE(ver_result$ok)) "VER" else NULL,
        if (!isTRUE(sac_result$ok)) "SAC" else NULL,
        if (!isTRUE(yol_result$ok)) "YOL" else NULL,
        if (!isTRUE(east_result$ok)) "EAST" else NULL,
        if (!isTRUE(xgeo_result$ok)) "XGEO" else NULL
      )

      if (length(failed_labels) > 0) {
        showNotification(
          paste0(
            "Live values are temporarily unavailable for: ",
            paste(failed_labels, collapse = ", "),
            ". Existing input values are being used; re-select the input method to retry."
          ),
          type = "warning",
          duration = 10
        )
      }
    },
    ignoreInit = FALSE
  )

  
  find_latest_master <- function() {
    files <- list.files(
      OUTPUT_DIR,
      pattern = "^All_PTM_ECOPTM_Event_Horizon_Results.*\\.xlsx$",
      full.names = TRUE,
      recursive = TRUE
    )
    validate(need(length(files) > 0, "No master results workbook was found."))
    files[which.max(file.info(files)$mtime)]
  }
  
  reference_data <- reactive({
    master_path <- find_latest_master()
    
    combined <- read_excel(master_path, sheet = "Combined_Results") %>%
      mutate(DSM2_Node = as.character(DSM2_Node))
    
    node_meta <- read_excel(master_path, sheet = "PTM_Results") %>%
      mutate(DSM2_Node = as.character(DSM2_Node)) %>%
      filter(!is.na(DSM2_Node)) %>%
      distinct(DSM2_Node, .keep_all = TRUE) %>%
      select(DSM2_Node, Location, Region, X, Y)
    
    ptm7_model_names <- c(
      "PTM Emulator 7-Day Entrainment",
      "PTM 7-Day Entrainment"
    )
    
    ptm30_model_names <- c(
      "PTM Emulator 30-Day Entrainment",
      "PTM 30-Day Entrainment"
    )
    
    nodes_7d <- combined %>%
      filter(
        Model %in% ptm7_model_names,
        !is.na(DSM2_Node)
      ) %>%
      distinct(DSM2_Node) %>%
      arrange(as.numeric(DSM2_Node)) %>%
      pull(DSM2_Node)
    
    nodes_30d <- combined %>%
      filter(
        Model %in% ptm30_model_names,
        !is.na(DSM2_Node)
      ) %>%
      distinct(DSM2_Node) %>%
      arrange(as.numeric(DSM2_Node)) %>%
      pull(DSM2_Node)
    
    list(
      node_meta = node_meta,
      nodes_7d = nodes_7d,
      nodes_30d = nodes_30d
    )
  })
  
  eh_baseline <- read_csv(
    file.path("STN_EMULATOR", "EH_baseline.csv"),
    show_col_types = FALSE
  )
  
  delta_boundary <- st_read(
    file.path(SHAPE_DIR, "Bay_Delta_Poly_New.shp"),
    quiet = TRUE
  ) %>% st_transform(26910)
  delta_channels <- st_read(
    file.path(SHAPE_DIR, "hydro_delta_marsh.shp"),
    quiet = TRUE
  ) %>% st_transform(26910)
  delta_boundary_wgs84 <- st_transform(
    delta_boundary,
    4326
  )
  
  delta_channels_wgs84 <- st_transform(
    delta_channels,
    4326
  )
  
  add_delta_background_layers <- function(map) {
    map %>%
      leaflet::addPolygons(
        data = delta_boundary_wgs84,
        fillColor = "#eef7f9",
        fillOpacity = 0.12,
        color = "#ffffff",
        opacity = 0.85,
        weight = 1,
        group = "DSM2 Delta Boundary"
      ) %>%
      leaflet::addPolylines(
        data = delta_channels_wgs84,
        color = "#67C1D4",
        weight = 1.2,
        opacity = 0.85,
        group = "Delta Channels"
      )
  }

  add_osm_basemap <- function(map) {
    map %>%
      leaflet::addTiles(
        urlTemplate = "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        attribution = "&copy; OpenStreetMap contributors",
        group = "OpenStreetMap",
        options = leaflet::tileOptions(
          maxZoom = 19
        )
      )
  }

  add_reset_view_control <- function(
    map,
    lng = -121.60,
    lat = 38.05,
    zoom = 9
  ) {
    js <- sprintf(
      "function(el, x) {
         var map = this;
         var ResetControl = L.Control.extend({
           options: {position: 'topleft'},
           onAdd: function(map) {
             var container = L.DomUtil.create('div', 'leaflet-bar leaflet-control');
             var link = L.DomUtil.create('a', '', container);
             link.href = '#';
             link.title = 'Reset map view';
             link.setAttribute('aria-label', 'Reset map view');
             link.innerHTML = '&#8962;';
             link.style.fontSize = '20px';
             link.style.lineHeight = '30px';
             link.style.textAlign = 'center';
             link.style.width = '30px';
             link.style.height = '30px';
             L.DomEvent.disableClickPropagation(container);
             L.DomEvent.on(link, 'click', function(e) {
               L.DomEvent.preventDefault(e);
               map.setView([%f, %f], %d);
             });
             return container;
           }
         });
         map.addControl(new ResetControl());
       }",
      lat, lng, zoom
    )
    htmlwidgets::onRender(map, js)
  }
  
  dsm2_nodes_raw <- st_read(
    DSM2_NODE_SHP,
    quiet = TRUE
  )
  
  dsm2_channels_raw <- st_read(
    DSM2_CHANNEL_SHP,
    quiet = TRUE
  )
  
  channel_definition <- parse_dsm2_channel_definition(
    DSM2_CHANNEL_DEF
  )
  
  eh_paths <- readr::read_csv(
    DSM2_PATH_FILE,
    show_col_types = FALSE
  )
  
  node_id_col <- find_first_existing_column(
    dsm2_nodes_raw,
    c(
      "DSM2_Node",
      "DSM2NODE",
      "NODE",
      "NODE_ID",
      "NODEID",
      "ID"
    ),
    "DSM2 node ID"
  )
  
  channel_id_col <- find_first_existing_column(
    dsm2_channels_raw,
    c(
      "CHAN_NO",
      "CHANO",
      "CHANNEL",
      "CHANNEL_ID",
      "CHANNELID",
      "CHAN",
      "ID"
    ),
    "DSM2 channel ID"
  )
  
  dsm2_nodes <- dsm2_nodes_raw |>
    dplyr::mutate(
      DSM2_Node = as.integer(.data[[node_id_col]])
    ) |>
    st_transform(26910)
  
  dsm2_channels <- dsm2_channels_raw |>
    dplyr::mutate(
      CHAN_NO = as.integer(.data[[channel_id_col]])
    ) |>
    dplyr::left_join(
      channel_definition,
      by = "CHAN_NO"
    ) |>
    st_transform(26910)
  
  river_centerline <- st_read(
    file.path(SHAPE_DIR, "CCF_OldRiver_CL.shp"),
    quiet = TRUE
  ) %>% st_transform(26910)
  
  river_length_miles <- as.numeric(st_length(river_centerline)) / 1609.344
  
  saved_runs <- reactiveVal(list())
  
  add_saved_run <- function(
    name,
    condition,
    family,
    data
  ) {
    
    current <- saved_runs()
    
    run_number <-
      length(current) + 1
    
    time_stamp <- format(
      Sys.time(),
      "%H:%M:%S"
    )
    
    input_method_label <- if (
      "Input_Method" %in% names(data) &&
      any(data$Input_Method == "folder", na.rm = TRUE)
    ) {
      "Archive Folder"
    } else if (
      "Input_Method" %in% names(data) &&
      any(data$Input_Method == "upload", na.rm = TRUE)
    ) {
      "Uploaded File"
    } else {
      "Single Values"
    }
    
    key <- paste0(
      run_number,
      ". ",
      condition,
      " | ",
      name,
      " | Input: ",
      input_method_label,
      " | ",
      family,
      " | ",
      time_stamp
    )
    
    data$Saved_Run_ID <-
      key
    
    current[[key]] <-
      data
    
    saved_runs(
      current
    )
    
    key
  }
  add_event_horizon_bridge_edges <- function(edges) {
    
    if (
      is.null(event_horizon_bridge_channels) ||
      nrow(event_horizon_bridge_channels) == 0 ||
      nrow(edges) == 0
    ) {
      edges$VISUAL_ONLY <- FALSE
      return(edges)
    }
    
    edges <- edges |>
      dplyr::mutate(
        VISUAL_ONLY = FALSE
      )
    
    bridge_rows <- lapply(
      seq_len(nrow(edges)),
      function(i) {
        
        edge <- edges[i, ]
        
        matching_bridge <- event_horizon_bridge_channels |>
          dplyr::filter(
            FROM_NODE == edge$FROM_NODE,
            TO_NODE == edge$TO_NODE
          )
        
        if (nrow(matching_bridge) == 0) {
          return(NULL)
        }
        
        dplyr::bind_rows(
          lapply(
            seq_len(nrow(matching_bridge)),
            function(j) {
              
              bridge <- matching_bridge[j, ]
              
              bridge_def <- channel_definition |>
                dplyr::filter(
                  CHAN_NO == bridge$BRIDGE_CHAN_NO
                )
              
              if (nrow(bridge_def) == 0) {
                warning(
                  "Bridge channel ",
                  bridge$BRIDGE_CHAN_NO,
                  " was not found in the channel definition table."
                )
                return(NULL)
              }
              
              path_order_offset <- ifelse(
                bridge$BRIDGE_POSITION == "before",
                -0.1,
                0.1
              )
              
              tibble::tibble(
                FROM_NODE = bridge_def$UPNODE[1],
                TO_NODE = bridge_def$DOWNNODE[1],
                PATH_ORDER = edge$PATH_ORDER + path_order_offset,
                CHAN_NO = bridge_def$CHAN_NO[1],
                LENGTH_FT = 0,
                UPNODE = bridge_def$UPNODE[1],
                DOWNNODE = bridge_def$DOWNNODE[1],
                CUM_START_FT = edge$CUM_START_FT,
                CUM_END_FT = edge$CUM_START_FT,
                VISUAL_ONLY = TRUE
              )
            }
          )
        )
      }
    )
    
    bridge_rows <- bridge_rows[
      !vapply(
        bridge_rows,
        is.null,
        logical(1)
      )
    ]
    
    if (length(bridge_rows) == 0) {
      return(edges)
    }
    
    dplyr::bind_rows(
      edges,
      dplyr::bind_rows(bridge_rows)
    ) |>
      dplyr::arrange(
        PATH_ORDER
      )
  }
  
  path_to_edges <- function(path_string) {
    
    nodes <- strsplit(as.character(path_string), "_", fixed = TRUE)[[1]]
    nodes <- as.integer(nodes)
    
    if (length(nodes) < 2) {
      return(
        data.frame(
          FROM_NODE = integer(),
          TO_NODE = integer(),
          PATH_ORDER = integer()
        )
      )
    }
    
    data.frame(
      FROM_NODE = nodes[-length(nodes)],
      TO_NODE = nodes[-1],
      PATH_ORDER = seq_len(length(nodes) - 1)
    )
  }
  orient_channel_geometry <- function(channel_geom, from_node, to_node, node_sf) {
    
    from_geom <- node_sf |>
      dplyr::filter(DSM2_Node == from_node) |>
      st_geometry()
    
    to_geom <- node_sf |>
      dplyr::filter(DSM2_Node == to_node) |>
      st_geometry()
    
    if (length(from_geom) == 0 || length(to_geom) == 0) {
      return(channel_geom)
    }
    
    line_cast <- st_cast(channel_geom, "LINESTRING", warn = FALSE)
    coords <- st_coordinates(line_cast)
    
    first_pt <- st_sfc(
      st_point(coords[1, c("X", "Y")]),
      crs = st_crs(line_cast)
    )
    
    last_pt <- st_sfc(
      st_point(coords[nrow(coords), c("X", "Y")]),
      crs = st_crs(line_cast)
    )
    
    from_to_first <- as.numeric(st_distance(from_geom, first_pt))
    from_to_last <- as.numeric(st_distance(from_geom, last_pt))
    
    if (from_to_last < from_to_first) {
      reversed_coords <- coords[nrow(coords):1, c("X", "Y")]
      return(
        st_sfc(
          st_linestring(as.matrix(reversed_coords)),
          crs = st_crs(line_cast)
        )
      )
    }
    
    channel_geom
  }
  event_horizon_bridge_channels <- tibble::tribble(
    ~FROM_NODE, ~TO_NODE, ~BRIDGE_CHAN_NO, ~BRIDGE_POSITION,
    121L,       123L,     144L,           "before"
  )
  make_event_horizon_path_geometry <- function(
    distance_miles,
    regions = c("OMR")
  ) {
    
    distance_ft <- as.numeric(distance_miles) * 5280
    
    validate(
      need(
        !is.na(distance_ft) && distance_ft > 0,
        "Event Horizon distance must be greater than zero."
      )
    )
    
    path_table <- eh_paths |>
      dplyr::filter(Start_Node == 72)
    
    if (!is.null(regions)) {
      path_table <- path_table |>
        dplyr::filter(Region %in% regions)
    }
    
    validate(
      need(
        nrow(path_table) > 0,
        "No matching Event Horizon paths were found."
      )
    )
    
    all_path_lines <- lapply(
      seq_len(nrow(path_table)),
      function(path_i) {
        
        one_path <- path_table[path_i, ]
        
        edges <- path_to_edges(one_path$Path)
        
        if (nrow(edges) == 0) {
          return(NULL)
        }
        
        edges <- edges |>
          dplyr::left_join(
            channel_definition |>
              dplyr::select(CHAN_NO, LENGTH_FT, UPNODE, DOWNNODE),
            by = c(
              "FROM_NODE" = "UPNODE",
              "TO_NODE" = "DOWNNODE"
            )
          )
        
        # If the path direction is opposite of the stored UPNODE/DOWNNODE,
        # try the reverse direction.
        missing_index <- is.na(edges$CHAN_NO)
        
        if (any(missing_index)) {
          
          reverse_lookup <- edges[missing_index, c("FROM_NODE", "TO_NODE", "PATH_ORDER")] |>
            dplyr::left_join(
              channel_definition |>
                dplyr::select(CHAN_NO, LENGTH_FT, UPNODE, DOWNNODE),
              by = c(
                "FROM_NODE" = "DOWNNODE",
                "TO_NODE" = "UPNODE"
              )
            )
          
          edges[missing_index, c("CHAN_NO", "LENGTH_FT")] <-
            reverse_lookup[, c("CHAN_NO", "LENGTH_FT")]
        }
        
        edges <- edges |>
          dplyr::filter(!is.na(CHAN_NO), !is.na(LENGTH_FT)) |>
          dplyr::arrange(PATH_ORDER) |>
          dplyr::mutate(
            CUM_START_FT = dplyr::lag(cumsum(LENGTH_FT), default = 0),
            CUM_END_FT = cumsum(LENGTH_FT)
          )
        
        if (nrow(edges) == 0) {
          return(NULL)
        }
        
        keep_edges <- edges |>
          dplyr::filter(CUM_START_FT < distance_ft)
        
        if (nrow(keep_edges) == 0) {
          return(NULL)
        }
        keep_edges <- add_event_horizon_bridge_edges(
          keep_edges
        )
        
        segment_geoms <- lapply(
          seq_len(nrow(keep_edges)),
          function(edge_i) {
            
            edge <- keep_edges[edge_i, ]
            
            channel <- dsm2_channels |>
              dplyr::filter(CHAN_NO == edge$CHAN_NO)
            
            if (nrow(channel) == 0) {
              return(NULL)
            }
            
            geom <- st_geometry(channel)
            
            geom <- orient_channel_geometry(
              channel_geom = geom,
              from_node = edge$FROM_NODE,
              to_node = edge$TO_NODE,
              node_sf = dsm2_nodes
            )
            
            if (
              "VISUAL_ONLY" %in% names(edge) &&
              isTRUE(edge$VISUAL_ONLY)
            ) {
              
              out_geom <- geom
              
            } else if (edge$CUM_END_FT <= distance_ft) {
              
              out_geom <- geom
              
            } else {
              
              remaining_ft <- distance_ft - edge$CUM_START_FT
              
              fraction <- max(
                0,
                min(
                  1,
                  remaining_ft / edge$LENGTH_FT
                )
              )
              
              out_geom <- lwgeom::st_linesubstring(
                st_cast(geom, "LINESTRING", warn = FALSE),
                0,
                fraction
              )
            }
            # A DSM2 channel can consist of several geometry features.
            # sf::st_sf requires one metadata record PER geometry. Previously
            # a single record was paired with e.g. 15 line features, producing:
            # "arguments imply differing number of rows: 1, 15".
            out_sfc <- sf::st_geometry(out_geom)
            n_parts <- length(out_sfc)
            if (n_parts == 0L) return(NULL)
            visual_only <- if ("VISUAL_ONLY" %in% names(edge)) {
              isTRUE(edge$VISUAL_ONLY[1])
            } else FALSE
            sf::st_sf(
              Region = rep(as.character(one_path$Region[1]), n_parts),
              Location = rep(as.character(one_path$`Insertion Location Name`[1]), n_parts),
              Target_Node = rep(one_path$DSM2_Node[1], n_parts),
              Start_Node = rep(one_path$Start_Node[1], n_parts),
              Path_TotalLength_ft = rep(one_path$TotalLength_ft[1], n_parts),
              Event_Horizon_miles = rep(distance_miles, n_parts),
              Event_Horizon_ft = rep(distance_ft, n_parts),
              CHAN_NO = rep(edge$CHAN_NO[1], n_parts),
              FROM_NODE = rep(edge$FROM_NODE[1], n_parts),
              TO_NODE = rep(edge$TO_NODE[1], n_parts),
              PATH_ORDER = rep(edge$PATH_ORDER[1], n_parts),
              Visual_Only = rep(visual_only, n_parts),
              geometry = out_sfc,
              crs = st_crs(dsm2_channels)
            )
          }
        )
        
        segment_geoms <- segment_geoms[!vapply(segment_geoms, is.null, logical(1))]
        
        if (length(segment_geoms) == 0) {
          return(NULL)
        }
        
        dplyr::bind_rows(segment_geoms)
      }
    )
    
    all_path_lines <- all_path_lines[!vapply(all_path_lines, is.null, logical(1))]
    
    validate(
      need(
        length(all_path_lines) > 0,
        "No Event Horizon channel geometry could be created from the selected paths."
      )
    )
    
    dplyr::bind_rows(all_path_lines)
  }
  make_empty_comparison_map <- function(message) {
    
    leaflet() %>%
      add_osm_basemap() %>%
      addPolygons(
        data = st_transform(
          delta_boundary,
          4326
        ),
        fillColor = "#eef7f9",
        fillOpacity = 0.2,
        color = "#0a7e8c",
        weight = 1,
        group = "DSM2 Delta Boundary"
      ) %>%
      addPolylines(
        data = st_transform(
          delta_channels,
          4326
        ),
        color = "#2b8cbe",
        weight = 1,
        opacity = 0.6,
        group = "Channels"
      ) %>%
      addControl(
        html = paste0(
          "<div style='background:white;padding:12px 14px;",
          "border-radius:6px;border:1px solid #bbb;",
          "box-shadow:0 2px 6px rgba(0,0,0,0.2);",
          "font-size:14px;max-width:360px;'>",
          message,
          "</div>"
        ),
        position = "topright"
      ) %>%
      add_reset_view_control(zoom = 9) %>%
      leaflet::setView(
        lng = -121.60,
        lat = 38.05,
        zoom = 9
      )
  }
  # =====================================================================
  # EXPLORATION ONLY: New CCWD public-release LightGBM models.
  # The archive workflow and existing display components are untouched.
  # These functions only run for Exploration (current / single inputs).
  # =====================================================================
  # Use the same validated model paths checked at server startup.
  new_model_paths <- NEW_CCWD_MODEL_FILES
  find_new_model <- function(key) {
    if (!key %in% names(NEW_CCWD_MODEL_PATHS)) {
      stop("Unrecognized CCWD model key: ", key)
    }
    path <- unname(NEW_CCWD_MODEL_PATHS[[key]])
    if (!file.exists(path)) stop("New CCWD model file not found: ", path)
    path
  }
  load_new_model <- local({
    cache <- new.env(parent = emptyenv())
    function(key) {
      if (!exists(key, envir = cache, inherits = FALSE)) {
        assign(key, lightgbm::lgb.load(find_new_model(key)), envir = cache)
      }
      get(key, envir = cache, inherits = FALSE)
    }
  })
  # The published README defines predictor ORDER. Use the order from the
  # trained feature header; do not call booster$feature_name() (not R API).
  # Only the feature count is validated here. Categorical node encoding
  # still needs cross-checking with the developer's training specification.
  new_predict <- function(key, feature_matrix) {
    file <- find_new_model(key)
    trained <- read_lgb_features(file)
    if (length(trained) != ncol(feature_matrix)) stop(
      key, ": expected ", length(trained), " features, got ",
      ncol(feature_matrix), ". Training feature names: ",
      paste(trained, collapse = ", ")
    )
    colnames(feature_matrix) <- trained
    vals <- as.numeric(predict(load_new_model(key), feature_matrix))
    if (length(vals) != nrow(feature_matrix) || any(!is.finite(vals))) {
      stop(key, ": predictions were missing or non-finite.")
    }
    vals
  }
  get_exploration_delta_inputs <- function(prefix) {
    qgcd <- parse_flow_input(input[[paste0(prefix, "_ptm_qgcd")]])
    qprec <- parse_flow_input(input[[paste0(prefix, "_ptm_qprec")]])
    dcc_status <- suppressWarnings(as.numeric(input[[paste0(prefix, "_ptm_dcc_status")]]))
    validate(need(length(dcc_status)==1L && is.finite(dcc_status) &&
                    dcc_status %in% c(0,1),
                  "Select the DCC gate status: Closed or Open."))
    # For Exploration historical conditions:
    # CLOSED => force zero. OPEN => measured DLC flow (CDEC plot 33744),
    # optionally user-edited scenario input. Never use CalSim3 regression.
    if (identical(dcc_status, 0)) {
      dcc_flow <- 0
      dcc_source <- "FORCED_ZERO_USER_SELECTED_CLOSED"
    } else {
      flow_text <- input[[paste0(prefix, "_ptm_dcc_flow")]]
      flow_text <- if (is.null(flow_text)) "" else trimws(as.character(flow_text)[1])
      measured <- ccwd_dcc_measured()
      if (!nzchar(flow_text)) {
        if (!is.null(measured) && is.finite(measured$value)) {
          dcc_flow <- measured$value
          dcc_source <- "CDEC_HISTORICAL_MEASURED"
        } else {
          sac_measured <- parse_flow_input(input[[paste0(prefix,"_ptm_sac")]])
          validate(need(length(sac_measured)==1L && is.finite(sac_measured),
            'DCC observation unavailable; valid measured SAC is required for regression.'))
          dcc_flow <- 0.1846*sac_measured + 306.52
          dcc_source <- "ESTIMATED_FROM_MEASURED_SAC"
        }
      } else {
        dcc_flow <- parse_flow_input(flow_text)
        sac_measured <- parse_flow_input(input[[paste0(prefix,"_ptm_sac")]])
        dcc_source <- if (!is.null(measured) &&
                         isTRUE(all.equal(dcc_flow, round(measured$value,0)))) {
          "CDEC_HISTORICAL_MEASURED"
        } else if (length(sac_measured)==1L && is.finite(sac_measured) &&
                   abs(dcc_flow-round(0.1846*sac_measured+306.52))<=1) {
          "ESTIMATED_FROM_MEASURED_SAC"
        } else {
          "USER_EDITED_SCENARIO_NOT_VERIFIED_MEASUREMENT"
        }
      }
    }
    inputs <- c(QGCD=qgcd, QPREC=qprec, DCC_FLOW=dcc_flow)
    validate(need(all(is.finite(inputs)),
      "Provide finite gross depletion/accretion inputs (TAF/month), ",
      "and a measured or explicitly entered DCC flow (cfs)."))
    list(qgcd=qgcd, qprec=qprec, dcc_flow=dcc_flow,
         dcc_open=dcc_status, dcc_source=dcc_source)
  }
  make_new_ptm_results <- function(condition, scenario_name, exp, ver, sac,
                                   east, qgcd, qprec, dcc_flow) {
    ref <- reference_data()
    make_one <- function(key, title, nodes) {
      # Exact public README predictor order (PTM7 = 7, PTM30 = 8).
      features <- cbind(as.numeric(nodes), exp, ver, sac, qgcd, qprec, east)
      if (identical(key, "PTM30")) features <- cbind(features, dcc_flow)
      raw <- new_predict(key, features)
      data.frame(
        Condition = condition, Input_Method = "single", Scenario_Name = scenario_name,
        Model = title, DSM2_Node = as.character(nodes),
        Prediction_Raw = raw, Prediction_Final = bound_percent(raw),
        Output_Unit = "Percent", EXP = exp, VER = ver, SAC = sac,
        EAST = east, QGCD = qgcd, QPREC = qprec, DCC_flow = dcc_flow,
        stringsAsFactors = FALSE
      ) %>% left_join(ref$node_meta, by = "DSM2_Node")
    }
    bind_rows(
      make_one("PTM7", "PTM Emulator 7-Day Entrainment", ref$nodes_7d),
      make_one("PTM30", "PTM Emulator 30-Day Entrainment", ref$nodes_30d)
    )
  }
  make_new_eco_results <- function(condition, scenario_name, sac, yol, moke, dcc) {
    # Each ECO LightGBM model predicts one scalar from four inputs. Construct
    # two explicitly aligned result rows rather than mixing scalar metadata
    # with a two-model prediction vector (R row-count 1 vs 2 errors).
    scalar <- function(value, label) {
      value <- suppressWarnings(as.numeric(value))
      if (length(value) != 1L || !is.finite(value))
        stop("ECO-PTM requires one finite value for ", label, ".", call. = FALSE)
      value
    }
    sac <- scalar(sac, "SAC")
    yol <- scalar(yol, "YOL")
    moke <- scalar(moke, "MOKE")
    dcc <- scalar(dcc, "DCC")
    validate(need(dcc %in% c(0,1), "DCC must be 0 (closed) or 1 (open)."))
    mat <- matrix(c(sac, yol, moke, dcc), nrow = 1L)
    surv <- new_predict("ECO_survival", mat)
    interior <- new_predict("ECO_interior", mat)
    if (length(surv) != 1L || length(interior) != 1L)
      stop("ECO-PTM models must each produce exactly one prediction.", call. = FALSE)
    raw <- c(as.numeric(surv), as.numeric(interior))
    n <- length(raw)
    data.frame(
      Condition = rep(as.character(condition)[1], n),
      Input_Method = rep("single", n),
      Scenario_Name = rep(as.character(scenario_name)[1], n),
      Model = c("ECO-PTM Emulator Survival", "ECO-PTM Emulator Interior Routing"),
      Prediction_Raw = raw,
      Prediction_Final = bound_percent(raw),
      Output_Unit = rep("Percent", n),
      SAC = rep(sac, n), YOL = rep(yol, n), MOKE = rep(moke, n),
      DCC = rep(dcc, n), stringsAsFactors = FALSE
    )
  }
  make_new_eh_result <- function(condition, scenario_name, risk, exp, ver,
                                 sac, qgcd, qprec, east) {
    validate(need(risk %in% seq(15,80,5), "Risk threshold must be 15–80 by 5."))
    # Risk, EXP, VER, SAC, QGCD, QPREC, EAST = seven features.
    mat <- matrix(c(risk, exp, ver, sac, qgcd, qprec, east), nrow = 1)
    raw <- new_predict("EH", mat)
    data.frame(
      Condition = condition, Input_Method = "single", Scenario_Name = scenario_name,
      Model = "Event Horizon", Risk_Level_Percent = risk,
      Prediction_Raw = raw, Prediction_Final = pmax(0, raw),
      Output_Unit = "River miles", EXP = exp, VER = ver, SAC = sac,
      EAST = east, QGCD = qgcd, QPREC = qprec, stringsAsFactors = FALSE
    )
  }

  make_ptm_results <- function(condition, scenario_name, exp, ver, sac, east, xgeo) {
    ref <- reference_data()
    
    make_one <- function(model, model_name, nodes) {
      model_input <- data.frame(
        DSM2_Node = as.numeric(nodes),
        EXP = exp,
        VER = ver,
        SAC = sac,
        EAST = east,
        XGEO = xgeo
      )
      
      raw <- predict(model, as.matrix(model_input))
      
      data.frame(
        Condition = condition,
        Input_Method = "single",
        Scenario_Name = scenario_name,
        Model = model_name,
        DSM2_Node = as.character(nodes),
        Prediction_Raw = raw,
        Prediction_Final = bound_percent(raw),
        Output_Unit = "Percent",
        EXP = exp,
        VER = ver,
        SAC = sac,
        EAST = east,
        XGEO = xgeo,
        stringsAsFactors = FALSE
      ) %>%
        left_join(ref$node_meta, by = "DSM2_Node")
    }
    
    bind_rows(
      make_one(ptm7_model, "PTM Emulator 7-Day Entrainment", ref$nodes_7d),
      make_one(ptm30_model, "PTM Emulator 30-Day Entrainment", ref$nodes_30d)
    )
  }
  
  build_eco_input <- function(sac, yol, moke, dcc, required_features) {
    values <- list(
      SAC = sac,
      FPT = sac,
      YOLO = yol,
      YOL = yol,
      MOKE = moke,
      MOK = moke,
      DCC = dcc
    )
    
    missing <- setdiff(required_features, names(values))
    if (length(missing) > 0) {
      stop("Missing ECO-PTM feature mapping for: ", paste(missing, collapse = ", "))
    }
    
    out <- as.data.frame(
      lapply(required_features, function(x) values[[x]]),
      check.names = FALSE
    )
    names(out) <- required_features
    out
  }
  
  make_eco_results <- function(condition, scenario_name, sac, yol, moke, dcc) {
    survival_input <- build_eco_input(
      sac, yol, moke, dcc, eco_survival_features
    )
    interior_input <- build_eco_input(
      sac, yol, moke, dcc, eco_interior_features
    )
    
    survival_raw <- predict(eco_survival_model, as.matrix(survival_input))
    interior_raw <- predict(eco_interior_model, as.matrix(interior_input))
    
    data.frame(
      Condition = condition,
      Input_Method = "single",
      Scenario_Name = scenario_name,
      Model = c("ECO-PTM Emulator Survival", "ECO-PTM Emulator Interior Routing"),
      Prediction_Raw = c(survival_raw, interior_raw),
      Prediction_Final = bound_percent(c(survival_raw, interior_raw)),
      Output_Unit = "Percent",
      SAC = sac,
      YOL = yol,
      MOKE = moke,
      DCC = dcc,
      stringsAsFactors = FALSE
    )
  }
  
  make_eh_result <- function(condition, scenario_name, exp, ver, east, xgeo, risk) {
    model_input <- data.frame(
      VER = ver,
      EXP = exp,
      EAST = east,
      XGEO = xgeo,
      Risk = risk
    )
    
    raw <- predict(get_event_horizon_model(), as.matrix(model_input))
    
    data.frame(
      Condition = condition,
      Input_Method = "single",
      Scenario_Name = scenario_name,
      Model = "Event Horizon",
      Risk_Level_Percent = risk,
      Prediction_Raw = raw,
      Prediction_Final = pmax(0, raw),
      Output_Unit = "River miles",
      EXP = exp,
      VER = ver,
      EAST = east,
      XGEO = xgeo,
      stringsAsFactors = FALSE
    )
  }
  
  # ===============================================================
  # Archive-folder workflow helpers
  # ===============================================================
  
  # ===============================================================
  # GitHub archive workflow helpers
  # ===============================================================
  
  .github_api_cache <- new.env(parent = emptyenv())
  
  github_contents_url <- function(path = "") {
    path <- gsub("^/+|/+$", "", path)
    
    base_url <- if (nzchar(path)) {
      paste0(
        GITHUB_API_BASE,
        "/",
        paste(
          vapply(
            strsplit(path, "/", fixed = TRUE)[[1]],
            utils::URLencode,
            character(1),
            reserved = TRUE
          ),
          collapse = "/"
        )
      )
    } else {
      GITHUB_API_BASE
    }
    
    paste0(
      base_url,
      "?ref=",
      utils::URLencode(GITHUB_BRANCH, reserved = TRUE)
    )
  }
  
  github_read_json <- function(url, cache_seconds = 300) {
    
    cache_key <- url
    
    # ------------------------------------------------------------
    # Return cached result when it is still fresh
    # ------------------------------------------------------------
    
    if (exists(
      cache_key,
      envir = .github_api_cache,
      inherits = FALSE
    )) {
      
      cached <- get(
        cache_key,
        envir = .github_api_cache,
        inherits = FALSE
      )
      
      cache_age <- as.numeric(
        difftime(
          Sys.time(),
          cached$retrieved_at,
          units = "secs"
        )
      )
      
      if (
        is.finite(cache_age) &&
        cache_age <= cache_seconds
      ) {
        return(cached$value)
      }
    }
    
    
    # ------------------------------------------------------------
    # Read GitHub API response
    #
    # Using url() here rather than download.file(..., libcurl)
    # because this connection method works reliably in the
    # Windows/R environment used by this application.
    # ------------------------------------------------------------
    
    json_text <- tryCatch(
      
      {
        connection <- url(
          url,
          open = "rt"
        )
        
        on.exit(
          close(connection),
          add = TRUE
        )
        
        paste(
          readLines(
            connection,
            warn = FALSE,
            encoding = "UTF-8"
          ),
          collapse = "\n"
        )
      },
      
      error = function(error_condition) {
        
        stop(
          paste0(
            "The GitHub archive could not be reached. ",
            conditionMessage(error_condition)
          ),
          call. = FALSE
        )
      }
    )
    
    
    # ------------------------------------------------------------
    # Make sure GitHub actually returned something
    # ------------------------------------------------------------
    
    if (
      is.null(json_text) ||
      length(json_text) == 0 ||
      !nzchar(json_text)
    ) {
      
      stop(
        "GitHub returned an empty response while reading the archive.",
        call. = FALSE
      )
    }
    
    
    # ------------------------------------------------------------
    # Parse JSON
    # ------------------------------------------------------------
    
    result <- tryCatch(
      
      {
        jsonlite::fromJSON(
          json_text,
          simplifyDataFrame = TRUE
        )
      },
      
      error = function(error_condition) {
        
        stop(
          paste0(
            "The GitHub archive response could not be parsed. ",
            conditionMessage(error_condition)
          ),
          call. = FALSE
        )
      }
    )
    
    
    # ------------------------------------------------------------
    # GitHub error responses contain a message field
    # ------------------------------------------------------------
    
    if (
      is.list(result) &&
      !is.data.frame(result) &&
      !is.null(result$message)
    ) {
      
      stop(
        paste0(
          "GitHub archive request failed: ",
          result$message
        ),
        call. = FALSE
      )
    }
    
    
    # ------------------------------------------------------------
    # Cache successful response
    # ------------------------------------------------------------
    
    assign(
      cache_key,
      list(
        retrieved_at = Sys.time(),
        value = result
      ),
      envir = .github_api_cache
    )
    
    
    result
  }
  
  github_directory_contents <- function(path, cache_seconds = 300) {
    contents <- github_read_json(
      github_contents_url(path),
      cache_seconds = cache_seconds
    )
    
    if (!is.data.frame(contents)) {
      stop(
        paste0(
          "GitHub did not return a directory listing for: ",
          path
        ),
        call. = FALSE
      )
    }
    
    required_columns <- c(
      "name",
      "path",
      "type"
    )
    
    missing_columns <- setdiff(
      required_columns,
      names(contents)
    )
    
    if (length(missing_columns) > 0) {
      stop(
        paste0(
          "The GitHub directory response is missing: ",
          paste(missing_columns, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }
    
    contents
  }
  
  list_archive_dates <- function() {
    contents <- tryCatch(
      {
        github_directory_contents(
          GITHUB_ARCHIVE_PATH,
          cache_seconds = 300
        )
      },
      error = function(error_condition) {
        warning(
          conditionMessage(error_condition),
          call. = FALSE
        )
        
        return(NULL)
      }
    )
    
    if (is.null(contents) || nrow(contents) == 0) {
      return(character(0))
    }
    
    archive_dates <- contents$name[
      contents$type == "dir" &
        grepl("^[0-9]{8}$", contents$name)
    ]
    
    sort(
      unique(archive_dates),
      decreasing = TRUE
    )
  }
  
  archive_csv_files <- function(archive_date) {
    req(archive_date)
    
    archive_date <- trimws(
      as.character(archive_date)[1]
    )
    
    validate(
      need(
        grepl("^[0-9]{8}$", archive_date),
        "The selected archive date is not in YYYYMMDD format."
      )
    )
    
    archive_path <- paste(
      GITHUB_ARCHIVE_PATH,
      archive_date,
      sep = "/"
    )
    
    contents <- tryCatch(
      {
        github_directory_contents(
          archive_path,
          cache_seconds = 300
        )
      },
      error = function(error_condition) {
        validate(
          need(
            FALSE,
            paste0(
              "The GitHub archive folder ",
              archive_date,
              " could not be read. ",
              conditionMessage(error_condition)
            )
          )
        )
      }
    )
    
    if (
      is.null(contents) ||
      !is.data.frame(contents) ||
      nrow(contents) == 0
    ) {
      return(character(0))
    }
    
    if (!"download_url" %in% names(contents)) {
      return(character(0))
    }
    
    matching_files <- contents[
      contents$type == "file" &
        grepl("\\.csv$", contents$name, ignore.case = TRUE) &
        grepl("OMRI", contents$name, ignore.case = TRUE),
      ,
      drop = FALSE
    ]
    
    if (nrow(matching_files) == 0) {
      return(character(0))
    }
    
    download_urls <- as.character(
      matching_files$download_url
    )
    
    download_urls <- download_urls[
      !is.na(download_urls) &
        nzchar(download_urls)
    ]
    
    sort(unique(download_urls))
  }
  extract_omri_scenario <- function(file_path) {
    
    file_name <- tools::file_path_sans_ext(
      basename(file_path)
    )
    
    matches <- regmatches(
      file_name,
      gregexpr(
        "-[0-9]+",
        file_name,
        perl = TRUE
      )
    )[[1]]
    
    if (length(matches) == 0) {
      return(file_name)
    }
    
    tail(matches, 1)
  }
  
  
  archive_scenario_choices <- function(archive_date) {
    
    files <- archive_csv_files(archive_date)
    
    if (length(files) == 0) {
      return(character(0))
    }
    
    scenarios <- vapply(
      files,
      extract_omri_scenario,
      character(1)
    )
    
    numeric_scenarios <- suppressWarnings(
      as.numeric(scenarios)
    )
    
    scenarios[
      order(
        is.na(numeric_scenarios),
        numeric_scenarios,
        scenarios
      )
    ]
  }
  
  
  archive_file_for_scenario <- function(
    archive_date,
    scenario
  ) {
    
    files <- archive_csv_files(archive_date)
    
    scenarios <- vapply(
      files,
      extract_omri_scenario,
      character(1)
    )
    
    matching <- files[
      scenarios == scenario
    ]
    
    validate(
      need(
        length(matching) > 0,
        paste0(
          "No archive CSV was found for OMRI scenario ",
          scenario,
          " in ",
          archive_date,
          "."
        )
      )
    )
    
    matching[1]
  }
  
  
  normalize_archive_name <- function(x) {
    
    toupper(
      gsub(
        "[^A-Za-z0-9]+",
        "",
        trimws(as.character(x))
      )
    )
  }
  
  
  find_archive_column <- function(
    data,
    aliases,
    required = TRUE
  ) {
    
    normalized_names <- normalize_archive_name(
      names(data)
    )
    
    normalized_aliases <- normalize_archive_name(
      aliases
    )
    
    match_index <- match(
      normalized_aliases,
      normalized_names
    )
    
    match_index <- match_index[
      !is.na(match_index)
    ]
    
    if (length(match_index) == 0) {
      
      if (required) {
        stop(
          paste0(
            "Archive file is missing a required column. Expected one of: ",
            paste(
              aliases,
              collapse = ", "
            )
          )
        )
      }
      
      return(NULL)
    }
    
    names(data)[
      match_index[1]
    ]
  }
  
  
  parse_archive_dates <- function(x) {
    
    if (inherits(x, "Date")) {
      return(x)
    }
    
    if (is.numeric(x)) {
      return(
        as.Date(
          x,
          origin = "1899-12-30"
        )
      )
    }
    
    x <- trimws(
      as.character(x)
    )
    
    formats <- c(
      "%m/%d/%Y",
      "%Y-%m-%d",
      "%m/%d/%y",
      "%Y%m%d"
    )
    
    result <- rep(
      as.Date(NA),
      length(x)
    )
    
    for (fmt in formats) {
      
      missing_index <- is.na(result)
      
      result[missing_index] <- as.Date(
        x[missing_index],
        format = fmt
      )
    }
    
    result
  }
  
  
  # ===============================================================
  # XGEO retrieval for archive files
  # ===============================================================
  # XGEO is calculated as:
  #   XGEO = flow at USGS 11447890 - flow at USGS 11447905
  #
  # Daily values are downloaded only when the archive CSV does not
  # already contain XGEO or XGEO_A / XGEO_C. A cache CSV is written
  # inside the selected dated archive folder so later runs do not
  # need to download the same dates again.
  
  download_usgs_daily_flow <- function(
    site_number,
    start_date,
    end_date,
    search_buffer_days = 14
  ) {

    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)

    query_start <- start_date - search_buffer_days
    query_end <- end_date + search_buffer_days

    query_url <- paste0(
      "https://waterservices.usgs.gov/nwis/iv/",
      "?format=json",
      "&sites=", site_number,
      "&startDT=", format(query_start, "%Y-%m-%d"),
      "&endDT=", format(query_end + 1, "%Y-%m-%d"),
      "&parameterCd=00060",
      "&siteStatus=all"
    )
    

    response_lines <- download_observed_source_file(
      url = query_url,
      fileext = ".json",
      attempts = 3,
      pause_seconds = 1
    )

    response_text <- paste(
      response_lines,
      collapse = "\n"
    )

    response_json <- tryCatch(
      jsonlite::fromJSON(
        response_text,
        simplifyVector = FALSE
      ),
      error = function(error_condition) {
        stop(
          paste0(
            "USGS JSON response for site ",
            site_number,
            " could not be parsed: ",
            conditionMessage(error_condition)
          )
        )
      }
    )

    time_series <- response_json$value$timeSeries

    if (
      is.null(time_series) ||
      length(time_series) == 0
    ) {
      stop(
        paste0(
          "USGS site ",
          site_number,
          " returned no discharge time series near the requested dates."
        )
      )
    }

    observation_rows <- list()
    row_index <- 1L

    for (series_item in time_series) {

      value_groups <- series_item$values

      if (
        is.null(value_groups) ||
        length(value_groups) == 0
      ) {
        next
      }

      for (value_group in value_groups) {

        observations <- value_group$value

        if (
          is.null(observations) ||
          length(observations) == 0
        ) {
          next
        }

        for (observation in observations) {

          observation_rows[[row_index]] <- data.frame(
            DATE_TIME = as.character(
              observation$dateTime
            ),
            FLOW = suppressWarnings(
              as.numeric(
                observation$value
              )
            ),
            stringsAsFactors = FALSE
          )

          row_index <- row_index + 1L
        }
      }
    }

    if (length(observation_rows) == 0) {
      stop(
        paste0(
          "USGS site ",
          site_number,
          " returned no usable discharge observations."
        )
      )
    }

    usgs_values <- bind_rows(
      observation_rows
    )

    parsed_date_time <- suppressWarnings(
      as.POSIXct(
        usgs_values$DATE_TIME,
        tz = "America/Los_Angeles"
      )
    )

    missing_date_time <- is.na(
      parsed_date_time
    )

    if (any(missing_date_time)) {
      parsed_date_time[missing_date_time] <- suppressWarnings(
        as.POSIXct(
          usgs_values$DATE_TIME[missing_date_time],
          format = "%Y-%m-%dT%H:%M:%S%z",
          tz = "America/Los_Angeles"
        )
      )
    }

    daily_data <- data.frame(
      DATE = as.Date(
        parsed_date_time,
        tz = "America/Los_Angeles"
      ),
      FLOW = usgs_values$FLOW
    ) %>%
      filter(
        !is.na(DATE),
        is.finite(FLOW)
      ) %>%
      group_by(
        DATE
      ) %>%
      summarise(
        FLOW = mean(
          FLOW,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      arrange(
        DATE
      )

    if (nrow(daily_data) == 0) {
      stop(
        paste0(
          "No usable daily discharge values were available from USGS site ",
          site_number,
          " within ",
          search_buffer_days,
          " days of the requested period."
        )
      )
    }

    daily_data
  }


  fill_with_nearest_available_date <- function(
    available_data,
    required_dates,
    value_columns
  ) {

    required_dates <- sort(
      unique(
        as.Date(
          required_dates
        )
      )
    )

    available_data <- available_data %>%
      mutate(
        DATE = as.Date(
          DATE
        )
      ) %>%
      filter(
        !is.na(DATE)
      ) %>%
      arrange(
        DATE
      )

    if (
      nrow(available_data) == 0 ||
      length(required_dates) == 0
    ) {
      return(
        tibble::tibble()
      )
    }

    nearest_rows <- lapply(
      required_dates,
      function(required_date) {

        usable_rows <- available_data %>%
          filter(
            if_all(
              all_of(
                value_columns
              ),
              ~ !is.na(.x)
            )
          )

        if (nrow(usable_rows) == 0) {
          return(NULL)
        }

        nearest_index <- which.min(
          abs(
            as.numeric(
              usable_rows$DATE - required_date
            )
          )
        )

        selected_row <- usable_rows[
          nearest_index,
          ,
          drop = FALSE
        ]

        selected_row$SOURCE_DATE <- selected_row$DATE
        selected_row$DATE <- required_date

        selected_row
      }
    )

    nearest_rows <- nearest_rows[
      !vapply(
        nearest_rows,
        is.null,
        logical(1)
      )
    ]

    if (length(nearest_rows) == 0) {
      return(
        tibble::tibble()
      )
    }

    bind_rows(
      nearest_rows
    )
  }


  get_archive_xgeo <- function(
    file_path,
    required_dates
  ) {

    required_dates <- sort(
      unique(
        as.Date(
          required_dates
        )
      )
    )

    required_dates <- required_dates[
      !is.na(
        required_dates
      )
    ]

    validate(
      need(
        length(required_dates) > 0,
        "No valid archive dates were available for calculating XGEO."
      )
    )

    archive_date_folder <- dirname(
      file_path
    )

    cache_file <- file.path(
      archive_date_folder,
      "XGEO_USGS_daily.csv"
    )

    cached_xgeo <- tibble::tibble(
      DATE = as.Date(character()),
      XGEO_A = numeric(),
      XGEO_C = numeric(),
      XGEO = numeric()
    )

    if (file.exists(cache_file)) {

      cached_xgeo <- tryCatch(
        {
          readr::read_csv(
            cache_file,
            show_col_types = FALSE
          ) %>%
            mutate(
              DATE = as.Date(
                DATE
              )
            ) %>%
            select(
              DATE,
              XGEO_A,
              XGEO_C,
              XGEO
            )
        },
        error = function(e) {
          tibble::tibble(
            DATE = as.Date(character()),
            XGEO_A = numeric(),
            XGEO_C = numeric(),
            XGEO = numeric()
          )
        }
      )
    }

    complete_cached_dates <- cached_xgeo$DATE[
      !is.na(cached_xgeo$XGEO_A) &
        !is.na(cached_xgeo$XGEO_C) &
        !is.na(cached_xgeo$XGEO)
    ]

    missing_dates <- setdiff(
      required_dates,
      complete_cached_dates
    )

    if (length(missing_dates) > 0) {

      download_start <- min(
        missing_dates
      )

      download_end <- max(
        missing_dates
      )

      xgeo_a_download <- tryCatch(
        {
          download_usgs_daily_flow(
            site_number = "11447890",
            start_date = download_start,
            end_date = download_end,
            search_buffer_days = 14
          ) %>%
            rename(
              XGEO_A = FLOW
            )
        },
        error = function(error_condition) {
          NULL
        }
      )

      xgeo_c_download <- tryCatch(
        {
          download_usgs_daily_flow(
            site_number = "11447905",
            start_date = download_start,
            end_date = download_end,
            search_buffer_days = 14
          ) %>%
            rename(
              XGEO_C = FLOW
            )
        },
        error = function(error_condition) {
          NULL
        }
      )

      available_xgeo <- full_join(
        if (is.null(xgeo_a_download)) {
          tibble::tibble(
            DATE = as.Date(character()),
            XGEO_A = numeric()
          )
        } else {
          xgeo_a_download
        },
        if (is.null(xgeo_c_download)) {
          tibble::tibble(
            DATE = as.Date(character()),
            XGEO_C = numeric()
          )
        } else {
          xgeo_c_download
        },
        by = "DATE"
      ) %>%
        bind_rows(
          cached_xgeo %>%
            select(
              DATE,
              XGEO_A,
              XGEO_C
            )
        ) %>%
        arrange(
          DATE
        ) %>%
        distinct(
          DATE,
          .keep_all = TRUE
        )

      filled_xgeo <- fill_with_nearest_available_date(
        available_data = available_xgeo,
        required_dates = missing_dates,
        value_columns = c(
          "XGEO_A",
          "XGEO_C"
        )
      )

      if (nrow(filled_xgeo) > 0) {

        filled_xgeo <- filled_xgeo %>%
          mutate(
            XGEO = XGEO_A - XGEO_C
          ) %>%
          select(
            DATE,
            XGEO_A,
            XGEO_C,
            XGEO
          )

        cached_xgeo <- bind_rows(
          cached_xgeo,
          filled_xgeo
        ) %>%
          arrange(
            DATE
          ) %>%
          distinct(
            DATE,
            .keep_all = TRUE
          )
      }
    }

    result <- cached_xgeo %>%
      filter(
        DATE %in% required_dates
      ) %>%
      arrange(
        DATE
      )

    missing_after_download <- setdiff(
      required_dates,
      result$DATE[
        !is.na(
          result$XGEO
        )
      ]
    )

    if (length(missing_after_download) > 0) {

      nearest_cached <- fill_with_nearest_available_date(
        available_data = cached_xgeo %>%
          filter(
            !is.na(XGEO_A),
            !is.na(XGEO_C),
            !is.na(XGEO)
          ),
        required_dates = missing_after_download,
        value_columns = c(
          "XGEO_A",
          "XGEO_C",
          "XGEO"
        )
      )

      if (nrow(nearest_cached) > 0) {

        result <- bind_rows(
          result,
          nearest_cached %>%
            select(
              DATE,
              XGEO_A,
              XGEO_C,
              XGEO
            )
        ) %>%
          arrange(
            DATE
          ) %>%
          distinct(
            DATE,
            .keep_all = TRUE
          )
      }
    }

    still_missing <- setdiff(
      required_dates,
      result$DATE[
        !is.na(
          result$XGEO
        )
      ]
    )

    if (length(still_missing) > 0) {

      # Final non-stopping safeguard. This is used only when neither the
      # USGS service nor the local cache contains a nearby complete record.
      fallback_rows <- tibble::tibble(
        DATE = as.Date(
          still_missing
        ),
        XGEO_A = 0,
        XGEO_C = 0,
        XGEO = 0
      )

      result <- bind_rows(
        result,
        fallback_rows
      ) %>%
        arrange(
          DATE
        ) %>%
        distinct(
          DATE,
          .keep_all = TRUE
        )

      warning(
        paste0(
          "No nearby USGS XGEO records were available for ",
          paste(
            format(
              still_missing
            ),
            collapse = ", "
          ),
          ". XGEO was set to 0 so the emulator could continue."
        ),
        call. = FALSE
      )
    }

    cached_xgeo <- bind_rows(
      cached_xgeo,
      result
    ) %>%
      arrange(
        DATE
      ) %>%
      distinct(
        DATE,
        .keep_all = TRUE
      )

    try(
      readr::write_csv(
        cached_xgeo,
        cache_file
      ),
      silent = TRUE
    )

    result %>%
      filter(
        DATE %in% required_dates
      ) %>%
      arrange(
        DATE
      )
  }

  is_remote_archive_file <- function(file_path) {
    is.character(file_path) &&
      length(file_path) == 1 &&
      !is.na(file_path) &&
      grepl(
        "^https?://",
        file_path,
        ignore.case = TRUE
      )
  }
  
  extract_archive_date_from_url <- function(file_url) {
    matches <- regmatches(
      file_url,
      gregexpr(
        "(?<![0-9])[0-9]{8}(?![0-9])",
        file_url,
        perl = TRUE
      )
    )[[1]]
    
    if (length(matches) == 0) {
      return("unknown_date")
    }
    
    tail(matches, 1)
  }
  
  materialize_archive_file <- function(file_path) {
    
    # Local files can pass through unchanged.
    if (!is_remote_archive_file(file_path)) {
      return(file_path)
    }
    
    archive_date <- extract_archive_date_from_url(
      file_path
    )
    
    # basename() retains URL encoding, so decode %20 back to a space.
    file_name <- utils::URLdecode(
      basename(file_path)
    )
    
    if (
      is.na(file_name) ||
      !nzchar(file_name)
    ) {
      stop(
        "The GitHub archive URL does not contain a usable filename.",
        call. = FALSE
      )
    }
    
    local_directory <- file.path(
      GITHUB_ARCHIVE_CACHE_DIR,
      archive_date
    )
    
    dir.create(
      local_directory,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    local_file <- file.path(
      local_directory,
      file_name
    )
    
    # Reuse a previously downloaded, nonempty CSV.
    if (
      file.exists(local_file) &&
      !is.na(file.info(local_file)$size) &&
      file.info(local_file)$size > 0
    ) {
      return(local_file)
    }
    
    temporary_file <- tempfile(
      pattern = "github_archive_",
      tmpdir = local_directory,
      fileext = ".download"
    )
    
    on.exit(
      {
        if (file.exists(temporary_file)) {
          unlink(temporary_file)
        }
      },
      add = TRUE
    )
    
    # Use url(), which has already been verified to work for
    # raw.githubusercontent.com in this Windows R environment.
    tryCatch(
      {
        connection <- url(
          file_path,
          open = "rb"
        )
        
        on.exit(
          close(connection),
          add = TRUE
        )
        
        file_bytes <- readBin(
          connection,
          what = "raw",
          n = 500 * 1024 * 1024
        )
        
        if (length(file_bytes) == 0) {
          stop(
            "GitHub returned an empty response."
          )
        }
        
        writeBin(
          file_bytes,
          temporary_file
        )
      },
      error = function(error_condition) {
        stop(
          paste0(
            "The selected GitHub archive CSV could not be downloaded: ",
            file_name,
            ". ",
            conditionMessage(error_condition)
          ),
          call. = FALSE
        )
      }
    )
    
    if (
      !file.exists(temporary_file) ||
      is.na(file.info(temporary_file)$size) ||
      file.info(temporary_file)$size <= 0
    ) {
      stop(
        paste0(
          "GitHub returned an empty archive CSV for: ",
          file_name,
          "."
        ),
        call. = FALSE
      )
    }
    
    # Confirm that the downloaded object appears to be text data,
    # rather than an HTML error page or JSON error response.
    first_lines <- tryCatch(
      {
        readLines(
          temporary_file,
          n = 5,
          warn = FALSE,
          encoding = "UTF-8"
        )
      },
      error = function(error_condition) {
        character(0)
      }
    )
    
    response_preview <- paste(
      first_lines,
      collapse = " "
    )
    
    if (
      grepl(
        "^\\s*<!DOCTYPE html|^\\s*<html|\"message\"\\s*:",
        response_preview,
        ignore.case = TRUE
      )
    ) {
      stop(
        paste0(
          "GitHub returned an error page instead of the requested CSV: ",
          file_name,
          "."
        ),
        call. = FALSE
      )
    }
    
    moved_successfully <- file.rename(
      temporary_file,
      local_file
    )
    
    # file.rename() can occasionally fail across Windows filesystem
    # boundaries. Fall back to copying the file.
    if (!isTRUE(moved_successfully)) {
      copied_successfully <- file.copy(
        temporary_file,
        local_file,
        overwrite = TRUE
      )
      
      if (!isTRUE(copied_successfully)) {
        stop(
          paste0(
            "The downloaded archive CSV could not be placed in ",
            "the local cache: ",
            file_name,
            "."
          ),
          call. = FALSE
        )
      }
      
      unlink(temporary_file)
    }
    
    if (
      !file.exists(local_file) ||
      is.na(file.info(local_file)$size) ||
      file.info(local_file)$size <= 0
    ) {
      stop(
        paste0(
          "The local cached archive CSV is missing or empty: ",
          file_name,
          "."
        ),
        call. = FALSE
      )
    }
    
    local_file
  }
  
  read_archive_csv <- function(file_path, require_xgeo = TRUE) {
    
    source_url <- if (is_remote_archive_file(file_path)) {
      file_path
    } else {
      NA_character_
    }
    
    file_path <- materialize_archive_file(
      file_path
    )
    
    raw <- readr::read_csv(
      file_path,
      col_names = FALSE,
      show_col_types = FALSE,
      progress = FALSE,
      na = c(
        "",
        "NA",
        "N/A",
        "#N/A",
        "-"
      )
    )
    
    header_scores <- apply(
      raw,
      1,
      function(row_values) {
        
        normalized <- normalize_archive_name(
          as.character(row_values)
        )
        
        sum(
          c(
            "DATE",
            "CCF",
            "TPP",
            "VNS",
            "FPT"
          ) %in% normalized
        )
      }
    )
    
    header_row <- which.max(
      header_scores
    )
    
    if (
      length(header_row) == 0 ||
      header_scores[header_row] < 3
    ) {
      stop(
        paste0(
          "Could not identify the data-header row in ",
          basename(file_path),
          "."
        )
      )
    }
    
    headers <- as.character(
      unlist(
        raw[
          header_row,
          ,
          drop = TRUE
        ]
      )
    )
    
    headers[
      is.na(headers) |
        trimws(headers) == ""
    ] <- paste0(
      "UNNAMED_",
      which(
        is.na(headers) |
          trimws(headers) == ""
      )
    )
    
    archive_data <- raw[
      seq.int(
        header_row + 1,
        nrow(raw)
      ),
      ,
      drop = FALSE
    ]
    
    names(archive_data) <- make.unique(
      headers
    )
    
    archive_data <- archive_data %>%
      filter(
        if_any(
          everything(),
          ~ !is.na(.x)
        )
      )
    
    date_col <- find_archive_column(
      archive_data,
      c(
        "DATE",
        "Date"
      )
    )
    
    type_col <- find_archive_column(
      archive_data,
      c(
        "data type",
        "data.type",
        "data_type",
        "datatype",
        "measured forecast",
        "measured/forecast",
        "record type",
        "type"
      ),
      required = FALSE
    )
    
    # In these archive files, the words "Measured" and "Forecast"
    # may be stored in a column whose header cell is blank because the
    # label "data type" appears on the metadata row above the true
    # table header. When the header lookup fails, identify that column
    # directly from its values.
    if (is.null(type_col)) {
      
      type_scores <- vapply(
        archive_data,
        function(column_values) {
          
          normalized_values <- normalize_archive_name(column_values)
          
          valid_values <- normalized_values %in% c(
            "MEASURED",
            "MEASURE",
            "FORECAST",
            "FORECASTED"
          )
          
          if (sum(valid_values, na.rm = TRUE) < 3) {
            return(0)
          }
          
          mean(valid_values, na.rm = TRUE)
        },
        numeric(1)
      )
      
      best_type_column <- which.max(type_scores)
      
      if (
        length(best_type_column) == 1 &&
        type_scores[best_type_column] >= 0.5
      ) {
        type_col <- names(archive_data)[best_type_column]
      }
    }
    
    ccf_col <- find_archive_column(
      archive_data,
      c(
        "CCF",
        "CLC"
      )
    )
    
    tpp_col <- find_archive_column(
      archive_data,
      c(
        "TPP",
        "TRP"
      )
    )
    
    ver_col <- find_archive_column(
      archive_data,
      c(
        "VNS",
        "VER",
        "Vernalis"
      )
    )
    
    sac_col <- find_archive_column(
      archive_data,
      c(
        "FPT",
        "SAC",
        "Freeport"
      )
    )
    
    moke_col <- find_archive_column(
      archive_data,
      c(
        "MOKE",
        "MOK"
      )
    )
    
    cal_col <- find_archive_column(
      archive_data,
      c(
        "CAL",
        "Calaveras"
      )
    )
    
    cos_col <- find_archive_column(
      archive_data,
      c(
        "COS",
        "Cosumnes"
      )
    )
    
    sacweir_col <- find_archive_column(
      archive_data,
      c(
        "SACWEIR",
        "SAC WEIR",
        "Sacramento Weir"
      ),
      required = FALSE
    )
    
    freweir_col <- find_archive_column(
      archive_data,
      c(
        "FREWEIR",
        "FRE WEIR",
        "Fremont Weir"
      ),
      required = FALSE
    )
    
    dcc_col <- find_archive_column(
      archive_data,
      c(
        "DCC",
        "Delta Cross Channel",
        "DCC Status",
        "DCC Gate Status",
        "DCC Open",
        "DCC_open",
        "DCCGate"
      ),
      required = FALSE
    )
    
    # Weekly OMRI files sometimes place the DCC gate flag in the FINAL
    # data column with a blank or nonstandard header.  Recognize common
    # header variants first, then use the trailing column only when its
    # contents demonstrably encode binary (0/1, open/closed) gate status.
    if (is.null(dcc_col)) {
      trailing_candidates <- rev(tail(names(archive_data), min(3L, ncol(archive_data))))
      is_binary_dcc_column <- function(values) {
        raw_values <- trimws(toupper(as.character(values)))
        raw_values <- raw_values[!is.na(raw_values) & nzchar(raw_values)]
        if (length(raw_values) < 5L) return(FALSE)
        numeric_values <- suppressWarnings(as.numeric(raw_values))
        matches <- raw_values %in% c("OPEN", "OPENED", "CLOSED", "CLOSE") |
          (is.finite(numeric_values) & numeric_values %in% c(0, 1))
        sum(matches) >= 5L && mean(matches) >= 0.9
      }
      for (candidate in trailing_candidates) {
        # Never mistake a named hydrologic variable for gate status.
        if (!grepl("^UNNAMED_|^X[0-9]+$|DCC|GATE|STATUS", candidate,
                   ignore.case = TRUE)) next
        if (is_binary_dcc_column(archive_data[[candidate]])) {
          dcc_col <- candidate
          break
        }
      }
    }

    xgeo_col <- find_archive_column(
      archive_data,
      c(
        "XGEO",
        "X GEO"
      ),
      required = FALSE
    )
    
    xgeo_a_col <- find_archive_column(
      archive_data,
      c(
        "XGEO_A",
        "XGEOA",
        "XGEO A"
      ),
      required = FALSE
    )
    
    xgeo_c_col <- find_archive_column(
      archive_data,
      c(
        "XGEO_C",
        "XGEOC",
        "XGEO C"
      ),
      required = FALSE
    )
    
    to_numeric <- function(column_name) {
      
      if (is.null(column_name)) {
        return(
          rep(
            NA_real_,
            nrow(archive_data)
          )
        )
      }
      
      readr::parse_number(
        as.character(
          archive_data[[column_name]]
        )
      )
    }
    
    xgeo_values <- if (!is.null(xgeo_col)) {
      
      to_numeric(
        xgeo_col
      )
      
    } else if (
      !is.null(xgeo_a_col) &&
      !is.null(xgeo_c_col)
    ) {
      
      to_numeric(
        xgeo_a_col
      ) -
        to_numeric(
          xgeo_c_col
        )
      
    } else {
      
      # The archive CSV does not contain XGEO. It will be downloaded
      # from the two USGS Georgiana Slough gauges after DATE and
      # DATA_TYPE have been parsed.
      rep(
        NA_real_,
        nrow(archive_data)
      )
    }
    
    parsed_dates <- parse_archive_dates(
      archive_data[[date_col]]
    )
    
    # Some archive CSVs do not carry a usable `data type` header even
    # though the file includes a Forecast start date above the data table.
    # When the data-type column is absent, infer Measured versus Forecast
    # from that metadata date.
    forecast_start_date <- as.Date(NA)
    
    if (is.null(type_col) && header_row > 1) {
      
      metadata <- raw[
        seq_len(header_row - 1),
        ,
        drop = FALSE
      ]
      
      metadata_matrix <- as.matrix(metadata)
      metadata_text <- as.character(metadata_matrix)
      metadata_norm <- normalize_archive_name(metadata_text)
      
      label_positions <- which(
        grepl(
          "FORECASTSTARTDATE",
          metadata_norm,
          fixed = TRUE
        )
      )
      
      if (length(label_positions) > 0) {
        
        label_pos <- label_positions[1]
        candidate_positions <- seq.int(
          label_pos + 1,
          min(length(metadata_text), label_pos + ncol(metadata_matrix) * 3)
        )
        
        candidate_dates <- parse_archive_dates(
          metadata_text[candidate_positions]
        )
        
        candidate_dates <- candidate_dates[
          !is.na(candidate_dates)
        ]
        
        if (length(candidate_dates) > 0) {
          forecast_start_date <- candidate_dates[1]
        }
      }
    }
    
    data_type_values <- if (!is.null(type_col)) {
      
      trimws(
        as.character(
          archive_data[[type_col]]
        )
      )
      
    } else if (!is.na(forecast_start_date)) {
      
      ifelse(
        parsed_dates >= forecast_start_date,
        "Forecast",
        "Measured"
      )
      
    } else {
      
      # Final fallback based on the archive request convention shown in
      # these files: 31 historical days followed by 14 forecast days.
      row_count <- length(parsed_dates)
      forecast_count <- min(14, row_count)
      
      c(
        rep("Measured", row_count - forecast_count),
        rep("Forecast", forecast_count)
      )
    }
    
    result <- tibble::tibble(
      DATE = parsed_dates,
      
      DATA_TYPE = data_type_values,
      
      # CCF and TPP are stored as acre-feet per day in the archive
      # files. Convert AF/day to cubic feet per second.
      CCF = to_numeric(
        ccf_col
      ) * 43560 / 86400,
      
      TPP = to_numeric(
        tpp_col
      ) * 43560 / 86400,
      
      VER = to_numeric(
        ver_col
      ),
      
      SAC = to_numeric(
        sac_col
      ),
      
      MOKE = to_numeric(
        moke_col
      ),
      
      CAL = to_numeric(
        cal_col
      ),
      
      COS = to_numeric(
        cos_col
      ),
      
      SACWEIR = if (is.null(sacweir_col)) {
        0
      } else {
        to_numeric(sacweir_col)
      },
      
      FREWEIR = if (is.null(freweir_col)) {
        0
      } else {
        to_numeric(freweir_col)
      },
      
      DCC = if (is.null(dcc_col)) {
        # Missing gate-status column is unknown, never automatically Closed.
        # The Weekly UI can request an explicit user assumption if needed.
        NA_real_
      } else {
        raw_dcc <- trimws(toupper(as.character(archive_data[[dcc_col]])))
        numeric_dcc <- suppressWarnings(as.numeric(gsub(",", "", raw_dcc, fixed=TRUE)))
        numeric_dcc[raw_dcc %in% c("OPEN", "OPENED")] <- 1
        numeric_dcc[raw_dcc %in% c("CLOSED", "CLOSE")] <- 0
        numeric_dcc[!is.finite(numeric_dcc) | !numeric_dcc %in% c(0, 1)] <- NA_real_
        numeric_dcc
      },
      DCC_STATUS_AVAILABLE = !is.null(dcc_col),
      
      XGEO = xgeo_values
    ) %>%
      mutate(
        DATA_TYPE = case_when(
          grepl(
            "MEAS",
            DATA_TYPE,
            ignore.case = TRUE
          ) ~ "Measured",
          
          grepl(
            "FORE",
            DATA_TYPE,
            ignore.case = TRUE
          ) ~ "Forecast",
          
          TRUE ~ DATA_TYPE
        ),
        
        EXP = CCF + TPP,
        EAST = MOKE + CAL + COS,
        YOL = SACWEIR + FREWEIR
      ) %>%
      filter(
        !is.na(DATE),
        DATA_TYPE %in% c(
          "Measured",
          "Forecast"
        )
      ) %>%
      arrange(DATE)
    
    # If XGEO was not present in the archive CSV, download daily
    # values for all measured dates and calculate XGEO = A - C.
    if (isTRUE(require_xgeo) && all(is.na(result$XGEO))) {
      
      measured_dates <- result %>%
        filter(
          DATA_TYPE == "Measured"
        ) %>%
        pull(
          DATE
        )
      
      downloaded_xgeo <- get_archive_xgeo(
        file_path = file_path,
        required_dates = measured_dates
      )
      
      result <- result %>%
        left_join(
          downloaded_xgeo %>%
            select(
              DATE,
              XGEO_DOWNLOADED = XGEO
            ),
          by = "DATE"
        ) %>%
        mutate(
          XGEO = ifelse(
            DATA_TYPE == "Measured",
            XGEO_DOWNLOADED,
            XGEO
          )
        ) %>%
        select(
          -XGEO_DOWNLOADED
        )
    }
    
    latest_measured_xgeo <- result %>%
      filter(
        DATA_TYPE == "Measured",
        !is.na(XGEO)
      ) %>%
      arrange(DATE) %>%
      slice_tail(n = 1) %>%
      pull(XGEO)
    
    if (
      length(latest_measured_xgeo) == 1 &&
      !is.na(latest_measured_xgeo)
    ) {
      
      result <- result %>%
        mutate(
          XGEO = ifelse(
            DATA_TYPE == "Forecast",
            latest_measured_xgeo,
            XGEO
          )
        )
    }
    
    validate(
      need(
        any(
          result$DATA_TYPE == "Measured"
        ),
        "The archive CSV contains no measured rows."
      ),
      
      need(!isTRUE(require_xgeo) || any(!is.na(result$XGEO)),
           "No XGEO values were found or downloaded. Check internet connection or XGEO cache.")
    )
    
    validate(
      need(
        nrow(result) > 0,
        paste0(
          "No measured or forecast records were parsed from ",
          basename(file_path),
          "."
        )
      ),
      
      need(
        all(
          c("EXP", "VER", "SAC", "EAST") %in% names(result)
        ),
        "The archive data could not be converted to the emulator input naming convention."
      )
    )
    
    result
  }
  
  
  mean_archive_inputs <- function(data) {
    
    data %>%
      summarise(
        EXP = mean(
          EXP,
          na.rm = TRUE
        ),
        VER = mean(
          VER,
          na.rm = TRUE
        ),
        SAC = mean(
          SAC,
          na.rm = TRUE
        ),
        EAST = mean(
          EAST,
          na.rm = TRUE
        ),
        XGEO = mean(
          XGEO,
          na.rm = TRUE
        ),
        YOL = mean(
          YOL,
          na.rm = TRUE
        ),
        MOKE = mean(
          MOKE,
          na.rm = TRUE
        ),
        DCC = fmt_int(
          mean(
            DCC,
            na.rm = TRUE
          )
        )
      )
  }
  
  
  rolling_measured_7day_inputs <- function(data) {
    
    measured <- data %>%
      filter(
        DATA_TYPE == "Measured"
      ) %>%
      arrange(DATE)
    
    validate(
      need(
        nrow(measured) >= 13,
        paste(
          "At least 13 measured rows are required to create",
          "seven rolling 7-day windows."
        )
      )
    )
    
    end_dates <- tail(
      measured$DATE,
      7
    )
    
    bind_rows(
      lapply(
        end_dates,
        function(end_date) {
          
          window_data <- measured %>%
            filter(
              DATE <= end_date
            ) %>%
            slice_tail(
              n = 7
            )
          
          mean_archive_inputs(
            window_data
          ) %>%
            mutate(
              Window_Start_Date = min(
                window_data$DATE
              ),
              Window_End_Date = max(
                window_data$DATE
              ),
              Window_Days = nrow(
                window_data
              )
            )
        }
      )
    )
  }
  
  
  weekly_measured_7day_inputs <- function(data) {

    measured <- data %>%
      filter(DATA_TYPE == "Measured") %>%
      arrange(DATE) %>%
      slice_tail(n = 7)

    validate(
      need(
        nrow(measured) >= 7,
        "At least seven measured rows are required for the selected archive week."
      )
    )

    latest_dcc <- measured %>%
      filter(!is.na(DCC)) %>%
      slice_tail(n = 1) %>%
      pull(DCC)

    if (length(latest_dcc) == 0) latest_dcc <- 0

    mean_archive_inputs(measured) %>%
      mutate(
        DCC = as.numeric(latest_dcc[1]),
        Window_Start_Date = min(measured$DATE),
        Window_End_Date = max(measured$DATE),
        Window_Days = nrow(measured)
      )
  }


  measured_30day_inputs <- function(data) {
    
    measured <- data %>%
      filter(
        DATA_TYPE == "Measured"
      ) %>%
      arrange(DATE) %>%
      slice_tail(
        n = 30
      )
    
    validate(
      need(
        nrow(measured) >= 30,
        "At least 30 measured rows are required."
      )
    )
    
    latest_dcc <- measured %>%
      filter(
        !is.na(DCC)
      ) %>%
      slice_tail(
        n = 1
      ) %>%
      pull(
        DCC
      )

    if (length(latest_dcc) == 0) {
      latest_dcc <- 0
    }

    mean_archive_inputs(
      measured
    ) %>%
      mutate(
        # DCC is a daily status: 0 = closed and 1 = open.
        # Use the status recorded on the latest measured date.
        DCC = as.numeric(
          latest_dcc[1]
        ),
        Window_Start_Date = min(
          measured$DATE
        ),
        Window_End_Date = max(
          measured$DATE
        ),
        Window_Days = nrow(
          measured
        )
      )
  }
  
  
  forecast_7day_inputs <- function(data) {
    
    forecast <- data %>%
      filter(
        DATA_TYPE == "Forecast"
      ) %>%
      arrange(DATE) %>%
      slice_head(
        n = 7
      )
    
    validate(
      need(
        nrow(forecast) >= 7,
        "At least seven forecast rows are required."
      )
    )
    
    mean_archive_inputs(
      forecast
    ) %>%
      mutate(
        Window_Start_Date = min(
          forecast$DATE
        ),
        Window_End_Date = max(
          forecast$DATE
        ),
        Window_Days = nrow(
          forecast
        )
      )
  }
  
  
  # ---------------------------------------------------------------
  # WEEKLY ASSESSMENTS ONLY — new five-model CCWD public release.
  # Historical inputs and displayed archive metadata are retained.
  # ---------------------------------------------------------------
  weekly_qgcd_for_dates <- function(dates) {
    dates <- as.Date(dates)
    validate(need(length(dates) > 0L && all(!is.na(dates)),
                  "QGCD requires valid assessment dates."))
    # The same workbook drives every measured window and OMRI forecast window.
    ccwd_qgcd_for_dates(dates)
  }

  # Use one forecast QPREC series per archive assessment, independent of
  # OMRI scenario. A verified archived CNRFC QPF takes priority. If the QPF
  # cannot be retrieved or validated, carry the last MEASURED SCKC1-derived
  # daily runoff rate through the forecast week. This is a PERSISTENCE
  # ASSUMPTION, never a claim that historical forecast rainfall was measured.
  weekly_qprec_cache <- new.env(parent=emptyenv())
  weekly_qprec_for_measured_date <- function(last_measured_date, forecast=FALSE) {
    last_measured_date <- as.Date(last_measured_date)
    validate(need(!is.na(last_measured_date), "Invalid measured QPREC cutoff"))
    function_for_dates <- function(dates) {
      dates <- as.Date(dates)
      if (!length(dates) || anyNA(dates)) stop("QPREC: invalid assessment dates")
      key <- paste(last_measured_date,forecast,min(dates),max(dates),sep="|")
      if (!exists(key,envir=weekly_qprec_cache,inherits=FALSE)) {
        item <- tryCatch({
          # An entire 5-day observed window through the assessment cutoff is
          # required even for a fully forecast seven-day period.
          first_observation <- min(min(dates)-4L,last_measured_date-4L)
          last_observation <- min(max(dates),last_measured_date)
          rain <- ccwd_sckc1_daily(first_observation,last_observation)
          src <- "SCKC1 observed daily precipitation"
          url <- attr(rain,"source_url")
          estimated_dates <- if ("estimated" %in% names(rain))
            as.Date(rain$date[which(rain$estimated %in% TRUE)]) else as.Date(character())
          verified_snapshot_dates <- attr(rain,"verified_snapshot_dates")
          if (length(verified_snapshot_dates))
            src <- paste0(src,"; verified SCKC1 observation records used for ",
                          paste(as.character(verified_snapshot_dates),collapse=", "))
          forecast_method <- if (forecast) "CNRFC_ARCHIVE_QPF" else "OBSERVED_SCKC1"
          forecast_issue <- NA_character_
          forecast_problem <- NULL
          if (!forecast) {
            calc <- ccwd_qprec_from_daily(rain,dates)
          } else {
            fc_dates <- dates[dates > last_measured_date]
            if (!length(fc_dates)) stop("QPREC: forecast window has no forecast dates")
            fc <- tryCatch({
              qpf <- ccwd_cnrfc_daily(last_measured_date+1L,
                                       fc_dates,last_measured_date)
              combined <- rbind(rain[,c("date","precip_in")],
                                qpf[,c("date","precip_in")])
              result <- ccwd_qprec_from_daily(combined,dates)
              list(qpf=qpf,calc=result)
            },error=function(e)e)
            if (!inherits(fc,"error")) {
              # Verified CNRFC QPF, plus the preceding observed days.
              forecast_issue <- attr(fc$qpf,"issue_utc")
              forecast_issue <- if (length(forecast_issue)) as.character(forecast_issue)[1] else NA_character_
              url <- attr(fc$qpf,"source_url")
              carried <- attr(fc$qpf,"carried_days")
              src <- paste0("SCKC1 measured precipitation + archived CNRFC QPF",
                            if (length(carried)) paste0("; CNRFC last-day rainfall carried forward for ",
                                   paste(as.character(carried),collapse=", ")) else "")
              calc <- fc$calc
            } else {
              # Historical forecast archive is missing or invalid. Keep model
              # runnable using ONLY verified rainfall up to the cutoff.
              # A daily runoff rate in TAF/day is held constant, then scaled
              # separately for each forecast date's calendar-month length.
              forecast_problem <- conditionMessage(fc)
              measured_day <- ccwd_qprec_from_daily(rain,last_measured_date)
              last_rate <- measured_day$qprec_taf_day[1]
              if (length(last_rate)!=1L || !is.finite(last_rate))
                stop("QPREC: cannot estimate forecast runoff because the last measured five-day SCKC1 window is incomplete. ",forecast_problem)
              calc <- data.frame(
                date=dates,
                qprec_taf_day=rep(last_rate,length(dates)),
                qprec_taf_month=last_rate*ccwd_qprec_days_in_month(dates)
              )
              forecast_method <- "PERSIST_LAST_MEASURED_DAILY_RUNOFF"
              src <- "SCKC1 measured QPREC held constant as a forecast assumption (CNRFC archived QPF unavailable)"
              url <- attr(rain,"source_url")
              message("CCWD Weekly forecast QPREC: no validated historical CNRFC QPF for cutoff ",
                      as.character(last_measured_date),". Holding measured SCKC1 daily runoff rate. CNRFC detail: ",
                      forecast_problem)
            }
          }
          list(value=mean(calc$qprec_taf_month),source=src,
               through=if (forecast) max(dates) else min(max(dates),last_measured_date),
               window_start=min(dates),source_url=url,
               estimated_dates=estimated_dates,daily=calc,
               forecast_method=forecast_method,
               forecast_issue=forecast_issue,
               forecast_problem=forecast_problem)
        },error=function(e)e)
        assign(key,item,envir=weekly_qprec_cache)
      }
      item <- get(key,envir=weekly_qprec_cache,inherits=FALSE)
      if (inherits(item,"error")) {
        rm(list=key,envir=weekly_qprec_cache) # retry after transient download failure
        stop(conditionMessage(item),call.=FALSE)
      }
      item
    }
    default_dates <- seq(last_measured_date-6L,last_measured_date,by="day")
    result <- if (forecast) {
      list(value=NA_real_,source="SCKC1 observed / CNRFC QPF",
           through=last_measured_date,window_start=last_measured_date,
           source_url=CCWD_CNRFC_ARCHIVE)
    } else function_for_dates(default_dates)
    result$for_dates <- function_for_dates
    result$forecast <- forecast
    result
  }

  # DCC is the 0/1 gate flag in the *selected archive CSV*.
  # Use the first forecast day or the last observed day, never a 7-day mean.
  # A remote GitHub CSV can differ from the copy opened locally in Excel;
  # do not manufacture a 0 when that remote file has a blank DCC cell.
  weekly_archive_gate <- function(archive_data, forecast=FALSE, dates=NULL,
                                  allow_manual_override=TRUE) {
    subset <- archive_data[
      archive_data$DATA_TYPE == if (forecast) "Forecast" else "Measured",
      , drop=FALSE]
    subset <- subset[order(subset$DATE),,drop=FALSE]
    if (forecast) {
      subset <- head(subset, 7L)
    } else if (!is.null(dates)) {
      subset <- subset[subset$DATE %in% as.Date(dates),,drop=FALSE]
    } else {
      subset <- tail(subset, 7L)
    }
    validate(need(nrow(subset) >= 7L,
      "At least seven daily archive rows are required to determine DCC status."))
    index <- if (forecast) 1L else nrow(subset)
    status <- suppressWarnings(as.numeric(subset$DCC[index]))
    available <- isTRUE(subset$DCC_STATUS_AVAILABLE[index]) &&
      length(status) == 1L && is.finite(status) && status %in% c(0,1)
    if (available) {
      result <- as.numeric(status)
      attr(result, "gate_source") <- "ARCHIVE_CSV"
      return(result)
    }

    # Only use an explicitly user-selected assumption if the CSV value is
    # truly absent/invalid.  Do not override a valid archived 0 or 1.
    chosen <- if (allow_manual_override) {
      input$forecast_weekly_gate_override
    } else NULL
    if (!is.null(chosen) && length(chosen)==1L && chosen %in% c("0","1")) {
      result <- as.numeric(chosen)
      attr(result, "gate_source") <- "USER_ASSUMPTION_MISSING_ARCHIVE_DCC"
      return(result)
    }
    day <- as.character(subset$DATE[index])
    stop(paste0(
      "DCC gate status is blank or invalid in the selected GitHub archive CSV ",
      "for ", if (forecast) "first forecast" else "last measured",
      " day (", day, "). Select an explicit Open/Closed assumption in ",
      "the Weekly Assessments panel, or add the correct 0/1 data to ",
      "the archived GitHub CSV."), call.=FALSE)
  }

  # Returns mean across the requested dates. For gate-open measured periods,
  # prefer CDEC DLC observations on each date, and fill ONLY missing dates with
  # the measured SAC value on those dates from the selected archive CSV.
  # Forecast windows always use regression with the forecast SAC series.
  weekly_dcc_flow <- function(archive_data, dates, gate, forecast=FALSE) {
    if (gate == 0) return(0)
    dates <- as.Date(dates)
    validate(need(length(dates)>0L && all(!is.na(dates)),
                  'DCC flow dates are unavailable.'))
    kind <- if (forecast) 'Forecast' else 'Measured'
    rows <- archive_data[archive_data$DATA_TYPE == kind &
                         archive_data$DATE %in% dates,,drop=FALSE]
    rows <- rows[order(rows$DATE),,drop=FALSE]
    idx <- match(dates, rows$DATE)
    validate(need(all(!is.na(idx)) && all(is.finite(rows$SAC[idx])),
      paste0('Cannot estimate DCC: ',kind,' Freeport SAC flow is missing for the selected dates.')))
    sac_daily <- as.numeric(rows$SAC[idx])
    estimate <- 0.1846*sac_daily + 306.52
    if (forecast) {
      out <- mean(estimate)
      attr(out,'dcc_source') <- 'Forecast regression (forecast SAC at Freeport)'
      return(out)
    }
    observed <- tryCatch(read_historical_dcc_daily(max(dates),
       lookback_days=max(14L,as.integer(max(dates)-min(dates))+1L)),
       error=function(e)e)
    flow <- estimate
    covered <- rep(FALSE,length(dates))
    if (!inherits(observed,'error') && nrow(observed)) {
      ix <- match(dates, observed$Date)
      covered <- !is.na(ix)
      covered[covered] <- is.finite(observed$Value[ix[covered]]) &
                           observed$Hours[ix[covered]] >= 1L
      flow[covered] <- observed$Value[ix[covered]]
    }
    if (any(!covered)) message('CCWD DCC: CDEC observations unavailable for ',
      paste(as.character(dates[!covered]),collapse=', '),
      '; using measured SAC regression for ',sum(!covered),' date(s).')
    out <- mean(flow)
    attr(out,'dcc_source') <- if (all(covered)) 'Measured CDEC DLC daily means' else
      if (any(covered)) 'CDEC measured with missing dates estimated from measured archive SAC' else
        'Estimated from measured archive SAC; CDEC DLC unavailable'
    attr(out,'dcc_missing_dates') <- dates[!covered]
    out
  }

  weekly_add_window_info <- function(df, window, archive_date, scenario) {
    n <- nrow(df)
    if (n == 0L) return(df)
    df$Input_Method <- rep("weekly_archive", n)
    df$Archive_Date <- rep(as.character(archive_date)[1], n)
    df$OMRI_Scenario <- rep(as.character(scenario)[1], n)
    df$Window_Start_Date <- rep(as.Date(window$Window_Start_Date[1]), n)
    df$Window_End_Date <- rep(as.Date(window$Window_End_Date[1]), n)
    df$Window_Days <- rep(as.integer(window$Window_Days[1]), n)
    df
  }

  weekly_new_ptm_windows <- function(windows, archive_data, condition,
                                     scenario_name, archive_date, scenario,
                                     qprec_ref, forecast=FALSE) {
    # Every LightGBM prediction is one row per DSM2 node.  Construct each
    # predictor and each result column at exactly that length; never rely on
    # implicit recycling of a one-row window into a multi-node result.
    ref <- reference_data()
    validate(need(nrow(windows) > 0L,
                  "No seven-day archive windows are available for PTM prediction."))
    bind_rows(lapply(seq_len(nrow(windows)), function(i) {
      w <- windows[i, , drop=FALSE]
      dates <- seq(as.Date(w$Window_Start_Date),
                   as.Date(w$Window_End_Date), by="day")
      gate <- weekly_archive_gate(archive_data, forecast=forecast, dates=dates)
      qgcd <- weekly_qgcd_for_dates(dates)
      this_qprec <- qprec_ref$for_dates(dates)
      qprec <- this_qprec$value
      dcc_flow <- weekly_dcc_flow(archive_data, dates, gate,
                                   forecast=forecast)

      predict_one <- function(key, label, nodes) {
        tryCatch({
          node_ids <- as.character(nodes)
          n <- length(node_ids)
          if (n == 0L || anyNA(node_ids)) {
            stop("The reference workbook has no valid DSM2 nodes for ", key)
          }
          # Windows are single rows. Convert each hydrologic value to an
          # explicit scalar, then repeat it once for every prediction node.
          one_number <- function(x, name) {
            val <- suppressWarnings(as.numeric(x))
            if (length(val) != 1L || !is.finite(val)) {
              stop("Invalid ", name, " input (expected one finite number).")
            }
            val
          }
          exp_value  <- one_number(w$EXP, "EXP")
          ver_value  <- one_number(w$VER, "VER")
          sac_value  <- one_number(w$SAC, "SAC")
          east_value <- one_number(w$EAST, "EAST")
          qgcd_value <- one_number(qgcd, "QGCD")
          qprec_value <- one_number(qprec, "QPREC")
          dcc_value  <- one_number(dcc_flow, "DCC flow")
          gate_value <- one_number(gate, "DCC gate status")
          numeric_nodes <- suppressWarnings(as.numeric(node_ids))
          if (length(numeric_nodes) != n || any(!is.finite(numeric_nodes))) {
            stop("DSM2 node identifiers must be numeric for ", key)
          }

          columns <- list(
            numeric_nodes,
            rep(exp_value, n), rep(ver_value, n), rep(sac_value, n),
            rep(qgcd_value, n), rep(qprec_value, n), rep(east_value, n)
          )
          if (identical(key, "PTM30")) {
            columns[[length(columns) + 1L]] <- rep(dcc_value, n)
          }
          features <- do.call(cbind, columns)
          storage.mode(features) <- "double"
          if (nrow(features) != n) {
            stop("PTM feature row count does not match node count.")
          }
          raw <- as.numeric(new_predict(key, features))
          if (length(raw) != n) {
            stop("Model returned ", length(raw), " predictions for ", n,
                 " DSM2 nodes.")
          }
          # Make the result columns the same explicit length as raw/nodes.
          result <- data.frame(
            Condition=rep(condition, n),
            Input_Method=rep("weekly_archive", n),
            Archive_Date=rep(archive_date, n),
            OMRI_Scenario=rep(scenario, n),
            Scenario_Name=rep(scenario_name, n),
            Model=rep(label, n),
            Window_Start_Date=rep(as.Date(w$Window_Start_Date), n),
            Window_End_Date=rep(as.Date(w$Window_End_Date), n),
            Window_Days=rep(as.numeric(w$Window_Days), n),
            DSM2_Node=node_ids,
            Prediction_Raw=raw,
            Prediction_Final=bound_percent(raw),
            Output_Unit=rep("Percent", n),
            EXP=rep(exp_value, n), VER=rep(ver_value, n),
            SAC=rep(sac_value, n), EAST=rep(east_value, n),
            QGCD=rep(qgcd_value, n), QPREC=rep(qprec_value, n),
            DCC_flow=rep(dcc_value, n), DCC=rep(gate_value, n),
            QPREC_Through_Date=rep(as.character(this_qprec$through), n),
            QPREC_Station=rep(as.character(this_qprec$source), n),
            stringsAsFactors=FALSE
          )
          if (nrow(result) != n) {
            stop("PTM output row count does not match DSM2 nodes.")
          }
          left_join(result, ref$node_meta, by="DSM2_Node")
        }, error=function(e) {
          stop(paste0("Weekly ", key, " prediction construction failed for ",
                      as.character(w$Window_Start_Date), " through ",
                      as.character(w$Window_End_Date), ": ",
                      conditionMessage(e)), call.=FALSE)
        })
      }
      bind_rows(
        predict_one("PTM7", "PTM Emulator 7-Day Entrainment", ref$nodes_7d),
        predict_one("PTM30", "PTM Emulator 30-Day Entrainment", ref$nodes_30d)
      )
    }))
  }

  weekly_new_eh_windows <- function(windows, condition,scenario_name,
                                    archive_date,scenario,risk,qprec_ref) {
    bind_rows(lapply(seq_len(nrow(windows)),function(i) {
      w <- windows[i,,drop=FALSE]
      dates <- seq(as.Date(w$Window_Start_Date),as.Date(w$Window_End_Date),by="day")
      qgcd <- weekly_qgcd_for_dates(dates)
      this_qprec <- qprec_ref$for_dates(dates)
      result <- make_new_eh_result(condition,scenario_name,risk,w$EXP,w$VER,
                                    w$SAC,qgcd,this_qprec$value,w$EAST)
      result <- weekly_add_window_info(result,w,archive_date,scenario)
      result$QPREC_Through_Date <- as.character(this_qprec$through)
      result$QPREC_Station <- this_qprec$source
      result
    }))
  }

  weekly_new_eco <- function(archive_data,window,condition,scenario_name,
                             archive_date,scenario,forecast=FALSE) {
    dates <- seq(as.Date(window$Window_Start_Date),as.Date(window$Window_End_Date),by="day")
    gate <- weekly_archive_gate(archive_data,forecast=forecast,dates=dates)
    result <- make_new_eco_results(condition,scenario_name,window$SAC,
              window$YOL,window$MOKE,gate)
    weekly_add_window_info(result,window,archive_date,scenario)
  }

  predict_ptm_from_windows <- function(
    windows,
    model,
    model_name,
    nodes,
    condition,
    scenario_name,
    archive_date,
    omri_scenario
  ) {
    
    ref <- reference_data()
    
    bind_rows(
      lapply(
        seq_len(
          nrow(windows)
        ),
        function(i) {
          
          window <- windows[
            i,
            ,
            drop = FALSE
          ]
          
          model_input <- data.frame(
            DSM2_Node = as.numeric(
              nodes
            ),
            EXP = window$EXP,
            VER = window$VER,
            SAC = window$SAC,
            EAST = window$EAST,
            XGEO = window$XGEO
          )
          
          raw <- predict(
            model,
            as.matrix(
              model_input
            )
          )
          
          data.frame(
            Condition = condition,
            Input_Method = "folder",
            Archive_Date = archive_date,
            OMRI_Scenario = omri_scenario,
            Scenario_Name = scenario_name,
            Model = model_name,
            Window_Start_Date = window$Window_Start_Date,
            Window_End_Date = window$Window_End_Date,
            Window_Days = window$Window_Days,
            DSM2_Node = as.character(
              nodes
            ),
            Prediction_Raw = raw,
            Prediction_Final = bound_percent(
              raw
            ),
            Output_Unit = "Percent",
            EXP = window$EXP,
            VER = window$VER,
            SAC = window$SAC,
            EAST = window$EAST,
            XGEO = window$XGEO,
            stringsAsFactors = FALSE
          ) %>%
            left_join(
              ref$node_meta,
              by = "DSM2_Node"
            )
        }
      )
    )
  }
  
  
  make_archive_ptm_current <- function(
    archive_date,
    scenario_name
  ) {
    
    source_file <- archive_csv_files(
      archive_date
    )[1]
    
    validate(
      need(
        length(source_file) > 0,
        paste0(
          "No OMRI CSV files were found in ",
          archive_date,
          "."
        )
      )
    )
    
    archive_data <- read_archive_csv(
      source_file
    )
    
    ref <- reference_data()
    
    rolling_7 <- rolling_measured_7day_inputs(
      archive_data
    )
    
    measured_30 <- measured_30day_inputs(
      archive_data
    )
    
    bind_rows(
      predict_ptm_from_windows(
        rolling_7,
        ptm7_model,
        "PTM Emulator 7-Day Entrainment",
        ref$nodes_7d,
        "Observed Conditions",
        scenario_name,
        archive_date,
        "Measured"
      ),
      
      predict_ptm_from_windows(
        measured_30,
        ptm30_model,
        "PTM Emulator 30-Day Entrainment",
        ref$nodes_30d,
        "Observed Conditions",
        scenario_name,
        archive_date,
        "Measured"
      )
    )
  }
  
  
  make_archive_ptm_forecast <- function(
    archive_date,
    omri_scenario,
    scenario_name
  ) {
    
    source_file <- archive_file_for_scenario(
      archive_date,
      omri_scenario
    )
    
    archive_data <- read_archive_csv(
      source_file
    )
    
    forecast_7 <- forecast_7day_inputs(
      archive_data
    )
    
    ref <- reference_data()
    
    predict_ptm_from_windows(
      forecast_7,
      ptm7_model,
      "PTM Emulator 7-Day Entrainment",
      ref$nodes_7d,
      "Forecast Conditions",
      scenario_name,
      archive_date,
      omri_scenario
    )
  }
  
  
  make_weekly_ptm_observed <- function(archive_date,scenario_name) {
    file <- archive_csv_files(archive_date)[1]
    validate(need(length(file)>0L,"No measured OMRI CSV found."))
    data <- read_archive_csv(file, require_xgeo=FALSE)
    windows <- rolling_measured_7day_inputs(data)
    last_measured <- max(data$DATE[data$DATA_TYPE=="Measured"])
    qprec <- weekly_qprec_for_measured_date(last_measured)
    weekly_new_ptm_windows(windows,data,"Observed Conditions",scenario_name,
       archive_date,"Measured",qprec,forecast=FALSE)
  }

  make_weekly_ptm_forecast <- function(archive_date,omri_scenario,scenario_name) {
    file <- archive_file_for_scenario(archive_date,omri_scenario)
    data <- read_archive_csv(file, require_xgeo=FALSE)
    windows <- forecast_7day_inputs(data)
    validate(need(any(data$DATA_TYPE=="Measured"),
                  "Forecast archive does not include a measured reference date for QPREC."))
    last_measured <- max(data$DATE[data$DATA_TYPE=="Measured"])
    qprec <- weekly_qprec_for_measured_date(last_measured,forecast=TRUE)
    weekly_new_ptm_windows(windows,data,"Forecast Conditions",scenario_name,
       archive_date,omri_scenario,qprec,forecast=TRUE)
  }

  make_weekly_eco_observed <- function(archive_date,scenario_name) {
    file <- archive_csv_files(archive_date)[1]
    data <- read_archive_csv(file, require_xgeo=FALSE)
    window <- weekly_measured_7day_inputs(data)
    weekly_new_eco(data,window,"Observed Conditions",scenario_name,
                   archive_date,"Measured",forecast=FALSE)
  }

  make_weekly_eco_forecast <- function(archive_date,omri_scenario,scenario_name) {
    file <- archive_file_for_scenario(archive_date,omri_scenario)
    data <- read_archive_csv(file, require_xgeo=FALSE)
    window <- forecast_7day_inputs(data)
    weekly_new_eco(data,window,"Forecast Conditions",scenario_name,
                   archive_date,omri_scenario,forecast=TRUE)
  }

  make_archive_eco_current <- function(
    archive_date,
    scenario_name
  ) {
    
    source_file <- archive_csv_files(
      archive_date
    )[1]
    
    archive_data <- read_archive_csv(
      source_file
    )
    
    inputs <- measured_30day_inputs(
      archive_data
    )
    
    result <- make_eco_results(
      condition = "Observed Conditions",
      scenario_name = scenario_name,
      sac = inputs$SAC,
      yol = inputs$YOL,
      moke = inputs$MOKE,
      dcc = inputs$DCC
    )
    
    result %>%
      mutate(
        Input_Method = "folder",
        Archive_Date = archive_date,
        OMRI_Scenario = "Measured",
        Window_Start_Date = inputs$Window_Start_Date,
        Window_End_Date = inputs$Window_End_Date,
        Window_Days = inputs$Window_Days
      )
  }
  
  
  predict_eh_from_windows <- function(
    windows,
    condition,
    scenario_name,
    archive_date,
    omri_scenario,
    risk
  ) {
    
    bind_rows(
      lapply(
        seq_len(
          nrow(windows)
        ),
        function(i) {
          
          window <- windows[
            i,
            ,
            drop = FALSE
          ]
          
          result <- make_eh_result(
            condition = condition,
            scenario_name = scenario_name,
            exp = window$EXP,
            ver = window$VER,
            east = window$EAST,
            xgeo = window$XGEO,
            risk = risk
          )
          
          result %>%
            mutate(
              Input_Method = "folder",
              Archive_Date = archive_date,
              OMRI_Scenario = omri_scenario,
              Window_Start_Date = window$Window_Start_Date,
              Window_End_Date = window$Window_End_Date,
              Window_Days = window$Window_Days
            )
        }
      )
    )
  }
  
  
  make_archive_eh_current <- function(
    archive_date,
    scenario_name,
    risk
  ) {
    
    source_file <- archive_csv_files(
      archive_date
    )[1]
    
    archive_data <- read_archive_csv(
      source_file
    )
    
    rolling_7 <- rolling_measured_7day_inputs(
      archive_data
    )
    
    predict_eh_from_windows(
      rolling_7,
      "Observed Conditions",
      scenario_name,
      archive_date,
      "Measured",
      risk
    )
  }
  
  
  make_archive_eh_forecast <- function(
    archive_date,
    omri_scenario,
    scenario_name,
    risk
  ) {
    
    source_file <- archive_file_for_scenario(
      archive_date,
      omri_scenario
    )
    
    archive_data <- read_archive_csv(
      source_file
    )
    
    forecast_7 <- forecast_7day_inputs(
      archive_data
    )
    
    predict_eh_from_windows(
      forecast_7,
      "Forecast Conditions",
      scenario_name,
      archive_date,
      omri_scenario,
      risk
    )
  }
  
  
  make_weekly_eh_observed <- function(archive_date,scenario_name,risk) {
    file <- archive_csv_files(archive_date)[1]
    data <- read_archive_csv(file, require_xgeo=FALSE)
    windows <- rolling_measured_7day_inputs(data)
    last_measured <- max(data$DATE[data$DATA_TYPE=="Measured"])
    qprec <- weekly_qprec_for_measured_date(last_measured)
    weekly_new_eh_windows(windows,"Observed Conditions",scenario_name,
                          archive_date,"Measured",risk,qprec)
  }

  make_weekly_eh_forecast <- function(archive_date,omri_scenario,scenario_name,risk) {
    file <- archive_file_for_scenario(archive_date,omri_scenario)
    data <- read_archive_csv(file, require_xgeo=FALSE)
    windows <- forecast_7day_inputs(data)
    validate(need(any(data$DATA_TYPE=="Measured"),
                  "Forecast archive is missing the measured QPREC reference date."))
    last_measured <- max(data$DATE[data$DATA_TYPE=="Measured"])
    qprec <- weekly_qprec_for_measured_date(last_measured,forecast=TRUE)
    weekly_new_eh_windows(windows,"Forecast Conditions",scenario_name,
                          archive_date,omri_scenario,risk,qprec)
  }

  latest_result_window <- function(
    data,
    model_name
  ) {
    
    filtered <- data %>%
      filter(
        Model == model_name
      )
    
    if (
      "Window_End_Date" %in% names(filtered) &&
      any(
        !is.na(
          filtered$Window_End_Date
        )
      )
    ) {
      
      latest_date <- max(
        filtered$Window_End_Date,
        na.rm = TRUE
      )
      
      filtered <- filtered %>%
        filter(
          Window_End_Date == latest_date
        )
    }
    
    filtered
  }
  
  
  selected_result_window <- function(
    data,
    model_name,
    selected_date = NULL
  ) {
    
    filtered <- data %>%
      filter(
        Model == model_name
      )
    
    if (
      !is.null(selected_date) &&
      "Window_End_Date" %in% names(filtered) &&
      any(!is.na(filtered$Window_End_Date))
    ) {
      
      selected_date <- as.Date(
        selected_date,
        origin = "1970-01-01"
      )
      
      filtered <- filtered %>%
        mutate(
          Window_End_Date = as.Date(
            Window_End_Date,
            origin = "1970-01-01"
          )
        ) %>%
        filter(
          Window_End_Date == selected_date
        )
    }
    
    if (nrow(filtered) == 0) {
      return(
        latest_result_window(
          data,
          model_name
        )
      )
    }
    
    filtered
  }
  
  
  make_ptm_timeseries <- function(
    data,
    selected_nodes,
    title
  ) {
    
    required_columns <- c(
      "Model",
      "DSM2_Node",
      "Prediction_Final",
      "Window_End_Date"
    )
    
    if (
      is.null(data) ||
      nrow(data) == 0 ||
      !all(required_columns %in% names(data))
    ) {
      return(
        plotly::plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = paste(
                  "The rolling time series is available only for",
                  "Read from Archive Folder current-condition runs."
                ),
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 15)
              )
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    plot_data <- data %>%
      filter(
        Model == "PTM Emulator 7-Day Entrainment",
        !is.na(Window_End_Date)
      )
    
    if (
      !is.null(selected_nodes) &&
      length(selected_nodes) > 0
    ) {
      plot_data <- plot_data %>%
        filter(
          DSM2_Node %in% selected_nodes
        )
    }
    
    if (nrow(plot_data) == 0) {
      return(
        plotly::plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = "No rolling PTM results are available for the selected nodes.",
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 15)
              )
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    if (!"Location" %in% names(plot_data)) {
      plot_data$Location <- NA_character_
    }
    
    plot_data <- plot_data %>%
      mutate(
        Window_End_Date = as.Date(
          Window_End_Date,
          origin = "1970-01-01"
        ),
        
        Node_Label = ifelse(
          is.na(Location) | Location == "",
          DSM2_Node,
          paste0(
            DSM2_Node,
            " - ",
            Location
          )
        ),
        
        Hover_Text = paste0(
          "<b>Window end:</b> ",
          format(
            Window_End_Date,
            "%b %d, %Y"
          ),
          "<br><b>Node:</b> ",
          Node_Label,
          "<br><b>Prediction:</b> ",
          sprintf(
            "%.0f%%",
            Prediction_Final
          )
        )
      )
    
    plot_ly(
      plot_data,
      x = ~Window_End_Date,
      y = ~Prediction_Final,
      color = ~Node_Label,
      type = "scatter",
      mode = "lines+markers",
      text = ~Hover_Text,
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>",
            title,
            "</b>"
          ),
          x = 0.02,
          xanchor = "left"
        ),
        
        xaxis = list(
          title = "<b>Rolling 7-Day Window End Date</b>",
          tickformat = "%b %d",
          automargin = TRUE
        ),
        
        yaxis = list(
          title = "<b>Predicted Entrainment (%)</b>",
          range = c(
            0,
            105
          )
        ),
        
        legend = list(
          orientation = "v"
        ),
        
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 13,
          color = "#1F1F1F"
        )
      ) %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "PTM_rolling_7day_timeseries",
          width = 1600,
          height = 900,
          scale = 2
        )
      )
  }
  
  
  make_eh_timeseries <- function(
    data,
    title
  ) {
    
    required_columns <- c(
      "Prediction_Final",
      "Window_End_Date",
      "Risk_Level_Percent"
    )
    
    if (
      is.null(data) ||
      nrow(data) == 0 ||
      !all(required_columns %in% names(data))
    ) {
      return(
        plotly::plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = paste(
                  "The rolling Event Horizon time series is available only",
                  "for Read from Archive Folder current-condition runs."
                ),
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(size = 15)
              )
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    plot_data <- data %>%
      filter(
        !is.na(Window_End_Date)
      ) %>%
      mutate(
        Window_End_Date = as.Date(
          Window_End_Date,
          origin = "1970-01-01"
        )
      ) %>%
      arrange(
        Window_End_Date
      )
    
    if (nrow(plot_data) == 0) {
      return(
        plotly::plot_ly() %>%
          layout(
            annotations = list(
              list(
                text = "No rolling Event Horizon results are available.",
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE
              )
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          ) %>%
          config(displayModeBar = FALSE)
      )
    }
    
    plot_data <- plot_data %>%
      mutate(
        Hover_Text = paste0(
          "<b>Window end:</b> ",
          format(
            Window_End_Date,
            "%b %d, %Y"
          ),
          "<br><b>Risk level:</b> ",
          Risk_Level_Percent,
          "%",
          "<br><b>Event Horizon:</b> ",
          sprintf(
            "%.0f river miles",
            Prediction_Final
          )
        )
      )
    
    plot_ly(
      plot_data,
      x = ~Window_End_Date,
      y = ~Prediction_Final,
      type = "scatter",
      mode = "lines+markers",
      line = list(
        color = "#B2182B",
        width = 3
      ),
      marker = list(
        color = "#B2182B",
        size = 9
      ),
      text = ~Hover_Text,
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste0(
            "<b>",
            title,
            "</b>"
          ),
          x = 0.02,
          xanchor = "left"
        ),
        
        xaxis = list(
          title = "<b>Rolling 7-Day Window End Date</b>",
          tickformat = "%b %d"
        ),
        
        yaxis = list(
          title = "<b>Predicted Event Horizon (River Miles)</b>"
        ),
        
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 14,
          color = "#1F1F1F"
        )
      ) %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "Event_Horizon_rolling_7day_timeseries",
          width = 1500,
          height = 850,
          scale = 2
        )
      )
  }
  
  
  make_ptm_bar <- function(df, title, show_plot_title = TRUE) {
    validate(need(nrow(df) > 0, "Run the model to display results."))
    
    plot_df <- df %>%
      mutate(
        DSM2_Node_Num = suppressWarnings(as.numeric(DSM2_Node)),
        node_label = as.character(DSM2_Node),
        value_label = sprintf("%.0f%%", Prediction_Final),
        hover_text = paste0(
          "<b>DSM2 Node:</b> ", DSM2_Node,
          "<br><b>Location:</b> ", Location,
          "<br><b>Region:</b> ", Region,
          "<br><b>Predicted entrainment:</b> ",
          sprintf("%.0f%%", Prediction_Final)
        )
      ) %>%
      arrange(DSM2_Node_Num, DSM2_Node)
    
    # Reverse only for display so the smallest node remains at the top.
    plot_df$node_label <- factor(
      plot_df$node_label,
      levels = rev(plot_df$node_label)
    )
    
    x_max <- max(
      105,
      ceiling(max(plot_df$Prediction_Final, na.rm = TRUE) + 12)
    )
    
    plot_ly(
      data = plot_df,
      x = ~Prediction_Final,
      y = ~node_label,
      type = "bar",
      orientation = "h",
      text = ~value_label,
      textposition = "outside",
      textfont = list(size = 14, color = "#1F1F1F"),
      hovertext = ~hover_text,
      hoverinfo = "text",
      marker = list(
        color = "#0072B2",
        line = list(color = "#003B5C", width = 0.8)
      ),
      cliponaxis = FALSE
    ) %>%
      layout(
        title = if (isTRUE(show_plot_title)) {
          list(
            text = paste0(
              "<b>", title, "</b>",
              "<br><span style='font-size:13px;'>",
              "Fixed numeric DSM2-node order; values are percent entrainment.",
              "</span>"
            ),
            x = 0.02,
            xanchor = "left",
            font = list(size = 20, color = "#1F1F1F")
          )
        } else {
          NULL
        },
        xaxis = list(
          title = list(
            text = "<b>Predicted Entrainment (%)</b>",
            font = list(
              family = "Arial, Segoe UI, sans-serif",
              size = 14,
              color = "#1F1F1F"
            )
          ),
          range = c(0, x_max),
          tickfont = list(
            family = "Arial, Segoe UI, sans-serif",
            size = 12,
            color = "#1F1F1F"
          ),
          gridcolor = "#D9D9D9",
          zerolinecolor = "#666666",
          showline = TRUE,
          linecolor = "#666666"
        ),
        yaxis = list(
          title = list(
            text = "<b>DSM2 Node</b>",
            font = list(
              family = "Arial, Segoe UI, sans-serif",
              size = 14,
              color = "#1F1F1F"
            )
          ),
          tickfont = list(
            family = "Arial, Segoe UI, sans-serif",
            size = 12,
            color = "#1F1F1F"
          ),
          automargin = TRUE,
          showgrid = FALSE
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 14,
          color = "#1F1F1F"
        ),
        margin = list(l = 90, r = 95, t = 100, b = 75),
        showlegend = FALSE,
        margin = list(
          l = 90,
          r = 95,
          t = if (isTRUE(show_plot_title)) 100 else 35,
          b = 35
        )
#        annotations = list(
#          list(
 #          text = "Note: Predictions are emulator outputs rounded to two decimal places.",
  #          x = 0,
#            y = -0.12,
 #           xref = "paper",
#            yref = "paper",
#            showarrow = FALSE,
 #           xanchor = "left",
#            font = list(size = 12, color = "#404040")
 #         )
 #       )
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "lasso2d", "select2d", "autoScale2d",
          "toggleSpikelines"
        ),
        toImageButtonOptions = list(
          format = "png",
          filename = gsub("[^A-Za-z0-9_-]+", "_", title),
          height = 1000,
          width = 1600,
          scale = 2
        )
      )
  }
  make_entrainment_risk_surface <- function(
    nodes,
    grid_cell_size = 300,
    idp = 2,
    nmax = 6,
    maximum_distance = 300000
  ) {
    
    validate(
      need(
        inherits(nodes, "sf"),
        "PTM node data must be an sf object."
      ),
      need(
        "entrainment" %in% names(nodes),
        "PTM node data are missing the entrainment field."
      )
    )
    
    # ------------------------------------------------------------
    # 1. Prepare supported DSM2 node predictions
    # ------------------------------------------------------------
    
    nodes_valid <- nodes %>%
      dplyr::filter(
        !is.na(entrainment),
        is.finite(entrainment)
      ) %>%
      sf::st_make_valid() %>%
      sf::st_transform(26910)
    
    validate(
      need(
        nrow(nodes_valid) >= 3,
        paste0(
          "At least three valid DSM2 node predictions are ",
          "required to interpolate risk."
        )
      )
    )
    
    nodes_valid$entrainment <- pmax(
      0,
      pmin(
        100,
        as.numeric(nodes_valid$entrainment)
      )
    )
    
    # ------------------------------------------------------------
    # 2. Prepare the Delta boundary in projected coordinates
    # ------------------------------------------------------------
    
    boundary_utm <- delta_boundary %>%
      sf::st_make_valid() %>%
      sf::st_transform(26910) %>%
      sf::st_union()
    
    # ------------------------------------------------------------
    # 3. Build a regular polygon grid in EPSG:26910
    # ------------------------------------------------------------
    
    grid_polygons <- sf::st_make_grid(
      boundary_utm,
      cellsize = grid_cell_size,
      what = "polygons",
      square = TRUE
    )
    
    grid_sf <- sf::st_sf(
      grid_id = seq_along(grid_polygons),
      geometry = grid_polygons
    )
    
    # Keep cells whose centers fall inside the Delta boundary.
    grid_centers <- sf::st_centroid(
      grid_sf
    )
    
    inside_delta <- lengths(
      sf::st_intersects(
        grid_centers,
        boundary_utm
      )
    ) > 0
    
    grid_sf <- grid_sf[
      inside_delta,
      ,
      drop = FALSE
    ]
    
    grid_centers <- grid_centers[
      inside_delta,
      ,
      drop = FALSE
    ]
    
    validate(
      need(
        nrow(grid_sf) > 0,
        "The Delta interpolation grid could not be created."
      )
    )
    
    # ------------------------------------------------------------
    # 4. Interpolate at the polygon centers
    # ------------------------------------------------------------
    
    node_sp <- methods::as(
      nodes_valid,
      "Spatial"
    )
    
    center_sp <- methods::as(
      grid_centers,
      "Spatial"
    )
    
    idw_result <- gstat::idw(
      formula = entrainment ~ 1,
      locations = node_sp,
      newdata = center_sp,
      idp = idp,
      nmax = min(
        nmax,
        nrow(nodes_valid)
      ),
      maxdist = maximum_distance,
      debug.level = 0
    )
    
    interpolated_values <- as.numeric(
      idw_result$var1.pred
    )
    
    grid_sf$risk <- interpolated_values
    
    grid_sf <- grid_sf %>%
      dplyr::filter(
        !is.na(risk),
        is.finite(risk)
      )
    
    validate(
      need(
        nrow(grid_sf) > 0,
        "No supported PTM interpolation cells were generated."
      )
    )
    
    grid_sf$risk <- pmax(
      0,
      pmin(
        100,
        grid_sf$risk
      )
    )
    
    # ------------------------------------------------------------
    # 5. Transform the completed grid for Leaflet
    #
    # sf performs the vector transformation before the layer is
    # sent to Leaflet. No RasterLayer reprojection is needed.
    # ------------------------------------------------------------
    
    grid_display <- grid_sf %>%
      sf::st_transform(4326)
    
    list(
      grid = grid_display,
      nodes = sf::st_transform(
        nodes_valid,
        4326
      ),
      boundary = sf::st_transform(
        sf::st_as_sf(boundary_utm),
        4326
      )
    )
  }
  make_entrainment_threshold_contour <- function(
    risk_raster,
    threshold
  ) {
    
    threshold <- suppressWarnings(
      as.numeric(threshold)[1]
    )
    
    if (!is.finite(threshold)) {
      threshold <- 25
    }
    
    raster_values <- raster::values(
      risk_raster
    )
    
    raster_values <- raster_values[
      is.finite(raster_values)
    ]
    
    if (
      length(raster_values) == 0 ||
      threshold < min(raster_values) ||
      threshold > max(raster_values)
    ) {
      return(NULL)
    }
    
    contour_sp <- tryCatch(
      {
        raster::rasterToContour(
          risk_raster,
          levels = threshold
        )
      },
      error = function(error_condition) {
        NULL
      }
    )
    
    if (
      is.null(contour_sp) ||
      length(contour_sp) == 0
    ) {
      return(NULL)
    }
    
    contour_sf <- sf::st_as_sf(
      contour_sp
    )
    
    if (nrow(contour_sf) == 0) {
      return(NULL)
    }
    
    contour_sf$threshold <- threshold
    
    sf::st_transform(
      contour_sf,
      4326
    )
  }
  make_entrainment_zones <- function(
    nodes,
    threshold,
    high_node_buffer = 7500,
    low_node_exclusion_buffer = 6500,
    smooth_buffer = 1800,
    concave_ratio = 0.38
  ) {
    
    validate(
      need(
        nrow(nodes) >= 1,
        "Not enough nodes to create risk zones."
      ),
      need(
        "entrainment" %in% names(nodes),
        "Node data are missing the entrainment field."
      )
    )
    
    threshold <- suppressWarnings(as.numeric(threshold))[1]
    
    if (is.na(threshold)) {
      threshold <- 25
    }
    
    delta_valid <- delta_boundary |>
      sf::st_make_valid()
    
    nodes <- nodes |>
      dplyr::filter(
        !is.na(entrainment),
        is.finite(entrainment)
      ) |>
      sf::st_make_valid() |>
      sf::st_transform(sf::st_crs(delta_valid))
    
    validate(
      need(
        nrow(nodes) > 0,
        "No valid mapped nodes are available."
      )
    )
    
    high_nodes <- nodes |>
      dplyr::filter(
        entrainment >= threshold
      )
    
    low_nodes <- nodes |>
      dplyr::filter(
        entrainment < threshold
      )
    
    validate(
      need(
        nrow(high_nodes) > 0,
        paste0(
          "No nodes are greater than or equal to ",
          threshold,
          "% entrainment."
        )
      )
    )
    
    # ------------------------------------------------------------
    # 1. Build the main high-risk envelope from high-risk nodes.
    # ------------------------------------------------------------
    
    high_node_core <- high_nodes |>
      sf::st_buffer(high_node_buffer) |>
      sf::st_union() |>
      sf::st_make_valid()
    
    if (nrow(high_nodes) >= 3) {
      
      high_points_union <- high_nodes |>
        sf::st_geometry() |>
        sf::st_union()
      
      high_hull <- tryCatch(
        {
          lwgeom::st_concave_hull(
            high_points_union,
            ratio = concave_ratio,
            allow_holes = FALSE
          )
        },
        error = function(e) {
          sf::st_convex_hull(high_points_union)
        }
      )
      
      high_hull_buffered <- high_hull |>
        sf::st_buffer(high_node_buffer) |>
        sf::st_make_valid()
      
      high_zone <- sf::st_union(
        high_node_core,
        high_hull_buffered
      ) |>
        sf::st_make_valid()
      
    } else {
      
      high_zone <- high_node_core
    }
    
    # ------------------------------------------------------------
    # 2. Carve out low-risk node neighborhoods.
    # This prevents the broad high-risk envelope from swallowing
    # nodes below the selected threshold.
    # ------------------------------------------------------------
    
    if (nrow(low_nodes) > 0) {
      
      low_exclusion_zone <- low_nodes |>
        sf::st_buffer(low_node_exclusion_buffer) |>
        sf::st_union() |>
        sf::st_make_valid()
      
      high_zone <- suppressWarnings(
        sf::st_difference(
          high_zone,
          low_exclusion_zone
        )
      ) |>
        sf::st_make_valid()
    }
    
    # ------------------------------------------------------------
    # 3. Re-add a smaller required buffer around high-risk nodes.
    # This guarantees every node >= threshold stays inside high risk,
    # even after the low-risk carve-out.
    # ------------------------------------------------------------
    
    high_node_required_zone <- high_nodes |>
      sf::st_buffer(high_node_buffer * 0.55) |>
      sf::st_union() |>
      sf::st_make_valid()
    
    high_zone <- sf::st_union(
      high_zone,
      high_node_required_zone
    ) |>
      sf::st_make_valid()
    
    # ------------------------------------------------------------
    # 4. Smooth the result, but not so much that it washes over
    # the low-risk carve-outs.
    # ------------------------------------------------------------
    
    high_zone <- high_zone |>
      sf::st_buffer(smooth_buffer) |>
      sf::st_buffer(-smooth_buffer) |>
      sf::st_make_valid()
    
    # Re-apply low-risk carve-out lightly after smoothing.
    if (nrow(low_nodes) > 0) {
      
      low_exclusion_zone_final <- low_nodes |>
        sf::st_buffer(low_node_exclusion_buffer * 0.75) |>
        sf::st_union() |>
        sf::st_make_valid()
      
      high_zone <- suppressWarnings(
        sf::st_difference(
          high_zone,
          low_exclusion_zone_final
        )
      ) |>
        sf::st_make_valid()
      
      # Re-add required high-node buffer one more time.
      high_zone <- sf::st_union(
        high_zone,
        high_node_required_zone
      ) |>
        sf::st_make_valid()
    }
    
    # ------------------------------------------------------------
    # 5. Clip to Delta boundary.
    # ------------------------------------------------------------
    
    high_zone <- suppressWarnings(
      sf::st_intersection(
        high_zone,
        delta_valid
      )
    ) |>
      sf::st_make_valid()
    
    high_zone <- suppressWarnings(
      sf::st_collection_extract(
        high_zone,
        "POLYGON"
      )
    )
    
    validate(
      need(
        length(high_zone) > 0,
        "The high-risk polygon could not be created."
      )
    )
    
    # ------------------------------------------------------------
    # 6. Low risk is everything in the Delta boundary outside
    # the high-risk envelope.
    # ------------------------------------------------------------
    
    low_zone <- suppressWarnings(
      sf::st_difference(
        delta_valid,
        sf::st_union(high_zone)
      )
    ) |>
      sf::st_make_valid()
    
    low_zone <- suppressWarnings(
      sf::st_collection_extract(
        low_zone,
        "POLYGON"
      )
    )
    
    list(
      high = sf::st_transform(
        sf::st_as_sf(high_zone),
        4326
      ),
      low = sf::st_transform(
        sf::st_as_sf(low_zone),
        4326
      )
    )
  }
  
  
  
  make_ptm_spatial_comparison_map <- function(df, threshold) {
    validate(
      need(nrow(df) > 0, "Select compatible PTM runs to compare."),
      need("Saved_Run_ID" %in% names(df), "PTM comparison data are missing Saved_Run_ID."),
      need("DSM2_Node" %in% names(df), "PTM comparison data are missing DSM2_Node."),
      need("Prediction_Final" %in% names(df), "PTM comparison data are missing Prediction_Final.")
    )

    map_df <- df
    for (nm in c("Location", "Region", "Scenario_Name")) {
      if (!nm %in% names(map_df)) map_df[[nm]] <- NA_character_
    }
    for (nm in c("X", "Y")) {
      if (!nm %in% names(map_df)) map_df[[nm]] <- NA_real_
    }

    if ("Window_End_Date" %in% names(map_df) && any(!is.na(map_df$Window_End_Date))) {
      map_df <- map_df %>%
        dplyr::mutate(.map_date = as.Date(Window_End_Date, origin = "1970-01-01")) %>%
        dplyr::group_by(Saved_Run_ID) %>%
        dplyr::mutate(.last_date = if (all(is.na(.map_date))) as.Date(NA) else max(.map_date, na.rm = TRUE)) %>%
        dplyr::filter(is.na(.map_date) | is.na(.last_date) | .map_date == .last_date) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.map_date, -.last_date)
    }

    run_ids <- unique(as.character(map_df$Saved_Run_ID))
    run_colors <- stats::setNames(viridisLite::viridis(length(run_ids), option = "D"), run_ids)
    m <- leaflet::leaflet() %>%
      leaflet::addMapPane("comparisonSurfacePane", zIndex = 410) %>%
      leaflet::addMapPane("comparisonChannelPane", zIndex = 420) %>%
      leaflet::addMapPane("comparisonNodePane", zIndex = 660) %>%
      add_osm_basemap() %>%
      leaflet::addPolygons(
        data = sf::st_transform(delta_boundary, 4326),
        fillColor = "#eef7f9", fillOpacity = 0.06,
        color = "#0a7e8c", weight = 1, group = "DSM2 Delta Boundary"
      ) %>%
      leaflet::addPolylines(
        data = sf::st_transform(delta_channels, 4326),
        color = "#285F73", weight = 1, opacity = 0.55,
        group = "Channels", options = leaflet::pathOptions(pane = "comparisonChannelPane")
      )

    groups <- c("DSM2 Delta Boundary", "Channels")
    legend_labels <- character(0)
    for (i in seq_along(run_ids)) {
      run_id <- run_ids[i]
      d <- map_df %>% dplyr::filter(as.character(Saved_Run_ID) == run_id)
      run_label <- d$Scenario_Name[which(!is.na(d$Scenario_Name) & nzchar(d$Scenario_Name))[1]]
      if (length(run_label) == 0 || is.na(run_label)) run_label <- run_id
      color <- unname(run_colors[run_id])
      legend_labels <- c(legend_labels, run_label)
      nodes <- d %>%
        dplyr::filter(is.finite(as.numeric(X)), is.finite(as.numeric(Y)), is.finite(as.numeric(Prediction_Final))) %>%
        dplyr::transmute(
          DSM2_Node, Location, Region,
          entrainment = pmax(0, pmin(100, as.numeric(Prediction_Final))),
          X = as.numeric(X), Y = as.numeric(Y)
        ) %>%
        sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE) %>%
        sf::st_transform(26910)
      if (nrow(nodes) < 3) next

      surface <- make_entrainment_risk_surface(
        nodes = nodes, grid_cell_size = 2000, idp = 2, nmax = 6,
        maximum_distance = Inf
      )
      surface$grid$Comparison_Opacity <- 0.68 * sqrt(pmax(0, pmin(100, surface$grid$risk)) / 100)
      group_name <- paste0(run_label, " - Shading and nodes")
      display_nodes <- sf::st_transform(nodes, 4326)
      display_nodes$ring_radius <- 4 + 11 * sqrt(display_nodes$entrainment / 100)
      display_nodes$Scenario_Label <- run_label

      m <- m %>%
        leaflet::addPolygons(
          data = surface$grid, fillColor = color,
          fillOpacity = ~Comparison_Opacity, stroke = FALSE,
          smoothFactor = 0, group = group_name,
          options = leaflet::pathOptions(pane = "comparisonSurfacePane", interactive = FALSE)
        ) %>%
        leaflet::addCircleMarkers(
          data = display_nodes, radius = ~ring_radius,
          color = color, opacity = 1, weight = 3,
          fill = FALSE, fillOpacity = 0,
          popup = ~paste0(
            "<b>Scenario:</b> ", Scenario_Label,
            "<br><b>DSM2 Node:</b> ", DSM2_Node,
            "<br><b>Location:</b> ", Location,
            "<br><b>Predicted entrainment:</b> ", sprintf("%.1f%%", entrainment)
          ),
          group = group_name,
          options = leaflet::pathOptions(pane = "comparisonNodePane")
        )
      groups <- c(groups, group_name)
    }

    m %>%
      leaflet::addLegend(
        position = "bottomright", colors = unname(run_colors),
        labels = legend_labels, title = "Compared scenarios", opacity = 1
      ) %>%
      leaflet::addControl(
        html = paste0(
          "<div style='background:rgba(255,255,255,.94);padding:9px 11px;",
          "border:1px solid #999;border-radius:5px;font-size:12px;max-width:330px;'>",
          "<b>Comparison interpretation</b><br>",
          "Hue identifies the scenario. Shading opacity and hollow-ring size represent predicted entrainment. ",
          "Interpolated shading is generalized; spatial transitions are approximate.</div>"
        ), position = "bottomleft"
      ) %>%
      leaflet::addLayersControl(
        overlayGroups = unique(groups),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
      add_reset_view_control(zoom = 9) %>%
      leaflet::setView(lng = -121.60, lat = 38.05, zoom = 9)
  }

  make_ptm_comparison_png_plot <- function(df, map_bounds = NULL, title = "PTM Scenario Comparison") {
    model_name <- unique(as.character(df$Model))[1]
    bounds <- normalize_leaflet_bounds(map_bounds, delta_boundary)
    bbox <- sf::st_bbox(c(xmin = bounds$west, xmax = bounds$east,
                          ymin = bounds$south, ymax = bounds$north), crs = sf::st_crs(4326))
    run_ids <- unique(as.character(df$Saved_Run_ID))
    run_colors <- stats::setNames(viridisLite::viridis(length(run_ids), option = "D"), run_ids)
    surface_list <- list(); node_list <- list()
    for (i in seq_along(run_ids)) {
      run_id <- run_ids[i]
      d <- df %>% dplyr::filter(as.character(Saved_Run_ID) == run_id)
      if (!"Scenario_Name" %in% names(d)) d$Scenario_Name <- NA_character_
      label <- d$Scenario_Name[which(!is.na(d$Scenario_Name) & nzchar(d$Scenario_Name))[1]]
      if (length(label) == 0 || is.na(label)) label <- run_id
      nodes <- d %>%
        dplyr::filter(is.finite(as.numeric(X)), is.finite(as.numeric(Y)), is.finite(as.numeric(Prediction_Final))) %>%
        dplyr::transmute(DSM2_Node, entrainment = pmax(0, pmin(100, as.numeric(Prediction_Final))), X = as.numeric(X), Y = as.numeric(Y)) %>%
        sf::st_as_sf(coords = c("X", "Y"), crs = 4326, remove = FALSE) %>%
        sf::st_transform(26910)
      if (nrow(nodes) < 3) next
      surf <- make_entrainment_risk_surface(nodes, grid_cell_size = 500, idp = 2, nmax = 6, maximum_distance = Inf)$grid
      surf$Scenario <- label; surf$Scenario_Color <- unname(run_colors[run_id]); surf$Alpha <- 0.68 * sqrt(surf$risk / 100)
      nd <- sf::st_transform(nodes, 4326); nd$Scenario <- label; nd$Scenario_Color <- unname(run_colors[run_id]); nd$Ring_Size <- 2.5 + 8 * sqrt(nd$entrainment / 100)
      surface_list[[length(surface_list)+1]] <- suppressWarnings(sf::st_crop(surf, bbox))
      node_list[[length(node_list)+1]] <- nd
    }
    surfaces <- do.call(rbind, surface_list); nodes <- do.call(rbind, node_list)
    channels <- suppressWarnings(sf::st_crop(sf::st_transform(delta_channels, 4326), bbox))
    boundary <- suppressWarnings(sf::st_crop(sf::st_transform(delta_boundary, 4326), bbox))
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = boundary, fill = "#F3F7F8", color = "#477A87", linewidth = .35)
    for (scenario in unique(surfaces$Scenario)) {
      z <- surfaces[surfaces$Scenario == scenario,]
      col <- unique(z$Scenario_Color)[1]
      p <- p + ggplot2::geom_sf(data = z, ggplot2::aes(alpha = Alpha), fill = col, color = NA, show.legend = FALSE)
    }
    p <- p + ggplot2::geom_sf(data = channels, color = "#285F73", linewidth = .25, alpha = .65)
    scenario_colors <- stats::setNames(
      vapply(split(nodes$Scenario_Color, nodes$Scenario), function(x) unique(x)[1], character(1)),
      names(split(nodes$Scenario_Color, nodes$Scenario))
    )
    p <- p + ggplot2::geom_sf(
      data = nodes,
      ggplot2::aes(color = Scenario, size = Ring_Size),
      shape = 21, fill = NA, stroke = 1.1, show.legend = TRUE
    )
    footer <- map_export_caption(TRUE)
    p +
      ggplot2::scale_color_manual(values = scenario_colors, name = "Compared scenarios") +
      ggplot2::scale_alpha_identity() + ggplot2::scale_size_identity() +
      ggplot2::coord_sf(xlim = c(bounds$west, bounds$east), ylim = c(bounds$south, bounds$north), expand = FALSE, datum = NA) +
      ggplot2::labs(title = title, subtitle = model_name, caption = footer) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 18),
        plot.subtitle = ggplot2::element_text(size = 11),
        plot.caption = ggplot2::element_text(size = 9, hjust = 0, lineheight = 1.1),
        panel.background = ggplot2::element_rect(fill = "#F8FAFB", color = "#D1D9DD"),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      )
  }


  make_eh_comparison_png_plot <- function(df, map_bounds = NULL, title = "Event Horizon Scenario Comparison") {
    bounds <- normalize_leaflet_bounds(map_bounds, delta_boundary)
    bbox <- sf::st_bbox(c(
      xmin = bounds$west, xmax = bounds$east,
      ymin = bounds$south, ymax = bounds$north
    ), crs = sf::st_crs(4326))

    if (!"Saved_Run_ID" %in% names(df)) df$Saved_Run_ID <- seq_len(nrow(df))
    if (!"Scenario_Name" %in% names(df)) df$Scenario_Name <- as.character(df$Saved_Run_ID)
    rows <- df %>%
      dplyr::filter(is.finite(as.numeric(Prediction_Final))) %>%
      dplyr::group_by(Saved_Run_ID) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
    scenario_labels <- ifelse(
      !is.na(rows$Scenario_Name) & nzchar(as.character(rows$Scenario_Name)),
      as.character(rows$Scenario_Name), as.character(rows$Saved_Run_ID)
    )
    colors <- stats::setNames(viridisLite::viridis(nrow(rows), option = "D"), scenario_labels)
    line_list <- vector("list", nrow(rows))
    for (i in seq_len(nrow(rows))) {
      path <- make_event_horizon_path_geometry(
        distance_miles = as.numeric(rows$Prediction_Final[i]), regions = c("OMR")
      ) %>% sf::st_transform(4326)
      path$Scenario <- scenario_labels[i]
      path$Event_Horizon_miles <- as.numeric(rows$Prediction_Final[i])
      line_list[[i]] <- suppressWarnings(sf::st_crop(path, bbox))
    }
    lines <- do.call(rbind, line_list)
    channels <- suppressWarnings(sf::st_crop(sf::st_transform(delta_channels, 4326), bbox))
    boundary <- suppressWarnings(sf::st_crop(sf::st_transform(delta_boundary, 4326), bbox))

    ggplot2::ggplot() +
      ggplot2::geom_sf(data = boundary, fill = "#F3F7F8", color = "#477A87", linewidth = .35) +
      ggplot2::geom_sf(data = channels, color = "#285F73", linewidth = .25, alpha = .60) +
      ggplot2::geom_sf(data = lines, ggplot2::aes(color = Scenario), linewidth = 1.15, alpha = .95) +
      ggplot2::scale_color_manual(values = colors, name = "Compared scenarios") +
      ggplot2::coord_sf(
        xlim = c(bounds$west, bounds$east), ylim = c(bounds$south, bounds$north),
        expand = FALSE, datum = NA
      ) +
      ggplot2::labs(
        title = title,
        subtitle = "Predicted upstream Event Horizon reaches",
        caption = map_export_caption(FALSE)
      ) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 18),
        plot.subtitle = ggplot2::element_text(size = 11),
        plot.caption = ggplot2::element_text(size = 9, hjust = 0, lineheight = 1.1),
        legend.position = "right",
        panel.background = ggplot2::element_rect(fill = "#F8FAFB", color = "#D1D9DD"),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      )
  }

  threshold_centered_risk_palette <- function(threshold) {
    threshold <- suppressWarnings(as.numeric(threshold)[1])
    if (!is.finite(threshold)) threshold <- 25
    threshold <- max(1, min(99, threshold))

    colors <- c(
      "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0",
      "#FFFFFF",
      "#FDDBC7", "#F4A582", "#D6604D", "#B2182B"
    )

    positions <- c(
      threshold * c(0, 0.38, 0.70, 0.90),
      threshold,
      threshold + (100 - threshold) * c(0.10, 0.30, 0.62, 1.00)
    )
    values <- positions / 100

    list(
      colors = colors,
      values = values,
      leaflet = leaflet::colorNumeric(
        palette = scales::gradient_n_pal(
          colours = colors,
          values = values,
          space = "Lab"
        ),
        domain = c(0, 100),
        na.color = "transparent"
      )
    )
  }

  make_ptm_map <- function(df, threshold) {
    
    validate(
      need(
        nrow(df) > 0,
        "Run the model to display results."
      )
    )
    
    threshold <- suppressWarnings(
      as.numeric(threshold)[1]
    )
    
    if (!is.finite(threshold)) {
      threshold <- 25
    }
    
    # ------------------------------------------------------------
    # Prepare mapped PTM nodes
    # ------------------------------------------------------------
    
    sf_nodes <- df %>%
      dplyr::filter(
        !is.na(X),
        !is.na(Y),
        !is.na(Prediction_Final),
        is.finite(Prediction_Final)
      ) %>%
      dplyr::transmute(
        DSM2_Node,
        Location,
        Region,
        entrainment = as.numeric(Prediction_Final),
        X = as.numeric(X),
        Y = as.numeric(Y)
      ) %>%
      sf::st_as_sf(
        coords = c("X", "Y"),
        crs = 4326,
        remove = FALSE
      ) %>%
      sf::st_transform(26910)
    
    validate(
      need(
        nrow(sf_nodes) >= 3,
        "At least three mapped PTM node predictions are required."
      )
    )
    
    # ------------------------------------------------------------
    # Create the continuous IDW surface and threshold contour
    # ------------------------------------------------------------
    
    risk_surface <- make_entrainment_risk_surface(
      nodes = sf_nodes,
      grid_cell_size = 300,
      idp = 2,
      nmax = 6,
      maximum_distance = 300000
    )
    
    # threshold_contour <- make_entrainment_threshold_contour(
    #   risk_raster = risk_surface,
    #   threshold = threshold
    # )
    
    display_nodes <- sf::st_transform(
      sf_nodes,
      4326
    )
    
    # A fixed 0-100 domain ensures that the same color means the
    # same entrainment percentage in every scenario.
    risk_palette_spec <- threshold_centered_risk_palette(threshold)
    risk_palette <- risk_palette_spec$leaflet
    
    
    popup_html <- ~paste0(
      "<div style='font-size:16px;line-height:1.55;min-width:300px;'>",
      "<div style='font-size:20px;font-weight:700;color:#075f6d;",
      "margin-bottom:8px;'>DSM2 Node ",
      DSM2_Node,
      "</div>",
      "<div><b>Node:</b> ",
      DSM2_Node,
      "</div>",
      "<div><b>Location:</b> ",
      Location,
      "</div>",
      "<div><b>Region:</b> ",
      Region,
      "</div>",
      "<div style='font-size:18px;font-weight:800;margin-top:8px;",
      "color:#8b1e1e;'>Predicted entrainment: ",
      sprintf("%.0f", entrainment),
      "%</div>",
      "</div>"
    )
    
    # ------------------------------------------------------------
    # Start map
    # ------------------------------------------------------------
    
    map <- leaflet::leaflet() %>%
      add_osm_basemap() %>%
      leaflet::addPolygons(
        data = sf::st_transform(
          delta_boundary,
          4326
        ),
        fillColor = "#eef7f9",
        fillOpacity = 0.10,
        color = "#0a7e8c",
        weight = 1,
        group = "DSM2 Delta Boundary"
      ) %>%
      leaflet::addPolygons(
        data = risk_surface$grid,
        fillColor = ~risk_palette(risk),
        fillOpacity = 0.68,
        stroke = FALSE,
        smoothFactor = 0,
        group = "Interpolated Entrainment Risk",
        options = leaflet::pathOptions(
          interactive = FALSE
        )
      )%>%
      leaflet::addPolylines(
        data = sf::st_transform(
          delta_channels,
          4326
        ),
        color = "#285F73",
        weight = 1,
        opacity = 0.55,
        group = "Channels"
      )
    
    # ------------------------------------------------------------
    # Add the selected threshold as a contour line when it exists
    # ------------------------------------------------------------
    
    # if (!is.null(threshold_contour)) {
    #   
    #   map <- map %>%
    #     leaflet::addPolylines(
    #       data = threshold_contour,
    #       color = "#202020",
    #       weight = 3,
    #       opacity = 0.95,
    #       dashArray = "8,6",
    #       label = paste0(
    #         threshold,
    #         "% interpolated entrainment threshold"
    #       ),
    #       popup = paste0(
    #         "<b>Risk threshold contour</b><br>",
    #         "Interpolated entrainment = ",
    #         threshold,
    #         "%"
    #       ),
    #       group = "Risk Threshold"
    #     )
    # }
    
    # ------------------------------------------------------------
    # Add actual PTM node predictions above the surface
    # ------------------------------------------------------------
    
    map <- map %>%
      leaflet::addCircleMarkers(
        data = display_nodes,
        radius = 7,
        color = "#252525",
        fillColor = ~risk_palette(entrainment),
        fillOpacity = 1,
        stroke = TRUE,
        weight = 2,
        popup = popup_html,
        group = "PTM Node Predictions"
      ) %>%
      leaflet::addLabelOnlyMarkers(
        data = display_nodes,
        label = ~paste0(
          round(entrainment, 0),
          "%"
        ),
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = FALSE,
          offset = c(0, -10),
          className = "entrainment-permanent-label"
        ),
        group = "Entrainment Labels"
      ) %>%
      leaflet::addLegend(
        position = "topright",
        pal = risk_palette,
        values = c(0, 100),
        title = paste0("Predicted entrainment (%)<br>White = ", sprintf("%.0f", threshold), "%"),
        opacity = 0.85,
        group = "Interpolated Entrainment Risk"
      ) %>%
      leaflet::addControl(
        html = paste0(
          "<div style='background:rgba(255,255,255,0.94);",
          "padding:9px 11px;border:1px solid #999;",
          "border-radius:5px;font-size:12px;",
          "line-height:1.4;max-width:290px;'>",
          "<b>Map interpretation</b><br>",
          "Circles are PTM emulator predictions at supported DSM2 nodes.",
          "<br>The faded surface is inverse-distance interpolation ",
          "between nearby nodes.",
          "</div>"
        ),
        position = "bottomleft"
      )
    
    overlay_groups <- c(
      "DSM2 Delta Boundary",
      "Interpolated Entrainment Risk",
      "Channels"
    )
    
    overlay_groups <- c(
      overlay_groups,
      "PTM Node Predictions",
      "Entrainment Labels"
    )
    
    map %>%
      leaflet::addLayersControl(
        overlayGroups = overlay_groups,
        options = leaflet::layersControlOptions(
          collapsed = FALSE
        )
      ) %>%
      add_reset_view_control(
        zoom = 9
      ) %>%
      leaflet::setView(
        lng = -121.60,
        lat = 38.05,
        zoom = 9
      )
  }
  make_combined_ptm_eh_map <- function(
    ptm_df,
    eh_df,
    threshold
  ) {
    
    validate(
      need(
        !is.null(ptm_df) && nrow(ptm_df) > 0,
        "Run the PTM emulator to display the risk map."
      ),
      need(
        !is.null(eh_df) && nrow(eh_df) > 0,
        "No Event Horizon emulator result is available."
      )
    )
    
    threshold <- suppressWarnings(
      as.numeric(threshold)[1]
    )
    
    if (!is.finite(threshold)) {
      threshold <- 25
    }
    
    # ------------------------------------------------------------
    # Prepare PTM node predictions
    # ------------------------------------------------------------
    
    sf_nodes <- ptm_df %>%
      dplyr::filter(
        !is.na(X),
        !is.na(Y),
        !is.na(Prediction_Final),
        is.finite(Prediction_Final)
      ) %>%
      dplyr::transmute(
        DSM2_Node,
        Location,
        Region,
        entrainment = as.numeric(Prediction_Final),
        X = as.numeric(X),
        Y = as.numeric(Y)
      ) %>%
      sf::st_as_sf(
        coords = c("X", "Y"),
        crs = 4326,
        remove = FALSE
      ) %>%
      sf::st_transform(26910)
    
    validate(
      need(
        nrow(sf_nodes) >= 3,
        "At least three mapped PTM node predictions are required."
      )
    )
    
    # ------------------------------------------------------------
    # Build the new continuous vector-grid risk surface
    # ------------------------------------------------------------
    
    risk_surface <- make_entrainment_risk_surface(
      nodes = sf_nodes,
      grid_cell_size = 300,
      idp = 2,
      nmax = 6,
      maximum_distance = 300000
    )
    
    display_nodes <- sf::st_transform(
      sf_nodes,
      4326
    )
    
    # ------------------------------------------------------------
    # Prepare Event Horizon geometry
    # ------------------------------------------------------------
    
    eh_distance <- suppressWarnings(
      as.numeric(eh_df$Prediction_Final[1])
    )
    
    validate(
      need(
        is.finite(eh_distance) && eh_distance > 0,
        "The Event Horizon result does not contain a valid distance."
      )
    )
    
    eh_lines <- tryCatch(
      make_event_horizon_path_geometry(
        distance_miles = eh_distance,
        regions = c("OMR")
      ),
      error = function(e) {
        stop("Unable to build the Event Horizon map geometry: ",
             conditionMessage(e), call. = FALSE)
      }
    )
    
    validate(
      need(
        !is.null(eh_lines) &&
          inherits(eh_lines, "sf") &&
          nrow(eh_lines) > 0,
        "The Event Horizon channel geometry could not be created."
      )
    )
    
    eh_lines_display <- eh_lines %>%
      sf::st_make_valid() %>%
      sf::st_transform(4326)
    
    eh_lines_display$Event_Horizon_miles <- eh_distance
    
    # ------------------------------------------------------------
    # Continuous blue-to-red palette
    # ------------------------------------------------------------
    
    risk_palette_spec <- threshold_centered_risk_palette(threshold)
    risk_palette <- risk_palette_spec$leaflet
    
    popup_html <- ~paste0(
      "<div style='font-size:16px;line-height:1.55;min-width:280px;'>",
      "<div><b>DSM2 Node:</b> ",
      DSM2_Node,
      "</div>",
      "<div><b>Location:</b> ",
      Location,
      "</div>",
      "<div><b>Region:</b> ",
      Region,
      "</div>",
      "<div style='font-size:18px;font-weight:800;color:#8b1e1e;'>",
      "Entrainment: ",
      sprintf("%.0f", entrainment),
      "%",
      "</div>",
      "</div>"
    )
    
    # ------------------------------------------------------------
    # Start map and explicitly control layer order
    # ------------------------------------------------------------
    
    map <- leaflet::leaflet() %>%
      leaflet::addMapPane(
        name = "riskSurfacePane",
        zIndex = 410
      ) %>%
      leaflet::addMapPane(
        name = "channelPane",
        zIndex = 420
      ) %>%
      leaflet::addMapPane(
        name = "eventHorizonPane",
        zIndex = 650
      ) %>%
      leaflet::addMapPane(
        name = "nodePane",
        zIndex = 660
      ) %>%
      add_osm_basemap() %>%
      
      leaflet::addPolygons(
        data = sf::st_transform(
          delta_boundary,
          4326
        ),
        fillColor = "#eef7f9",
        fillOpacity = 0.10,
        color = "#0a7e8c",
        weight = 1,
        group = "DSM2 Delta Boundary"
      ) %>%
      
      leaflet::addPolygons(
        data = risk_surface$grid,
        fillColor = ~risk_palette(risk),
        fillOpacity = 0.68,
        stroke = FALSE,
        smoothFactor = 0,
        group = "Interpolated Entrainment Risk",
        options = leaflet::pathOptions(
          pane = "riskSurfacePane",
          interactive = FALSE
        )
      ) %>%
      
      leaflet::addPolylines(
        data = sf::st_transform(
          delta_channels,
          4326
        ),
        color = "#285F73",
        weight = 1,
        opacity = 0.55,
        group = "Channels",
        options = leaflet::pathOptions(
          pane = "channelPane"
        )
      ) %>%
      
      leaflet::addCircleMarkers(
        data = display_nodes,
        radius = 7,
        color = "#252525",
        fillColor = ~risk_palette(entrainment),
        fillOpacity = 1,
        stroke = TRUE,
        weight = 2,
        popup = popup_html,
        group = "PTM Node Predictions",
        options = leaflet::pathOptions(
          pane = "nodePane"
        )
      ) %>%
      
      leaflet::addLabelOnlyMarkers(
        data = display_nodes,
        label = ~paste0(
          round(entrainment, 0),
          "%"
        ),
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = FALSE,
          offset = c(0, -10),
          className = "entrainment-permanent-label",
          pane = "nodePane"
        ),
        group = "Entrainment Labels"
      ) %>%
      
      # Event Horizon must be added after the risk grid.
      leaflet::addPolylines(
        data = eh_lines_display,
        color = "#65000B",
        weight = 4,
        opacity = 1,
        label = ~paste0(
          "Event Horizon: ",
          sprintf("%.1f", Event_Horizon_miles),
          " river miles"
        ),
        popup = ~paste0(
          "<div style='font-size:15px;line-height:1.5;'>",
          "<b>Event Horizon</b><br>",
          "Upstream extent: ",
          sprintf("%.1f", Event_Horizon_miles),
          " river miles",
          "<br>Selected entrainment risk: ",
          sprintf("%.0f", threshold),
          "%",
          "</div>"
        ),
        group = "Event Horizon Reach",
        options = leaflet::pathOptions(
          pane = "eventHorizonPane"
        )
      ) %>%
      
      leaflet::addLegend(
        position = "topright",
        pal = risk_palette,
        values = c(0, 100),
        title = paste0("Predicted entrainment (%)<br>White = ", sprintf("%.0f", threshold), "%"),
        opacity = 0.88,
        group = "Interpolated Entrainment Risk"
      ) %>%
      
      leaflet::addControl(
        html = paste0(
          "<div style='background:rgba(255,255,255,0.94);",
          "padding:9px 11px;border:1px solid #999;",
          "border-radius:5px;font-size:12px;",
          "line-height:1.4;max-width:300px;'>",
          "<b>Map interpretation</b><br>",
          "Circles are PTM emulator predictions at supported DSM2 nodes.",
          "<br>The faded surface is inverse-distance interpolation ",
          "between nearby nodes.",
          "<br>The dark-red channel line is the predicted Event Horizon ",
          "at the selected ",
          sprintf("%.0f", threshold),
          "% risk level.",
          "</div>"
        ),
        position = "bottomleft"
      ) %>%
      
      leaflet::addLayersControl(
        overlayGroups = c(
          "DSM2 Delta Boundary",
          "Interpolated Entrainment Risk",
          "Channels",
          "PTM Node Predictions",
          "Entrainment Labels",
          "Event Horizon Reach"
        ),
        options = leaflet::layersControlOptions(
          collapsed = FALSE
        )
      ) %>%
      
      add_reset_view_control(
        zoom = 9
      ) %>%
      
      leaflet::setView(
        lng = -121.60,
        lat = 38.05,
        zoom = 9
      )
    
    map
  }
  
   
  make_event_geometry <- function(distance_miles) {
    fraction <- max(0, min(1, distance_miles / river_length_miles))
    list(
      high_line = lwgeom::st_linesubstring(river_centerline, 0, fraction),
      point = lwgeom::st_linesubstring(river_centerline, fraction, fraction)
    )
  }
  make_eh_spatial_comparison_map <- function(df) {
    
    validate(
      need(
        nrow(df) > 0,
        "Select compatible Event Horizon runs to compare."
      ),
      need(
        all(
          c(
            "Saved_Run_ID",
            "Prediction_Final"
          ) %in% names(df)
        ),
        "Event Horizon comparison data are missing required fields."
      )
    )
    
    # If archive-current results include multiple rolling windows,
    # keep the latest window per saved run.
    map_df <- df
    
    if (
      "Window_End_Date" %in% names(map_df) &&
      any(!is.na(map_df$Window_End_Date))
    ) {
      
      map_df <- map_df %>%
        mutate(
          Window_End_Date_Map = as.Date(
            Window_End_Date,
            origin = "1970-01-01"
          )
        ) %>%
        group_by(
          Saved_Run_ID
        ) %>%
        filter(
          is.na(Window_End_Date_Map) |
            Window_End_Date_Map == max(
              Window_End_Date_Map,
              na.rm = TRUE
            )
        ) %>%
        ungroup() %>%
        select(
          -Window_End_Date_Map
        )
    }
    
    run_ids <- unique(
      map_df$Saved_Run_ID
    )
    
    run_colors <- viridisLite::viridis(
      length(run_ids),
      option = "D",
      end = 0.9
    )
    
    names(run_colors) <- run_ids
    
    channels_display <- st_transform(
      dsm2_channels,
      4326
    )
    
    nodes_display <- st_transform(
      dsm2_nodes,
      4326
    )
    
    m <- leaflet() %>%
      add_osm_basemap() %>%
      addPolylines(
        data = channels_display,
        color = "#9ECAE1",
        weight = 1,
        opacity = 0.65,
        group = "DSM2 Channels"
      ) %>%
      addCircleMarkers(
        data = nodes_display %>%
          dplyr::filter(
            DSM2_Node == 72
          ),
        radius = 7,
        color = "#1F1F1F",
        fillColor = "#FFD34E",
        fillOpacity = 1,
        weight = 2,
        label = "Start node 72: Clifton Court Forebay",
        group = "Start Node"
      )
    
    overlay_groups <- c(
      "DSM2 Channels",
      "Start Node"
    )
    
    for (run_id in run_ids) {
      
      scenario_df <- map_df %>%
        filter(
          Saved_Run_ID == run_id
        ) %>%
        slice(1)
      
      scenario_color <- run_colors[[run_id]]
      
      eh_lines <- tryCatch(
        make_event_horizon_path_geometry(
          distance_miles = scenario_df$Prediction_Final[1],
          regions = c("OMR")
        ),
        error = function(e) NULL,
        shiny.silent.error = function(e) NULL
      )
      
      if (is.null(eh_lines)) {
        next
      }
      
      eh_lines_display <- st_transform(
        eh_lines,
        4326
      )
      
      line_group <- paste0(
        "Event Horizon - ",
        run_id
      )
      
      overlay_groups <- c(
        overlay_groups,
        line_group
      )
      
      m <- m %>%
        addPolylines(
          data = eh_lines_display,
          color = scenario_color,
          weight = 5,
          opacity = 0.90,
          popup = paste0(
            "<div style='font-size:15px;line-height:1.45;min-width:280px;'>",
            "<b>Scenario:</b> ",
            run_id,
            "<br><b>Event Horizon:</b> ",
            sprintf(
              "%.0f",
              scenario_df$Prediction_Final[1]
            ),
            " river miles",
            if (
              "Risk_Level_Percent" %in% names(scenario_df)
            ) {
              paste0(
                "<br><b>Risk level:</b> ",
                scenario_df$Risk_Level_Percent[1],
                "%"
              )
            } else {
              ""
            },
            "</div>"
          ),
          group = line_group
        )
    }
    
    m %>%
      addLegend(
        position = "bottomright",
        colors = run_colors,
        labels = names(run_colors),
        title = "Compared Scenarios",
        opacity = 0.85
      ) %>%
      addLayersControl(
        overlayGroups = unique(
          overlay_groups
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      ) %>%
      add_reset_view_control(zoom = 10) %>%
      leaflet::setView(
        lng = -121.60,
        lat = 38.05,
        zoom = 10
      )
  }
  # Lightweight comparison maps. Avoid expensive interpolation in the
  # interactive map; show predicted values at their actual DSM2 nodes.
  # Saved-run/OMRI request errors are handled by each output renderer below.
  ccwd_minimal_map <- function(label) {
    leaflet::leaflet(options=leaflet::leafletOptions(preferCanvas=TRUE)) %>%
      leaflet::addTiles() %>%
      leaflet::setView(lng=-121.60,lat=38.05,zoom=9) %>%
      leaflet::addControl(html=paste0('<div style="background:white;padding:10px;border:1px solid #ccc;border-radius:5px;">',label,'</div>'),position='topright')
  }

  ccwd_fast_comparison_map <- function(df, model, threshold=25, context='Comparison') {
    started <- Sys.time()
    message('[CCWD comparison map] ',context, ' preparing node map')
    if (!is.data.frame(df) || nrow(df)==0L) return(ccwd_minimal_map('No comparison results are available. Run or select the scenarios first.'))
    if (length(model)==0L || is.na(model[1])) model <- 'PTM Emulator 7-Day Entrainment'
    if (!'Saved_Run_ID' %in% names(df)) {
      if ('OMRI_Scenario' %in% names(df)) df$Saved_Run_ID <- paste0('OMRI ',df$OMRI_Scenario)
      else df$Saved_Run_ID <- 'Scenario'
    }
    if (!'Scenario_Name' %in% names(df)) df$Scenario_Name <- as.character(df$Saved_Run_ID)
    if (!'Prediction_Final' %in% names(df)) return(ccwd_minimal_map('Predictions are missing from the comparison data.'))
    map <- leaflet::leaflet(options=leaflet::leafletOptions(preferCanvas=TRUE)) %>% leaflet::addTiles() %>%
      leaflet::setView(lng=-121.60,lat=38.05,zoom=9)
    # Optional geographic layers must never prevent the prediction markers from loading.
    map <- tryCatch({
      if (inherits(delta_boundary,'sf') && nrow(delta_boundary)>0L)
        leaflet::addPolygons(map,data=sf::st_transform(delta_boundary,4326),color='#698f99',
                             weight=1,fillOpacity=0.03,group='Delta boundary')
      else map
    },error=function(e) {message('[CCWD comparison map] boundary: ',conditionMessage(e));map})
    # For Event Horizon, preserve the real computed path geometry but avoid
    # interpolation. If path geometry fails, show the predicted miles in the map.
    is_eh <- identical(as.character(model[1]),'Event Horizon')
    is_ptm <- grepl('PTM Emulator',as.character(model[1]),fixed=TRUE)
    run_ids <- unique(as.character(df$Saved_Run_ID)); run_ids <- run_ids[!is.na(run_ids)]
    if (length(run_ids)==0L) run_ids <- 'Scenario'
    colors <- stats::setNames(viridisLite::viridis(length(run_ids)),run_ids)
    overlays <- character()
    if (is_eh) {
      for (rid in run_ids) {
        d <- df[as.character(df$Saved_Run_ID)==rid,,drop=FALSE]
        if (!nrow(d)) next
        miles <- suppressWarnings(as.numeric(as.character(d$Prediction_Final[1])))
        if (!is.finite(miles)) next
        path <- tryCatch(make_event_horizon_path_geometry(distance_miles=miles,regions=c('OMR')),
                         error=function(e) {message('[CCWD comparison map] EH geometry: ',conditionMessage(e));NULL})
        if (!is.null(path) && inherits(path,'sf') && nrow(path)>0L) {
          map <- tryCatch(leaflet::addPolylines(map,data=sf::st_transform(path,4326),
             color=unname(colors[rid]),weight=5,opacity=0.85,group=rid,
             popup=paste0(rid,': ',sprintf('%.1f',miles),' river miles')),
             error=function(e) {message('[CCWD comparison map] EH layer: ',conditionMessage(e));map})
          overlays <- c(overlays,rid)
        }
      }
    } else if (is_ptm) {
      if (!'DSM2_Node' %in% names(df)) return(ccwd_minimal_map('Predictions do not include DSM2 node IDs.'))
      # Map node identifiers to canonical GIS coordinates. This also works
      # when saved result tables do not carry X/Y fields.
      geo <- tryCatch({
        nodes <- sf::st_transform(dsm2_nodes,4326)
        xy <- sf::st_coordinates(nodes)
        data.frame(DSM2_Node=as.character(nodes$DSM2_Node),
                   longitude=as.numeric(xy[,1]),latitude=as.numeric(xy[,2]))
      },error=function(e) {message('[CCWD comparison map] node geometry: ',conditionMessage(e));NULL})
      df$node_key <- as.character(df$DSM2_Node)
      if (!is.null(geo)) {
        matches <- match(df$node_key,geo$DSM2_Node)
        df$longitude <- geo$longitude[matches]
        df$latitude <- geo$latitude[matches]
      } else {
        df$longitude <- NA_real_;df$latitude <- NA_real_
      }
      # If GIS IDs cannot be matched, use explicit geographic coordinates
      # only when they lie within a plausible geographic range.
      if (all(c('X','Y') %in% names(df))) {
        x <- suppressWarnings(as.numeric(as.character(df$X)))
        y <- suppressWarnings(as.numeric(as.character(df$Y)))
        missing <- !is.finite(df$longitude) | !is.finite(df$latitude)
        good <- is.finite(x)&is.finite(y)&x>=-180&x<=180&y>=-90&y<=90
        idx <- which(missing&good)
        df$longitude[idx] <- x[idx];df$latitude[idx] <- y[idx]
      }
      score <- suppressWarnings(as.numeric(as.character(df$Prediction_Final)))
      good <- is.finite(df$longitude)&is.finite(df$latitude)&is.finite(score)
      df <- df[which(good),,drop=FALSE];score <- score[good]
      if (nrow(df)) {
        df$score <- pmax(0,pmin(100,score))
        for (rid in run_ids) {
          d <- df[as.character(df$Saved_Run_ID)==rid,,drop=FALSE]
          if (!nrow(d)) next
          map <- leaflet::addCircleMarkers(map,lng=d$longitude,lat=d$latitude,
            radius=4+9*sqrt(d$score/100),color=unname(colors[rid]),
            fillOpacity=0.5,weight=2,group=rid,
            popup=paste0('<b>',rid,'</b><br>DSM2 node: ',d$DSM2_Node,
                         '<br>Predicted entrainment: ',sprintf('%.1f%%',d$score)))
          overlays <- c(overlays,rid)
        }
      }
    } else return(ccwd_minimal_map('Spatial maps are supported for PTM and Event Horizon comparisons.'))
    if (length(overlays)) {
      map <- leaflet::addLayersControl(map,overlayGroups=unique(overlays),
          options=leaflet::layersControlOptions(collapsed=FALSE))
      map <- leaflet::addLegend(map,position='bottomright',colors=unname(colors),
           labels=names(colors),title='Compared scenarios',opacity=1)
    } else {
      map <- leaflet::addControl(map,html='<div style="background:white;padding:9px;">The predictions are available, but no usable map geometry was found. Check the RStudio Console.</div>',position='topright')
    }
    message('[CCWD comparison map] ',context,' complete in ',
      round(as.numeric(difftime(Sys.time(),started,units='secs')),1),' seconds')
    map
  }
  ccwd_safe_map_request <- function(build, context) {
    tryCatch(build(), error=function(e) {
      message('[CCWD comparison map] ',context,' FAILED: ',conditionMessage(e))
      ccwd_minimal_map(paste0('Map could not be created: ',
        htmltools::htmlEscape(conditionMessage(e))))
    })
  }
  make_eh_map <- function(df) {
    
    validate(
      need(
        nrow(df) > 0,
        "Run the Event Horizon model to display results."
      )
    )
    
    eh_lines <- make_event_horizon_path_geometry(
      distance_miles = df$Prediction_Final[1],
      regions = c("OMR")
    )
    
    eh_lines_display <- st_transform(eh_lines, 4326)
    channels_display <- st_transform(dsm2_channels, 4326)
    nodes_display <- st_transform(dsm2_nodes, 4326)
    
    leaflet() |>
      add_osm_basemap() |>
      
      addPolylines(
        data = channels_display,
        color = "#9ECAE1",
        weight = 1,
        opacity = 0.65,
        group = "DSM2 Channels"
      ) |>
      
      addPolylines(
        data = eh_lines_display,
        color = "#B2182B",
        weight = 5,
        opacity = 0.95,
        popup = ~paste0(
          "<div style='font-size:15px;line-height:1.45;'>",
          "<b>Region:</b> ", Region,
          "<br><b>Path target:</b> ", Location,
          "<br><b>Target node:</b> ", Target_Node,
          "<br><b>Channel:</b> ", CHAN_NO,
          "<br><b>From node:</b> ", FROM_NODE,
          "<br><b>To node:</b> ", TO_NODE,
          "<br><b>Event Horizon:</b> ",
          sprintf("%.0f", Event_Horizon_miles),
          " river miles",
          "</div>"
        ),
        group = "Event Horizon"
      ) |>
      
      addCircleMarkers(
        data = nodes_display |>
          dplyr::filter(DSM2_Node == 72),
        radius = 7,
        color = "#1F1F1F",
        fillColor = "#FFD34E",
        fillOpacity = 1,
        weight = 2,
        label = "Start node 72: Clifton Court Forebay",
        group = "Start Node"
      ) |>
      
      addLayersControl(
        overlayGroups = c(
          "DSM2 Channels",
          "Event Horizon",
          "Start Node"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      
    add_reset_view_control(zoom = 10) |>
      leaflet::setView(
      lng = -121.60,
      lat = 38.05,
      zoom = 10
    )
  }
  
  make_eh_scatter <- function(df, compact_title_below_legend = FALSE) {
    
    validate(
      need(
        nrow(df) > 0,
        "Run the Event Horizon model to display results."
      )
    )
    
    risk <- as.numeric(
      df$Risk_Level_Percent[1]
    )
    
    horizon_col <- paste0(
      "Horizon_",
      risk
    )
    
    validate(
      need(
        horizon_col %in% names(eh_baseline),
        paste0(
          "Historical Event Horizon background data are not available for ",
          risk,
          "% risk. Historical background data currently support 25%, 50%, and 75% risk."
        )
      )
    )
    
    background <- eh_baseline %>%
      transmute(
        
        EXP =
          CCF + TPP,
        
        VER =
          VNS,
        
        Historical_Event_Horizon =
          .data[[horizon_col]]
        
      ) %>%
      filter(
        !is.na(EXP),
        !is.na(VER),
        !is.na(Historical_Event_Horizon),
        EXP > 0,
        VER > 0
      )
    
    if (nrow(background) > 10000) {
      
      set.seed(123)
      
      background <- background %>%
        slice_sample(
          n = 10000
        )
    }
    # ---------------------------------------------------------------
    # Create smooth Event Horizon surface for contour lines
    # Contours represent Event Horizon distance in river miles.
    # ---------------------------------------------------------------
    
    contour_breaks <- c(5, 10, 15, 20)
    
    contour_background <- background %>%
      filter(
        !is.na(EXP),
        !is.na(VER),
        !is.na(Historical_Event_Horizon),
        EXP > 0,
        VER > 0
      ) %>%
      mutate(
        log_VER = log10(VER)
      )
    eh_breaks <- seq(
      floor(min(contour_background$Historical_Event_Horizon, na.rm = TRUE)),
      ceiling(max(contour_background$Historical_Event_Horizon, na.rm = TRUE)),
      length.out = 21
    )
    
    eh_labels <- paste0(
      round(head(eh_breaks, -1)),
      "-",
      round(tail(eh_breaks, -1)),
      " mi"
    )
    
    contour_background <- contour_background %>%
      mutate(
        EH_Bin = cut(
          Historical_Event_Horizon,
          breaks = eh_breaks,
          labels = eh_labels,
          include.lowest = TRUE
        )
      )
    validate(
      need(
        nrow(contour_background) >= 30,
        "Not enough historical Event Horizon points to draw contour lines."
      )
    )
    
    contour_model <- mgcv::gam(
      Historical_Event_Horizon ~ te(EXP, log_VER, k = c(12, 12)),
      data = contour_background,
      method = "REML"
    )
    
    contour_grid <- expand.grid(
      EXP = seq(
        min(contour_background$EXP, na.rm = TRUE),
        max(contour_background$EXP, na.rm = TRUE),
        length.out = 180
      ),
      log_VER = seq(
        min(contour_background$log_VER, na.rm = TRUE),
        max(contour_background$log_VER, na.rm = TRUE),
        length.out = 180
      )
    )
    
    # Mask the grid to the convex hull of the historical point cloud.
    # This prevents contours from extrapolating far outside the observed data.
    hull_index <- chull(
      contour_background$EXP,
      contour_background$log_VER
    )
    
    hull_points <- contour_background[hull_index, ]
    
    inside_hull <- sp::point.in.polygon(
      point.x = contour_grid$EXP,
      point.y = contour_grid$log_VER,
      pol.x = hull_points$EXP,
      pol.y = hull_points$log_VER
    )
    
    contour_prediction <- contour_grid %>%
      mutate(
        Historical_Event_Horizon = as.numeric(
          predict(
            contour_model,
            newdata = contour_grid
          )
        ),
        inside_hull = inside_hull > 0
      ) %>%
      filter(
        inside_hull,
        !is.na(Historical_Event_Horizon)
      )
    scenario_point <- df %>%
      filter(
        EXP > 0,
        VER > 0
      ) %>%
      mutate(
        log_VER = log10(VER)
      )
    contour_df <- ggplot_build(
      ggplot(
        contour_prediction,
        aes(
          x = EXP,
          y = log_VER,
          z = Historical_Event_Horizon
        )
      ) +
        geom_contour(
          breaks = c(5, 10, 15, 20)
        )
    )$data[[1]]
    contour_labels <- contour_df %>%
      group_by(level) %>%
      slice(n() %/% 2) %>%
      ungroup() %>%
      mutate(
        label = paste0(level, " mi")
      )
 
    
    p <- ggplot() +
      
      geom_contour(
        data = contour_prediction,
        aes(
          x = EXP,
          y = log_VER,
          z = Historical_Event_Horizon
        ),
        breaks = c(5, 10, 15, 20),
        color = "black",
        linewidth = 0.4,
        alpha = 0.7
      ) +
      
      geom_text(
        data = contour_labels,
        aes(
          x = x,
          y = y,
          label = label
        ),
        inherit.aes = FALSE,
        size = 4,
        fontface = "bold",
        color = "black"
      )+
      
      geom_point(
        data = contour_background,
        aes(
          x = EXP,
          y = log_VER,
          color = EH_Bin,
          text = paste0(
            "Historical Point",
            "<br>Export: ",
            fmt_int(EXP),
            " cfs",
            "<br>Vernalis: ",
            fmt_int(VER),
            " cfs",
            "<br>Event Horizon: ",
            fmt_int(Historical_Event_Horizon),
            " miles"
          )
        ),
        size = 2,
        alpha = 0.45
      ) +
      
      geom_point(
        data = scenario_point,
        aes(
          x = EXP,
          y = log_VER,
          text = paste0(
            "Selected Scenario",
            "<br>Name: ",
            Scenario_Name,
            "<br>Condition: ",
            Condition,
            if (
              "Window_End_Date" %in% names(scenario_point)
            ) {
              paste0(
                "<br>Observed date: ",
                format(
                  as.Date(
                    Window_End_Date,
                    origin = "1970-01-01"
                  ),
                  "%m/%d/%Y"
                )
              )
            } else {
              ""
            },
            "<br>Export: ",
            fmt_int(EXP),
            " cfs",
            "<br>Vernalis: ",
            fmt_int(VER),
            " cfs",
            "<br>Event Horizon: ",
            fmt_int(Prediction_Final),
            " miles"
          )
        ),
        inherit.aes = FALSE,
        shape = 21,
        size = 7,
        fill = "red",
        color = "white",
        stroke = 2
      ) +
    
      
      scale_color_viridis_d(
        option = "viridis",
        name = "Historical Event Horizon Distance (miles)",
        drop = FALSE
      )+
      
      scale_x_continuous(
        labels = scales::comma
      ) +
      
      scale_y_continuous(
        breaks = log10(
          c(
            1000,
            2000,
            5000,
            10000,
            20000
          )
        ),
        labels = scales::comma(
          c(
            1000,
            2000,
            5000,
            10000,
            20000
          )
        )
      ) +
      
      labs(
        title = if (isTRUE(compact_title_below_legend)) {
          NULL
        } else {
          paste0(
            "Historical Event Horizon Conditions > ",
            risk,
            "% Risk"
          )
        },
        subtitle = if (isTRUE(compact_title_below_legend)) {
          NULL
        } else {
          "The selected emulator result(s) are highlighted in red."
        },
        x = "Combined Export, EXP (cfs)",
        y = "Vernalis Flow, VER (cfs)"
      ) +
      
      theme_bw(base_family = "Arial", base_size = 14) +
      theme(
        text = element_text(
          family = "Arial",
          size = 14,
          color = "#1F1F1F"
        ),
        
        plot.title = element_text(
          family = "Arial",
          face = "bold",
          size = 18,
          color = "#1F1F1F"
        ),
        
        plot.subtitle = element_text(
          family = "Arial",
          size = 13,
          color = "#404040"
        ),
        
        axis.title = element_text(
          family = "Arial",
          face = "bold",
          size = 14,
          color = "#1F1F1F"
        ),
        
        axis.text = element_text(
          family = "Arial",
          size = 12,
          color = "#1F1F1F"
        ),
        
        legend.title = element_text(
          family = "Arial",
          face = "bold",
          size = 12,
          color = "#1F1F1F"
        ),
        
        legend.text = element_text(
          family = "Arial",
          size = 11,
          color = "#1F1F1F"
        )
      )
    
    
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      layout(
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 14,
          color = "#1F1F1F"
        ),
        
        legend = if (isTRUE(compact_title_below_legend)) {
          list(
            orientation = "h",
            x = 0,
            xanchor = "left",
            y = 1.18,
            yanchor = "bottom",
            font = list(
              family = "Arial, Segoe UI, sans-serif",
              size = 12,
              color = "#1F1F1F"
            )
          )
        } else {
          list(
            font = list(
              family = "Arial, Segoe UI, sans-serif",
              size = 12,
              color = "#1F1F1F"
            )
          )
        },
        annotations = if (isTRUE(compact_title_below_legend)) {
          list(
            list(
              text = paste0(
                "Historical Event Horizon Conditions &gt; ",
                risk,
                "% Risk"
              ),
              x = 0.5,
              xref = "paper",
              xanchor = "center",
              y = 1.03,
              yref = "paper",
              yanchor = "bottom",
              showarrow = FALSE,
              bgcolor = "#F7F9FB",
              bordercolor = "#C7D3DD",
              borderwidth = 1,
              borderpad = 5,
              font = list(
                family = "Arial, Segoe UI, sans-serif",
                size = 11,
                color = "#404040"
              )
            )
          )
        } else {
          list()
        },
        margin = if (isTRUE(compact_title_below_legend)) {
          list(t = 120)
        } else {
          list()
        }
      ) %>%
      
      config(
        displaylogo = FALSE
      )
    
  }
  normalize_leaflet_bounds <- function(
    bounds,
    fallback_geometry
  ) {
    
    fallback_geometry <- sf::st_transform(
      fallback_geometry,
      4326
    )
    
    fallback_bbox <- sf::st_bbox(
      fallback_geometry
    )
    
    fallback <- list(
      west = as.numeric(fallback_bbox["xmin"]),
      east = as.numeric(fallback_bbox["xmax"]),
      south = as.numeric(fallback_bbox["ymin"]),
      north = as.numeric(fallback_bbox["ymax"])
    )
    
    if (
      is.null(bounds) ||
      !is.list(bounds) ||
      !all(
        c(
          "west",
          "east",
          "south",
          "north"
        ) %in% names(bounds)
      )
    ) {
      return(fallback)
    }
    
    normalized <- list(
      west = suppressWarnings(
        as.numeric(bounds$west)[1]
      ),
      east = suppressWarnings(
        as.numeric(bounds$east)[1]
      ),
      south = suppressWarnings(
        as.numeric(bounds$south)[1]
      ),
      north = suppressWarnings(
        as.numeric(bounds$north)[1]
      )
    )
    
    bounds_are_valid <- (
      all(
        is.finite(
          unlist(normalized)
        )
      ) &&
        normalized$west < normalized$east &&
        normalized$south < normalized$north &&
        normalized$west >= -180 &&
        normalized$east <= 180 &&
        normalized$south >= -90 &&
        normalized$north <= 90
    )
    
    if (!bounds_are_valid) {
      return(fallback)
    }
    
    normalized
  }
  ptm_png_dimensions <- function(
    map_bounds,
    fallback_geometry
  ) {
    
    bounds <- normalize_leaflet_bounds(
      bounds = map_bounds,
      fallback_geometry = fallback_geometry
    )
    
    center_latitude <- (
      bounds$south +
        bounds$north
    ) / 2
    
    longitude_span <- (
      bounds$east -
        bounds$west
    ) * cos(
      center_latitude * pi / 180
    )
    
    latitude_span <- (
      bounds$north -
        bounds$south
    )
    
    geographic_ratio <- (
      longitude_span /
        latitude_span
    )
    
    geographic_ratio <- max(
      1.25,
      min(
        geographic_ratio,
        3.5
      )
    )
    
    width_inches <- 13
    
    # Map-panel height plus room for title, subtitle and caption.
    height_inches <- max(
      5.2,
      min(
        8.5,
        width_inches / geographic_ratio + 1.75
      )
    )
    
    list(
      width = width_inches,
      height = height_inches
    )
  }
  make_ptm_png_plot <- function(
    df,
    threshold,
    title,
    map_bounds = NULL,
    eh_df = NULL
  ) {
    
    if (
      is.null(df) ||
      !is.data.frame(df) ||
      nrow(df) == 0
    ) {
      stop(
        paste(
          "No PTM results are available.",
          "Run the PTM models before downloading."
        )
      )
    }
    
    threshold <- suppressWarnings(
      as.numeric(threshold)[1]
    )
    
    if (!is.finite(threshold)) {
      threshold <- 25
    }
    
    # ------------------------------------------------------------
    # Prepare the selected PTM node predictions
    # ------------------------------------------------------------
    
    nodes_utm <- df %>%
      dplyr::filter(
        !is.na(X),
        !is.na(Y),
        !is.na(Prediction_Final),
        is.finite(Prediction_Final)
      ) %>%
      dplyr::transmute(
        DSM2_Node = as.character(DSM2_Node),
        Location,
        Region,
        entrainment = as.numeric(Prediction_Final),
        X = as.numeric(X),
        Y = as.numeric(Y)
      ) %>%
      sf::st_as_sf(
        coords = c("X", "Y"),
        crs = 4326,
        remove = FALSE
      ) %>%
      sf::st_transform(26910)
    
    if (nrow(nodes_utm) < 3) {
      stop(
        paste(
          "At least three mapped PTM node predictions",
          "are required to create the PNG."
        )
      )
    }
    
    # ------------------------------------------------------------
    # Recreate the same interpolation used by the Leaflet map
    # ------------------------------------------------------------
    
    risk_surface <- make_entrainment_risk_surface(
      nodes = nodes_utm,
      grid_cell_size = 300,
      idp = 2,
      nmax = 6,
      maximum_distance = 300000
    )
    
    risk_grid <- risk_surface$grid %>%
      sf::st_make_valid() %>%
      sf::st_transform(4326)
    
    nodes_wgs84 <- sf::st_transform(
      nodes_utm,
      4326
    )
    
    channels_wgs84 <- delta_channels %>%
      sf::st_make_valid() %>%
      sf::st_transform(4326)
    
    boundary_wgs84 <- delta_boundary %>%
      sf::st_make_valid() %>%
      sf::st_transform(4326)
    
    # ------------------------------------------------------------
    # Use the visible Leaflet frame
    # ------------------------------------------------------------
    
    bounds <- normalize_leaflet_bounds(
      bounds = map_bounds,
      fallback_geometry = delta_boundary
    )
    
    viewport_bbox <- sf::st_bbox(
      c(
        xmin = bounds$west,
        xmax = bounds$east,
        ymin = bounds$south,
        ymax = bounds$north
      ),
      crs = sf::st_crs(4326)
    )
    
    viewport_polygon <- sf::st_as_sfc(
      viewport_bbox
    )
    
    # Crop the large grid before plotting to reduce PNG render time.
    risk_grid_visible <- suppressWarnings(
      sf::st_crop(
        risk_grid,
        viewport_bbox
      )
    )
    
    channels_visible <- suppressWarnings(
      sf::st_crop(
        channels_wgs84,
        viewport_bbox
      )
    )
    
    boundary_visible <- suppressWarnings(
      sf::st_crop(
        boundary_wgs84,
        viewport_bbox
      )
    )
    
    node_is_visible <- lengths(
      sf::st_intersects(
        nodes_wgs84,
        viewport_polygon
      )
    ) > 0
    
    nodes_visible <- nodes_wgs84[
      node_is_visible,
      ,
      drop = FALSE
    ]
    
    # ------------------------------------------------------------
    # Prepare percentage-label coordinates
    # ------------------------------------------------------------
    
    if (nrow(nodes_visible) > 0) {
      
      node_coordinates <- sf::st_coordinates(
        nodes_visible
      )
      
      node_labels <- nodes_visible %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(
          longitude = node_coordinates[, "X"],
          latitude = node_coordinates[, "Y"],
          Entrainment_Label = paste0(
            round(entrainment, 0),
            "%"
          )
        )
      
    } else {
      
      node_labels <- data.frame(
        longitude = numeric(),
        latitude = numeric(),
        Entrainment_Label = character()
      )
    }
    
    # ------------------------------------------------------------
    # Prepare Event Horizon geometry for the 7-day export
    # ------------------------------------------------------------
    
    event_horizon_visible <- NULL
    event_horizon_distance <- NA_real_
    
    if (
      !is.null(eh_df) &&
      is.data.frame(eh_df) &&
      nrow(eh_df) > 0 &&
      "Prediction_Final" %in% names(eh_df)
    ) {
      
      event_horizon_distance <- suppressWarnings(
        as.numeric(
          eh_df$Prediction_Final[1]
        )
      )
      
      if (
        is.finite(event_horizon_distance) &&
        event_horizon_distance > 0
      ) {
        
        event_horizon <- make_event_horizon_path_geometry(
          distance_miles = event_horizon_distance,
          regions = c("OMR")
        ) %>%
          sf::st_make_valid() %>%
          sf::st_transform(4326)
        
        event_horizon_visible <- suppressWarnings(
          sf::st_crop(
            event_horizon,
            viewport_bbox
          )
        )
        
        if (nrow(event_horizon_visible) == 0) {
          event_horizon_visible <- NULL
        }
      }
    }
    
    # ------------------------------------------------------------
    # Extract figure metadata
    # ------------------------------------------------------------
    
    first_nonempty_value <- function(
    data,
    column_name,
    fallback = NA_character_
    ) {
      
      if (
        is.null(data) ||
        !column_name %in% names(data)
      ) {
        return(fallback)
      }
      
      values <- as.character(
        data[[column_name]]
      )
      
      values <- values[
        !is.na(values) &
          nzchar(trimws(values))
      ]
      
      if (length(values) == 0) {
        return(fallback)
      }
      
      values[1]
    }
    
    scenario_name <- first_nonempty_value(
      df,
      "Scenario_Name",
      "Unnamed scenario"
    )
    
    archive_date <- first_nonempty_value(
      df,
      "Archive_Date"
    )
    
    omri_scenario <- first_nonempty_value(
      df,
      "OMRI_Scenario"
    )
    
    window_end_date <- NA_character_
    
    if (
      "Window_End_Date" %in% names(df) &&
      any(!is.na(df$Window_End_Date))
    ) {
      
      parsed_window_date <- as.Date(
        df$Window_End_Date,
        origin = "1970-01-01"
      )
      
      parsed_window_date <- parsed_window_date[
        !is.na(parsed_window_date)
      ]
      
      if (length(parsed_window_date) > 0) {
        window_end_date <- format(
          parsed_window_date[1],
          "%B %d, %Y"
        )
      }
    }
    
    subtitle_parts <- c(
      paste0(
        "Scenario: ",
        scenario_name
      ),
      if (!is.na(window_end_date)) {
        paste0(
          "Assessment/window end: ",
          window_end_date
        )
      } else {
        NULL
      },
      if (
        !is.na(archive_date) &&
        nzchar(archive_date)
      ) {
        paste0(
          "Archive: ",
          archive_date
        )
      } else {
        NULL
      },
      if (
        !is.na(omri_scenario) &&
        nzchar(omri_scenario)
      ) {
        paste0(
          "OMRI: ",
          omri_scenario
        )
      } else {
        NULL
      },
      paste0(
        "Selected risk level: ",
        sprintf("%.0f", threshold),
        "%"
      ),
      if (is.finite(event_horizon_distance)) {
        paste0(
          "Event Horizon: ",
          sprintf(
            "%.1f",
            event_horizon_distance
          ),
          " river miles"
        )
      } else {
        NULL
      }
    )
    
    subtitle_text <- paste(
      subtitle_parts,
      collapse = "  |  "
    )
    
    # ------------------------------------------------------------
    # Use the same continuous blue-to-red scale as Leaflet
    # ------------------------------------------------------------
    
    risk_palette_spec <- threshold_centered_risk_palette(threshold)
    risk_colors <- risk_palette_spec$colors
    
    # ------------------------------------------------------------
    # Build the static publication-style image
    # ------------------------------------------------------------
    
    plot <- ggplot2::ggplot() +
      
      ggplot2::geom_sf(
        data = boundary_visible,
        fill = "#F3F7F8",
        color = "#477A87",
        linewidth = 0.45
      ) +
      
      ggplot2::geom_sf(
        data = risk_grid_visible,
        ggplot2::aes(
          fill = risk
        ),
        color = NA,
        alpha = 0.72
      ) +
      
      ggplot2::geom_sf(
        data = channels_visible,
        color = "#285F73",
        linewidth = 0.30,
        alpha = 0.75
      )
    
    # Event Horizon is deliberately added after the interpolation
    # and channels so the line remains visible.
    if (!is.null(event_horizon_visible)) {
      
      plot <- plot +
        ggplot2::geom_sf(
          data = event_horizon_visible,
          color = "#65000B",
          linewidth = 1.05,
          alpha = 0.95
        )
    }
    
    if (nrow(nodes_visible) > 0) {
      
      plot <- plot +

        ggplot2::geom_point(
          data = node_labels,
          ggplot2::aes(
            x = longitude,
            y = latitude,
            fill = entrainment
          ),
          shape = 21,
          size = 4.0,
          color = "#252525",
          stroke = 0.9
        ) +
        
        ggrepel::geom_label_repel(
          data = node_labels,
          ggplot2::aes(
            x = longitude,
            y = latitude,
            label = Entrainment_Label
          ),
          size = 3.2,
          fontface = "bold",
          color = "#1F1F1F",
          fill = grDevices::adjustcolor(
            "white",
            alpha.f = 0.88
          ),
          box.padding = 0.35,
          point.padding = 0.30,
          min.segment.length = 0,
          segment.color = "#606060",
          segment.linewidth = 0.25,
          max.overlaps = Inf,
          seed = 123,
          label.size = 0.25,
          show.legend = FALSE
        )
    }
    caption_text <- paste0(
      "Circles represent PTM emulator predictions at supported DSM2 nodes. ",
      "Background colors are inverse-distance interpolation between nearby nodes.",
      if (is.finite(event_horizon_distance)) {
        " The dark-red channel reach represents the predicted Event Horizon."
      } else {
        ""
      }
    )
    
    caption_text <- paste(
      strwrap(
        caption_text,
        width = 175
      ),
      collapse = "\n"
    )
    plot +
      ggplot2::scale_fill_gradientn(
        colours = risk_colors,
        limits = c(0, 100),
        breaks = c(
          0,
          20,
          40,
          60,
          80,
          100
        ),
        name = paste0("Predicted\nentrainment (%)\nWhite = ", sprintf("%.0f", threshold), "%"),
        oob = scales::squish
      ) +

      ggplot2::coord_sf(
        xlim = c(
          bounds$west,
          bounds$east
        ),
        ylim = c(
          bounds$south,
          bounds$north
        ),
        expand = FALSE,
        datum = NA,
        crs = sf::st_crs(4326)
      ) +
      ggplot2::labs(
        title = title,
        subtitle = subtitle_text,
        caption = paste(caption_text, map_export_caption(TRUE), sep = "\n")
      ) +
      ggplot2::theme_void(
        base_size = 13
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 19,
          face = "bold",
          color = "#1F1F1F",
          margin = ggplot2::margin(
            b = 5
          )
        ),
        plot.subtitle = ggplot2::element_text(
          size = 11.5,
          color = "#333333",
          lineheight = 1.15,
          margin = ggplot2::margin(
            b = 10
          )
        ),
        plot.caption = ggplot2::element_text(
          size = 9.5,
          hjust = 0,
          color = "#404040",
          lineheight = 1.15,
          margin = ggplot2::margin(
            t = 9
          )
        ),
        legend.position = "right",
        legend.title = ggplot2::element_text(
          face = "bold",
          size = 10.5
        ),
        legend.text = ggplot2::element_text(
          size = 9.5
        ),
        legend.key.height = grid::unit(
          0.55,
          "in"
        ),
        plot.background = ggplot2::element_rect(
          fill = "white",
          color = NA
        ),
        panel.background = ggplot2::element_rect(
          fill = "#F8FAFB",
          color = "#AAB8BF",
          linewidth = 0.5
        ),
        plot.margin = ggplot2::margin(
          t = 16,
          r = 18,
          b = 12,
          l = 18
        )
      )
  }
  
  make_eh_png_plot <- function(df, title) {
    validate(need(nrow(df) > 0, "Run the model before downloading the map."))
    
    geom <- make_event_geometry(df$Prediction_Final[1])
    eh_line <- st_transform(geom$high_line, 26910)
    eh_point <- st_transform(geom$point, 26910)
    
    ggplot() +
      geom_sf(
        data = delta_boundary,
        fill = "#F7FBFC",
        color = "#2B6573",
        linewidth = 0.5
      ) +
      geom_sf(
        data = delta_channels,
        color = "#6BAED6",
        linewidth = 0.3,
        alpha = 0.70
      ) +
      geom_sf(
        data = eh_line,
        color = "#B2182B",
        linewidth = 2.2
      ) +
      geom_sf(
        data = eh_point,
        color = "#B2182B",
        fill = "#B2182B",
        shape = 21,
        size = 4.5
      ) +
      coord_sf(datum = NA) +
      labs(
        title = title,
        subtitle = paste0(
          "Predicted Event Horizon: ",
          sprintf("%.0f", df$Prediction_Final[1]),
          " river miles at ",
          df$Risk_Level_Percent[1],
          "% risk"
        ),
        caption = paste(
          "The red reach represents the predicted upstream Event Horizon distance.",
          map_export_caption(FALSE), sep = "\n"
        )
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 19, face = "bold", color = "#1F1F1F"),
        plot.subtitle = element_text(size = 13, color = "#333333"),
        plot.caption = element_text(size = 11, hjust = 0, color = "#404040"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
  }
  
  # ===============================================================
  # Weekly Assessment scenario-name helpers
  # ===============================================================
  
  clean_scenario_component <- function(value) {
    
    value <- trimws(
      as.character(value)[1]
    )
    
    if (
      is.na(value) ||
      !nzchar(value)
    ) {
      return("")
    }
    
    # Keep letters, numbers, underscores, and minus signs.
    value <- gsub(
      "[^A-Za-z0-9_-]+",
      "_",
      value
    )
    
    # Collapse repeated underscores.
    value <- gsub(
      "_+",
      "_",
      value
    )
    
    # Remove leading/trailing underscores while preserving a
    # leading minus sign in scenarios such as -2000.
    value <- gsub(
      "^_+|_+$",
      "",
      value
    )
    
    value
  }
  # ===============================================================
  # Archive selector controls
  # ===============================================================
  
  available_archive_dates <- reactive({
    list_archive_dates()
  })
  
  
  output$current_archive_date_ui <- renderUI({
    
    choices <- available_archive_dates()
    
    selectInput(
      "current_archive_date",
      "Archive Run Date:",
      choices = choices,
      selected = if (length(choices) > 0) choices[1] else NULL
    )
  })
  
  
  output$forecast_archive_date_ui <- renderUI({
    choices <- available_archive_dates()
    selectInput(
      "forecast_archive_date",
      "Archive Week:",
      choices = c(
        "Select an archive week..." = "",
        stats::setNames(choices, choices)
      ),
      selected = ""
    )
  })


  output$forecast_archive_scenario_ui <- renderUI({
    req(
      identical(input$forecast_flow_source, "forecast"),
      nzchar(input$forecast_archive_date)
    )
    scenarios <- archive_scenario_choices(input$forecast_archive_date)
    selectInput(
      "forecast_archive_scenario",
      "OMRI Forecast Scenario:",
      choices = c(
        "Select an OMRI scenario..." = "",
        stats::setNames(scenarios, paste0("OMRI ", scenarios))
      ),
      selected = ""
    )
  })
  weekly_assessment_default_name <- reactive({
    
    archive_date <- input$forecast_archive_date
    
    if (
      is.null(archive_date) ||
      !nzchar(trimws(archive_date))
    ) {
      return("")
    }
    
    archive_date <- clean_scenario_component(
      archive_date
    )
    
    flow_source <- input$forecast_flow_source
    
    # ------------------------------------------------------------
    # Observed-flow Weekly Assessment
    # ------------------------------------------------------------
    
    if (
      is.null(flow_source) ||
      identical(flow_source, "observed")
    ) {
      
      return(
        paste0(
          archive_date,
          "_OBS_01"
        )
      )
    }
    
    # ------------------------------------------------------------
    # Forecast-OMRI Weekly Assessment
    # ------------------------------------------------------------
    
    omri_scenario <- input$forecast_archive_scenario
    
    if (
      is.null(omri_scenario) ||
      !nzchar(trimws(omri_scenario))
    ) {
      return("")
    }
    
    omri_scenario <- clean_scenario_component(
      omri_scenario
    )
    
    paste0(
      archive_date,
      "_OMRI_",
      omri_scenario,
      "_01"
    )
  })
  observeEvent(
    weekly_assessment_default_name(),
    {
      
      default_name <- weekly_assessment_default_name()
      
      updateTextInput(
        session = session,
        inputId = "forecast_ptm_archive_name",
        value = default_name
      )
      
      updateTextInput(
        session = session,
        inputId = "forecast_eco_archive_name",
        value = default_name
      )
    },
    ignoreInit = FALSE
  )

  output$current_ptm_archive_summary <- renderUI({
    req(input$current_archive_date)
    tags$div(
      class = "alert alert-light",
      tags$b("Selected archive: "),
      input$current_archive_date,
      tags$br(),
      "Measured rows will be used."
    )
  })


  weekly_selected_inputs <- reactive({
    req(nzchar(input$forecast_archive_date))
    flow_source <- input$forecast_flow_source

    if (is.null(flow_source) || identical(flow_source, "observed")) {
      source_file <- archive_csv_files(input$forecast_archive_date)[1]
      validate(need(length(source_file) > 0, "No archive CSV file was found for the selected week."))
      weekly_measured_7day_inputs(read_archive_csv(source_file, require_xgeo=FALSE))
    } else {
      req(nzchar(input$forecast_archive_scenario))
      source_file <- archive_file_for_scenario(
        input$forecast_archive_date,
        input$forecast_archive_scenario
      )
      forecast_7day_inputs(read_archive_csv(source_file, require_xgeo=FALSE))
    }
  })


  format_weekly_flow <- function(value) {
    if (is.null(value) || length(value) == 0 || !is.finite(as.numeric(value)[1])) return("")
    format(round(as.numeric(value)[1]), big.mark = ",", scientific = FALSE, trim = TRUE)
  }


  output$forecast_ptm_exp_value <- renderText({ format_weekly_flow(weekly_selected_inputs()$EXP) })
  output$forecast_ptm_ver_value <- renderText({ format_weekly_flow(weekly_selected_inputs()$VER) })
  output$forecast_ptm_sac_value <- renderText({ format_weekly_flow(weekly_selected_inputs()$SAC) })
  output$forecast_ptm_east_value <- renderText({ format_weekly_flow(weekly_selected_inputs()$EAST) })
  # Keep DAYFLOW QGCD and precipitation-derived QPREC independent of gate
  # validation.  One missing DCC status must NOT blank unrelated inputs.
  weekly_reference_context <- reactive({
    w <- weekly_selected_inputs()
    req(w$Window_Start_Date, w$Window_End_Date)
    dates <- seq(as.Date(w$Window_Start_Date),
                 as.Date(w$Window_End_Date), by="day")
    archive_date <- input$forecast_archive_date
    src <- if (is.null(input$forecast_flow_source)) "observed" else input$forecast_flow_source
    file <- if (identical(src, "forecast")) {
      req(input$forecast_archive_scenario)
      archive_file_for_scenario(archive_date, input$forecast_archive_scenario)
    } else {
      archive_csv_files(archive_date)[1]
    }
    data <- read_archive_csv(file, require_xgeo=FALSE)
    measured_dates <- data$DATE[data$DATA_TYPE=="Measured"]
    list(data=data, dates=dates, forecast=identical(src,"forecast"),
         measured_date=if (length(measured_dates)) max(measured_dates) else as.Date(NA))
  })
  weekly_reference_qgcd <- reactive({
    weekly_qgcd_for_dates(weekly_reference_context()$dates)
  })
  weekly_reference_qprec <- reactive({
    ctx <- weekly_reference_context()
    tryCatch({
      x <- weekly_qprec_for_measured_date(ctx$measured_date,forecast=ctx$forecast)
      x$for_dates(ctx$dates)
    },error=function(e)e)
  })
  # Check the archive's *raw* gate value separately; the override control
  # must stay visible after a choice (otherwise the selector would disappear).
  weekly_reference_gate_raw <- reactive({
    ctx <- weekly_reference_context()
    tryCatch(weekly_archive_gate(ctx$data, forecast=ctx$forecast,
                                 dates=ctx$dates, allow_manual_override=FALSE),
             error=function(e) e)
  })
  weekly_reference_gate <- reactive({
    ctx <- weekly_reference_context()
    tryCatch(weekly_archive_gate(ctx$data, forecast=ctx$forecast,
                                 dates=ctx$dates), error=function(e) e)
  })
  output$forecast_weekly_gate_override_ui <- renderUI({
    raw_gate <- weekly_reference_gate_raw()
    if (!inherits(raw_gate,"error")) return(NULL)
    tags$div(
      class="alert alert-warning",
      style="margin-top:12px;margin-bottom:0;padding:10px 14px;",
      tags$strong("DCC gate status is missing in the selected GitHub archive"),
      tags$p(style="margin:5px 0 9px;",
        conditionMessage(raw_gate),
        " The local Excel copy may contain 0/1 values that are not present ",
        "in the GitHub copy used by this app."),
      selectInput("forecast_weekly_gate_override",
        "Optional scenario assumption (required to run models when DCC is absent):",
        choices=c("Choose gate status (no assumption)"="",
                  "Closed (0) — user assumption"="0",
                  "Open (1) — user assumption"="1"),
        selected=if (!is.null(input$forecast_weekly_gate_override))
          input$forecast_weekly_gate_override else "", width="380px"),
      tags$small("A selected assumption is applied to the entire 7-day window; ",
                 "the archive source is not changed. If valid archived DCC exists, ",
                 "the archived value always takes priority.")
    )
  })
  observeEvent(list(input$forecast_archive_date,
                    input$forecast_flow_source,
                    input$forecast_archive_scenario), {
    updateSelectInput(session,"forecast_weekly_gate_override",selected="")
  },ignoreInit=TRUE)

  weekly_reference_dcc <- reactive({
    ctx <- weekly_reference_context()
    gate <- weekly_reference_gate()
    if (inherits(gate,"error")) return(gate)
    tryCatch(weekly_dcc_flow(ctx$data, ctx$dates, gate, ctx$forecast),
             error=function(e) e)
  })
  output$forecast_ptm_qgcd_value <- renderText({
    format_weekly_flow(weekly_reference_qgcd())
  })
  output$forecast_ptm_qprec_value <- renderText({
    x <- weekly_reference_qprec()
    if (inherits(x,"error")) "Unavailable" else format_weekly_flow(x$value)
  })
  output$forecast_ptm_dcc_flow_value <- renderText({
    x <- weekly_reference_dcc()
    if (inherits(x,"error")) "Unavailable" else format_weekly_flow(x)
  })
  output$forecast_ptm_dcc_source_note <- renderUI({
    flow <- weekly_reference_dcc()
    if (inherits(flow,'error')) return(tags$span('DCC flow unavailable: ',conditionMessage(flow)))
    source <- attr(flow,'dcc_source')
    if (is.null(source)) source <- if (identical(as.numeric(weekly_reference_gate()),0))
      'Gate closed: forced 0 cfs' else 'Observed CDEC flow'
    tags$span('DCC source: ',source,'. ',
      'When gates are open and CDEC observations are missing, ',
      'the gap is estimated using 0.1846 × measured SAC + 306.52 cfs.')
  })
  output$forecast_ptm_dcc_status_value <- renderText({
    gate <- weekly_reference_gate()
    if (inherits(gate,"error")) return("Missing in archive — select status above")
    label <- if (gate == 1) "Open (1)" else "Closed (0)"
    if (identical(attr(gate,"gate_source"),
                  "USER_ASSUMPTION_MISSING_ARCHIVE_DCC")) {
      paste0(label," — assumed")
    } else paste0(label," — archive")
  })
  output$forecast_ptm_qprec_source_note <- renderUI({
    x <- weekly_reference_qprec()
    if (inherits(x,"error")) return(tags$span("Precipitation unavailable: ",conditionMessage(x)))
    tags$span("Rainfall source: ",x$source,". Window: ",
      format(x$window_start,"%b %d"),"–",format(x$through,"%b %d, %Y"),
      ". Five-day DAYFLOW-style runoff; 682,230 acres; daily monthly-equivalent scaled by calendar-month length. ",
      if (identical(x$forecast_method,"PERSIST_LAST_MEASURED_DAILY_RUNOFF"))
        tags$strong(" Forecast precipitation is not a verified CNRFC forecast: the last measured SCKC1 runoff rate was held constant for all seven forecast days. "),
      if (identical(x$forecast_method,"CNRFC_ARCHIVE_QPF") && !is.na(x$forecast_issue))
        tags$span(" Archived CNRFC QPF issue: ",x$forecast_issue,". "),
      if (length(x$estimated_dates)) tags$span(
        "Estimated missing SCKC1 rainfall for: ",paste(as.character(x$estimated_dates),collapse=", "),
        " (interpolated from adjacent measured days, not assumed zero). "),
      tags$a(href=x$source_url,target="_blank", "Precipitation source"))
  })

  output$forecast_eco_sac_value <- renderText({ format_weekly_flow(weekly_selected_inputs()$SAC) })
  output$forecast_eco_yol_value <- renderText({ format_weekly_flow(weekly_selected_inputs()$YOL) })
  output$forecast_eco_moke_value <- renderText({ format_weekly_flow(weekly_selected_inputs()$MOKE) })
  output$forecast_eco_dcc_value <- renderText({
    gate <- weekly_reference_gate()
    if (inherits(gate,"error")) return("Missing in archive — select status above")
    label <- if (gate == 1) "Open (1)" else "Closed (0)"
    if (identical(attr(gate,"gate_source"),
                  "USER_ASSUMPTION_MISSING_ARCHIVE_DCC")) {
      paste0(label," — assumed")
    } else paste0(label," — archive")
  })


  observeEvent(
    list(
      input$forecast_archive_date,
      input$forecast_flow_source,
      input$forecast_archive_scenario
    ),
    {
      flow_source <- input$forecast_flow_source
      scenario_name <- if (
        !is.null(flow_source) &&
        identical(flow_source, "forecast") &&
        !is.null(input$forecast_archive_date) &&
        nzchar(input$forecast_archive_date)
      ) {
        week_token <- gsub("[^0-9A-Za-z]+", "", input$forecast_archive_date)
        paste0("Week_", week_token, "_01")
      } else {
        "Week_Obs_01"
      }
      updateTextInput(session, "forecast_ptm_archive_name", value = scenario_name)
      updateTextInput(session, "forecast_eco_archive_name", value = scenario_name)
    },
    ignoreInit = FALSE
  )


  register_condition <- function(
    prefix,
    condition_label
  ) {
    
    is_current <- identical(
      prefix,
      "current"
    )
    
    
    ptm_result <- eventReactive(
      input[[paste0(
        "run_",
        prefix,
        "_ptm"
      )]],
      {
        
        input_method <- input[[paste0(
          prefix,
          "_input_method"
        )]]
        
        if (identical(input_method, "single")) {
          
          delta_values <- get_exploration_delta_inputs(prefix)
          result <- make_new_ptm_results(
            condition = condition_label,
            scenario_name = input[[paste0(
              prefix,
              "_ptm_name"
            )]],
            exp = parse_flow_input(
              input[[paste0(
              prefix,
              "_ptm_exp"
            )]]
            ),
            ver = parse_flow_input(
              input[[paste0(
              prefix,
              "_ptm_ver"
            )]]
            ),
            sac = parse_flow_input(
              input[[paste0(
              prefix,
              "_ptm_sac"
            )]]
            ),
            east = parse_flow_input(
              input[[paste0(
              prefix,
              "_ptm_east"
            )]]
            ),
            qgcd = delta_values$qgcd,
            qprec = delta_values$qprec,
            dcc_flow = delta_values$dcc_flow
          )
          
          run_name <- input[[paste0(
            prefix,
            "_ptm_name"
          )]]
          
        } else {
          
          req(
            identical(
              input_method,
              "folder"
            )
          )
          
          archive_date <- input[[paste0(
            prefix,
            "_archive_date"
          )]]
          
          run_name <- input[[paste0(
            prefix,
            "_ptm_archive_name"
          )]]
          
          if (is_current) {

            result <- make_archive_ptm_current(
              archive_date = archive_date,
              scenario_name = run_name
            )

          } else {

            req(nzchar(archive_date))

            if (
              is.null(input$forecast_flow_source) ||
              identical(input$forecast_flow_source, "observed")
            ) {
              result <- make_weekly_ptm_observed(
                archive_date = archive_date,
                scenario_name = run_name
              )
            } else {
              req(nzchar(input$forecast_archive_scenario))
              result <- make_weekly_ptm_forecast(
                archive_date = archive_date,
                omri_scenario = input$forecast_archive_scenario,
                scenario_name = run_name
              )
            }
          }
        }
        
        add_saved_run(
          run_name,
          condition_label,
          "PTM",
          result
        )
        
        result
      }
    )
    
    
    eco_result <- eventReactive(
      input[[paste0(
        "run_",
        prefix,
        "_eco"
      )]],
      {
        
        input_method <- input[[paste0(
          prefix,
          "_input_method"
        )]]
        
        if (identical(input_method, "single")) {
          
          result <- make_new_eco_results(
            condition = condition_label,
            scenario_name = input[[paste0(
              prefix,
              "_eco_name"
            )]],
            sac = parse_flow_input(
              input[[paste0(
              prefix,
              "_eco_sac"
            )]]
            ),
            yol = parse_flow_input(
              input[[paste0(
              prefix,
              "_eco_yol"
            )]]
            ),
            moke = parse_flow_input(
              input[[paste0(
              prefix,
              "_eco_moke"
            )]]
            ),
            dcc = as.numeric(
              input[[paste0(
                prefix,
                "_eco_dcc"
              )]]
            )
          )
          
          run_name <- input[[paste0(
            prefix,
            "_eco_name"
          )]]
          
        } else {
          
          archive_date <- input[[paste0(
            prefix,
            "_archive_date"
          )]]

          run_name <- input[[paste0(
            prefix,
            "_eco_archive_name"
          )]]

          if (is_current) {
            result <- make_archive_eco_current(
              archive_date = archive_date,
              scenario_name = run_name
            )
          } else {
            req(nzchar(archive_date))

            if (
              is.null(input$forecast_flow_source) ||
              identical(input$forecast_flow_source, "observed")
            ) {
              result <- make_weekly_eco_observed(
                archive_date = archive_date,
                scenario_name = run_name
              )
            } else {
              req(nzchar(input$forecast_archive_scenario))
              result <- make_weekly_eco_forecast(
                archive_date = archive_date,
                omri_scenario = input$forecast_archive_scenario,
                scenario_name = run_name
              )
            }
          }
        }
        
        add_saved_run(
          run_name,
          condition_label,
          "ECO-PTM",
          result
        )
        
        result
      }
    )
    
    
    eh_result <- eventReactive(
      input[[paste0(
        "run_",
        prefix,
        "_ptm"
      )]],
      {
        
        input_method <- input[[paste0(
          prefix,
          "_input_method"
        )]]
        
        risk <- as.numeric(
          input[[paste0(
            prefix,
            "_ptm_threshold"
          )]]
        )
        
        if (identical(input_method, "single")) {
          
          delta_values <- get_exploration_delta_inputs(prefix)
          result <- make_new_eh_result(
            condition = condition_label,
            scenario_name = input[[paste0(
              prefix,
              "_ptm_name"
            )]],
            exp = parse_flow_input(
              input[[paste0(
              prefix,
              "_ptm_exp"
            )]]
            ),
            ver = parse_flow_input(
              input[[paste0(
              prefix,
              "_ptm_ver"
            )]]
            ),
            east = parse_flow_input(
              input[[paste0(
              prefix,
              "_ptm_east"
            )]]
            ),
            sac = parse_flow_input(input[[paste0(prefix, "_ptm_sac")]]),
            qgcd = delta_values$qgcd,
            qprec = delta_values$qprec,
            risk = risk
          )
          
          run_name <- input[[paste0(
            prefix,
            "_ptm_name"
          )]]
          
        } else {
          
          archive_date <- input[[paste0(
            prefix,
            "_archive_date"
          )]]
          
          run_name <- input[[paste0(
            prefix,
            "_ptm_archive_name"
          )]]
          
          if (is_current) {

            result <- make_archive_eh_current(
              archive_date = archive_date,
              scenario_name = run_name,
              risk = risk
            )

          } else {

            req(nzchar(archive_date))

            if (
              is.null(input$forecast_flow_source) ||
              identical(input$forecast_flow_source, "observed")
            ) {
              result <- make_weekly_eh_observed(
                archive_date = archive_date,
                scenario_name = run_name,
                risk = risk
              )
            } else {
              req(nzchar(input$forecast_archive_scenario))
              result <- make_weekly_eh_forecast(
                archive_date = archive_date,
                omri_scenario = input$forecast_archive_scenario,
                scenario_name = run_name,
                risk = risk
              )
            }
          }
        }
        
        add_saved_run(
          run_name,
          condition_label,
          "Event Horizon",
          result
        )
        
        result
      }
    )
    
    
    # ---------------------------------------------------------------
    # Archive-current time-window controls
    # ---------------------------------------------------------------
    
    output[[paste0(
      prefix,
      "_ptm7_map_date_ui"
    )]] <- renderUI({
      
      req(
        ptm_result()
      )
      
      result_data <- ptm_result()
      
      if (
        !"Window_End_Date" %in% names(result_data)
      ) {
        return(NULL)
      }
      
      dates <- result_data %>%
        filter(
          Model == "PTM Emulator 7-Day Entrainment",
          !is.na(Window_End_Date)
        ) %>%
        pull(
          Window_End_Date
        ) %>%
        as.Date(origin = "1970-01-01") %>%
        unique() %>%
        sort()
      
      if (
        length(dates) <= 1
      ) {
        return(NULL)
      }
      
      sliderInput(
        paste0(
          prefix,
          "_ptm7_map_date"
        ),
        NULL,
        min = min(dates),
        max = max(dates),
        value = max(dates),
        step = 1,
        timeFormat = "%b %d, %Y",
        animate = animationOptions(
          interval = 1400,
          loop = TRUE
        )
      )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm7_timeseries_nodes_ui"
    )]] <- renderUI({
      
      req(
        ptm_result()
      )
      
      data <- ptm_result() %>%
        filter(
          Model == "PTM Emulator 7-Day Entrainment"
        ) %>%
        distinct(
          DSM2_Node,
          Location
        ) %>%
        arrange(
          suppressWarnings(
            as.numeric(
              DSM2_Node
            )
          )
        )
      
      selectizeInput(
        paste0(
          prefix,
          "_ptm7_timeseries_nodes"
        ),
        "DSM2 Nodes Displayed:",
        choices = stats::setNames(
          data$DSM2_Node,
          paste0(
            data$DSM2_Node,
            " - ",
            data$Location
          )
        ),
        selected = head(
          data$DSM2_Node,
          5
        ),
        multiple = TRUE,
        options = list(
          plugins = list(
            "remove_button"
          )
        )
      )
    })
    
    
    output[[paste0(
      prefix,
      "_eh_map_date_ui"
    )]] <- renderUI({
      
      req(
        eh_result()
      )
      
      result_data <- eh_result()
      
      if (
        !"Window_End_Date" %in% names(result_data)
      ) {
        return(NULL)
      }
      
      dates <- result_data %>%
        filter(
          !is.na(Window_End_Date)
        ) %>%
        pull(
          Window_End_Date
        ) %>%
        as.Date(origin = "1970-01-01") %>%
        unique() %>%
        sort()
      
      if (
        length(dates) <= 1
      ) {
        return(NULL)
      }
      
      sliderInput(
        paste0(
          prefix,
          "_eh_map_date"
        ),
        "Map Window End Date:",
        min = min(dates),
        max = max(dates),
        value = max(dates),
        step = 1,
        timeFormat = "%b %d, %Y",
        animate = animationOptions(
          interval = 1400,
          loop = TRUE
        )
      )
    })
    
    
    # ---------------------------------------------------------------
    # PTM displays
    # ---------------------------------------------------------------
    
    output[[paste0(
      prefix,
      "_ptm7_timeseries"
    )]] <- renderPlotly({
      
      make_ptm_timeseries(
        ptm_result(),
        input[[paste0(
          prefix,
          "_ptm7_timeseries_nodes"
        )]],
        paste(
          condition_label,
          ": 7-Day Rolling Entrainment Prediction"
        )
      )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm7_plot"
    )]] <- renderPlotly({
      
      data <- latest_result_window(
        ptm_result(),
        "PTM Emulator 7-Day Entrainment"
      )
      
      make_ptm_bar(
        data,
        paste(
          condition_label,
          ": PTM Emulator 7-Day Entrainment"
        ),
        show_plot_title = FALSE
      )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm30_plot"
    )]] <- renderPlotly({
      
      data <- latest_result_window(
        ptm_result(),
        "PTM Emulator 30-Day Entrainment"
      )
      
      make_ptm_bar(
        data,
        paste(
          condition_label,
          ": PTM Emulator 30-Day Entrainment"
        ),
        show_plot_title = FALSE
      )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm7_map"
    )]] <- renderLeaflet({
      
      ptm_data <- selected_result_window(
        ptm_result(),
        "PTM Emulator 7-Day Entrainment",
        input[[paste0(
          prefix,
          "_ptm7_map_date"
        )]]
      )
      
      make_combined_ptm_eh_map(
        ptm_df = ptm_data,
        eh_df = selected_eh_result(),
        threshold = as.numeric(
          input[[paste0(
            prefix,
            "_ptm_threshold"
          )]]
        )
      )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm30_map"
    )]] <- renderLeaflet({
      
      data <- latest_result_window(
        ptm_result(),
        "PTM Emulator 30-Day Entrainment"
      )
      
      make_ptm_map(
        data,
        as.numeric(
          input[[paste0(
            prefix,
            "_ptm_threshold"
          )]]
        )
      )
    })
    
    
    output[[paste0(
      "download_",
      prefix,
      "_ptm7_map"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_PTM_7day_Event_Horizon_map_",
          format(
            Sys.Date(),
            "%Y-%m-%d"
          ),
          ".png"
        )
      },
      
      contentType = "image/png",
      
      content = function(file) {
        
        ptm_data <- selected_result_window(
          ptm_result(),
          "PTM Emulator 7-Day Entrainment",
          input[[paste0(
            prefix,
            "_ptm7_map_date"
          )]]
        )
        
        current_bounds <- input[[paste0(
          prefix,
          "_ptm7_map_bounds"
        )]]
        
        plot <- make_ptm_png_plot(
          df = ptm_data,
          threshold = input[[paste0(
            prefix,
            "_ptm_threshold"
          )]],
          title = paste(
            condition_label,
            ": 7-Day PTM Entrainment and Event Horizon"
          ),
          map_bounds = current_bounds,
          eh_df = selected_eh_result()
        )
        export_dimensions <- ptm_png_dimensions(
          map_bounds = current_bounds,
          fallback_geometry = delta_boundary
        )
        ggplot2::ggsave(
          filename = file,
          plot = plot,
          device = "png",
          width = export_dimensions$width,
          height = export_dimensions$height,
          units = "in",
          dpi = 300,
          bg = "white",
          limitsize = FALSE
        )
      }
    )
    
    
    output[[paste0(
      "download_",
      prefix,
      "_ptm30_map"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_PTM_30day_map_",
          format(
            Sys.Date(),
            "%Y-%m-%d"
          ),
          ".png"
        )
      },
      
      contentType = "image/png",
      
      content = function(file) {
        
        ptm_data <- latest_result_window(
          ptm_result(),
          "PTM Emulator 30-Day Entrainment"
        )
        
        current_bounds <- input[[paste0(
          prefix,
          "_ptm30_map_bounds"
        )]]
        
        plot <- make_ptm_png_plot(
          df = ptm_data,
          threshold = input[[paste0(
            prefix,
            "_ptm_threshold"
          )]],
          title = paste(
            condition_label,
            ": 30-Day PTM Entrainment Risk Map"
          ),
          map_bounds = current_bounds,
          eh_df = NULL
        )

        export_dimensions <- ptm_png_dimensions(
          map_bounds = current_bounds,
          fallback_geometry = delta_boundary
        )
        
        ggplot2::ggsave(
          filename = file,
          plot = plot,
          device = "png",
          width = export_dimensions$width,
          height = export_dimensions$height,
          units = "in",
          dpi = 300,
          bg = "white",
          limitsize = FALSE
        )
      }
    )
    
    
    output[[paste0(
      prefix,
      "_ptm7_table"
    )]] <- renderTable({
      
      latest_result_window(
        ptm_result(),
        "PTM Emulator 7-Day Entrainment"
      ) %>%
        transmute(
          DSM2_Node,
          Location,
          Entrainment_Percentage = fmt_int(
            Prediction_Final
          )
        ) %>%
        arrange(
          suppressWarnings(
            as.numeric(
              DSM2_Node
            )
          ),
          DSM2_Node
        )
    })
    
    
    output[[paste0(
      prefix,
      "_ptm30_table"
    )]] <- renderTable({
      
      latest_result_window(
        ptm_result(),
        "PTM Emulator 30-Day Entrainment"
      ) %>%
        transmute(
          DSM2_Node,
          Location,
          Entrainment_Percentage = fmt_int(
            Prediction_Final
          )
        ) %>%
        arrange(
          suppressWarnings(
            as.numeric(
              DSM2_Node
            )
          ),
          DSM2_Node
        )
    })
    
    
    # ---------------------------------------------------------------
    # ECO-PTM displays
    # ---------------------------------------------------------------
    
    output[[paste0(
      prefix,
      "_eco_table"
    )]] <- renderTable({
      
      eco_result() %>%
        transmute(
          Model,
          
          Predicted_Population_Percentage = fmt_int(
            Prediction_Final
          ),
          
          SAC = fmt_int(
            SAC
          ),
          
          YOL = fmt_int(
            YOL
          ),
          
          MOKE = fmt_int(
            MOKE
          ),
          
          DCC
        )
    })
    
    
    # ---------------------------------------------------------------
    # Event Horizon displays
    # ---------------------------------------------------------------
    
    output[[paste0(
      prefix,
      "_eh_timeseries"
    )]] <- renderPlotly({
      
      make_eh_timeseries(
        eh_result(),
        paste(
          condition_label,
          ": Event Horizon Rolling Predictions"
        )
      )
    })
    
    
    selected_eh_result <- reactive({
      
      result <- eh_result()
      
      if (
        "Window_End_Date" %in% names(result) &&
        any(
          !is.na(
            result$Window_End_Date
          )
        )
      ) {
        
        selected_date <- input[[paste0(
          prefix,
          "_ptm7_map_date"
        )]]
        
        if (
          !is.null(selected_date)
        ) {
          
          selected_date <- as.Date(
            selected_date,
            origin = "1970-01-01"
          )
          
          result <- result %>%
            filter(
              Window_End_Date == selected_date
            )
        }
        
        if (nrow(result) == 0) {
          
          latest_date <- max(
            eh_result()$Window_End_Date,
            na.rm = TRUE
          )
          
          result <- eh_result() %>%
            filter(
              Window_End_Date == latest_date
            )
        }
      }
      
      result
    })
  
    output[[paste0(
      prefix,
      "_eh_summary"
    )]] <- renderUI({
      
      result <- selected_eh_result()
      
      req(
        nrow(result) > 0
      )
      
      risk_level <- as.numeric(
        result$Risk_Level_Percent[1]
      )
      
      river_miles <- as.numeric(
        result$Prediction_Final[1]
      )
      
      tags$div(
        class = "event-horizon-summary",
        
        tags$div(
          class = "event-horizon-summary-title",
          "Event Horizon"
        ),
        
        tags$div(
          class = "event-horizon-summary-text",
          
          "The predicted upstream extent (distance) of the Event Horizon, ",
          "measured in river miles from Clifton Court Forebay, under the ",
          "specified hydrologic conditions and selected entrainment risk level ",
          
          tags$strong(
            class = "event-horizon-highlight",
            paste0(
              risk_level,
              "%"
            )
          ),
          
          " is ",
          
          tags$strong(
            class = "event-horizon-highlight",
            paste0(
              sprintf(
                "%.1f",
                river_miles
              ),
              " miles"
            )
          ),
          
          "."
        )
      )
    })
    
    
    
    output[[paste0(
      prefix,
      "_eh_table"
    )]] <- renderTable({
      
      latest <- eh_result()
      
      if (
        "Window_End_Date" %in% names(latest) &&
        any(
          !is.na(
            latest$Window_End_Date
          )
        )
      ) {
        
        latest <- latest %>%
          filter(
            Window_End_Date == max(
              Window_End_Date,
              na.rm = TRUE
            )
          )
      }
      
      latest %>%
        transmute(
          Archive_Date = if (
            "Archive_Date" %in% names(.)
          ) {
            Archive_Date
          } else {
            NA_character_
          },
          
          OMRI_Scenario = if (
            "OMRI_Scenario" %in% names(.)
          ) {
            OMRI_Scenario
          } else {
            NA_character_
          },
          
          Window_End_Date = if (
            "Window_End_Date" %in% names(.)
          ) {
            format(
              as.Date(
                Window_End_Date,
                origin = "1970-01-01"
              ),
              "%m/%d/%Y"
            )
          } else {
            NA_character_
          },
          
          Scenario_Name,
          Risk_Level_Percent = fmt_int(
            Risk_Level_Percent
          ),

          Event_Horizon_Miles = fmt_one(
            Prediction_Final
          ),
          
          EXP = fmt_int(
            EXP
          ),
          
          VER = fmt_int(
            VER
          ),
          
          EAST = fmt_int(
            EAST
          ),
          
          XGEO = fmt_int(
            XGEO
          )
        )
    })
    
    
    output[[paste0(
      prefix,
      "_eh7_table"
    )]] <- renderTable({

      result <- eh_result()

      req(
        nrow(result) > 0
      )

      if (
        "Window_End_Date" %in% names(result) &&
        any(
          !is.na(
            result$Window_End_Date
          )
        )
      ) {
        result <- result %>%
          mutate(
            Observed_Date = as.Date(
              Window_End_Date,
              origin = "1970-01-01"
            )
          ) %>%
          arrange(
            Observed_Date
          ) %>%
          slice_tail(
            n = 7
          )
      } else {
        result <- result %>%
          mutate(
            Observed_Date = as.Date(NA)
          )
      }

      result %>%
        transmute(
          Observed_Date = ifelse(
            is.na(Observed_Date),
            NA_character_,
            format(
              Observed_Date,
              "%m/%d/%Y"
            )
          ),

          Event_Horizon_Miles = fmt_one(
            Prediction_Final
          )
        )
    })


    output[[paste0(
      prefix,
      "_eh_map"
    )]] <- renderLeaflet({
      
      make_eh_map(
        selected_eh_result()
      )
    })
    
    
    output[[paste0(
      "download_",
      prefix,
      "_eh_map"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_Event_Horizon_map_",
          Sys.Date(),
          ".png"
        )
      },
      
      contentType = "image/png",
      
      content = function(file) {
        
        plot <- make_eh_png_plot(
          selected_eh_result(),
          paste(
            condition_label,
            ": Event Horizon Map"
          )
        )
        
        ggsave(
          filename = file,
          plot = plot,
          device = "png",
          width = 13,
          height = 9,
          units = "in",
          dpi = 300,
          bg = "white"
        )
      }
    )
    
    
    output[[paste0(
      prefix,
      "_eh_scatter"
    )]] <- renderPlotly({

      input_method <- input[[paste0(
        prefix,
        "_input_method"
      )]]

      scatter_data <- if (
        (
          is_current &&
          identical(
            input_method,
            "folder"
          )
        ) ||
        (
          !is_current &&
          identical(
            input_method,
            "folder"
          ) &&
          (
            is.null(input$forecast_flow_source) ||
            identical(input$forecast_flow_source, "observed")
          )
        )
      ) {
        eh_result()
      } else {
        selected_eh_result()
      }

      make_eh_scatter(
        scatter_data,
        compact_title_below_legend = TRUE
      )
    })
    
    
    # ---------------------------------------------------------------
    # Downloads
    # ---------------------------------------------------------------
    
    output[[paste0(
      "download_",
      prefix,
      "_ptm"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_PTM_results_",
          Sys.Date(),
          ".csv"
        )
      },
      
      content = function(file) {
        
        write_csv(
          prepare_ptm_export(ptm_result()) %>%
            dplyr::mutate(
              dplyr::across(
                -dplyr::all_of(
                  c(
                    "X",
                    "Y"
                  )
                ),
                ~ if (is.numeric(.x)) {
                  round(.x)
                } else {
                  .x
                }
              ),
              X = round(
                as.numeric(X),
                digits = 6
              ),
              Y = round(
                as.numeric(Y),
                digits = 6
              )
            ),
          file
        )
      }
    )
    
    
    output[[paste0(
      "download_",
      prefix,
      "_eco"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_ECO_PTM_results_",
          Sys.Date(),
          ".csv"
        )
      },
      
      content = function(file) {
        
        write_csv(
          add_export_metadata(eco_result()) %>%
            mutate(
              across(
                where(
                  is.numeric
                ),
                ~ round(
                  .x
                )
              )
            ),
          file
        )
      }
    )
    
    
    output[[paste0(
      "download_",
      prefix,
      "_eh"
    )]] <- downloadHandler(
      
      filename = function() {
        
        paste0(
          prefix,
          "_Event_Horizon_result_",
          Sys.Date(),
          ".csv"
        )
      },
      
      content = function(file) {
        
        write_csv(
          add_export_metadata(eh_result()) %>%
            mutate(
              across(
                where(
                  is.numeric
                ),
                ~ round(
                  .x
                )
              )
            ),
          file
        )
      }
    )
  }
  
  
  register_condition(
    "current",
    "Observed Conditions"
  )
  
  register_condition(
    "forecast",
    "Forecast Conditions"
  )
  output$comparison_library_status <- renderUI({
    selected <- input$comparison_runs
    if (is.null(selected) || length(selected) < 2L) return(NULL)
    NULL
  })

  # Spatial interpolation is intentionally opt-in; plots/tables must not wait for it.
  comparison_map_request <- eventReactive(input$generate_library_map, {
    list(data=comparison_data(), model=isolate(input$comparison_model),
         threshold=isolate(input$comparison_map_threshold))
  }, ignoreInit=TRUE)
  output$comparison_spatial_map <- renderLeaflet({
    ccwd_safe_map_request(function() {
      request <- comparison_map_request()
      ccwd_fast_comparison_map(request$data, as.character(request$model)[1],request$threshold,'Library of Runs')
    },'Library of Runs')
  })

  output$comparison_run_selector <- renderUI({
    
    runs <- saved_runs()
    
    if (
      length(runs) == 0 ||
      is.null(input$comparison_model)
    ) {
      return(
        tags$div(
          class = "alert alert-info",
          "Run at least two scenarios before using Scenario Comparison."
        )
      )
    }
    
    selected_model <- input$comparison_model
    
    # Keep only saved entries that actually contain the model
    # currently selected in Comparison Controls.
    model_compatible <- vapply(
      runs,
      function(run_data) {
        
        is.data.frame(run_data) &&
          "Model" %in% names(run_data) &&
          any(
            as.character(run_data$Model) == selected_model,
            na.rm = TRUE
          )
      },
      logical(1)
    )
    
    compatible_runs <- runs[model_compatible]
    available <- names(compatible_runs)
    
    if (length(available) == 0) {
      return(
        tags$div(
          class = "alert alert-info",
          paste(
            "No saved runs contain results for",
            selected_model,
            ". Run additional compatible scenarios first."
          )
        )
      )
    }
    
    display_labels <- vapply(
      available,
      function(key) {
        
        run_data <- compatible_runs[[key]]
        
        scenario_name <- if (
          "Scenario_Name" %in% names(run_data) &&
          length(run_data$Scenario_Name) > 0
        ) {
          as.character(
            run_data$Scenario_Name[
              which(
                !is.na(run_data$Scenario_Name) &
                  nzchar(
                    trimws(
                      as.character(
                        run_data$Scenario_Name
                      )
                    )
                  )
              )[1]
            ]
          )
        } else {
          NA_character_
        }
        
        if (
          length(scenario_name) == 0 ||
          is.na(scenario_name) ||
          !nzchar(scenario_name)
        ) {
          scenario_name <- key
        }
        scenario_name
      },
      character(1)
    )
    
    # Guarantee unique visible labels if the same scenario name
    # was deliberately run more than once.
    display_labels <- make.unique(
      display_labels,
      sep = " - Run "
    )
    
    checkboxGroupInput(
      inputId = "comparison_runs",
      label = paste0(
        "Select Two or More Runs for ",
        selected_model,
        ":"
      ),
      choices = stats::setNames(
        available,
        display_labels
      ),
      selected = available,
      inline = TRUE
    )
  })
  
  comparison_data <- reactive({
    req(input$comparison_runs, input$comparison_model)
    runs <- saved_runs()

    selected <- lapply(
      input$comparison_runs,
      function(name) {
        run_data <- runs[[name]]

        if (is.null(run_data) || !is.data.frame(run_data)) {
          return(NULL)
        }

        required_columns <- c(
          "Saved_Run_ID",
          "Scenario_Name",
          "Condition",
          "Model",
          "DSM2_Node",
          "Location",
          "Region",
          "Prediction_Final",
          "Output_Unit",
          "Window_Start_Date",
          "Window_End_Date"
        )

        for (column_name in required_columns) {
          if (!column_name %in% names(run_data)) {
            run_data[[column_name]] <- NA
          }
        }

        run_data
      }
    )

    selected <- selected[!vapply(selected, is.null, logical(1))]

    validate(
      need(length(selected) > 0, "No saved runs are available for comparison.")
    )

    bind_rows(selected) %>%
      filter(Model == input$comparison_model)
  })

  output$comparison_table <- renderTable({
    comparison_table_data <- format_comparison_table(
      comparison_data()
    )

    if ("Window_Start_Date" %in% names(comparison_table_data)) {
      comparison_table_data$Window_Start_Date <- format(
        as.Date(
          comparison_table_data$Window_Start_Date,
          origin = "1970-01-01"
        ),
        "%m/%d/%Y"
      )
    }

    if ("Window_End_Date" %in% names(comparison_table_data)) {
      comparison_table_data$Window_End_Date <- format(
        as.Date(
          comparison_table_data$Window_End_Date,
          origin = "1970-01-01"
        ),
        "%m/%d/%Y"
      )
    }

    comparison_table_data
  })
  
  
  output$comparison_plot <- renderPlotly({
    df <- comparison_data()

    validate(
      need(
        nrow(df) > 0,
        "Select saved runs that contain results for the selected model."
      )
    )

    optional_character_columns <- c(
      "DSM2_Node",
      "Location",
      "Saved_Run_ID",
      "Scenario_Name",
      "Condition",
      "Output_Unit"
    )

    for (column_name in optional_character_columns) {
      if (!column_name %in% names(df)) {
        df[[column_name]] <- NA_character_
      }
    }

    plot_df <- df %>%
      mutate(
        Run_Label = ifelse(
          !is.na(Scenario_Name) & nzchar(as.character(Scenario_Name)),
          as.character(Scenario_Name),
          ifelse(
            !is.na(Saved_Run_ID) & nzchar(as.character(Saved_Run_ID)),
            as.character(Saved_Run_ID),
            paste(Condition, "Run")
          )
        ),
        hover_text = paste0(
          "<b>Run:</b> ", Run_Label,
          "<br><b>Model:</b> ", Model,
          ifelse(
            !is.na(DSM2_Node),
            paste0("<br><b>DSM2 Node:</b> ", DSM2_Node),
            ""
          ),
          ifelse(
            !is.na(Location) & nzchar(as.character(Location)),
            paste0("<br><b>Location:</b> ", Location),
            ""
          ),
          "<br><b>Prediction:</b> ",
          sprintf("%.0f", Prediction_Final),
          " ",
          ifelse(is.na(Output_Unit), "", Output_Unit)
        )
      )

    if (
      input$comparison_model %in%
      c(
        "PTM Emulator 7-Day Entrainment",
        "PTM Emulator 30-Day Entrainment"
      )
    ) {
      plot_df <- plot_df %>%
        mutate(
          DSM2_Node_Num = suppressWarnings(as.numeric(DSM2_Node)),
          Node_Label = as.character(DSM2_Node),
          Legend_Label = vapply(
            Run_Label,
            function(x) paste(strwrap(x, width = 28), collapse = "<br>"),
            character(1)
          )
        ) %>%
        arrange(DSM2_Node_Num, DSM2_Node)

      plot_df$Node_Label <- factor(
        plot_df$Node_Label,
        levels = rev(unique(plot_df$Node_Label))
      )

      p <- plot_ly(
        plot_df,
        x = ~Prediction_Final,
        y = ~Node_Label,
        color = ~Legend_Label,
        type = "bar",
        orientation = "h",
        text = ~sprintf("%.0f", Prediction_Final),
        textposition = "auto",
        hovertext = ~hover_text,
        hoverinfo = "text"
      ) %>%
        layout(
          barmode = "group",
          xaxis = list(
            title = list(
              text = "<b>Predicted Entrainment (%)</b>",
              font = list(
                family = "Arial, Segoe UI, sans-serif",
                size = 14,
                color = "#1F1F1F"
              )
            ),
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = list(
              text = "<b>DSM2 Node</b>",
              font = list(
                family = "Arial, Segoe UI, sans-serif",
                size = 14,
                color = "#1F1F1F"
              )
            ),
            tickfont = list(size = 12),
            automargin = TRUE
          ),
          legend = list(
            orientation = "h",
            x = 0,
            xanchor = "left",
            y = -0.18,
            yanchor = "top",
            font = list(size = 11)
          ),
          margin = list(l = 100, r = 60, t = 70, b = 180)
        )
    } else {
      p <- plot_ly(
        plot_df,
        x = ~Run_Label,
        y = ~Prediction_Final,
        color = ~Run_Label,
        type = "bar",
        text = ~sprintf("%.0f", Prediction_Final),
        textposition = "outside",
        hovertext = ~hover_text,
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(
            title = "<b>Scenario Run</b>",
            tickangle = -25,
            automargin = TRUE,
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = if (
              input$comparison_model == "Event Horizon"
            ) {
              "<b>Event Horizon Distance (River Miles)</b>"
            } else {
              "<b>Prediction (%)</b>"
            },
            tickfont = list(size = 12)
          ),
          showlegend = FALSE,
          margin = list(l = 100, r = 60, t = 70, b = 120)
        )
    }

    p %>%
      layout(
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        font = list(
          family = "Arial, Segoe UI, sans-serif",
          size = 14,
          color = "#1F1F1F"
        )
      ) %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          format = "png",
          filename = "scenario_comparison",
          width = 1600,
          height = 1000,
          scale = 2
        )
      )
  })


  output$download_comparison <- downloadHandler(
    filename = function() {
      paste0("scenario_comparison_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(
        prepare_ptm_export(comparison_data()) %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(.x, 1))),
        file, na = ""
      )
    }
  )


  output$download_comparison_map <- downloadHandler(
    filename = function() paste0("scenario_comparison_map_", Sys.Date(), ".png"),
    contentType = "image/png",
    content = function(file) {
      df <- comparison_data()
      validate(need(nrow(df) > 0, "Select compatible runs before downloading the map."))
      if (input$comparison_model %in% c("PTM Emulator 7-Day Entrainment", "PTM Emulator 30-Day Entrainment")) {
        plot <- make_ptm_comparison_png_plot(
          df, input$comparison_spatial_map_bounds,
          paste0("Scenario Comparison: ", input$comparison_model)
        )
      } else if (identical(input$comparison_model, "Event Horizon")) {
        plot <- make_eh_comparison_png_plot(
          df, input$comparison_spatial_map_bounds,
          "Scenario Comparison: Event Horizon"
        )
      } else {
        stop("Spatial PNG export is available for PTM and Event Horizon comparisons.")
      }
      dims <- ptm_png_dimensions(input$comparison_spatial_map_bounds, delta_boundary)
      ggplot2::ggsave(file, plot, width = dims$width, height = dims$height,
                      units = "in", dpi = 300, bg = "white", limitsize = FALSE)
    }
  )
  
  
  
  # ===============================================================
  # Compare 2 Scenarios - same feature mappings and new LightGBM models
  # as Exploration. Only user-edited scenario values differ.
  compare_defaults_initialized <- reactiveVal(FALSE)
  observe({
    if (isTRUE(compare_defaults_initialized())) return()
    state <- observed_flow_state()
    if (!identical(state$status, "ready")) return()
    getv <- function(name, fallback) {
      item <- state[[name]]
      if (!is.null(item) && isTRUE(item$ok) &&
          is.finite(as.numeric(item$value)[1]))
        format_observed_value(item$value) else fallback
    }
    moke_value <- if (!is.null(state$MOK) && isTRUE(state$MOK$ok) &&
      is.finite(as.numeric(state$MOK$value)[1]))
      format_observed_value(state$MOK$value) else "800"
    defaults <- list(exp=getv("EXP", "6,000"), ver=getv("VER", "3,000"),
      sac=getv("SAC", "18,000"), east=getv("EAST", "1,500"),
      yol=getv("YOL", "1,000"), moke=moke_value)
    original_defaults <- c(exp="6,000", ver="3,000", sac="18,000",
                           east="1,500", yol="1,000", moke="800")
    for (sid in c("s1", "s2")) for (nm in names(defaults)) {
      id <- paste0("compare_", sid, "_", nm)
      existing <- isolate(input[[id]])
      # Do not silently overwrite values the user has already edited.
      if (is.null(existing) || !nzchar(trimws(existing)) ||
          identical(existing, unname(original_defaults[[nm]])))
        updateTextInput(session, id, value=defaults[[nm]])
    }
    compare_defaults_initialized(TRUE)
  })
  # Reference defaults mirror Exploration, without changing edited scenario
  # values after the first successful source retrieval.
  observeEvent(ccwd_qprec_detail(), {
    detail <- ccwd_qprec_detail()
    if (is.null(detail) || !is.finite(detail$value)) return()
    v <- ccwd_display_whole(detail$value)
    for (sid in c("s1", "s2")) {
      id <- paste0("compare_", sid, "_qprec")
      existing <- isolate(input[[id]])
      if (is.null(existing) || !nzchar(trimws(existing)))
        updateTextInput(session, id, value=v)
    }
  }, ignoreInit=FALSE)
  observeEvent(ccwd_dcc_measured(), {
    measured <- ccwd_dcc_measured()
    if (is.null(measured) || !is.finite(measured$value)) return()
    for (sid in c("s1", "s2")) {
      id <- paste0("compare_", sid, "_dcc_flow")
      existing <- isolate(input[[id]])
      if (is.null(existing) || !nzchar(trimws(existing)))
        updateTextInput(session, id, value=ccwd_display_whole(measured$value))
    }
  }, ignoreInit=FALSE)

  run_compare_scenario <- function(id) {
    nm <- input[[paste0("compare_",id,"_name")]]
    getnum <- function(key) parse_flow_input(input[[paste0("compare_",id,"_",key)]])
    keys <- c("exp","ver","sac","east","qgcd","qprec","yol","moke")
    vals <- setNames(vapply(keys, getnum, numeric(1)), keys)
    dcc <- suppressWarnings(as.numeric(input[[paste0("compare_",id,"_dcc")]]))
    risk <- suppressWarnings(as.numeric(input[[paste0("compare_",id,"_risk")]]))
    validate(
      need(!is.null(nm) && nzchar(trimws(nm)), "Enter a scenario name."),
      need(all(is.finite(vals)), "Enter valid numeric values for all eight flow and volume inputs (including QGCD/QPREC)."),
      need(length(dcc)==1L && is.finite(dcc) && dcc %in% c(0,1), "Select the DCC gate status."),
      need(length(risk)==1L && is.finite(risk) && risk %in% seq(15,80,5), "Select a risk threshold (15–80%).")
    )
    dcc_flow <- if (dcc == 0) 0 else getnum("dcc_flow")
    if (dcc == 1 && !is.finite(dcc_flow)) {
      dcc_flow <- 0.1846*as.numeric(vals['sac']) + 306.52
      message('CCWD Comparison ',id,': DCC input missing; estimated from scenario SAC.')
    }
    validate(need(is.finite(dcc_flow),
      "When DCC gates are open, enter a DCC flow or a valid Freeport SAC for regression estimation."))
    ptm <- make_new_ptm_results("User Defined Comparison", nm,
      vals["exp"], vals["ver"], vals["sac"], vals["east"],
      vals["qgcd"], vals["qprec"], dcc_flow)
    eh <- make_new_eh_result("User Defined Comparison", nm, risk,
      vals["exp"], vals["ver"], vals["sac"], vals["qgcd"],
      vals["qprec"], vals["east"])
    eco <- make_new_eco_results("User Defined Comparison", nm,
      vals["sac"], vals["yol"], vals["moke"], dcc)
    dplyr::bind_rows(ptm, eh, eco) %>% dplyr::mutate(Saved_Run_ID=nm)
  }

  # Run comparisons eagerly on button clicks. eventReactive() alone is lazy:
  # a model may not begin until a downstream chart/table requests its value.
  # Preserve the previous successful result if a subsequent run fails.
  compare_s1_value <- reactiveVal(NULL)
  compare_s2_value <- reactiveVal(NULL)
  compare_s1_result <- reactive({ compare_s1_value() })
  compare_s2_result <- reactive({ compare_s2_value() })

  execute_compare_scenario <- function(sid, destination) {
    started <- Sys.time()
    session$sendCustomMessage("ccwdComparisonStatus", list(
      id=sid, state="running", message="Running five LightGBM models..."))
    tryCatch({
      result <- run_compare_scenario(sid)
      if (!is.data.frame(result) || nrow(result) < 1L)
        stop("Model execution returned no prediction rows.")
      destination(result)
      elapsed <- round(as.numeric(difftime(Sys.time(), started, units="secs")), 1)
      session$sendCustomMessage("ccwdComparisonStatus", list(
        id=sid, state="success", seconds=elapsed,
        message=paste0("Completed: ", nrow(result), " predictions in ", elapsed, " s.")))
      message("[Scenario Comparison ", sid, "] completed ", nrow(result),
              " predictions in ", elapsed, " seconds")
    }, error=function(e) {
      error_text <- conditionMessage(e)
      elapsed <- round(as.numeric(difftime(Sys.time(), started, units="secs")), 1)
      session$sendCustomMessage("ccwdComparisonStatus", list(
        id=sid, state="error", seconds=elapsed,
        message=paste0("Model run failed: ", error_text)))
      message("[Scenario Comparison ", sid, "] FAILED after ",
              elapsed, " seconds: ", error_text)
    })
  }
  observeEvent(input$run_compare_s1, {
    execute_compare_scenario("s1", compare_s1_value)
  }, ignoreInit=TRUE, priority=10)
  observeEvent(input$run_compare_s2, {
    execute_compare_scenario("s2", compare_s2_value)
  }, ignoreInit=TRUE, priority=10)
  output$compare_two_readiness <- renderUI({ NULL })

  compare_two_data <- reactive({req(compare_s1_result(),compare_s2_result(),input$compare_two_model); bind_rows(compare_s1_result(),compare_s2_result()) %>% filter(Model==input$compare_two_model)})
  output$compare_two_table <- renderTable({format_comparison_table(compare_two_data())})
  compare_two_map_request <- eventReactive(input$generate_compare_two_map, {
    list(data=compare_two_data(), model=isolate(input$compare_two_model),
         threshold=isolate(input$compare_two_map_threshold))
  }, ignoreInit=TRUE)
  output$compare_two_spatial_map <- renderLeaflet({
    ccwd_safe_map_request(function() {
      request <- compare_two_map_request()
      ccwd_fast_comparison_map(request$data, as.character(request$model)[1],request$threshold,'Compare 2 Scenarios')
    },'Compare 2 Scenarios')
  })

  output$download_compare_two_map <- downloadHandler(
    filename = function() paste0("two_scenario_comparison_map_", Sys.Date(), ".png"),
    contentType = "image/png",
    content = function(file) {
      df <- compare_two_data()
      validate(need(nrow(df) > 0, "Run both scenarios before downloading the map."))
      if (input$compare_two_model %in% c("PTM Emulator 7-Day Entrainment", "PTM Emulator 30-Day Entrainment")) {
        plot <- make_ptm_comparison_png_plot(
          df, input$compare_two_spatial_map_bounds,
          paste0("Two-Scenario Comparison: ", input$compare_two_model)
        )
      } else if (identical(input$compare_two_model, "Event Horizon")) {
        plot <- make_eh_comparison_png_plot(
          df, input$compare_two_spatial_map_bounds,
          "Two-Scenario Comparison: Event Horizon"
        )
      } else {
        stop("Spatial PNG export is available for PTM and Event Horizon comparisons.")
      }
      dims <- ptm_png_dimensions(input$compare_two_spatial_map_bounds, delta_boundary)
      ggplot2::ggsave(file, plot, width = dims$width, height = dims$height,
                      units = "in", dpi = 300, bg = "white", limitsize = FALSE)
    }
  )

  output$compare_two_plot <- renderPlotly({
    df <- compare_two_data(); validate(need(nrow(df)>0,"Run both scenarios to compare results."))
    if (!"Location" %in% names(df)) df$Location <- NA_character_
    if (!"DSM2_Node" %in% names(df)) df$DSM2_Node <- NA_character_
    plot_df <- df %>% mutate(Run_Label=Scenario_Name, hover_text=paste0("<b>Scenario:</b> ",Scenario_Name,"<br><b>Model:</b> ",Model,ifelse(!is.na(DSM2_Node),paste0("<br><b>DSM2 Node:</b> ",DSM2_Node),""),ifelse(!is.na(Location),paste0("<br><b>Location:</b> ",Location),""),"<br><b>Prediction:</b> ",sprintf("%.0f",Prediction_Final)," ",Output_Unit))
    if (input$compare_two_model %in% c("PTM Emulator 7-Day Entrainment","PTM Emulator 30-Day Entrainment")) {
      plot_df <- plot_df %>% mutate(DSM2_Node_Num=suppressWarnings(as.numeric(DSM2_Node)),Node_Label=as.character(DSM2_Node)) %>% arrange(DSM2_Node_Num,DSM2_Node); plot_df$Node_Label <- factor(plot_df$Node_Label,levels=rev(unique(plot_df$Node_Label)))
      p <- plot_ly(plot_df,x=~Prediction_Final,y=~Node_Label,color=~Run_Label,type="bar",orientation="h",text=~sprintf("%.0f",Prediction_Final),textposition="auto",hovertext=~hover_text,hoverinfo="text") %>% layout(barmode="group",xaxis=list(title=list(text="<b>Predicted Entrainment (%)</b>",font=list(family="Arial, Segoe UI, sans-serif",size=14,color="#1F1F1F")),tickfont=list(size=12)),yaxis=list(title=list(text="<b>DSM2 Node</b>",font=list(family="Arial, Segoe UI, sans-serif",size=14,color="#1F1F1F")),tickfont=list(size=12),automargin=TRUE),legend=list(orientation="h",x=0,xanchor="left",y=-0.18,yanchor="top"),margin=list(l=100,r=60,t=70,b=150))
    } else {
      p <- plot_ly(plot_df,x=~Run_Label,y=~Prediction_Final,color=~Run_Label,type="bar",text=~sprintf("%.0f",Prediction_Final),textposition="outside",hovertext=~hover_text,hoverinfo="text") %>% layout(xaxis=list(title="<b>Scenario</b>",tickfont=list(size=12)),yaxis=list(title=if(identical(input$compare_two_model,"Event Horizon")) "<b>Event Horizon Distance (River Miles)</b>" else "<b>Prediction (%)</b>",tickfont=list(size=12)),showlegend=FALSE,margin=list(l=100,r=60,t=70,b=100))
    }
    p %>% layout(title=list(text=paste0("<b>",input$compare_two_model," Comparison</b>"),x=0.02,xanchor="left"),plot_bgcolor="#FFFFFF",paper_bgcolor="#FFFFFF",font=list(family="Arial, Segoe UI, sans-serif",size=14,color="#1F1F1F")) %>% config(displaylogo=FALSE)
  })

  # ===============================================================
  # Automatic OMRI archive comparison
  # ===============================================================
  
  output$omri_archive_dates_ui <- renderUI({
    
    choices <- available_archive_dates()
    
    selectizeInput(
      "omri_archive_dates",
      "Archive Run Dates:",
      choices = choices,
      selected = choices[1],
      multiple = TRUE,
      options = list(
        plugins = list(
          "remove_button"
        )
      )
    )
  })
  
  
  # OMRI archive comparisons are explicitly initiated and their success/failure
  # state is stored independently of charts and the expensive spatial map.
  omri_comparison_value <- reactiveVal(NULL)
  omri_comparison_error <- reactiveVal(NULL)
  omri_comparison_results <- reactive({req(omri_comparison_value()); omri_comparison_value()})
  observeEvent(input$build_omri_comparison, {
    started <- Sys.time()
    omri_comparison_value(NULL)
    omri_comparison_error(NULL)
    dates <- isolate(input$omri_archive_dates)
    model_name <- isolate(input$omri_comparison_model)
    risk <- suppressWarnings(as.numeric(isolate(input$omri_comparison_risk)))
    if (is.null(dates) || !length(dates) || is.null(model_name) || !nzchar(model_name)) {
      omri_comparison_error("Choose at least one archive date and a forecast model.")
      return()
    }
    session$sendCustomMessage("ccwdOMRIStatus", list(state="running", message=paste0(
      "Reading ", length(dates), " archive week(s) and running OMRI scenarios...")))
    tryCatch({
      all_results <- dplyr::bind_rows(lapply(seq_along(dates), function(i) {
        archive_date <- dates[[i]]
        message("[OMRI Comparison] Archive ", i, "/", length(dates), ": ", archive_date)
        scenarios <- archive_scenario_choices(archive_date)
        if (!length(scenarios)) stop("No OMRI CSV files found for ", archive_date)
        dplyr::bind_rows(lapply(scenarios, function(scenario) {
          scenario_name <- paste(archive_date, scenario, sep=" - OMRI ")
          message("[OMRI Comparison] Processing ", scenario_name)
          if (identical(model_name, "PTM Emulator 7-Day Entrainment")) {
            make_archive_ptm_forecast(archive_date=archive_date,
              omri_scenario=scenario, scenario_name=scenario_name)
          } else if (identical(model_name, "Event Horizon")) {
            make_archive_eh_forecast(archive_date=archive_date,
              omri_scenario=scenario, scenario_name=scenario_name, risk=risk)
          } else stop("Unsupported OMRI forecast model: ", model_name)
        }))
      }))
      if (!is.data.frame(all_results) || nrow(all_results) < 1L)
        stop("No predictions were returned by the selected OMRI scenarios.")
      omri_comparison_value(all_results)
      seconds <- round(as.numeric(difftime(Sys.time(), started, units="secs")), 1)
      message("[OMRI Comparison] Complete: ", nrow(all_results), " rows in ", seconds, " s")
      session$sendCustomMessage("ccwdOMRIStatus", list(state="success", message=paste0(
        "Predictions complete: ", nrow(all_results), " rows in ", seconds,
        " s. Charts/tables can now render. The map is optional.")))
    }, error=function(e) {
      msg <- conditionMessage(e)
      if (!nzchar(msg)) msg <- "A model or archive operation failed; inspect the RStudio Console."
      message("[OMRI Comparison] FAILED: ", msg)
      omri_comparison_error(msg)
      session$sendCustomMessage("ccwdOMRIStatus", list(state="error", message=paste0("OMRI run failed: ", msg)))
    })
  }, ignoreInit=TRUE, priority=10)

  output$omri_comparison_status <- renderUI({
    error <- omri_comparison_error()
    if (!is.null(error)) return(tags$div(class="alert alert-danger", paste("Run failed:", error)))
    result <- omri_comparison_value()
    if (is.null(result)) return(tags$div(class="alert alert-info",
      "No completed OMRI comparison yet. Choose dates and click Run All Available OMRI Scenarios. A live timer will appear beside the button."))
    NULL
  })

  output$omri_comparison_table <- renderTable({
    
    data <- omri_comparison_results()

    result_model <- unique(
      as.character(
        data$Model
      )
    )

    result_model <- result_model[
      !is.na(result_model) &
        nzchar(result_model)
    ][1]

    if (
      identical(
        result_model,
        "PTM Emulator 7-Day Entrainment"
      )
    ) {
      
      data %>%
        transmute(
          Archive_Date,
          OMRI_Scenario,
          Window_Start_Date = format(
            as.Date(
              Window_Start_Date,
              origin = "1970-01-01"
            ),
            "%m/%d/%Y"
          ),
          Window_End_Date = format(
            as.Date(
              Window_End_Date,
              origin = "1970-01-01"
            ),
            "%m/%d/%Y"
          ),
          DSM2_Node,
          Location,
          Entrainment_Percentage = fmt_int(
            Prediction_Final
          )
        ) %>%
        arrange(
          Archive_Date,
          suppressWarnings(
            as.numeric(
              OMRI_Scenario
            )
          ),
          suppressWarnings(
            as.numeric(
              DSM2_Node
            )
          )
        )
      
    } else {
      
      data %>%
        transmute(
          Archive_Date,
          OMRI_Scenario,
          Window_Start_Date = format(
            as.Date(
              Window_Start_Date,
              origin = "1970-01-01"
            ),
            "%m/%d/%Y"
          ),
          Window_End_Date = format(
            as.Date(
              Window_End_Date,
              origin = "1970-01-01"
            ),
            "%m/%d/%Y"
          ),
          Risk_Level_Percent = fmt_int(
            Risk_Level_Percent
          ),
          Event_Horizon_Miles = fmt_one(
            Prediction_Final
          ),
          EXP = fmt_int(
            EXP
          ),
          VER = fmt_int(
            VER
          ),
          EAST = fmt_int(
            EAST
          ),
          XGEO = fmt_int(
            XGEO
          )
        ) %>%
        arrange(
          Archive_Date,
          suppressWarnings(
            as.numeric(
              OMRI_Scenario
            )
          )
        )
    }
  })
  
  
  output$omri_comparison_plot <- renderPlotly({
    
    data <- omri_comparison_results()
    
    validate(
      need(
        nrow(data) > 0,
        "No OMRI comparison results are available."
      )
    )
    
    result_model <- unique(
      as.character(
        data$Model
      )
    )

    result_model <- result_model[
      !is.na(result_model) &
        nzchar(result_model)
    ][1]

    data <- data %>%
      mutate(
        Comparison_Label = paste0(
          Archive_Date,
          " | OMRI ",
          OMRI_Scenario
        )
      )
    
    if (
      identical(
        result_model,
        "PTM Emulator 7-Day Entrainment"
      )
    ) {
      
      plot_data <- data %>%
        mutate(
          DSM2_Node_Number = suppressWarnings(
            as.numeric(
              DSM2_Node
            )
          ),
          
          Node_Label = ifelse(
            is.na(Location) | Location == "",
            as.character(DSM2_Node),
            paste0(
              DSM2_Node,
              " - ",
              Location
            )
          )
        ) %>%
        arrange(
          DSM2_Node_Number,
          DSM2_Node
        )
      
      plot_data$Node_Label <- factor(
        plot_data$Node_Label,
        levels = rev(
          unique(
            plot_data$Node_Label
          )
        )
      )
      
      plot_data <- plot_data %>%
        mutate(
          Hover_Text = paste0(
            "<b>Archive date:</b> ",
            Archive_Date,
            "<br><b>OMRI scenario:</b> ",
            OMRI_Scenario,
            "<br><b>Node:</b> ",
            DSM2_Node,
            "<br><b>Location:</b> ",
            Location,
            "<br><b>Prediction:</b> ",
            sprintf(
              "%.0f%%",
              Prediction_Final
            )
          )
        )
      
      plot_ly(
        plot_data,
        x = ~Prediction_Final,
        y = ~Node_Label,
        color = ~Comparison_Label,
        type = "bar",
        orientation = "h",
        text = ~sprintf(
          "%.0f",
          Prediction_Final
        ),
        hovertext = ~Hover_Text,
        hoverinfo = "text"
      ) %>%
        layout(
          barmode = "group",
          
          title = list(
            text = "<b>OMRI Forecast PTM 7-Day Comparison</b>",
            x = 0.02,
            xanchor = "left"
          ),
          
          xaxis = list(
            title = "<b>Predicted Entrainment (%)</b>"
          ),
          
          yaxis = list(
            title = "<b>DSM2 Node and Location</b>",
            automargin = TRUE
          ),
          
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF",
          
          font = list(
            family = "Arial, Segoe UI, sans-serif",
            size = 13,
            color = "#1F1F1F"
          ),
          
          margin = list(
            l = 300,
            r = 50,
            t = 80,
            b = 80
          )
        ) %>%
        config(
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "png",
            filename = "OMRI_PTM_7day_comparison",
            width = 1800,
            height = 1100,
            scale = 2
          )
        )
      
    } else {
      
      plot_data <- data %>%
        mutate(
          Hover_Text = paste0(
            "<b>Archive date:</b> ",
            Archive_Date,
            "<br><b>OMRI scenario:</b> ",
            OMRI_Scenario,
            "<br><b>Risk:</b> ",
            Risk_Level_Percent,
            "%",
            "<br><b>Event Horizon:</b> ",
            sprintf(
              "%.0f river miles",
              Prediction_Final
            )
          )
        )
      
      plot_ly(
        plot_data,
        x = ~Comparison_Label,
        y = ~Prediction_Final,
        color = ~Comparison_Label,
        type = "bar",
        text = ~sprintf(
          "%.0f",
          Prediction_Final
        ),
        textposition = "outside",
        hovertext = ~Hover_Text,
        hoverinfo = "text"
      ) %>%
        layout(
          title = list(
            text = "<b>OMRI Forecast Event Horizon Comparison</b>",
            x = 0.02,
            xanchor = "left"
          ),
          
          xaxis = list(
            title = "<b>Archive Date and OMRI Scenario</b>",
            tickangle = -30,
            automargin = TRUE
          ),
          
          yaxis = list(
            title = "<b>Event Horizon Distance (River Miles)</b>"
          ),
          
          showlegend = FALSE,
          
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF",
          
          font = list(
            family = "Arial, Segoe UI, sans-serif",
            size = 13,
            color = "#1F1F1F"
          ),
          
          margin = list(
            l = 90,
            r = 50,
            t = 80,
            b = 150
          )
        ) %>%
        config(
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "png",
            filename = "OMRI_Event_Horizon_comparison",
            width = 1800,
            height = 1000,
            scale = 2
          )
        )
    }
  })
  
  
  omri_map_request <- eventReactive(input$generate_omri_map, {
    list(data=omri_comparison_results(),
         threshold=isolate(input$omri_comparison_map_threshold))
  }, ignoreInit=TRUE)
  output$omri_comparison_spatial_map <- renderLeaflet({
    ccwd_safe_map_request(function() {
      request <- omri_map_request()
      data <- request$data
      if (!is.data.frame(data) || nrow(data)==0L) return(ccwd_minimal_map('No OMRI results are available.'))
      if (!'Saved_Run_ID' %in% names(data)) data$Saved_Run_ID <- paste0('OMRI ',data$OMRI_Scenario)
      ccwd_fast_comparison_map(data,as.character(data$Model)[1],request$threshold,'OMRI Comparison')
    },'OMRI Comparison')
  })

  output$download_omri_comparison <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "OMRI_archive_comparison_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write_csv(
        prepare_ptm_export(omri_comparison_results()) %>%
          mutate(
            across(
              where(
                is.numeric
              ),
              ~ round(
                .x
              )
            )
          ),
        file
      )
    }
  )
  
  


  output$download_omri_comparison_map <- downloadHandler(
    filename = function() paste0("OMRI_comparison_map_", Sys.Date(), ".png"),
    contentType = "image/png",
    content = function(file) {
      df <- omri_comparison_results()
      validate(need(nrow(df) > 0, "Run an OMRI comparison before downloading the map."))
      selected_model <- unique(as.character(df$Model))[1]
      if (grepl("PTM Emulator", selected_model)) {
        plot <- make_ptm_comparison_png_plot(
          df, input$omri_comparison_spatial_map_bounds,
          "OMRI Forecast Scenario Comparison"
        )
      } else if (identical(selected_model, "Event Horizon")) {
        plot <- make_eh_comparison_png_plot(
          df, input$omri_comparison_spatial_map_bounds,
          "OMRI Event Horizon Scenario Comparison"
        )
      } else {
        stop("Spatial PNG export is available for PTM and Event Horizon comparisons.")
      }
      dims <- ptm_png_dimensions(input$omri_comparison_spatial_map_bounds, delta_boundary)
      ggplot2::ggsave(file, plot, width = dims$width, height = dims$height,
                      units = "in", dpi = 300, bg = "white", limitsize = FALSE)
    }
  )

  output$about_info_table <- render_gt({
    data <- data.frame(
      Name = c(
        "Adam Witt",
        "Laura Manuel",
        "Roja Kaveh Garna",
        "Ching-Fu Chang",
        "Deanna Sereno",
        "Jo Anna Beck",
        "Josh Israel"
      ),
      Role = c(
        "Activity Lead",
        "App Developer",
        "App Developer",
        "Entrainment Emulator and Event Horizon Developer",
        "Entrainment Emulator and Event Horizon Developer",
        "Contracting Officers Representative",
        "Reclamation Activity Oversight"
      ),
      Email = c(
        "adam.witt@stantec.com",
        "Laura.Manuel@stantec.com",
        "Roja.KavehGarna@stantec.com",
        "CChang@ccwater.com",
        "dsereno@ccwater.com",
        "jbeck@usbr.gov",
        "jaisrael@usbr.gov"
      )
    )
    about_info_table <- gt(data)
    about_info_table |>
      tab_row_group(
        label = html("<strong><em>Bureau of Reclamation,</strong><br>Bay-Delta Office</em>"), 
        rows = 6:7
      )|>
      tab_row_group(
        label = html("<strong><em>Stantec Inc.,</strong><br>Sacramento C St. Office</em>"), 
        rows = 1:5
      )|>
      cols_align(
        align = "center",
        columns = 2:3
      )|>
      tab_style(
        style = cell_text(align = "center", size = "medium"),
        locations = cells_row_groups()
      )|>
      tab_style(
        style = cell_text(align = "center", size = "medium"),
        locations = cells_row_groups()
      )|>
      tab_style(
        style = cell_text(weight = "bold", align = "center", size = "large"),
        locations = cells_column_labels(columns = 1:3)
      )
  })
  
  output$ecoptm_inputs_table <- gt::render_gt({
    
    badge <- function(text) sprintf(
      "<span style='display:inline-block;background:#f9e9ee;color:#c7254e;border:1px solid #efcbd5;border-radius:4px;padding:3px 8px;font-family:monospace;white-space:nowrap;'>%s</span>",
      text
    )
    
    link <- function(text, url) sprintf(
      "<a href='%s' target='_blank' rel='noopener noreferrer'>%s</a>",
      url, text
    )
    
    group_label <- function(number, content = "") sprintf(
      "<div style='display:flex;align-items:center;gap:12px;width:100%%;min-height:46px;'><strong>Input Number %s</strong>%s</div>",
      number, content
    )
    
    arrow <- "<span style='display:inline-flex;align-items:center;margin-left:6px;color:#0a7e8c;font-size:30px;line-height:1;transform:translateY(12px);'><i class='fa fa-level-down' aria-hidden='true'></i></span>"
    
    yol_equation <- sprintf(
      "<div style='display:flex;align-items:center;gap:8px;white-space:nowrap;'>%s<strong>=</strong>%s<strong>+</strong>%s%s</div>",
      badge("YOL"), badge("SACWEIR"), badge("FREWEIR"), arrow
    )
    
    qaqc_source_values <- c(
      link("USGS - 11447650", "https://waterdata.usgs.gov/monitoring-location/USGS-11447650/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11426000", "https://waterdata.usgs.gov/monitoring-location/11426000/"),
      link("WDL - A02170", "https://wdl.water.ca.gov/waterdatalibrary/StationDetails.aspx?dateFrom2=01%2f01%2f1900&StationTypeCode=&Station=A02170&IncludeVarData=False&SelectedAll=False&dateTo2=01%2f01%2f1900&source=search"),
      link("USGS - 11325500", "https://waterdata.usgs.gov/monitoring-location/USGS-11325500/"),
      "0 = closed; 1 = open"
    )
    
    realtime_source_values <- c(
      link("CDEC - FPT", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FPT"),
      link("USGS - 11426000", "https://waterdata.usgs.gov/monitoring-location/11426000/"),
      link("CDEC - FRE", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FRE"),
      link("USACE - CA00173", "https://water.usace.army.mil/overview/spk/locations/camanche"),
      "0 = closed; 1 = open"
    )
    
    input_data <- data.frame(
      Group_ID = c("input_1", "input_2", "input_2", "input_3", "input_4"),
      Acronym = c("FPT", "SACWEIR", "FREWEIR", "MOK", "DCC"),
      Full_Name = c(
        "Freeport",
        "Sacramento Weir",
        "Fremont Weir",
        "Mokelumne River",
        "Delta Cross Channel"
      ),
      Description = c(
        "Sacramento River flow at Freeport",
        "Sacramento Weir flow spill to Yolo Bypass",
        "Fremont Weir flow spill to Yolo Bypass",
        "Mokelumne River before its confluence with Cosumnes River",
        "Delta Cross Channel gate opening status recorded as a Boolean value"
      ),
      QAQC_Stage_Level_Data = qaqc_source_values,
      Real_Time_Data = realtime_source_values,
      stringsAsFactors = FALSE
    )
    
    input_data$QAQC_Stage_Level_Data <- lapply(
      input_data$QAQC_Stage_Level_Data,
      gt::html
    )
    
    input_data$Real_Time_Data <- lapply(
      input_data$Real_Time_Data,
      gt::html
    )
    
    input_data |>
      gt::gt() |>
      gt::cols_hide(columns = Group_ID) |>
      gt::cols_label(
        Acronym = "Model Input Acronym",
        Full_Name = "Full Name",
        Description = "Description",
        QAQC_Stage_Level_Data = "QA/QC Daily Data",
        Real_Time_Data = "Real-Time Data"
      ) |>
      gt::tab_row_group(
        label = gt::html(group_label(1)),
        rows = Group_ID == "input_1",
        id = "input_1"
      ) |>
      gt::tab_row_group(
        label = gt::html(group_label(2, yol_equation)),
        rows = Group_ID == "input_2",
        id = "input_2"
      ) |>
      gt::tab_row_group(
        label = gt::html(group_label(3)),
        rows = Group_ID == "input_3",
        id = "input_3"
      ) |>
      gt::tab_row_group(
        label = gt::html(group_label(4)),
        rows = Group_ID == "input_4",
        id = "input_4"
      ) |>
      gt::row_group_order(
        groups = c("input_1", "input_2", "input_3", "input_4")
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = Acronym),
        fn = function(x) vapply(x, badge, character(1))
      ) |>
      gt::text_transform(
        locations = gt::cells_body(
          columns = c(QAQC_Stage_Level_Data, Real_Time_Data)
        ),
        fn = identity
      ) |>
      gt::cols_width(
        Acronym ~ gt::px(160),
        Full_Name ~ gt::px(210),
        Description ~ gt::px(420),
        QAQC_Stage_Level_Data ~ gt::px(300),
        Real_Time_Data ~ gt::px(300)
      ) |>
      gt::cols_align(
        align = "center",
        columns = everything()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(
            align = "center",
            v_align = "middle"
          ),
          gt::cell_borders(
            sides = c("left", "right", "bottom"),
            color = "#c7d7da",
            weight = gt::px(1)
          )
        ),
        locations = gt::cells_body(columns = everything())
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#eef7f9"),
          gt::cell_text(
            align = "left",
            v_align = "middle",
            size = "medium"
          ),
          gt::cell_borders(
            sides = c("top", "bottom"),
            color = "#0a7e8c",
            weight = gt::px(1)
          )
        ),
        locations = gt::cells_row_groups()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#0a7e8c"),
          gt::cell_text(
            color = "white",
            weight = "bold",
            align = "center",
            v_align = "middle"
          ),
          gt::cell_borders(
            sides = c("left", "right", "top", "bottom"),
            color = "#c7d7da",
            weight = gt::px(1)
          )
        ),
        locations = gt::cells_column_labels(columns = everything())
      ) |>
      gt::tab_source_note(
        source_note = gt::html(
          "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage-discharge rating curve."
        )
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          align = "left",
          color = "#555555",
          size = gt::px(12)
        ),
        locations = gt::cells_source_notes()
      ) |>
      gt::tab_options(
        table.width = gt::pct(100),
        table.font.names = "Segoe UI",
        table.font.size = gt::px(14),
        data_row.padding = gt::px(10),
        row_group.padding = gt::px(10)
      )
  })
  
  
  output$ptm_inputs_table <- gt::render_gt({
    
    badge <- function(text) sprintf(
      "<span style='display:inline-block;background:#f9e9ee;color:#c7254e;border:1px solid #efcbd5;border-radius:4px;padding:3px 8px;font-family:monospace;white-space:nowrap;'>%s</span>",
      text
    )
    
    link <- function(text, url) sprintf(
      "<a href='%s' target='_blank' rel='noopener noreferrer'>%s</a>",
      url, text
    )
    
    group_label <- function(number, content = "") sprintf(
      "<div style='display:flex;align-items:center;gap:12px;width:100%%;min-height:46px;'><strong>Input Number %s</strong>%s</div>",
      number, content
    )
    
    arrow <- "<span style='display:inline-flex;align-items:center;margin-left:6px;color:#0a7e8c;font-size:30px;line-height:1;transform:translateY(12px);'><i class='fa fa-level-down' aria-hidden='true'></i></span>"
    
    equation <- function(result, terms, operators) {
      rhs <- badge(terms[1])
      if (length(terms) > 1) {
        for (i in seq_len(length(terms) - 1)) {
          rhs <- paste0(rhs, sprintf("<strong style='color:#333333;'>%s</strong>", operators[i]), badge(terms[i + 1]))
        }
      }
      paste0(
        "<div style='display:flex;align-items:center;gap:8px;white-space:nowrap;'>",
        badge(result), "<strong style='color:#333333;'>=</strong>", rhs, arrow, "</div>"
      )
    }
    
    exp_equation <- equation("EXP", c("CCF", "TPP"), "+")
    east_equation <- equation("EAST", c("MOK", "CAL", "COS"), c("+", "+"))
    xgeo_equation <- equation("XGEO", c("XGEO_A", "XGEO_C"), "-")
    
    qaqc_source_values <- c(
      "Click the link in <strong> Model Parameters </strong> to view the map",
      link("Historical O & M Reports - Available by Request", "https://water.ca.gov/Programs/State-Water-Project/Operations-and-Maintenance/Monthly-and-Annual-Operations-Reports"),
      link("Historical O & M Reports - Available by Request", "https://water.ca.gov/Programs/State-Water-Project/Operations-and-Maintenance/Monthly-and-Annual-Operations-Reports"),
      link("USGS - 11303500", "https://waterdata.usgs.gov/monitoring-location/USGS-11303500/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11447650", "https://waterdata.usgs.gov/monitoring-location/USGS-11447650/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11325500", "https://waterdata.usgs.gov/monitoring-location/11325500/"),
      link("USACE - CA10109", "https://water.usace.army.mil/overview/spk/locations/new%20hogan"),
      link("USGS - 11335000", "https://waterdata.usgs.gov/monitoring-location/USGS-11335000/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11447890", "https://waterdata.usgs.gov/monitoring-location/USGS-11447890/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11447905", "https://waterdata.usgs.gov/monitoring-location/USGS-11447905/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true")
    )
    
    realtime_source_values <- c(
      "Click the link in <strong> Model Parameters </strong> to view the map",
      link("CDEC - CLC", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=CLC"),
      link("CDEC - TRP", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=TRP"),
      link("CDEC - VNS", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=VNS"),
      link("CDEC - FPT", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FPT"),
      link("USACE - CA00173", "https://water.usace.army.mil/overview/spk/locations/camanche"),
      link("CDEC - NHG", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=NHG"),
      link("USGS - 11335000", "https://waterdata.usgs.gov/monitoring-location/USGS-11335000"),
      link("USGS - 11447890", "https://waterdata.usgs.gov/monitoring-location/USGS-11447890"),
      link("USGS - 11447905", "https://waterdata.usgs.gov/monitoring-location/USGS-11447905")
    )
    
    input_data <- data.frame(
      Group_ID = c("input_1", "input_2", "input_2", "input_3", "input_4", "input_5", "input_5", "input_5", "input_6", "input_6"),
      Acronym = c("Location", "CCF", "TPP", "VNS", "SAC", "MOK", "CAL", "COS", "XGEO_A", "XGEO_C"),
      Full_Name = c(
        "Location nodes", "Clifton Court Forebay", "Tracy Pumping Plant", "Vernalis", "= FPT",
        "Mokelumne River", "Calaveras River", "Cosumnes River", "Georgiana Slough", "Georgiana Slough"
      ),
      Description = c(
        "One or a list of location node numbers from DSM2 model",
        "Reservoir inflow of the export facility Clifton Court Forebay",
        "Reservoir inflow of the export facility Tracy Pumping Plant",
        "San Joaquin River flow at Vernalis",
        "Sacramento River flow at Freeport",
        "Mokelumne River before its confluence with Cosumnes River",
        "Calaveras River flow from New Hogan Lake Reservoir outflow",
        "Cosumnes River at Michigan Bar",
        "Sacramento River above Delta Cross Channel",
        "Sacramento River below Georgiana Slough"
      ),
      QAQC_Daily_Data = qaqc_source_values,
      Real_Time_Data = realtime_source_values,
      stringsAsFactors = FALSE
    )
    
    input_data$QAQC_Daily_Data <- lapply(input_data$QAQC_Daily_Data, gt::html)
    input_data$Real_Time_Data <- lapply(input_data$Real_Time_Data, gt::html)
    
    input_data |>
      gt::gt() |>
      gt::cols_hide(columns = Group_ID) |>
      gt::cols_label(
        Acronym = "Model Input Acronyms",
        Full_Name = "Full Name",
        Description = "Description",
        QAQC_Daily_Data = "QA/QC Daily Data",
        Real_Time_Data = "Real-Time Data"
      ) |>
      gt::tab_row_group(label = gt::html(group_label(1)), rows = Group_ID == "input_1", id = "input_1") |>
      gt::tab_row_group(label = gt::html(group_label(2, exp_equation)), rows = Group_ID == "input_2", id = "input_2") |>
      gt::tab_row_group(label = gt::html(group_label(3)), rows = Group_ID == "input_3", id = "input_3") |>
      gt::tab_row_group(label = gt::html(group_label(4)), rows = Group_ID == "input_4", id = "input_4") |>
      gt::tab_row_group(label = gt::html(group_label(5, east_equation)), rows = Group_ID == "input_5", id = "input_5") |>
      gt::tab_row_group(label = gt::html(group_label(6, xgeo_equation)), rows = Group_ID == "input_6", id = "input_6") |>
      gt::row_group_order(groups = paste0("input_", 1:6)) |>
      gt::text_transform(
        locations = gt::cells_body(columns = Acronym),
        fn = function(x) vapply(x, badge, character(1))
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = c(QAQC_Daily_Data, Real_Time_Data)),
        fn = identity
      ) |>
      gt::cols_width(
        Acronym ~ gt::px(170),
        Full_Name ~ gt::px(220),
        Description ~ gt::px(480),
        QAQC_Daily_Data ~ gt::px(280),
        Real_Time_Data ~ gt::px(280)
      ) |>
      gt::cols_align(align = "center", columns = everything()) |>
      gt::tab_style(
        style = list(
          gt::cell_text(align = "center", v_align = "middle"),
          gt::cell_borders(sides = c("left", "right", "bottom"), color = "#c7d7da", weight = gt::px(1))
        ),
        locations = gt::cells_body(columns = everything())
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#eef7f9"),
          gt::cell_text(align = "left", v_align = "middle", size = "medium"),
          gt::cell_borders(sides = c("top", "bottom"), color = "#0a7e8c", weight = gt::px(1))
        ),
        locations = gt::cells_row_groups()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#0a7e8c"),
          gt::cell_text(color = "white", weight = "bold", align = "center", v_align = "middle"),
          gt::cell_borders(sides = c("left", "right", "top", "bottom"), color = "#c7d7da", weight = gt::px(1))
        ),
        locations = gt::cells_column_labels(columns = everything())
      ) |>
      gt::tab_source_note(
        source_note = gt::html(
          "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage-discharge rating curve."
        )
      ) |>
      gt::tab_style(
        style = gt::cell_text(align = "left", color = "#555555", size = gt::px(12)),
        locations = gt::cells_source_notes()
      ) |>
      gt::tab_options(
        table.width = gt::pct(100),
        table.font.names = "Segoe UI",
        table.font.size = gt::px(14),
        data_row.padding = gt::px(10),
        row_group.padding = gt::px(10)
      )
  })
  
  output$horizon_inputs_table <- gt::render_gt({
    
    badge <- function(text) sprintf(
      "<span style='display:inline-block;background:#f9e9ee;color:#c7254e;border:1px solid #efcbd5;border-radius:4px;padding:3px 8px;font-family:monospace;white-space:nowrap;'>%s</span>",
      text
    )
    
    link <- function(text, url) sprintf(
      "<a href='%s' target='_blank' rel='noopener noreferrer'>%s</a>",
      url, text
    )
    
    group_label <- function(number, content = "") sprintf(
      "<div style='display:flex;align-items:center;gap:12px;width:100%%;min-height:46px;'><strong>Input Number %s</strong>%s</div>",
      number, content
    )
    
    arrow <- "<span style='display:inline-flex;align-items:center;margin-left:6px;color:#0a7e8c;font-size:30px;line-height:1;transform:translateY(12px);'><i class='fa fa-level-down' aria-hidden='true'></i></span>"
    
    equation <- function(result, terms, operators) {
      rhs <- badge(terms[1])
      if (length(terms) > 1) {
        for (i in seq_len(length(terms) - 1)) {
          rhs <- paste0(
            rhs,
            sprintf("<strong style='color:#333333;'>%s</strong>", operators[i]),
            badge(terms[i + 1])
          )
        }
      }
      paste0(
        "<div style='display:flex;align-items:center;gap:8px;white-space:nowrap;'>",
        badge(result), "<strong style='color:#333333;'>=</strong>", rhs, arrow, "</div>"
      )
    }
    
    exp_equation <- equation("EXP", c("CCF", "TPP"), "+")
    east_equation <- equation("EAST", c("MOK", "CAL", "COS"), c("+", "+"))
    xgeo_equation <- equation("XGEO", c("XGEO_A", "XGEO_C"), "-")
    
    xgeo_group <- paste0(
      "<span style='white-space:nowrap;'>Combined interior Delta flow</span>",
      "<strong style='color:#333333;'>=</strong>",
      xgeo_equation
    )
    
    qaqc_source_values <- c(
      link("USGS - 11447650", "https://waterdata.usgs.gov/monitoring-location/USGS-11447650/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      link("Historical O & M Reports - Available by Request", "https://water.ca.gov/Programs/State-Water-Project/Operations-and-Maintenance/Monthly-and-Annual-Operations-Reports"),
      link("Historical O & M Reports - Available by Request", "https://water.ca.gov/Programs/State-Water-Project/Operations-and-Maintenance/Monthly-and-Annual-Operations-Reports"),
      link("USGS - 11325500", "https://waterdata.usgs.gov/monitoring-location/USGS-11325500/"),
      link("USACE - CA10109", "https://water.usace.army.mil/overview/spk/locations/new%20hogan"),
      link("USGS - 11335000", "https://waterdata.usgs.gov/monitoring-location/USGS-11335000/#dataTypeId=daily-00060-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11447890", "https://waterdata.usgs.gov/monitoring-location/USGS-11447890/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      link("USGS - 11447905", "https://waterdata.usgs.gov/monitoring-location/USGS-11447905/#dataTypeId=daily-72137-0&period=P1Y&showFieldMeasurements=true"),
      "{15+5k???k=0,1,???,13}"
    )
    
    realtime_source_values <- c(
      link("CDEC - FPT", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=FPT"),
      link("CDEC - CLC", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=CLC"),
      link("CDEC - TRP", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=TRP"),
      link("USACE - CA00173", "https://water.usace.army.mil/overview/spk/locations/camanche"),
      link("CDEC - NHG", "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=NHG"),
      link("USGS - 11335000", "https://waterdata.usgs.gov/monitoring-location/USGS-11335000/"),
      link("USGS - 11447890", "https://waterdata.usgs.gov/monitoring-location/USGS-11447890/"),
      link("USGS - 11447905", "https://waterdata.usgs.gov/monitoring-location/USGS-11447905/"),
      "{15+5k???k=0,1,???,13}"
    )
    
    input_data <- data.frame(
      Group_ID = c("input_1", "input_2", "input_2", "input_3", "input_3", "input_3", "input_4", "input_4", "input_5"),
      Acronym = c("SAC", "CCF", "TPP", "MOK", "CAL", "COS", "XGEO_A", "XGEO_C", "ERL"),
      Full_Name = c(
        "= FPT", "Clifton Court Forebay", "Tracy Pumping Plant",
        "Mokelumne River", "Calaveras River", "Cosumnes River",
        "Georgiana Slough", "Georgiana Slough", "Entrainment Risk Level"
      ),
      Description = c(
        "Sacramento River flow at Freeport",
        "Reservoir inflow of the export facility Clifton Court Forebay",
        "Reservoir inflow of the export facility Tracy Pumping Plant",
        "Mokelumne River before its confluence with Cosumnes River",
        "Calaveras River flow from New Hogan Lake Reservoir outflow",
        "Cosumnes River flow at Michigan Bar",
        "Sacramento River above Delta Cross Channel",
        "Sacramento River below Georgiana Slough",
        "ERL expressed as an entrainment percentage from 15 to 80 in increments of 5"
      ),
      QAQC_Daily_Data = qaqc_source_values,
      Real_Time_Data = realtime_source_values,
      stringsAsFactors = FALSE
    )
    
    input_data$QAQC_Daily_Data <- lapply(input_data$QAQC_Daily_Data, gt::html)
    input_data$Real_Time_Data <- lapply(input_data$Real_Time_Data, gt::html)
    
    input_data |>
      gt::gt() |>
      gt::cols_hide(columns = Group_ID) |>
      gt::cols_label(
        Acronym = "Model Input Acronyms",
        Full_Name = "Full Name",
        Description = "Description",
        QAQC_Daily_Data = "QA/QC Daily Data",
        Real_Time_Data = "Real-Time Data"
      ) |>
      gt::tab_row_group(label = gt::html(group_label(1)), rows = Group_ID == "input_1", id = "input_1") |>
      gt::tab_row_group(label = gt::html(group_label(2, exp_equation)), rows = Group_ID == "input_2", id = "input_2") |>
      gt::tab_row_group(label = gt::html(group_label(3, east_equation)), rows = Group_ID == "input_3", id = "input_3") |>
      gt::tab_row_group(label = gt::html(group_label(4, xgeo_group)), rows = Group_ID == "input_4", id = "input_4") |>
      gt::tab_row_group(label = gt::html(group_label(5)), rows = Group_ID == "input_5", id = "input_5") |>
      gt::row_group_order(groups = paste0("input_", 1:5)) |>
      gt::text_transform(
        locations = gt::cells_body(columns = Acronym),
        fn = function(x) vapply(x, badge, character(1))
      ) |>
      gt::text_transform(
        locations = gt::cells_body(columns = c(QAQC_Daily_Data, Real_Time_Data)),
        fn = identity
      ) |>
      gt::cols_width(
        Acronym ~ gt::px(170),
        Full_Name ~ gt::px(220),
        Description ~ gt::px(480),
        QAQC_Daily_Data ~ gt::px(280),
        Real_Time_Data ~ gt::px(280)
      ) |>
      gt::cols_align(align = "center", columns = everything()) |>
      gt::tab_style(
        style = list(
          gt::cell_text(align = "center", v_align = "middle"),
          gt::cell_borders(
            sides = c("left", "right", "bottom"),
            color = "#c7d7da",
            weight = gt::px(1)
          )
        ),
        locations = gt::cells_body(columns = everything())
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#eef7f9"),
          gt::cell_text(align = "left", v_align = "middle", size = "medium"),
          gt::cell_borders(
            sides = c("top", "bottom"),
            color = "#0a7e8c",
            weight = gt::px(1)
          )
        ),
        locations = gt::cells_row_groups()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#0a7e8c"),
          gt::cell_text(
            color = "white",
            weight = "bold",
            align = "center",
            v_align = "middle"
          ),
          gt::cell_borders(
            sides = c("left", "right", "top", "bottom"),
            color = "#c7d7da",
            weight = gt::px(1)
          )
        ),
        locations = gt::cells_column_labels(columns = everything())
      ) |>
      gt::tab_source_note(
        source_note = gt::html(
          "<strong>Note:</strong> For some QA/QC data sources, only reviewed or approved stage data are available. Corresponding discharge values are calculated using a stage-discharge rating curve."
        )
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          align = "left",
          color = "#555555",
          size = gt::px(12)
        ),
        locations = gt::cells_source_notes()
      ) |>
      gt::tab_options(
        table.width = gt::pct(100),
        table.font.names = "Segoe UI",
        table.font.size = gt::px(14),
        data_row.padding = gt::px(10),
        row_group.padding = gt::px(10)
      )
  })
  
  node_data <- readr::read_csv(
    file.path("STN_EMULATOR", "models", "delta_locations_coordinates.csv"),
    show_col_types = FALSE
  )
  
  nodes_7day <- c(1,7,21,25,34,39,41,75,86,99,113,145,174,232,469)
  
  node_data_7day <- node_data[
    node_data$DSM2_Node %in% nodes_7day,
    ,
    drop = FALSE
  ]
  
  create_node_map <- function(data, marker_color) {
    
    leaflet::leaflet(data) %>%
      add_osm_basemap() %>%
      add_delta_background_layers() %>%
      
      leaflet::addAwesomeMarkers(
        lng = ~X,
        lat = ~Y,
        icon = leaflet::awesomeIcons(
          icon = "map-marker",
          library = "fa",
          markerColor = marker_color,
          iconColor = "white"
        ),
        popup = ~paste0(
          "<strong>Node: </strong>", DSM2_Node,
          "<br><strong>Location: </strong>", Location,
          "<br><strong>Region: </strong>", Region
        ),
        group = "DSM2 Nodes"
      ) %>%
      
      leaflet::addLabelOnlyMarkers(
        lng = ~X,
        lat = ~Y,
        label = ~as.character(DSM2_Node),
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          offset = c(0, -17),
          textsize = "11px",
          style = list(
            "color" = "white",
            "font-weight" = "bold",
            "text-shadow" = "0 0 2px black"
          )
        ),
        group = "Node Labels"
      ) %>%
      
      leaflet::addLayersControl(
        baseGroups = c(
          "OpenStreetMap"
        ),
        overlayGroups = c(
          "DSM2 Delta Boundary",
          "Delta Channels",
          "DSM2 Nodes",
          "Node Labels"
        ),
        options = leaflet::layersControlOptions(
          collapsed = FALSE
        )
      ) %>%
      
      add_reset_view_control(zoom = 9) %>%
      leaflet::setView(
        lng = -121.60,
        lat = 38.05,
        zoom = 9
      )
  }
  
  observeEvent(input$open_node_map_7day, {
    showModal(
      modalDialog(
        title = "DSM2 Location Nodes: 7-Day Model",
        leaflet::leafletOutput("node_map_7day", height = "650px"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  observeEvent(input$open_node_map_30day, {
    showModal(
      modalDialog(
        title = "DSM2 Location Nodes: 30-Day Model",
        leaflet::leafletOutput("node_map_30day", height = "650px"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  output$node_map_7day <- leaflet::renderLeaflet({
    create_node_map(node_data_7day, "orange")
  })
  
  output$node_map_30day <- leaflet::renderLeaflet({
    create_node_map(node_data, "blue")
  })
  
  output$ptm_parameters_table <- render_gt({
    
    map_link <- function(text, input_id) paste0(
      "<a href='#' onclick=\"",
      "Shiny.setInputValue('", input_id, "', Date.now(), {priority:'event'});",
      "return false;",
      "\">", text, "</a>"
    )
    
    data <- data.frame(
      Names = c(
        "Number of Trees",
        "Number of Leaves per Tree",
        "Total Number of Leaf Nodes",
        "Number of Split Nodes per Tree",
        "Total Number of Split Nodes",
        "Maximum Observed Tree Depth",
        "`DSM2_Node`",
        "`EXP`",
        "`VER`",
        "`SAC`",
        "`EAST`",
        "`XGEO`"
      ),
      `7-day` = c(
        "1000",
        "128",
        "12800",
        "127",
        "12700",
        "35",
        map_link("15 available nodes", "open_node_map_7day"),
        "321.580 - 14763.154",
        "639.961 - 62032.795",
        "6155.703 - 86690.764",
        "13.438 - 24552.754",
        "1808.487 - 14164.103"
      ),
      `30-day` = c(
        "1000",
        "128",
        "12800",
        "127",
        "12700",
        "36",
        map_link("39 available nodes", "open_node_map_30day"),
        "321.580 - 14763.154",
        "639.961 - 62032.795",
        "6155.703 - 86690.764",
        "13438 - 24552.754",
        "1808.487 - 14164.103"
      ),
      check.names = FALSE
    )
    
    data$`7-day` <- lapply(data$`7-day`, gt::html)
    data$`30-day` <- lapply(data$`30-day`, gt::html)
    
    gt(data) |>
      tab_row_group(
        label = html("<strong><em>Parameters</em></strong>"),
        rows = 1:6
      ) |>
      tab_row_group(
        label = html("<strong><em>Data Range</em></strong>"),
        rows = 7:12
      ) |>
      fmt_markdown(
        columns = Names,
        rows = 7:12
      ) |>
      cols_align(
        align = "center",
        columns = everything()
      ) |>
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center"
        ),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_text(
          align = "center",
          size = "medium"
        ),
        locations = cells_row_groups()
      ) |>
      tab_style(
        style = cell_borders(
          sides = c("left", "right", "top", "bottom"),
          color = "#c7d7da",
          weight = px(1)
        ),
        locations = list(
          cells_column_labels(),
          cells_body()
        )
      )
  })
  
  output$ecoptm_parameters_table <- render_gt({
    
    data <- data.frame(
      Names = c(
        "Number of Trees",
        "Number of Leaves per Tree",
        "Total Number of Leaf Nodes",
        "Number of Split Nodes per Tree",
        "Total Number of Split Nodes",
        "Maximum Observed Tree Depth",
        "`FPT`",
        "`YOL`",
        "`MOK`",
        "`DCC`"
      ),
      Interior = c(
        "500",
        "126 - 128",
        "63994",
        "125 - 127",
        "63494",
        "31",
        "6311.930 - 86690.764",
        "54.870 - 192337.793",
        "66.176 - 8652.699",
        "0, 1"
      ),
      Survival = c(
        "500",
        "124 - 128",
        "63988",
        "123 - 127",
        "63488",
        "30",
        "6311.930 - 86690.764",
        "54.870 - 192337.793",
        "66.176 - 8652.699",
        "0, 1"
      ),
      check.names = FALSE
    )
    
    gt(data) |>
      tab_row_group(
        label = html("<strong><em>Parameters</em></strong>"),
        rows = 1:6,
        id = "parameters"
      ) |>
      tab_row_group(
        label = html("<strong><em>Range</em></strong>"),
        rows = 7:10,
        id = "range"
      ) |>
      row_group_order(
        groups = c("parameters", "range")
      ) |>
      fmt_markdown(
        columns = Names,
        rows = 7:10
      ) |>
      cols_align(
        align = "center",
        columns = everything()
      ) |>
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center"
        ),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_text(
          weight = "bold",
          style = "italic",
          align = "center",
          size = "medium"
        ),
        locations = cells_row_groups()
      ) |>
      tab_style(
        style = cell_borders(
          sides = c("left", "right", "top", "bottom"),
          color = "#c7d7da",
          weight = px(1)
        ),
        locations = list(
          cells_column_labels(),
          cells_body()
        )
      )
  })
  
  output$horizon_datarange_table <- render_gt({
    
    badge <- function(text) sprintf(
      "<span style='display:inline-block;background:#f9e9ee;color:#c7254e;border:1px solid #efcbd5;border-radius:4px;padding:3px 8px;font-family:monospace;white-space:nowrap;'>%s</span>",
      text
    )
    
    data <- data.frame(
      Names = c("SAC", "EXP", "EAST", "XGEO", "ERL"),
      `Data Range` = c(
        "6155.703 - 86690.764",
        "321.580 - 14763.154",
        "13.438 - 24552.754",
        "1808.487 - 14164.103",
        "15:5:80, int"
      ),
      check.names = FALSE
    )
    
    gt(data) |>
      text_transform(
        locations = cells_body(columns = Names),
        fn = function(x) vapply(x, badge, character(1))
      ) |>
      cols_align(align = "center", columns = everything()) |>
      tab_style(
        style = cell_text(weight = "bold", align = "center"),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_borders(
          sides = c("left", "right", "top", "bottom"),
          color = "#c7d7da",
          weight = px(1)
        ),
        locations = list(cells_column_labels(), cells_body())
      ) |>
      cols_width(Names ~ px(220), `Data Range` ~ px(300)) |>
      tab_options(
        table.width = pct(100),
        table.font.names = "Segoe UI",
        table.font.size = px(14),
        data_row.padding = px(8)
      )
  })
  
  output$horizon_parameter_table <- render_gt({
    
    data <- data.frame(
      Names = c(
        "Number of Boosted Trees",
        "Total Number of Nodes",
        "Total Number of Internal Nodes",
        "Total Number of Leaf Nodes",
        "Minimum Nodes per Tree",
        "Maximum Nodes per Tree",
        "Maximum Tree Depth",
        "Number of Features"
      ),
      Parameters = c("1,200", "1,209,428", "604,114", "605,314", "475", "1567", "10", "5"),
      check.names = FALSE
    )
    
    gt(data) |>
      cols_align(align = "center", columns = everything()) |>
      tab_style(
        style = cell_text(weight = "bold", align = "center"),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_borders(
          sides = c("left", "right", "top", "bottom"),
          color = "#c7d7da",
          weight = px(1)
        ),
        locations = list(cells_column_labels(), cells_body())
      ) |>
      cols_width(Names ~ px(320), Parameters ~ px(220)) |>
      tab_options(
        table.width = pct(100),
        table.font.names = "Segoe UI",
        table.font.size = px(14),
        data_row.padding = px(8)
      )
  })
  ptm_maps_server(input = input,output = output,session = session)
  
}
