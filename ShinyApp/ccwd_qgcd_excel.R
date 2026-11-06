# CCWD QGCD input: local daily gross-channel-depletion Excel table (cfs)
# Expected layout: month headings OCT ... SEP; day numbers 1..31 down the rows.
# One CFS value is a DAILY RATE and its monthly-equivalent model input is
#   (daily cfs) * (86400 sec/day) / (43560 ft3/AF) * (days in that month) / 1000
# No hard-coded DAYFLOW monthly reference values are used.

ccwd_qgcd_excel_file <- function() {
  preferred <- trimws(Sys.getenv("CCWD_QGCD_FILE", ""))
  if (nzchar(preferred)) {
    path <- if (file.exists(preferred)) preferred else file.path("data", preferred)
    if (!file.exists(path)) stop("QGCD Excel file not found: ", preferred,
                                 ". Set CCWD_QGCD_FILE to a path under ShinyApp/data.")
    return(normalizePath(path))
  }
  files <- list.files("data", pattern="\\.xlsx?$", full.names=TRUE, ignore.case=TRUE)
  files <- files[!grepl("^~\\$", basename(files))]
  if (!length(files)) stop("No QGCD Excel workbook found under ShinyApp/data. Add the daily-depletion Excel file there.")
  hits <- files[grepl("qgcd|gross.?channel|depletion|qgcd|gcd", basename(files), ignore.case=TRUE)]
  if (length(hits) == 1L) return(normalizePath(hits))
  if (length(hits) > 1L) stop("More than one depletion workbook in ShinyApp/data: ",
                              paste(basename(hits), collapse=", "),
                              ". Set CCWD_QGCD_FILE to the desired filename.")
  if (length(files) == 1L) return(normalizePath(files))
  stop("Multiple Excel files in ShinyApp/data, and QGCD source is ambiguous: ",
       paste(basename(files), collapse=", "),
       ". Set CCWD_QGCD_FILE to the depletion workbook filename.")
}

ccwd_qgcd_parse_cell <- function(z) {
  if (is.null(z) || length(z) == 0L || is.na(z)) return(NA_real_)
  if (is.numeric(z)) return(as.numeric(z))
  x <- trimws(gsub(",", "", as.character(z), fixed=TRUE))
  if (!nzchar(x)) return(NA_real_)
  # Ignore footnote asterisks and whitespace after numbers; never turn blanks into zero.
  x <- sub("\\s*\\*+\\s*$", "", x)
  suppressWarnings(as.numeric(x))
}

ccwd_qgcd_read_table <- function(path=ccwd_qgcd_excel_file()) {
  if (!requireNamespace("readxl", quietly=TRUE)) stop("Package readxl is needed to read QGCD workbook. Install it with install.packages('readxl').")
  months <- c("OCT", "NOV", "DEC", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
  mm <- c(OCT=10L,NOV=11L,DEC=12L,JAN=1L,FEB=2L,MAR=3L,APR=4L,MAY=5L,JUN=6L,JUL=7L,AUG=8L,SEP=9L)
  sheets <- readxl::excel_sheets(path)
  for (sheet in sheets) {
    raw <- as.data.frame(readxl::read_excel(path, sheet=sheet, col_names=FALSE,
                                            .name_repair="minimal"), stringsAsFactors=FALSE)
    if (!nrow(raw) || !ncol(raw)) next
    raw[] <- lapply(raw, as.character)
    txt <- as.matrix(raw)
    norm <- toupper(trimws(txt))
    norm[is.na(norm)] <- ""
    norm <- matrix(gsub("[^A-Z]", "", as.vector(norm)), nrow=nrow(txt), ncol=ncol(txt))
    header_rows <- which(rowSums(matrix(norm %in% months, nrow=nrow(norm), ncol=ncol(norm))) >= 8L)
    if (!length(header_rows)) next
    header <- header_rows[1]
    cols <- vapply(months, function(m) {
      candidates <- which(norm[header, ] == m)
      if (length(candidates) == 1L) candidates else NA_integer_
    }, integer(1))
    if (anyNA(cols) || length(unique(cols)) != 12L) next
    potential_day_cols <- setdiff(seq_len(ncol(raw)), unname(cols))
    if (!length(potential_day_cols)) next
    # Day column has >=25 valid day integers below month header.
    score <- vapply(potential_day_cols, function(j) {
      v <- suppressWarnings(as.integer(trimws(txt[seq.int(header+1L, nrow(raw)), j])))
      sum(!is.na(v) & v >= 1L & v <= 31L)
    }, integer(1))
    if (max(score) < 25L) next
    day_col <- potential_day_cols[which.max(score)]
    dayvals <- suppressWarnings(as.integer(trimws(txt[seq.int(header+1L, nrow(raw)), day_col])))
    records <- list()
    for (m in months) {
      v <- rep(NA_real_,31)
      for (d in seq_len(31)) {
        matches <- which(!is.na(dayvals) & dayvals == d)
        if (length(matches)) v[d] <- ccwd_qgcd_parse_cell(txt[header+matches[1],cols[[m]]])
      }
      records[[m]] <- data.frame(month=mm[[m]],day=seq_len(31),cfs=v)
    }
    table <- do.call(rbind,records)
    rownames(table) <- NULL
    if (sum(is.finite(table$cfs)) < 330L) stop("QGCD workbook contains too few valid daily CFS values on sheet '",sheet,"'.")
    attr(table,"source_file") <- normalizePath(path)
    attr(table,"source_sheet") <- sheet
    return(table)
  }
  stop("Cannot find the day-by-month QGCD cfs table in ",basename(path),
       ". Expected header OCT NOV DEC JAN ... SEP and day numbers 1..31.")
}

ccwd_qgcd_table <- local({
  last_path <- NULL; last_mtime <- NULL; table <- NULL
  function() {
    p <- ccwd_qgcd_excel_file()
    mt <- file.info(p)$mtime
    if (is.null(table) || !identical(last_path,p) || !identical(last_mtime,mt)) {
      table <<- ccwd_qgcd_read_table(p)
      last_path <<- p; last_mtime <<- mt
    }
    table
  }
})

ccwd_qgcd_daily_month_equivalent <- function(dates) {
  dates <- as.Date(dates)
  if (!length(dates) || anyNA(dates)) stop("QGCD: valid calendar dates are required.")
  t <- ccwd_qgcd_table()
  m <- as.integer(format(dates,"%m")); d <- as.integer(format(dates,"%d"))
  idx <- match(paste(m,d), paste(t$month,t$day))
  cfs <- t$cfs[idx]
  if (any(!is.finite(cfs))) {
    stop("QGCD: daily depletion values missing from Excel workbook for: ",
         paste(format(dates[!is.finite(cfs)], "%Y-%m-%d"),collapse=", "))
  }
  month_first <- as.Date(format(dates,"%Y-%m-01"))
  month_next <- as.Date(vapply(seq_along(month_first), function(i) {
    format(seq(month_first[i], by="month", length.out=2)[2], "%Y-%m-%d")
  }, character(1)))
  ndays <- as.integer(month_next - month_first)
  # 1 cfs = 86400/43560 AF/day. Daily cfs is scaled to monthly-equivalent TAF.
  cfs * (86400/43560) * ndays / 1000
}

ccwd_qgcd_for_dates <- function(dates) {
  mean(ccwd_qgcd_daily_month_equivalent(dates))
}

ccwd_qgcd_latest_default <- function() {
  # Latest 7 complete calendar dates, ending yesterday to avoid partial-day defaults.
  dates <- seq(Sys.Date()-7, Sys.Date()-1, by="day")
  ccwd_qgcd_for_dates(dates)
}

ccwd_qgcd_default_or_na <- function() {
  tryCatch(ccwd_qgcd_latest_default(), error=function(e) {
    warning("QGCD Excel: ",conditionMessage(e), call.=FALSE)
    NA_real_
  })
}
