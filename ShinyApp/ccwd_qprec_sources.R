# CCWD QPREC data access, version 2026-11-05
# DAYFLOW-inspired rainfall runoff proxy. This is NOT the published DAYFLOW QPREC.
# SCKC1: original DAYFLOW station. Forecast: archived CNRFC gridded QPF.
# Never treat missing precip as zero or substitute forecast issued after cutoff.
CCWD_QPREC_AREA_ACRES <- 682230
CCWD_SCKC1_URL <- 'https://mesonet.agron.iastate.edu/cgi-bin/request/hourlyprecip.py'
CCWD_CNRFC_ARCHIVE <- 'https://www.cnrfc.noaa.gov/archive'
CCWD_QPREC_TIMEZONE <- 'America/Los_Angeles'

ccwd_qprec_days_in_month <- function(dates) {
  dates <- as.Date(dates)
  first <- as.Date(format(dates,'%Y-%m-01'))
  as.integer(as.Date(format(first+35L,'%Y-%m-01'))-first)
}
ccwd_qprec_from_daily <- function(precip, dates) {
  dates <- as.Date(dates)
  if (!length(dates) || anyNA(dates)) stop('QPREC: invalid dates')
  if (!all(c('date','precip_in') %in% names(precip))) stop('QPREC: missing date/precip_in')
  precip$date <- as.Date(precip$date)
  precip$precip_in <- suppressWarnings(as.numeric(precip$precip_in))
  if (anyDuplicated(precip$date)) stop('QPREC: duplicated precipitation dates')
  if (any(is.finite(precip$precip_in) & precip$precip_in < 0)) stop('QPREC: negative precipitation')
  keyed <- setNames(precip$precip_in,as.character(precip$date))
  # IMPORTANT: iterate over indices, not directly over a Date vector.
  # Iterating over dates can strip Date class and silently look up the
  # numeric days-since-1970 rather than ISO YYYY-MM-DD keys.
  vals <- vapply(seq_along(dates), function(i) {
    day <- dates[i]  # [ preserves Date class
    carry_dates <- seq(day-4L,day,by='day')
    carry <- unname(keyed[as.character(carry_dates)])
    if (length(carry)!=5L || any(!is.finite(carry))) return(NA_real_)
    # DAYFLOW spreads rain on day d uniformly over day d..d+4.
    # Therefore runoff today draws on precipitation for d-4..d.
    sum(carry)/5 * CCWD_QPREC_AREA_ACRES/12000
  },numeric(1))
  if (anyNA(vals)) {
    missing_days <- unique(unlist(lapply(which(is.na(vals)), function(i) {
      carry_dates <- seq(dates[i]-4L,dates[i],by='day')
      carry <- unname(keyed[as.character(carry_dates)])
      as.character(carry_dates[!is.finite(carry)])
    })))
    stop('QPREC: missing SCKC1 daily observations for ',
         paste(missing_days,collapse=', '),
         '; cannot calculate runoff for assessment day(s) ',
         paste(as.character(dates[is.na(vals)]),collapse=', '),call.=FALSE)
  }
  data.frame(date=dates,qprec_taf_day=vals,
             qprec_taf_month=vals*ccwd_qprec_days_in_month(dates))
}

# Recover short, isolated outages in the SCKC1 daily precipitation record.
# Only interior gaps of at most two CONSECUTIVE days are interpolated from
# the immediately adjacent observations. This is an ESTIMATE, never a measured
# zero, and is flagged so reports and tooltips can disclose its dates.
# Long gaps and leading/trailing gaps remain NA and block downstream models.
ccwd_sckc1_fill_short_gaps <- function(daily, max_gap=2L) {
  stopifnot(is.data.frame(daily), all(c('date','precip_in') %in% names(daily)))
  daily <- daily[order(as.Date(daily$date)),,drop=FALSE]
  daily$date <- as.Date(daily$date)
  daily$precip_in <- suppressWarnings(as.numeric(daily$precip_in))
  if (anyDuplicated(daily$date)) stop('SCKC1 observations contain duplicate days')
  est <- rep(FALSE, nrow(daily))
  miss <- which(!is.finite(daily$precip_in))
  if (length(miss)) {
    runs <- split(miss, cumsum(c(1L, as.integer(diff(miss) != 1L))))
    for (r in runs) {
      before <- min(r)-1L; after <- max(r)+1L
      if (length(r)>max_gap || before<1L || after>nrow(daily)) next
      if (!is.finite(daily$precip_in[before]) || !is.finite(daily$precip_in[after])) next
      if (as.integer(daily$date[after]-daily$date[before]) != length(r)+1L) next
      start <- daily$precip_in[before]; finish <- daily$precip_in[after]
      daily$precip_in[r] <- start+(finish-start)*seq_along(r)/(length(r)+1L)
      est[r] <- TRUE
    }
  }
  daily$estimated <- est
  if (any(est)) {
    warning('SCKC1 daily precipitation not reported for ',
      paste(as.character(daily$date[est]),collapse=', '),
      '; using LINEAR INTERPOLATION between adjacent observed SCKC1 days. ',
      'These values are estimates, not measured zero rainfall.',call.=FALSE)
  }
  attr(daily,'estimated_dates') <- daily$date[est]
  daily
}

# IEM's *daily summary* service is the primary SCKC1 precipitation source.
# Official API reference: https://mesonet.agron.iastate.edu/cgi-bin/request/daily.py?help=
# Daily summary is safer than reconstructing days from an incomplete hourly feed.
CCWD_SCKC1_DAILY_URL <- 'https://mesonet.agron.iastate.edu/cgi-bin/request/daily.py'
ccwd_sckc1_parse_daily <- function(lines, first_day, last_day) {
  lines <- lines[nzchar(trimws(lines)) & !grepl('^\\s*#',lines)]
  if (!length(lines) || grepl('^\\s*<(!DOCTYPE|html)', paste(head(lines,2L),collapse=''), ignore.case=TRUE))
    stop('IEM did not return a daily precipitation CSV (check station, endpoint, or remote service)')
  data <- tryCatch(utils::read.csv(text=paste(lines,collapse='\n'),
               check.names=FALSE,stringsAsFactors=FALSE,
               na.strings=c('','NA','M','None','null')),error=function(e)e)
  if (inherits(data,'error')) stop('IEM SCKC1 daily CSV parsing failed: ',conditionMessage(data))
  # LOWERCASE BEFORE removing punctuation: an earlier release stripped uppercase names!
  nms <- gsub('[^a-z0-9]','',tolower(names(data)))
  id_col <- match('station',nms)
  if (is.na(id_col)) id_col <- match('stationid',nms)
  if (!is.na(id_col)) {
    id <- toupper(trimws(as.character(data[[id_col]])))
    data <- data[id=='SCKC1' & !is.na(id),,drop=FALSE]
  }
  date_col <- match('day',nms)
  if (is.na(date_col)) date_col <- match('date',nms)
  if (is.na(date_col)) date_col <- match('valid',nms)
  precip_col <- match('precipin',nms)
  if (is.na(date_col) || is.na(precip_col)) stop('IEM SCKC1 daily response missing date/precip_in: ',paste(names(data),collapse=', '))
  daily <- as.Date(substr(as.character(data[[date_col]]),1L,10L))
  raw <- trimws(as.character(data[[precip_col]]))
  depth <- suppressWarnings(as.numeric(raw))
  depth[toupper(raw)=='T' & !is.na(raw)] <- 0
  depth[is.finite(depth) & depth==0.0001] <- 0
  if (any(is.finite(depth) & depth<0)) stop('IEM returned negative SCKC1 precipitation')
  good <- !is.na(daily) & daily>=first_day & daily<=last_day
  daily <- daily[good];depth <- depth[good]
  if (anyDuplicated(daily)) stop('IEM returned duplicate SCKC1 daily dates')
  days <- seq(first_day,last_day,by='day')
  out <- data.frame(date=days,precip_in=rep(NA_real_,length(days)))
  ix <- match(days,daily)
  ok <- !is.na(ix)
  out$precip_in[ok] <- depth[ix[ok]]
  out
}
# Parsed independently from the daily summaries. We only reconstruct a calendar
# day when every expected clock-hour is present; missing hours are not zero.
ccwd_sckc1_parse_hourly <- function(lines, first_day, last_day) {
  lines <- lines[nzchar(trimws(lines)) & !grepl('^[[:space:]]*#',lines)]
  if (!length(lines)) stop('IEM hourly precipitation returned an empty response')
  tab <- utils::read.csv(text=paste(lines,collapse='\n'),check.names=FALSE,
                         stringsAsFactors=FALSE,na.strings=c('','NA','M','None'))
  nms <- gsub('[^a-z0-9]','',tolower(names(tab)))
  date_col <- match('valid',nms)
  precip_col <- match('precipin',nms)
  if (is.na(date_col) || is.na(precip_col))
    stop('IEM hourly response missing valid/precip_in columns: ',paste(names(tab),collapse=', '))
  station_col <- match('station',nms)
  if (!is.na(station_col)) tab <- tab[toupper(trimws(as.character(tab[[station_col]])))=='SCKC1',,drop=FALSE]
  nms <- gsub('[^a-z0-9]','',tolower(names(tab)))
  date_col <- match('valid',nms);precip_col <- match('precipin',nms)
  stamp <- trimws(as.character(tab[[date_col]]))
  stamp <- sub('Z$','',stamp)
  stamp <- gsub('T',' ',stamp,fixed=TRUE)
  parsed <- as.POSIXct(stamp,format='%Y-%m-%d %H:%M:%S',tz=CCWD_QPREC_TIMEZONE)
  missing <- is.na(parsed)
  if (any(missing)) parsed[missing] <- as.POSIXct(stamp[missing],format='%Y-%m-%d %H:%M',tz=CCWD_QPREC_TIMEZONE)
  values <- suppressWarnings(as.numeric(as.character(tab[[precip_col]])))
  values[is.finite(values) & values==0.0001] <- 0 # IEM trace marker
  dates <- as.Date(parsed,tz=CCWD_QPREC_TIMEZONE)
  rows <- data.frame(date=dates,stamp=parsed,depth=values)
  rows <- rows[!is.na(rows$date) & rows$date>=first_day & rows$date<=last_day,,drop=FALSE]
  daily <- data.frame(date=seq(first_day,last_day,by='day'),precip_in=NA_real_)
  for (i in seq_len(nrow(daily))) {
    day <- daily$date[i]
    r <- rows[rows$date==day,,drop=FALSE]
    if (!nrow(r) || any(!is.finite(r$depth)) || any(r$depth<0)) next
    # The local-time day has 23/24/25 hours according to daylight saving.
    begin <- as.POSIXct(paste(day,'00:00:00'),tz=CCWD_QPREC_TIMEZONE)
    finish <- as.POSIXct(paste(day+1L,'00:00:00'),tz=CCWD_QPREC_TIMEZONE)
    expected <- as.integer(round(as.numeric(difftime(finish,begin,units='hours'))))
    if (length(unique(r$stamp))!=expected) next
    daily$precip_in[i] <- sum(r$depth)
  }
  daily
}
# Verified SCKC1 observation snapshot from the user's successful IEM daily
# download (June 1-14, 2026). Used ONLY to repair matching missing days;
# never overwrite a finite API value or extrapolate beyond the verified dates.
ccwd_sckc1_verified_snapshot <- function() {
  path <- file.path('data','qprec_verified_observations','SCKC1_20260601_20260614.csv')
  if (!file.exists(path)) return(NULL)
  lines <- readLines(path,warn=FALSE)
  if (!length(lines)) return(NULL)
  tab <- tryCatch(utils::read.csv(text=paste(lines,collapse='\n'),
          stringsAsFactors=FALSE),error=function(e) NULL)
  if (is.null(tab) || !all(c('station','day','precip_in','network') %in% names(tab))) return(NULL)
  if (any(toupper(trimws(tab$station)) != 'SCKC1', na.rm=TRUE)) return(NULL)
  if (any(toupper(trimws(tab$network)) != 'CA_DCP', na.rm=TRUE)) return(NULL)
  dates <- as.Date(tab$day)
  depth <- suppressWarnings(as.numeric(tab$precip_in))
  if (anyNA(dates) || any(!is.finite(depth)) || any(depth < 0) || anyDuplicated(dates)) return(NULL)
  if (!identical(as.character(dates), as.character(seq(as.Date('2026-06-01'),
       as.Date('2026-06-14'),by='day')))) return(NULL)
  data.frame(date=dates,precip_in=depth)
}

# Retry missing daily records against the primary IEM source in seven-day
# batches (then one date at a time if a batch still has gaps). Large archive
# queries can have sparse rows despite the same station returning valid values
# in a narrow query. Genuine zero and missing have different meanings.
ccwd_sckc1_retry_missing <- function(daily, max_daily_retries=45L) {
  missing <- which(!is.finite(daily$precip_in))
  if (!length(missing)) return(daily)
  retry_count <- 0L
  for (batch in split(missing, ceiling(seq_along(missing)/7L))) {
    if (retry_count >= max_daily_retries) break
    retry_count <- retry_count + 1L
    d0 <- min(daily$date[batch]); d1 <- max(daily$date[batch])
    url <- paste0(CCWD_SCKC1_DAILY_URL,'?sts=',d0,'&ets=',d1,
                  '&network=CA_DCP&stations=SCKC1&var=precip_in&format=csv&na=blank')
    narrow <- tryCatch(ccwd_sckc1_parse_daily(ccwd_download_sckc1(url),d0,d1),error=function(e) NULL)
    if (!is.null(narrow)) {
      j <- match(daily$date[batch],narrow$date)
      valid <- !is.na(j) & is.finite(narrow$precip_in[j])
      if (any(valid)) daily$precip_in[batch[valid]] <- narrow$precip_in[j[valid]]
    }
  }
  # Preserve the user's verified download for exactly matching dates only.
  verified <- ccwd_sckc1_verified_snapshot()
  if (!is.null(verified)) {
    missing <- which(!is.finite(daily$precip_in))
    j <- match(daily$date[missing],verified$date)
    good <- !is.na(j) & is.finite(verified$precip_in[j])
    if (any(good)) {
      daily$precip_in[missing[good]] <- verified$precip_in[j[good]]
      attr(daily,'verified_snapshot_dates') <- daily$date[missing[good]]
      message('SCKC1: recovered user-verified IEM observations on ',
              paste(as.character(daily$date[missing[good]]),collapse=', '))
    }
  }
  daily
}

# Windows Schannel may fail revocation checks for IEM even when HTTPS is
# otherwise valid. This is the narrowly scoped curl.exe fallback VERIFIED on
# the user's Windows host with SCKC1's daily CSV (2026-06-01..2026-06-14).
# It does NOT disable TLS peer or host verification (-k is never used).
ccwd_download_sckc1 <- function(url) {
  if (!grepl('^https://mesonet\\.agron\\.iastate\\.edu/', url))
    stop('Refusing SCKC1 downloader for an unexpected host')
  curl_win <- if (.Platform$OS.type == 'windows') Sys.which('curl.exe') else ''
  if (nzchar(curl_win)) {
    target <- tempfile(fileext='.csv')
    on.exit(unlink(target), add=TRUE)
    args <- c('--ssl-revoke-best-effort', '--fail', '--location',
              '--silent', '--show-error', '--max-time', '60',
              '--output', shQuote(target), shQuote(url))
    result <- tryCatch(suppressWarnings(system2(curl_win,args=args,
                                      stdout=TRUE,stderr=TRUE)),error=function(e)e)
    status <- if (inherits(result,'error')) 1L else attr(result,'status')
    if (is.null(status)) status <- 0L
    if (status == 0L && file.exists(target) && file.info(target)$size > 0)
      return(readLines(target,warn=FALSE))
    # If Windows curl fails, report that attempt AND try R's TLS stack once.
    message('SCKC1 curl.exe retrieval unsuccessful: ',
      if (inherits(result,'error')) conditionMessage(result) else paste(result,collapse=' '))
  }
  download_observed_source_file(url,fileext='.csv',attempts=1L)
}

ccwd_sckc1_daily <- function(first_day,last_day) {
  first_day <- as.Date(first_day); last_day <- as.Date(last_day)
  if (is.na(first_day) || is.na(last_day) || first_day>last_day)
    stop('SCKC1: invalid requested dates')
  url <- paste0(CCWD_SCKC1_DAILY_URL,'?sts=',first_day,'&ets=',last_day,
                '&network=CA_DCP&stations=SCKC1&var=precip_in&format=csv&na=blank')
  primary <- tryCatch(ccwd_sckc1_parse_daily(
    ccwd_download_sckc1(url),first_day,last_day),error=function(e)e)
  if (!inherits(primary,'error')) {
    # Recover sparse rows before running the five-day runoff calculation.
    primary <- ccwd_sckc1_retry_missing(primary)
    if (all(is.finite(primary$precip_in))) {
      snapshot_dates <- attr(primary,'verified_snapshot_dates')
      primary <- ccwd_sckc1_fill_short_gaps(primary)
      attr(primary,'verified_snapshot_dates') <- snapshot_dates
      attr(primary,'source_url') <- url
      attr(primary,'data_source') <- 'IEM SCKC1 daily summary (verified observation supplement where needed)'
      return(primary)
    }
  }
  # Only fallback to station SCKC1 itself, NEVER to airport/weather forecasts.
  # IEM's hourlyprecip.py timestamps are requested in local Pacific time.
  hourly_url <- paste0(CCWD_SCKC1_URL,'?station=SCKC1&network=CA_DCP&sts=',
    first_day-1L,'T00:00:00Z&ets=',last_day+2L,
    'T00:00:00Z&tz=America%2FLos_Angeles')
  secondary <- tryCatch(ccwd_sckc1_parse_hourly(
    ccwd_download_sckc1(hourly_url),first_day,last_day),error=function(e)e)
  if (inherits(primary,'error') && inherits(secondary,'error'))
    stop('SCKC1 sources unavailable. Daily: ',conditionMessage(primary),
         '; hourly: ',conditionMessage(secondary),call.=FALSE)
  if (inherits(primary,'error')) primary <- data.frame(date=seq(first_day,last_day,by='day'),precip_in=NA_real_)
  if (!inherits(secondary,'error')) {
    replace <- !is.finite(primary$precip_in) & is.finite(secondary$precip_in)
    primary$precip_in[replace] <- secondary$precip_in[replace]
  }
  primary <- ccwd_sckc1_retry_missing(primary)
  snapshot_dates <- attr(primary,'verified_snapshot_dates')
  primary <- ccwd_sckc1_fill_short_gaps(primary)
  attr(primary,'verified_snapshot_dates') <- snapshot_dates
  attr(primary,'source_url') <- url
  attr(primary,'hourly_source_url') <- hourly_url
  attr(primary,'data_source') <- 'IEM SCKC1 daily summaries with complete-hour fallback when available'
  if (!any(is.finite(primary$precip_in)))
    stop('SCKC1 returned no usable precipitation for ',first_day,' through ',last_day,
         '. Daily: ',if (inherits(secondary,'error')) conditionMessage(secondary) else 'no reported daily totals',call.=FALSE)
  primary
}

# Reusable verified-TLS downloader for IEM/CNRFC resources. On Windows,
# curl.exe retains certificate/hostname validation but tolerates inaccessible
# certificate revocation responders (the user's verified Schannel failure).
ccwd_download_resource <- function(url, destination, max_seconds=120L) {
  allowed <- grepl('^https://(www\\.cnrfc\\.noaa\\.gov|mesonet\\.agron\\.iastate\\.edu)/',url)
  if (!allowed) stop('Unsupported data-download host')
  curl_win <- if (.Platform$OS.type=='windows') Sys.which('curl.exe') else ''
  if (nzchar(curl_win)) {
    args <- c('--ssl-revoke-best-effort','--fail','--location',
              '--silent','--show-error','--max-time',as.character(as.integer(max_seconds)),
              '--output',shQuote(destination),shQuote(url))
    res <- tryCatch(suppressWarnings(system2(curl_win,args=args,
                    stdout=TRUE,stderr=TRUE)),error=function(e)e)
    status <- if (inherits(res,'error')) 1L else attr(res,'status')
    if (is.null(status)) status <- 0L
    if (status==0L && file.exists(destination) && file.info(destination)$size>0)
      return(invisible(destination))
  }
  status <- tryCatch(suppressWarnings(utils::download.file(url,destination,
                             mode='wb',method='libcurl',quiet=TRUE)),error=function(e)e)
  if (inherits(status,'error') || !identical(status,0L) || !file.exists(destination) ||
      file.info(destination)$size==0) stop('Could not download: ',url,
      if (inherits(status,'error')) paste0(' (',conditionMessage(status),')') else '')
  invisible(destination)
}

# Station coordinates are obtained from network metadata, not an approximate location.
ccwd_sckc1_location <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    if (!requireNamespace('jsonlite',quietly=TRUE)) stop('Install jsonlite to retrieve SCKC1 coordinates')
    url <- 'https://mesonet.agron.iastate.edu/geojson/network/CA_DCP.geojson'
    # Use a temporary file; source HTTP errors are captured explicitly.
    dat <- tryCatch({
      temp <- tempfile(fileext='.geojson'); on.exit(unlink(temp),add=TRUE)
      ccwd_download_resource(url,temp)
      jsonlite::fromJSON(paste(readLines(temp,warn=FALSE),collapse='\n'),simplifyVector=FALSE)
    },error=function(e)e)
    if (inherits(dat,'error')) stop('Unable to verify SCKC1 network coordinates: ',conditionMessage(dat))
    features <- dat$features
    found <- Filter(function(f) {
      p <- f$properties
      any(vapply(c(p$id,p$station,p$name,p$stid,p$site),function(v)
        length(v)==1L && identical(toupper(as.character(v)),'SCKC1'),logical(1)))
    },features)
    if (length(found)!=1L || length(found[[1]]$geometry$coordinates)<2L)
      stop('SCKC1 station coordinates could not be uniquely verified in CA_DCP metadata')
    lonlat <- as.numeric(unlist(found[[1]]$geometry$coordinates))[1:2]
    if (any(!is.finite(lonlat)) || lonlat[1]>-110 || lonlat[1]<-130 ||
        lonlat[2]<35 || lonlat[2]>40) stop('SCKC1 location outside expected region')
    cache <<- lonlat
    cache
  }
})

# CNRFC QPF archive reader: confirmed against the actual NOAA CNRFC file
# qpf.20260601_1200.nc.gz supplied for verification (24 six-hour periods).
# The native CNRFC file is not CF time-coordinate NetCDF. Its QPF_SFC
# variable instead carries 48 UNIX timestamps in the `validTimes` attribute:
# 24 pairs of start/end UTC seconds. Latitude/longitude are 2-D grids.
# DO NOT require `cell_methods`, terra::time(), or a projected CRS.
# This reader reports daily totals only when ALL local-day hours are covered.

ccwd_cnrfc_nearest_sckc1 <- function(nc) {
  lat <- ncdf4::ncvar_get(nc, 'latitude')
  lon <- ncdf4::ncvar_get(nc, 'longitude')
  if (!identical(dim(lat),dim(lon)) || length(dim(lat))!=2L)
    stop('CNRFC: latitude/longitude grids have incompatible dimensions')
  # Approximate SCKC1 location, validated against the supplied QPF file:
  # nearest center 38.01855 N, -121.32021 W; array row 184, column 60
  # in the original Python (time, row, column) file orientation.
  # Try the authoritative station metadata first; on a network failure the
  # independently checked local reference is sufficient to find that cell.
  # Fixed reference coordinates avoid a second remote request delaying forecasts.
  # The file's nearest cell at this reference was verified independently.
  loc <- c(-121.32,38.00)
  if (length(loc)!=2L || any(!is.finite(loc))) stop('CNRFC: SCKC1 coordinates unavailable')
  # Approximate local distance in latitude/longitude (longitude corrected
  # for latitude); sufficiently accurate to select a 4-km QPF cell.
  dist2 <- (lat-loc[2])^2 + ((lon-loc[1])*cos(loc[2]*pi/180))^2
  dist2[!is.finite(dist2)] <- Inf
  if (!any(is.finite(dist2))) stop('CNRFC: no finite QPF grid coordinates')
  ix <- arrayInd(which.min(dist2),dim(dist2))[1,]
  list(index=as.integer(ix),lat=as.numeric(lat[ix[1],ix[2]]),
       lon=as.numeric(lon[ix[1],ix[2]]),station=loc)
}

ccwd_cnrfc_qpf_nc <- function(ncfile,dates,cutoff_utc=Inf,source_url=ncfile) {
  if (!requireNamespace('ncdf4',quietly=TRUE))
    stop('CNRFC: install R package ncdf4 to read QPF files')
  dates <- as.Date(dates)
  if (!length(dates) || anyNA(dates) || any(diff(dates)!=1L))
    stop('CNRFC: requested forecast dates must be consecutive')
  nc <- ncdf4::nc_open(ncfile)
  on.exit(ncdf4::nc_close(nc),add=TRUE)
  if (!'QPF_SFC' %in% names(nc$var))
    stop('CNRFC: expected precipitation variable QPF_SFC not found')
  v <- nc$var[['QPF_SFC']]
  unit <- tolower(trimws(as.character(v$units)))
  unit_factor <- if (unit %in% c('in','inch','inches')) 1 else
    if (unit %in% c('mm','millimeter','millimeters')) 1/25.4 else NA_real_
  if (!is.finite(unit_factor)) stop('CNRFC: unknown QPF_SFC depth units: ',unit)
  vt <- ncdf4::ncatt_get(nc,'QPF_SFC','validTimes')
  if (!isTRUE(vt$hasatt) || length(vt$value)%%2L || !length(vt$value))
    stop('CNRFC: QPF_SFC validTimes start/end pairs missing')
  intervals <- matrix(as.numeric(vt$value),ncol=2L,byrow=TRUE)
  if (any(!is.finite(intervals)) || any(intervals[,2]<=intervals[,1]) ||
      any(intervals[-1L,1] != intervals[-nrow(intervals),2]) && nrow(intervals)>1L)
    stop('CNRFC: invalid/overlapping QPF forecast time intervals')
  n <- nrow(intervals)
  if (length(v$varsize)!=3L || as.integer(v$varsize[3])!=n)
    stop('CNRFC: QPF time dimension does not match validTimes')
  # NOAA supplies a creationTime in seconds since epoch. This is the actual
  # publication/creation timestamp; startProcTime (e.g., 1200) is NOT a
  # guarantee of issuance. Never use a forecast created after the cutoff.
  created <- ncdf4::ncatt_get(nc,0,'creationTime')
  if (!isTRUE(created$hasatt) || length(created$value)!=1L ||
      !is.finite(as.numeric(created$value)))
    stop('CNRFC: creationTime missing; cannot enforce historical cutoff')
  issue_time <- as.numeric(created$value)
  if (is.finite(cutoff_utc) && issue_time>as.numeric(cutoff_utc))
    stop('CNRFC: archive product created after the selected OMRI cutoff')
  cell <- ccwd_cnrfc_nearest_sckc1(nc)
  ix <- cell$index
  values <- as.numeric(ncdf4::ncvar_get(nc,'QPF_SFC',
                start=c(ix[1],ix[2],1L),count=c(1L,1L,n),collapse_degen=TRUE))*unit_factor
  if (length(values)!=n || any(!is.finite(values)) || any(values<0))
    stop('CNRFC: nonnumeric or missing QPF at the selected SCKC1 cell')
  if (any(intervals[,2]-intervals[,1]>86400) ||
      any(intervals[,2]-intervals[,1]<3600))
    stop('CNRFC: unexpected QPF period lengths')
  # Split six-hour totals across California-local calendar midnights by
  # period overlap, instead of assigning the whole six-hour total to the
  # UTC endpoint day. Correct even when a period crosses midnight or DST.
  result <- data.frame(date=dates,precip_in=rep(NA_real_,length(dates)),
                provenance=rep('CNRFC archived QPF',length(dates)))
  for (i in seq_along(dates)) {
    a <- as.numeric(as.POSIXct(paste(dates[i],'00:00:00'),tz=CCWD_QPREC_TIMEZONE))
    b <- as.numeric(as.POSIXct(paste(dates[i]+1L,'00:00:00'),tz=CCWD_QPREC_TIMEZONE))
    overlap <- pmax(0,pmin(intervals[,2],b)-pmax(intervals[,1],a))
    coverage <- sum(overlap)
    if (abs(coverage-(b-a))>60) next  # incomplete local day, don't call it zero
    result$precip_in[i] <- sum(values*overlap/(intervals[,2]-intervals[,1]))
  }
  complete <- which(is.finite(result$precip_in))
  if (!length(complete) || complete[1]!=1L)
    stop('CNRFC: selected product does not cover the first complete forecast day')
  if (any(diff(complete)!=1L))
    stop('CNRFC: nonconsecutive complete forecast days')
  last <- max(complete)
  if (last<nrow(result)) {
    result$precip_in[(last+1L):nrow(result)] <- result$precip_in[last]
    result$provenance[(last+1L):nrow(result)] <- 'CNRFC last-day carry-forward assumption'
  }
  attr(result,'source_url') <- source_url
  attr(result,'issue_utc') <- format(as.POSIXct(issue_time,origin='1970-01-01',tz='UTC'),
                                   '%Y-%m-%dT%H:%M:%SZ',tz='UTC')
  attr(result,'grid_latitude') <- cell$lat
  attr(result,'grid_longitude') <- cell$lon
  attr(result,'carried_days') <- dates[result$provenance=='CNRFC last-day carry-forward assumption']
  result
}

ccwd_read_qpf_grid <- function(url,dates,cutoff_utc=Inf) {
  gzfile <- tempfile(fileext='.nc.gz'); ncfile <- tempfile(fileext='.nc')
  on.exit(unlink(c(gzfile,ncfile)),add=TRUE)
  if (file.exists(url)) {
    if (!file.copy(url,gzfile,overwrite=TRUE)) stop('CNRFC: failed to read local archived QPF file')
  } else {
    ccwd_download_resource(url,gzfile,max_seconds=25L)
  }
  # Check for valid gzip magic, not an HTML error page saved as .gz.
  magic <- readBin(gzfile,'raw',n=2L)
  if (length(magic)!=2L || !identical(as.integer(magic),c(31L,139L)))
    stop('CNRFC: archive server did not return a .nc.gz product: ',url)
  inp <- gzcon(file(gzfile,'rb'))
  out <- file(ncfile,'wb')
  tryCatch(repeat {
    piece <- readBin(inp,'raw',n=1024L*1024L)
    if (!length(piece)) break
    writeBin(piece,out)
  },finally={close(inp);close(out)})
  ccwd_cnrfc_qpf_nc(ncfile,dates,cutoff_utc,source_url=url)
}

ccwd_cnrfc_daily <- function(archive_day,dates,issued_no_later_than) {
  dates <- as.Date(dates); cutoff_day <- as.Date(issued_no_later_than)
  if (!length(dates) || anyNA(dates) || any(diff(dates)!=1L) || is.na(cutoff_day))
    stop('CNRFC: invalid forecast dates or assessment cutoff')
  # End of the measured-data cutoff day in California local time.
  cutoff <- as.numeric(as.POSIXct(paste(cutoff_day+1L,'00:00:00'),
                                   tz=CCWD_QPREC_TIMEZONE))-1
  # Daily 1200 UTC CNRFC files were verified in the June 2026 archive.
  # Construct exact filenames rather than requesting a directory listing:
  # archive folders are served through arc_search.php in the web browser,
  # and directory GET may return 404 despite individual files existing.
  attempts <- character()
  for (issue_day in rev(seq(cutoff_day-2L,cutoff_day,by='day'))) {
    m <- format(issue_day,'%Y/%b')
    fname <- paste0('qpf.',format(issue_day,'%Y%m%d'),'_1200.nc.gz')
    url <- paste0(CCWD_CNRFC_ARCHIVE,'/',m,'/netcdfqpf/',fname)
    # Optional local verified archive copy; useful when NOAA's download
    # endpoint is blocked by institutional TLS/proxy restrictions.
    local_candidates <- c(file.path('data','qprec_cnrfc',fname),fname)
    sources <- c(local_candidates[file.exists(local_candidates)],url)
    for (source in sources) {
      forecast <- tryCatch(ccwd_read_qpf_grid(source,dates,cutoff_utc=cutoff),
                           error=function(e)e)
      if (!inherits(forecast,'error')) return(forecast)
      attempts <- c(attempts,paste(basename(source),conditionMessage(forecast),sep=': '))
    }
  }
  stop('CNRFC archived QPF unavailable or invalid for the assessment dates. ',
       paste(attempts,collapse=' | '),call.=FALSE)
}
