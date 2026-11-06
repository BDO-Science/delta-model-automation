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
  if (!inherits(primary,'error') && all(is.finite(primary$precip_in))) {
    attr(primary,'source_url') <- url
    attr(primary,'data_source') <- 'IEM SCKC1 daily summary'
    return(primary)
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
ccwd_download_resource <- function(url, destination) {
  allowed <- grepl('^https://(www\\.cnrfc\\.noaa\\.gov|mesonet\\.agron\\.iastate\\.edu)/',url)
  if (!allowed) stop('Unsupported data-download host')
  curl_win <- if (.Platform$OS.type=='windows') Sys.which('curl.exe') else ''
  if (nzchar(curl_win)) {
    args <- c('--ssl-revoke-best-effort','--fail','--location',
              '--silent','--show-error','--max-time','120',
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

# Native CNRFC archive reader. Uses a gridded QPF issued no later than selected
# historical assessment and only accepts explicit valid times and units.
ccwd_cnrfc_daily <- function(archive_day,dates,issued_no_later_than) {
  dates <- as.Date(dates)
  if (!length(dates) || anyNA(dates) || any(diff(dates)!=1L))
    stop('CNRFC: forecast dates must be consecutive')
  if (!requireNamespace('terra',quietly=TRUE))
    stop('CNRFC: install R package terra for archived NetCDF forecast extraction')
  if (!requireNamespace('xml2',quietly=TRUE))
    stop('CNRFC: install R package xml2 for automatic archive discovery')
  if (!requireNamespace('ncdf4',quietly=TRUE))
    stop('CNRFC: install R package ncdf4 for NetCDF unit verification')
  cutoff <- as.POSIXct(paste(as.Date(issued_no_later_than)+1L,'00:00:00'),
                       tz=CCWD_QPREC_TIMEZONE)-1
  archive_dates <- seq(as.Date(issued_no_later_than)-3L,
                       as.Date(issued_no_later_than),by='day')
  monthly <- unique(format(archive_dates,'%Y/%b'))
  candidates <- list()
  for (month in monthly) {
    base <- paste0(CCWD_CNRFC_ARCHIVE,'/',month,'/netcdfqpf/')
    page <- tryCatch({
      temp <- tempfile(fileext='.html'); on.exit(unlink(temp),add=TRUE)
      ccwd_download_resource(base,temp)
      xml2::read_html(temp)
    },error=function(e)NULL)
    if (is.null(page)) next
    href <- xml2::xml_attr(xml2::xml_find_all(page,'.//a[@href]'),'href')
    href <- unique(basename(sub('\\?.*','',href)))
    href <- href[grepl('\\.nc(\\.gz)?$',href,ignore.case=TRUE)]
    for (fname in href) {
      stamp <- regmatches(fname,regexpr('[0-9]{8}[_\\.]?[0-9]{4}',fname))
      if (!length(stamp) || !nzchar(stamp)) next
      iss <- as.POSIXct(gsub('[^0-9]','',stamp),format='%Y%m%d%H%M',tz='UTC')
      if (!is.na(iss) && iss<=cutoff)
        candidates[[length(candidates)+1L]] <- list(url=paste0(base,fname),issue=iss)
    }
  }
  if (!length(candidates)) stop('CNRFC: no archived NetCDF QPF found for issuance on/before ',
    format(cutoff,tz='UTC'),' (archive may be temporarily inaccessible)')
  issuance <- vapply(candidates,function(x) as.numeric(x$issue),numeric(1))
  # Latest issue first, try next if newest grid fails validation.
  order_ids <- order(issuance,decreasing=TRUE)
  last_error <- NULL
  for (index in order_ids) {
    item <- candidates[[index]]
    result <- tryCatch(ccwd_read_qpf_grid(item$url,dates,item$issue),error=function(e)e)
    if (inherits(result,'error')) { last_error <- conditionMessage(result); next }
    return(result)
  }
  stop('CNRFC: archived QPF grids found but none passed time, units, coverage and location checks. ',
       'Last error: ',last_error)
}

ccwd_read_qpf_grid <- function(url,dates,issue_utc) {
  gzfile <- tempfile(fileext='.nc.gz')
  ncfile <- tempfile(fileext='.nc')
  on.exit(unlink(c(gzfile,ncfile)),add=TRUE)
  res <- tryCatch({ccwd_download_resource(url,gzfile); 0L},error=function(e)e)
  if (inherits(res,'error') || !file.exists(gzfile) || file.info(gzfile)$size==0L)
    stop('Could not download CNRFC grid ',url)
  input <- if (grepl('\\.gz$',url,ignore.case=TRUE)) gzcon(file(gzfile,'rb')) else file(gzfile,'rb')
  output <- file(ncfile,'wb')
  tryCatch({
    repeat {
      chunk <- readBin(input,what='raw',n=1024L*1024L)
      if (!length(chunk)) break
      writeBin(chunk,output)
    }
  },finally={close(input);close(output)})
  nc <- ncdf4::nc_open(ncfile)
  on.exit(ncdf4::nc_close(nc),add=TRUE)
  vars <- names(nc$var)
  choices <- grep('qpf|precip|rain',vars,ignore.case=TRUE,value=TRUE)
  if (length(choices)!=1L) stop('CNRFC: precipitation variable is not unambiguous: ',paste(vars,collapse=', '))
  variable <- choices[1]
  unit <- tolower(trimws(as.character(nc$var[[variable]]$units)))
  factor <- if (unit %in% c('mm','millimeter','millimeters')) 1/25.4 else
            if (unit %in% c('in','inch','inches')) 1 else NA_real_
  if (!is.finite(factor)) stop('CNRFC: unsupported QPF depth units: ',unit)
  r <- terra::rast(ncfile,subds=variable)
  if (is.na(terra::crs(r)) || !nzchar(terra::crs(r)))
    stop('CNRFC: grid projection is unavailable')
  valid <- terra::time(r)
  if (!inherits(valid,'POSIXct') || length(valid)!=terra::nlyr(r) || anyNA(valid))
    stop('CNRFC: forecast-valid timestamps not available from NetCDF metadata')
  if (any(diff(as.numeric(valid))<=0)) stop('CNRFC: QPF valid times are not increasing')
  if (any(valid<=issue_utc)) stop('CNRFC: valid period precedes forecast issue')
  # Reject unknown cumulative-vs-period definitions (cannot sum unclassified layers).
  method <- ncdf4::ncatt_get(nc,variable,'cell_methods')
  if (!isTRUE(method$hasatt) || !grepl('time:\\s*sum',method$value,ignore.case=TRUE))
    stop('CNRFC: NetCDF does not confirm that layers are period precipitation totals')
  loc <- ccwd_sckc1_location()
  station <- terra::vect(matrix(loc,nrow=1),type='points',crs='EPSG:4326')
  point <- terra::project(station,terra::crs(r))
  values <- as.numeric(terra::extract(r,point)[1,-1,drop=TRUE])*factor
  if (length(values)!=length(valid) || any(!is.finite(values)) || any(values<0))
    stop('CNRFC: invalid or outside-grid SCKC1 QPF values')
  # Valid-time day is in local station timezone; per-period precipitation
  # must have fixed nonoverlapping period timing.
  step <- diff(as.numeric(valid))/3600
  if (length(step) && (any(step>24) || any(step<1)))
    stop('CNRFC: unsupported forecast time spacing')
  if (!length(step) || any(abs(step-step[1])>.01))
    stop('CNRFC: irregular QPF periods; cannot safely attribute to local days')
  # Assign each timestamp to day of *period midpoint*, not to the end day.
  interval <- step[1]
  center <- valid - interval*3600/2
  day <- as.Date(center,tz=CCWD_QPREC_TIMEZONE)
  daily <- stats::aggregate(values,list(day),sum)
  names(daily) <- c('date','precip_in')
  daily$date <- as.Date(daily$date)
  # A daily sum is valid only if all expected hourly periods are represented.
  counts <- table(day)
  expected <- 24/interval
  daily$precip_in[as.numeric(counts[as.character(daily$date)]) < expected] <- NA_real_
  idx <- match(dates,daily$date)
  available <- !is.na(idx) & is.finite(daily$precip_in[idx])
  if (!any(available) || !available[1]) stop('CNRFC: no complete precipitation forecast for first forecast day')
  if (any(!available[seq_len(max(which(available)))]))
    stop('CNRFC: missing forecast days within available QPF horizon')
  out <- data.frame(date=dates,precip_in=NA_real_,provenance='CNRFC archived QPF')
  out$precip_in[available] <- daily$precip_in[idx[available]]
  last <- max(which(available))
  if (last<length(dates)) {
    out$precip_in[(last+1L):length(dates)] <- out$precip_in[last]
    out$provenance[(last+1L):length(dates)] <- 'CNRFC last-day carry-forward assumption'
  }
  attr(out,'source_url') <- url
  attr(out,'issue_utc') <- format(issue_utc,'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
  out
}
