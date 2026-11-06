# Source diagnostics: run from the ShinyApp folder after installing app files.
# This script intentionally makes no assumptions about missing precipitation.
source('ccwd_qprec_sources.R')
if (!exists('download_observed_source_file')) {
  download_observed_source_file <- function(url,fileext='.csv',attempts=2L) {
    f <- tempfile(fileext=fileext); on.exit(unlink(f),add=TRUE)
    status <- utils::download.file(url,f,method='libcurl',mode='wb',quiet=TRUE)
    if (!identical(as.integer(status),0L)) stop('HTTP download failed: ',url)
    readLines(f,warn=FALSE)
  }
}
cat('\n--- QPREC: SCKC1 observed station ---\n')
start <- as.Date('2026-06-01'); finish <- as.Date('2026-06-14')
obs <- tryCatch(ccwd_sckc1_daily(start,finish),error=function(e)e)
if (inherits(obs,'error')) {
  cat('SCKC1 DOWNLOAD/FORMAT ERROR:',conditionMessage(obs),'\n')
} else {
  print(obs)
  cat('Valid days:',sum(is.finite(obs$precip_in)),'of',nrow(obs),'\n')
  cat('Source:',attr(obs,'source_url'),'\n')
  print(tryCatch(ccwd_qprec_from_daily(obs,seq(as.Date('2026-06-08'),as.Date('2026-06-14'),by='day')),
                 error=function(e) paste('QPREC cannot be calculated:',conditionMessage(e))))
}
cat('\n--- DCC: source URLs for measured flow ---\n')
for (sensor in c(20L,41L)) {
  for (duration in if (sensor==20L) c('H','D') else 'D') {
    u <- paste0('https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?',
       'Stations=DLC&SensorNums=',sensor,'&dur_code=',duration,
       '&Start=2026-06-08&End=2026-06-15')
    x <- tryCatch(download_observed_source_file(u),error=function(e)e)
    cat('\n',u,'\n')
    if (inherits(x,'error')) cat('ERROR:',conditionMessage(x),'\n')
    else print(head(x,5L))
  }
}
cat('\nMissing data are NOT assumed to be zero.\n')
