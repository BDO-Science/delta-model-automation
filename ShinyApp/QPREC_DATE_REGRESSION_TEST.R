# Run from within the extracted ShinyApp folder. No network required.
source("ccwd_qprec_sources.R")
f <- "SCKC1_VERIFIED_FORMAT_SAMPLE.csv"
stopifnot(file.exists(f))
lines <- readLines(f, warn=FALSE)
daily <- ccwd_sckc1_parse_daily(lines,as.Date("2026-06-01"),as.Date("2026-06-14"))
stopifnot(nrow(daily)==14L, all(is.finite(daily$precip_in)),
          all(daily$precip_in == 0))
result <- ccwd_qprec_from_daily(daily,seq(as.Date("2026-06-05"),as.Date("2026-06-14"),by="day"))
stopifnot(nrow(result)==10L,all(result$qprec_taf_day==0),
          all(result$qprec_taf_month==0))
# One inch of rain on 2026-06-05 contributes equally to June 5..9.
wet <- daily; wet$precip_in[wet$date==as.Date("2026-06-05")] <- 1
wetcalc <- ccwd_qprec_from_daily(wet,seq(as.Date("2026-06-05"),as.Date("2026-06-10"),by="day"))
expected <- 682230 / 12000 / 5
stopifnot(isTRUE(all.equal(wetcalc$qprec_taf_day[1:5],rep(expected,5))),
          wetcalc$qprec_taf_day[6]==0,
          isTRUE(all.equal(wetcalc$qprec_taf_month[1],expected*30)))
# Missing data must not be converted to zero.
broken <- daily;broken$precip_in[broken$date==as.Date("2026-06-06")] <- NA_real_
err <- tryCatch(ccwd_qprec_from_daily(broken,as.Date("2026-06-08")),error=function(e)e)
stopifnot(inherits(err,"error"),grepl("2026-06-06",conditionMessage(err),fixed=TRUE))
# Ensure Exploration's indexed looping preserves class.
ends <- rev(daily$date)
for (i in seq_along(ends)) {
  d <- ends[i]
  stopifnot(inherits(d,"Date"),inherits(seq(d-6L,d,by="day"),"Date"))
}
cat("PASS: SCKC1 daily parse, legitimate zero, five-day runoff, monthly scaling, missing-day detection, Date-class iteration.\\n")
