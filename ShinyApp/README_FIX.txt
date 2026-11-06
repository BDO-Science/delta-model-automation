CCWD QPREC Date-Window Repair (2026-11-06)

Baseline: CCWD_Exploration_QPREC_Initialization_Fix.zip

FIX 1: ccwd_qprec_from_daily() iterates using seq_along(dates) with dates[i] so every five-day lookup uses ISO calendar dates rather than accidental numeric Date values. Missing-date errors now report actual missing SCKC1 observation dates.
FIX 2: Exploration seven-day end-date search uses indexed Date extraction; R for-in loops strip the Date class on many vector types, which can produce invalid dates or fail silently.
FIX 3: Added offline regression test using the user-verified 2026-06-01..14 SCKC1 0.0-inch daily CSV, a synthetic rainfall pulse, and missing-value checks.

This is a targeted update. No LightGBM, DCC, QGCD, map, or UI changes.

INSTALL: Replace app.R, server.R, ui.R, ccwd_qprec_sources.R in existing ShinyApp. Keep other files and LightGBM model assets. From that folder run source("QPREC_DATE_REGRESSION_TEST.R") then launch Shiny. 20260623 measured should compute valid zero QPREC for windows fully covered by the verified SCKC1 dates; any earlier date with genuinely missing rain remains missing and is named in the error.

VALIDATION LIMIT: An R interpreter and live HTTPS access are unavailable in this environment. Offline R regression test provided but NOT executed here; do not claim a runtime-verified fix. Historical CNRFC forecast QPF continues to use the existing unverified archive extraction code.
