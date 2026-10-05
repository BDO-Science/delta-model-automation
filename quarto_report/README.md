# Delta Real-Time Forecast Summary

## Overview

This repository generates the weekly **Delta Real-Time Forecast Summary Report** using **Quarto** and **R**.

A single Quarto source file is used to generate all three report formats:

- HTML
- Microsoft Word
- PDF

The report automatically reads model outputs from a selected weekly results folder and generates a formatted report containing:

- Forecast Flow and Export summaries
- Zone of Influence (ZOI) analysis
- PTM analysis
- ECO-PTM analysis
- Longfin Smelt Larval Population and PTM analysis
- Interactive visualizations in the HTML report
- Static figure versions for Word and PDF where applicable
- Tables, figures, and supporting documentation

The workflow is designed so that routine weekly updates require **no modifications to the Quarto or R source code**. Users only need to update the weekly results folder, update the report configuration if needed, and render the desired report format.

---

# Repository Structure

```text
stantec_delta_realtime_forecast/
│
├── YYYYMMDD_results/
│   ├── average_exports_by_week.csv
│   ├── zoi_bins.csv
│   ├── common_assumptions.txt
│   ├── table2_notes.txt
│   ├── survival_combined.csv
│   ├── FlowExportReviewPlots.html
│   ├── flow_export.png
│   ├── ZOI_Map_week1.html
│   ├── ZOI_Map_week2.html
│   ├── ZOI_Map_week3.html
│   ├── ZOI_0.75Contour_week1.png
│   ├── ZOI_0.75Contour_week2.png
│   ├── ZOI_0.75Contour_week3.png
│   ├── ZOI_Proportional_ChannelLength_week1.html
│   ├── ZOI_Proportional_ChannelLength_week2.html
│   ├── ZOI_Proportional_ChannelLength_week3.html
│   ├── ZOI_Proportional_ChannelLength_week1.png
│   ├── ZOI_Proportional_ChannelLength_week2.png
│   ├── ZOI_Proportional_ChannelLength_week3.png
│   ├── PTM_*.html
│   ├── NP_*.png
│   ├── PP_*.png
│   ├── LFS_Scenario_*.html
│   ├── LFS_PP_*.png
│   ├── *_entrainment_OMR_*.html
│   ├── *.csv
│   ├── *.png
│   └── ...
│
└── quarto_report_new/
    ├── Delta_realtime_forecast_summary.qmd
    ├── Delta_realtime_forecast_summary.docx
    ├── Delta_realtime_forecast_summary.html
    ├── Delta_realtime_forecast_summary.pdf
    ├── helper_functions.R
    ├── report_config.yml
    ├── styles.css
    ├── static_longfin_nodes.csv
    ├── static_sls_processing.csv
    ├── map_delta_longfinsmelt.png
    ├── map_delta_longfinsmelt_region.png
    ├── ptm_injection_flux_locations.png
    └── README.md
```

---

# Main Files

## Delta_realtime_forecast_summary.qmd

The main Quarto report containing:

- Report structure
- Narrative text
- Section headings
- Figure captions
- Table captions
- Calls to helper functions
- HTML, Word, and PDF output configuration

The same `.qmd` file is used to render all three report formats.

The report automatically determines the available OMR scenarios from the weekly input files. No scenario-specific code modifications are required.

---

## helper_functions.R

Contains the functions used to generate and format the report.

### Forecast Section

- Weekly Averaged Forecasted Flow Data
- Weekly Averaged Export Tables
- Interactive Flow Export visualization for HTML
- Static PNG figure insertion for Word and PDF

### Zone of Influence (ZOI)

- Weekly ZOI summary tables
- Interactive ZOI maps
- Interactive proportional channel length figures
- Static PNG versions for Word and PDF
- Grayscale basemap formatting for improved visibility of ZOI boundaries
- Colored legend lines matching the corresponding OMR boundary colors
- Reset Map control for interactive Leaflet maps

### PTM Analysis

- Particle fate summary tables
- Interactive Neutral Particle figures
- Interactive Surface-Oriented Particle figures
- Static PNG versions for Word and PDF
- Figure sizing adjustments to reduce unnecessary blank space

### ECO-PTM Analysis

- Route ratio summary tables
- Route survival summary tables
- Automatic detection of all model runs from `survival_combined.csv`

### Longfin Smelt Analysis

- Static processing tables
- Injection node tables
- Weekly entrainment estimate tables
- Interactive LFS scenario figures
- Static PNG versions of LFS scenario figures for Word and PDF
- Interactive cumulative entrainment maps

The final cumulative entrainment maps are included only in the HTML report. These figures and their captions are intentionally omitted from the Word and PDF versions.

### Utility Functions

- Automatic results folder selection
- Automatic OMR scenario detection
- Format-specific figure handling
- Interactive HTML figure insertion
- Static PNG figure insertion for Word and PDF
- Leaflet map formatting
- Reset Map functionality
- Caption formatting
- Static CSV table generation
- Number formatting
- PDF-safe table rendering

---

## report_config.yml

Controls which weekly results folder is used when rendering the report.

Example:

```yaml
results_date: "20260623"
```

The report automatically uses:

```text
20260623_results
```

If left blank:

```yaml
results_date: ""
```

the report automatically selects the most recent available results folder.

---

## styles.css

Contains HTML report styling including:

- Fonts
- Table formatting
- Figure and caption formatting
- Interactive HTML figure sizing
- Layout adjustments

Additional figure and Leaflet-specific formatting is handled through `helper_functions.R`.

---

# Multi-Format Report Workflow

The report uses the same:

```text
Delta_realtime_forecast_summary.qmd
```

to generate HTML, Word, and PDF outputs.

Format-specific behavior is handled automatically by the helper functions.

## HTML

The HTML version includes:

- Interactive Plotly figures
- Interactive Leaflet maps
- Reset Map controls
- Grayscale OpenStreetMap basemaps
- ZOI legend colors matching the map boundaries
- Hover functionality
- Interactive cumulative entrainment maps

The original CARTO basemap was replaced with **OpenStreetMap** to avoid the CARTO API key requirement.

## Word and PDF

For figures that are interactive in HTML, the Word and PDF reports use the corresponding available PNG versions.

This provides consistent static versions of the figures without requiring HTML or browser-based rendering during Word or PDF generation.

The cumulative entrainment maps at the end of the Longfin Smelt section are HTML-only and are not included in Word or PDF.

---

# Weekly Update Workflow

## Step 1

Copy the new weekly outputs into a folder named:

```text
YYYYMMDD_results
```

Example:

```text
20260623_results
```

This folder should contain all required CSV, PNG, HTML, and text files, including:

```text
common_assumptions.txt
table2_notes.txt
```

as well as the corresponding HTML and PNG figure outputs required by the report.

---

## Step 2

Update `report_config.yml`.

Example:

```yaml
results_date: "20260623"
```

Or leave blank:

```yaml
results_date: ""
```

to automatically select the newest available results folder.

---

## Step 3

Open:

```text
Delta_realtime_forecast_summary.qmd
```

and render the desired report format.

No additional code modifications should be required for routine weekly updates.

---

# Static Files

The following files remain in the repository and typically do not change between weekly reports.

## Static Figures

```text
map_delta_longfinsmelt.png
map_delta_longfinsmelt_region.png
ptm_injection_flux_locations.png
```

## Static Tables

```text
static_longfin_nodes.csv
static_sls_processing.csv
```

---

# Rendering the Report

## Using RStudio

Open:

```text
Delta_realtime_forecast_summary.qmd
```

and use the Render options to generate the desired format.

---

## Using the Terminal

### HTML

```bash
quarto render Delta_realtime_forecast_summary.qmd --to html
```

### Microsoft Word

```bash
quarto render Delta_realtime_forecast_summary.qmd --to docx
```

### PDF

```bash
quarto render Delta_realtime_forecast_summary.qmd --to pdf
```

---

# Required R Packages

```r
install.packages(c(
  "readr",
  "dplyr",
  "tidyr",
  "readxl",
  "yaml",
  "knitr",
  "kableExtra",
  "htmltools",
  "xml2",
  "stringr"
))
```

Quarto must also be installed.

PDF rendering requires a working LaTeX installation, such as TinyTeX or TeX Live.

---

# Output

The same Quarto source file can generate:

```text
Delta_realtime_forecast_summary.html
Delta_realtime_forecast_summary.docx
Delta_realtime_forecast_summary.pdf
```

When the HTML, Word, and PDF files are stored together in the same accessible folder, the Word and PDF download links included in the HTML report can be used to access the corresponding files.

---

# Figure Handling by Format

## HTML

Interactive HTML figures are retained for:

- Forecast Flow and Export
- ZOI
- PTM
- Longfin Smelt scenarios
- Cumulative entrainment maps

Leaflet maps use a grayscale OpenStreetMap basemap to improve the visibility of model boundaries and overlays.

ZOI map legends display line colors corresponding to the actual OMR boundary colors.

The cumulative entrainment maps include explanatory notes describing:

- Cumulative entrainment flux (%)
- Estimated LFS subregional abundance
- Hover functionality for marker values
- Subregions where no LFS larvae were estimated
- ZOI boundary colors for Weeks 1, 2, and 3

## Word and PDF

Where corresponding PNG files are available, static versions of the interactive figures are inserted automatically.

The final cumulative entrainment maps are excluded entirely from Word and PDF, including their figure captions.

---

# Notes

- All weekly report content is automatically loaded from the selected results folder.
- OMR scenarios are detected dynamically from the available input files.
- A single `.qmd` file is used to generate HTML, Word, and PDF outputs.
- Interactive figures are preserved in HTML and replaced with corresponding PNG versions in Word and PDF where applicable.
- OpenStreetMap is used as the Leaflet basemap instead of CARTO.
- Leaflet basemaps are displayed in grayscale so model boundaries and overlays are easier to distinguish.
- ZOI map legends use the same colors as the corresponding OMR boundaries.
- Interactive Leaflet maps include a Reset Map option.
- ECO-PTM route survival tables are generated automatically from `survival_combined.csv`.
- Weekly assumptions and Forecast Table notes are populated directly from text files within the results folder.
- Static maps and lookup tables remain in the repository.
- The cumulative entrainment maps are HTML-only and are omitted from Word and PDF.
- Routine weekly updates should only require replacing the contents of the weekly results folder, updating `report_config.yml` if desired, and rendering the report.
