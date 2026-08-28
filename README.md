# delta-model-automation

Automated workflows supporting Delta modeling and real-time forecasting for the Bay-Delta Office of Science (BDO-Science), developed by Stantec in coordination with the U.S. Bureau of Reclamation.

This repository is the central home for:
- **DSM2** automated weekly modeling workflow (Hydro, PTM, ECO-PTM, no-pumping scenarios)
- **PTM/ECO-PTM** post-processing scripts (neutrally-buoyant, position-oriented, and salmon particle results)
- **Zone of Influence (ZOI)** analysis (proportional overlap between pumping and no-pumping scenarios)
- **SacPAS** reporting code (Quarto documents feeding the SacPAS forecast summary)
- **Emulator / Event Horizon** dashboard code (Shiny app for real-time assessment)

## Background

Reclamation's Central Valley Operations (CVO) office issues weekly Delta inflow/export forecasts each Monday, which trigger a DSM2-based modeling pipeline used to produce the **Reclamation Real-Time Forecast Summary**. This repo automates that pipeline end-to-end, from raw forecast inputs through report-ready figures and tables.

## Repository Structure

| Folder | Contents |
|---|---|
| `DSM2/Model/` | Model configuration, input templates, and the `WeeklyWorkflow.bat` driver script |
| `DSM2/Model/DataExternal/` | Weekly forecast inputs (organized by `YYYYMMDD`); excluded from version control via `.gitignore` |
| `DSM2/Model/Output/`, `DSM2/Model/Figures/` | Model run outputs and generated figures (also excluded via `.gitignore`) |
| `DSM2/ZOI/` | Zone of Influence preprocessing and plotting scripts |
| `SacPAS/` | Quarto markdown and code supporting the SacPAS forecast summary |
| `ShinyApp/` | PTM Emulator / Event Horizon real-time assessment dashboard |

> **Note:** Data-heavy folders (`DataExternal`, `Output`, etc.) are intentionally excluded from the repo per `.gitignore` and must be created locally in the directory structure described in this documentation before running the workflow.

## System Requirements

| Use | Software | Link |
|---|---|---|
| Model | DSM2 8.2.2 | [data.cnra.ca.gov/dataset/dsm2](https://data.cnra.ca.gov/dataset/dsm2) |
| Contour plots | DSM2 Animator | [data.cnra.ca.gov/dataset/dsm2-animator](https://data.cnra.ca.gov/dataset/dsm2-animator) |
| Text editor | VS Code, Notepad++, or similar | [code.visualstudio.com](https://code.visualstudio.com/download) |
| HEC-DSS editor | HEC-DSSVue 2.0.1 | [hec.usace.army.mil](https://www.hec.usace.army.mil/software/hec-dssvue/downloads.aspx) |
| Time series viewer | HDFView | [hdfgroup.org](https://www.hdfgroup.org/download-hdfview/) |

Recommended OS: Windows. Recommended Python distribution: Miniconda.

## Getting Started

1. Clone this repository and create the required local folders (`DataExternal`, `Output`, `Figures`) alongside the tracked directories.
2. From the `/Model` directory, create the Python environment:
   ```
   conda env create -f environment.yml --solver=libmamba
   conda activate ops_dsm2
   conda install -c conda-forge -c cadwr-dms pyhecdss
   ```
3. Place the current week's forecast data (from CVO/Reclamation) into `Model/DataExternal/YYYYMMDD/`.
4. Run the weekly pipeline from `/Model`:
   ```
   WeeklyWorkflow.bat
   ```
   Enter the forecast date folder (`YYYYMMDD`) and confirm each prompt to step through: input updates → DSM2 Hydro/PTM/ECO-PTM → no-pumping Hydro → ZOI and PTM post-processing.
5. Outputs are written to `Model/Output/<week>`, `Model/Figures/<week>`, `Model_NOPUMP/Output/<week>`, and `ZOI/<week>` for use in the weekly forecast summary report.

## Key Acronyms

DSM2 – Delta Simulation Model II · PTM – Particle Tracking Model · ECO-PTM – Ecological Particle Tracking Model · NP – Neutrally Buoyant Particles (delta smelt larvae) · PP – Position/Surface-Oriented Particles (longfin smelt larvae) · SP – Salmon Particles (Chinook salmon) · OMR – Old and Middle River (Index) · ZOI – Delta Zone of Influence · CVO – Reclamation Central Valley Operations Office

## Contributors

Adam Witt, Laura Manuel, Puneet Khatavkar, Roja Kaveh-Garna, Sarah Hamilton, Elizabeth Simon (Stantec), in coordination with U.S. Bureau of Reclamation.

## License

Apache-2.0
