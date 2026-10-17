# -*- coding: utf-8 -*-
"""Create an interactive LFS entrainment map directly from DSM2 outputs.

Usage:
    python entrainment_map.py YYYYMMDD
    python entrainment_map.py YYYYMMDD --omr -2000
"""
from __future__ import annotations

import argparse
import json
import re
import sys
import warnings
from pathlib import Path

import branca.colormap as cm
import folium
import geojsoncontour
import geopandas as gpd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from pyhecdss import DSSFile
from pyproj import Transformer
from scipy.interpolate import griddata

# Repository-relative directories. entrainment_map.py is stored in DSM2/Model.
MODEL_DIR = Path(__file__).resolve().parent
DSM2_DIR = MODEL_DIR.parent
PTM_OUTPUT_ROOT = MODEL_DIR / "Output"
FIGURE_ROOT = MODEL_DIR / "Figures"
SLS_ROOT = DSM2_DIR / "SLS"
ZOI_ROOT = DSM2_DIR / "ZOI"
DSM2_GRID_FILE = (
    DSM2_DIR / "references" / "DSM2nodeCOOR_8_2_0calibratednodes.csv"
)
EDSM_SHAPEFILE = (
    DSM2_DIR / "references" / "fmwt_regions_contiguous"
    / "fmwt_regions_contiguous.shp"
)

SCENARIO_OMR = {"A": -5000, "B": -3500, "C": -2000, "D": -6500}
ZOI_SCENARIO_ROW = {-2000: 0, -3500: 1, -5000: 2, -6500: 3}
PTM_NODES = [
    293, 323, 350, 304, 249, 34, 41, 46, 351, 353, 29,
    86, 99, 225, 75, 145, 329, 420, 227, 365, 359,
]
REQUIRED_SLS_COLUMNS = {"survDate", "Survey", "SubRegion", "Subregion_Abund"}
REQUIRED_GRID_COLUMNS = {"id", "POINT_X", "POINT_Y"}
CVP_NODE = 181
SWP_LONGITUDE = -121.619535
SWP_LATITUDE = 37.801938
CONTOUR_LEVELS = list(range(0, 101, 10))
WEEK_COLORS = ["#440154", "#31688e", "#35b779"]


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Create the LFS entrainment map from PTM DSS and SLS CSV data."
    )
    parser.add_argument("date", help="Injection/workflow date in YYYYMMDD format.")
    parser.add_argument(
        "--omr",
        type=int,
        choices=sorted(SCENARIO_OMR.values()),
        default=None,
        help=(
            "Optional single OMR scenario. If omitted, maps are generated for "
            "all simulated scenarios found in the PTM DSS output."
        ),
    )
    return parser.parse_args()


def parse_date(value: str) -> pd.Timestamp:
    if not re.fullmatch(r"\d{8}", value):
        raise ValueError(f"Invalid date '{value}'. Expected YYYYMMDD.")
    return pd.to_datetime(value, format="%Y%m%d", errors="raise")


def require_file(path: Path, label: str) -> None:
    if not path.is_file():
        raise FileNotFoundError(f"{label} does not exist: {path}")


def require_dir(path: Path, label: str) -> None:
    if not path.is_dir():
        raise FileNotFoundError(f"{label} does not exist: {path}")


def warn_and_stop(message: str, exc_type=RuntimeError) -> None:
    warnings.warn(message, RuntimeWarning, stacklevel=2)
    raise exc_type(message)


def load_sls(run_date: pd.Timestamp, sls_dir: Path):
    """Load SLS survey data from the folder matching the workflow date."""
    require_dir(sls_dir, "SLS weekly folder")

    # The workflow date selects the folder. File modification time is not used.
    candidates = [
        path
        for path in sls_dir.glob("*.csv")
        if "processing_summary" not in path.name.lower()
    ]

    if not candidates:
        warn_and_stop(
            f"No eligible SLS CSV files were found in {sls_dir}. "
            "Survey data are required.",
            FileNotFoundError,
        )

    rejected = []
    valid_files = []

    # Identify survey CSVs by required columns rather than filesystem metadata.
    for csv_path in candidates:
        try:
            df = pd.read_csv(csv_path)
        except Exception as exc:
            rejected.append(f"{csv_path.name}: could not be read ({exc})")
            continue

        missing = REQUIRED_SLS_COLUMNS - set(df.columns)
        if missing:
            rejected.append(f"{csv_path.name}: missing {sorted(missing)}")
            continue

        valid_files.append((csv_path, df))

    if not valid_files:
        details = "\n".join(f"  - {item}" for item in rejected)
        warn_and_stop(
            f"No valid SLS survey CSV was found in {sls_dir}.\n"
            "Survey data are required.\n"
            f"Files checked:\n{details}"
        )

    if len(valid_files) > 1:
        valid_names = "\n".join(
            f"  - {csv_path.name}" for csv_path, _ in valid_files
        )
        warn_and_stop(
            f"Multiple valid SLS survey CSV files were found in {sls_dir}.\n"
            "Expected exactly one valid survey CSV.\n"
            f"Valid files found:\n{valid_names}"
        )

    csv_path, df = valid_files[0]
    df = df.copy()

    # UTC timestamps are converted to timezone-naive values for comparison
    # with the timezone-naive command-line workflow date.
    df["survDate"] = pd.to_datetime(df["survDate"], errors="coerce", utc=True)
    df["survDate"] = df["survDate"].dt.tz_convert(None)
    df["Survey"] = pd.to_numeric(df["Survey"], errors="coerce")
    df["Subregion_Abund"] = pd.to_numeric(
        df["Subregion_Abund"], errors="coerce"
    )

    usable = df.dropna(
        subset=["survDate", "Survey", "SubRegion", "Subregion_Abund"]
    )
    usable = usable.loc[usable["survDate"] <= run_date]

    if usable.empty:
        warn_and_stop(
            f"{csv_path.name} contains no survey dated on or before "
            f"{run_date:%Y-%m-%d}."
        )

    # Select the latest survey represented inside the chosen weekly CSV.
    # This does not use file creation or modification timestamps.
    survey_date = usable["survDate"].max()
    selected = usable.loc[usable["survDate"] == survey_date].copy()
    survey_values = selected["Survey"].unique()

    if len(survey_values) != 1:
        warn_and_stop(
            f"{csv_path.name}: inconsistent Survey values for "
            f"{survey_date:%Y-%m-%d}"
        )

    survey_number = int(survey_values[0])
    selected = selected.drop_duplicates(subset=["SubRegion"], keep="last")
    abundance = selected[["SubRegion", "Subregion_Abund"]].rename(
        columns={"SubRegion": "Subregion", "Subregion_Abund": "Abundance"}
    )

    if abundance.empty:
        warn_and_stop(f"{csv_path.name}: selected survey has no abundance rows")

    return abundance, survey_date, survey_number, csv_path


def parse_dss_filename(path: Path):
    match = re.match(r"(\d{8}-\d+[A-D]).*_(\d+)\.dss$", path.name, re.I)
    if not match:
        return None
    scenario_id = match.group(1)
    return scenario_id, scenario_id[-1].upper(), int(match.group(2))


def get_rts_series(dss: DSSFile, flux_name: str, scenario_id: str) -> pd.Series:
    for pathname in dss.get_pathnames():
        parts = pathname.strip("/").split("/")
        if len(parts) != 6:
            continue
        a, b, c, _, e, f = parts
        if (
            a.startswith("PTMV") and b == flux_name and c == "FLUX"
            and e == "15MIN" and f.strip() == scenario_id
        ):
            ts = dss.read_rts(pathname)
            data = ts.data.copy()
            data.index = pd.to_datetime(data.index).round("min")
            return pd.Series(
                pd.to_numeric(data.iloc[:, 0], errors="coerce").to_numpy(),
                index=data.index,
            )
    raise KeyError(f"Missing {flux_name} record for {scenario_id}")


def load_ptm(ptm_dir: Path, run_date: pd.Timestamp, scenario: str):
    require_dir(ptm_dir, "PTM DSS folder")
    week_times = [
        run_date + pd.Timedelta(days=6, hours=23, minutes=45),
        run_date + pd.Timedelta(days=13, hours=23, minutes=45),
        run_date + pd.Timedelta(days=20, hours=23, minutes=45),
    ]
    values = {}
    errors = []
    matching = 0
    for path in sorted(ptm_dir.glob("*.dss")):
        parsed = parse_dss_filename(path)
        if not parsed:
            continue
        scenario_id, file_scenario, node = parsed
        if file_scenario != scenario or node not in PTM_NODES:
            continue
        matching += 1
        try:
            with DSSFile(str(path)) as dss:
                cvp = get_rts_series(dss, "EXPORT_CVP", scenario_id)
                swp = get_rts_series(dss, "EXPORT_SWP", scenario_id)
            total = pd.concat([cvp, swp], axis=1).sum(axis=1, min_count=2)
            missing = [stamp for stamp in week_times if stamp not in total.index]
            if missing:
                raise KeyError(
                    "missing timestamps "
                    + ", ".join(x.strftime("%Y-%m-%d %H:%M") for x in missing)
                )
            values[node] = np.array([float(total.loc[x]) for x in week_times])
        except Exception as exc:
            errors.append(f"{path.name}: {exc}")

    if matching == 0:
        warn_and_stop(
            f"No DSS files for scenario {scenario} were found in {ptm_dir}."
        )
    missing_nodes = sorted(set(PTM_NODES) - set(values))
    if missing_nodes:
        details = "\n".join(f"  - {item}" for item in errors)
        warn_and_stop(
            f"PTM data are incomplete. Missing nodes: {missing_nodes}.\n{details}"
        )
    return values, week_times


def load_grid() -> pd.DataFrame:
    require_file(DSM2_GRID_FILE, "DSM2 node-coordinate CSV")
    grid = pd.read_csv(DSM2_GRID_FILE)
    missing = REQUIRED_GRID_COLUMNS - set(grid.columns)
    if missing:
        raise ValueError(f"Node-coordinate CSV is missing {sorted(missing)}")
    for col in REQUIRED_GRID_COLUMNS:
        grid[col] = pd.to_numeric(grid[col], errors="coerce")
    grid = grid.dropna(subset=list(REQUIRED_GRID_COLUMNS)).copy()
    grid["id"] = grid["id"].astype(int)
    transformer = Transformer.from_crs("EPSG:26910", "EPSG:4326", always_xy=True)
    lon, lat = transformer.transform(grid["POINT_X"].values, grid["POINT_Y"].values)
    grid["longitude"] = lon
    grid["latitude"] = lat
    absent = sorted(set(PTM_NODES + [CVP_NODE]) - set(grid["id"]))
    if absent:
        raise ValueError(f"Coordinate CSV is missing required nodes: {absent}")
    return grid


def load_regions() -> gpd.GeoDataFrame:
    require_file(EDSM_SHAPEFILE, "EDSM subregion shapefile")
    regions = gpd.read_file(EDSM_SHAPEFILE)
    if "Name" not in regions.columns:
        raise ValueError(f"EDSM shapefile lacks the 'Name' field: {EDSM_SHAPEFILE}")
    if regions.crs is None:
        raise ValueError(f"EDSM shapefile has no CRS: {EDSM_SHAPEFILE}")
    return regions.to_crs("EPSG:4326")


def load_zoi(run_string: str, row_number: int):
    features = []
    for week in range(1, 4):
        path = (
            ZOI_ROOT / run_string / f"0.75Contour_week{week}_shapefile"
            / f"0.75Contour_week{week}.shp"
        )
        require_file(path, f"Week {week} ZOI shapefile")
        frame = gpd.read_file(path)
        if frame.crs is None:
            raise ValueError(f"Week {week} ZOI shapefile has no CRS: {path}")
        if len(frame) <= row_number:
            raise ValueError(
                f"Week {week} ZOI shapefile has {len(frame)} rows; "
                f"row {row_number} is required."
            )
        frame = frame.to_crs("EPSG:4326")
        feature = json.loads(frame.iloc[[row_number]].to_json())
        feature["features"][0]["properties"]["line_color"] = WEEK_COLORS[week - 1]
        features.append(feature)
    return features


def create_contours(grid: pd.DataFrame, values: dict[int, np.ndarray]):
    coords = grid.set_index("id")[["longitude", "latitude"]]
    ent_x = np.array([coords.loc[node, "longitude"] for node in PTM_NODES])
    ent_y = np.array([coords.loc[node, "latitude"] for node in PTM_NODES])
    grid_x, grid_y = np.meshgrid(
        np.linspace(grid["longitude"].min(), grid["longitude"].max(), 100),
        np.linspace(grid["latitude"].min(), grid["latitude"].max(), 100),
    )
    contours = []
    for week_index in range(3):
        ent_values = np.array([values[node][week_index] for node in PTM_NODES])
        grid_z = griddata((ent_x, ent_y), ent_values, (grid_x, grid_y), method="linear")
        if np.isnan(grid_z).all():
            raise RuntimeError(f"Contour interpolation failed for week {week_index + 1}.")
        fig, ax = plt.subplots(figsize=(12, 15))
        contour = ax.contourf(
            grid_x, grid_y, grid_z, levels=CONTOUR_LEVELS,
            vmin=0, vmax=100, cmap="jet",
        )
        contours.append((fig, contour))
    return contours


def build_map(
    grid,
    regions,
    abundance,
    contours,
    zoi_features,
    run_date,
    omr,
    survey_date,
    survey_number,
):
    result = folium.Map(
        location=[grid["latitude"].mean(), grid["longitude"].mean()],
        tiles="CartoDB positron",
        zoom_start=10,
    )

    folium.GeoJson(
        json.loads(regions.to_json()),
        name="EDSM Subregions",
        style_function=lambda _: {
            "fillColor": "transparent",
            "color": "#555555",
            "weight": 1.0,
            "fillOpacity": 0,
        },
    ).add_to(result)

    for week, feature in enumerate(zoi_features, 1):
        folium.GeoJson(
            feature,
            name=f"ZOI after Week {week}",
            show=True,
            style_function=lambda item: {
                "fillColor": "transparent",
                "color": item["properties"]["line_color"],
                "weight": 3,
                "opacity": 1.0,
                "fillOpacity": 0,
            },
        ).add_to(result)

    for week, (_, contour) in enumerate(contours, 1):
        contour_geojson = geojsoncontour.contourf_to_geojson(
            contourf=contour,
            min_angle_deg=3.0,
            ndigits=10,
            fill_opacity=0.4,
            stroke_width=1,
        )
        folium.GeoJson(
            contour_geojson,
            show=(week == 1),
            name=f"Cumulative Entrainment Flux Week {week}",
            style_function=lambda feature: {
                "color": feature["properties"]["stroke"],
                "weight": feature["properties"]["stroke-width"],
                "fillColor": feature["properties"]["fill"],
                "opacity": 0.4,
                "fillOpacity": feature["properties"]["fill-opacity"],
            },
        ).add_to(result)

    abundance_lookup = abundance.set_index("Subregion")["Abundance"]
    positive = folium.FeatureGroup(name="LFS Abundance", show=True)
    zero = folium.FeatureGroup(name="No larva detected", show=True)
    points = regions.geometry.representative_point()
    matches = 0

    for index, region in regions.iterrows():
        name = region["Name"]
        if name not in abundance_lookup.index:
            continue

        matches += 1
        value = float(abundance_lookup.loc[name])
        point = points.loc[index]
        location = [point.y, point.x]

        if value <= 0:
            folium.CircleMarker(
                location=location,
                radius=2,
                color="#ff3030",
                weight=1,
                fill=True,
                fill_color="#ff3030",
                fill_opacity=1.0,
                tooltip=f"{name}: 0",
            ).add_to(zero)
        else:
            folium.CircleMarker(
                location=location,
                radius=max(value / 500000.0, 2.0),
                color="#111111",
                weight=3,
                fill=True,
                fill_color="#A9A9A9",
                fill_opacity=0.4,
                opacity=1.0,
                tooltip=f"{name}: {value:,.0f}",
            ).add_to(positive)

    if matches == 0:
        warn_and_stop(
            "No SLS SubRegion values matched the EDSM shapefile Name values."
        )

    positive.add_to(result)
    zero.add_to(result)

    cm.LinearColormap(
        colors=["blue", "cyan", "green", "yellow", "orange", "red"],
        vmin=0,
        vmax=100,
        caption="Cumulative CVP/SWP Entrainment Flux (%)",
    ).add_to(result)

    lfs_legend = """
    <div style="position: fixed; top: 500px; right: 35px; width: 350px;
                min-height: 64px; border: 2px solid #999; z-index: 9999;
                font-size: 13px; background-color: white; opacity: 0.92;
                padding: 8px 10px; line-height: 1.35;">
      <span style="display:inline-block;width:12px;height:12px;border-radius:50%;
                   border:3px solid #111;background:rgba(169,169,169,0.4);
                   margin-right:6px;vertical-align:middle;"></span>
      Estimated LFS Subregional Abundance<br>
      <span style="margin-left:21px;font-size:12px;">(hover over marker for value)</span><br>
      <span style="display:inline-block;width:7px;height:7px;border-radius:50%;
                   background:#ff3030;margin:3px 9px 0 3px;"></span>
      No Larva Estimated in LFS Subregion
    </div>
    """
    result.get_root().html.add_child(folium.Element(lfs_legend))

    zoi_legend = """
    <div style="position: fixed; top: 600px; right: 35px; width: 350px;
                min-height: 92px; border: 2px solid #999; z-index: 9999;
                font-size: 13px; background-color: white; opacity: 0.92;
                padding: 8px 10px; line-height: 1.45;">
      <b>Zone of Influence (ZOI) (hydrologic alteration &gt;25%)</b><br>
      <span style="display:inline-block;width:36px;border-top:3px solid #440154;
                   margin-right:7px;vertical-align:middle;"></span>ZOI after Week 1<br>
      <span style="display:inline-block;width:36px;border-top:3px solid #31688e;
                   margin-right:7px;vertical-align:middle;"></span>ZOI after Week 2<br>
      <span style="display:inline-block;width:36px;border-top:3px solid #35b779;
                   margin-right:7px;vertical-align:middle;"></span>ZOI after Week 3
    </div>
    """
    result.get_root().html.add_child(folium.Element(zoi_legend))

    folium.LayerControl(collapsed=True).add_to(result)

    title = (
        "<h3 align='center' style='font-size:16px;line-height:1.15;margin:4px 0;'>"
        "<b>Cumulative Entrainment at CVP/SWP Facilities Over 3 Weeks After "
        f"{run_date.strftime('%Y%m%d')} for OMR = {omr}<br>"
        f"LFS Abundance Based on SLS Survey {survey_number} "
        f"({survey_date.strftime('%m/%d/%Y')})</b></h3>"
    )
    result.get_root().html.add_child(folium.Element(title))
    return result


def main() -> int:
    args = parse_args()
    run_date = parse_date(args.date)
    run_string = run_date.strftime("%Y%m%d")

    ptm_dir = PTM_OUTPUT_ROOT / run_string / "PTM" / "pp"
    sls_dir = SLS_ROOT / run_string
    output_dir = FIGURE_ROOT / run_string / "LFS"
    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"Run date: {run_date:%Y-%m-%d}")
    print(f"PTM folder: {ptm_dir}")
    print(f"SLS folder: {sls_dir}")
    print(f"ZOI folder: {ZOI_ROOT / run_string}")
    print(f"DSM2 coordinate file: {DSM2_GRID_FILE}")
    print(f"EDSM shapefile: {EDSM_SHAPEFILE}")

    abundance, survey_date, survey_number, survey_file = load_sls(
        run_date, sls_dir
    )
    print(f"Selected SLS survey: {survey_number}")
    print(f"Selected SLS survey date: {survey_date:%Y-%m-%d}")
    print(f"Selected SLS file: {survey_file}")

    grid = load_grid()
    regions = load_regions()

    if args.omr is None:
        scenarios_to_run = list(SCENARIO_OMR.items())
    else:
        scenario_letter = next(
            key for key, value in SCENARIO_OMR.items() if value == args.omr
        )
        scenarios_to_run = [(scenario_letter, args.omr)]

    saved_files = []
    failed_scenarios = []

    for scenario, omr_value in scenarios_to_run:
        print(f"\nProcessing OMR scenario {omr_value:,} ({scenario})...")
        contours = []
        try:
            values, week_times = load_ptm(ptm_dir, run_date, scenario)
            print(
                "PTM timestamps: "
                + ", ".join(x.strftime("%Y-%m-%d %H:%M") for x in week_times)
            )

            zoi = load_zoi(run_string, ZOI_SCENARIO_ROW[omr_value])
            contours = create_contours(grid, values)
            result = build_map(
                grid=grid,
                regions=regions,
                abundance=abundance,
                contours=contours,
                zoi_features=zoi,
                run_date=run_date,
                omr=omr_value,
                survey_date=survey_date,
                survey_number=survey_number,
            )

            output_file = output_dir / (
                f"{run_string}_entrainment_OMR_{omr_value}.html"
            )
            result.save(str(output_file))
            saved_files.append(output_file)
            print(f"Saved entrainment map: {output_file}")
        except Exception as exc:
            failed_scenarios.append((scenario, omr_value, str(exc)))
            print(
                f"WARNING: Scenario {scenario} (OMR {omr_value:,}) failed: {exc}",
                file=sys.stderr,
            )
        finally:
            for figure, _ in contours:
                plt.close(figure)

    if failed_scenarios:
        details = "\n".join(
            f"  - {scenario}, OMR {omr:,}: {message}"
            for scenario, omr, message in failed_scenarios
        )
        raise RuntimeError(
            "One or more requested scenarios could not be generated:\n" + details
        )

    if not saved_files:
        raise RuntimeError("No entrainment maps were generated.")

    print(f"\nGenerated {len(saved_files)} entrainment map(s).")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        sys.exit(1)
