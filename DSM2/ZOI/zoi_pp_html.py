#!/usr/bin/env python3
"""
zoi_clean.py
Python translation of zoi_clean.R
Original R authors: Catarina Pien and Lisa Elliott (USBR)
cpien@usbr.gov; lelliott@usbr.gov

This code uses zone of influence modeling results (DSM2) to create contours
showing how zone of influence changes from operational facilities based on pumping.
Contour lines of a specific level are then compared between different flow levels
to indicate how changing OMR will influence the zone of influence.

Usage:
    python zoi_clean.py <Week> <InflowBin_W1> <InflowBin_W2> <InflowBin_W3>
                        <Week1_dates> <Week2_dates> <Week3_dates>
                        <SacramentoFlow_W1> <SanJoaquinFlow_W1>
                        <SacramentoFlow_W2> <SanJoaquinFlow_W2>
                        <SacramentoFlow_W3> <SanJoaquinFlow_W3>
"""

import sys
import os
import re
import warnings
import glob

import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import matplotlib.lines as mlines
import matplotlib.patches as mpatches
from matplotlib.colors import to_hex
from scipy.interpolate import griddata
from scipy.ndimage import gaussian_filter
from shapely.geometry import MultiLineString, LineString
import rasterio
from rasterio.transform import from_bounds
from rasterio.features import geometry_mask
import cartopy.crs as ccrs
import cartopy.feature as cfeature
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import matplotlib_scalebar.scalebar as scalebar_mod
from matplotlib_scalebar.scalebar import ScaleBar
import folium

# scenario_discovery.py lives in the Model folder (sibling of the ZOI folder)
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "Model"))
from scenario_discovery import load_scenarios
warnings.filterwarnings("ignore")

# ─────────────────────────────────────────────────────────────────────────────
# 0.  Parse command-line arguments (mirrors R args <- commandArgs(...))
# ─────────────────────────────────────────────────────────────────────────────
args = sys.argv[1:]
if len(args) != 13:
    print("Usage: python zoi_clean.py Week IB_W1 IB_W2 IB_W3 "
          "W1dates W2dates W3dates SacW1 SJW1 SacW2 SJW2 SacW3 SJW3")
    sys.exit(1)

(Week, InflowBin_W1, InflowBin_W2, InflowBin_W3,
 Week1_dates, Week2_dates, Week3_dates,
 SacramentoFlow_W1, SanJoaquinFlow_W1,
 SacramentoFlow_W2, SanJoaquinFlow_W2,
 SacramentoFlow_W3, SanJoaquinFlow_W3) = args

# ─────────────────────────────────────────────────────────────────────────────
# 1.  Paths  (mirrors here() + setwd() in R)
# ─────────────────────────────────────────────────────────────────────────────
script_dir   = os.path.dirname(os.path.abspath(__file__))
week_dir     = os.path.join(script_dir, Week)
ZOI_path     = os.path.join(week_dir, "output_csvs")
shp_base     = os.path.join(script_dir, "ZOI_Shapefiles",
                            "BDO-Science contour-zone-of-influence main shapefiles")

os.chdir(week_dir)
print(f"ZOI path: {ZOI_path}")

# ─────────────────────────────────────────────────────────────────────────────
# 2.  Read shapefiles that are constant across weeks
# ─────────────────────────────────────────────────────────────────────────────
delta = gpd.read_file(os.path.join(shp_base, "Bay_Delta_Poly_New.shp")).to_crs(epsg=4326)
delta["line"] = "analysis boundary"

nodes = gpd.read_file(os.path.join(shp_base, "nodes.shp"))[["node", "geometry"]].to_crs(epsg=4326)
nodes["points"] = "DSM2 nodes"

# ─────────────────────────────────────────────────────────────────────────────
# 3.  Read channel CSV (constant across weeks)
# ─────────────────────────────────────────────────────────────────────────────
channels0 = pd.read_csv(os.path.join(script_dir, "ZOI_Shapefiles",
                                     "channel_std_delta_grid_NAVD.csv"))
channels0.columns = [c.lower().strip() for c in channels0.columns]   # clean_names()
channels0 = channels0.rename(columns={"chan_no": "channel_number",
                                       "length":  "length_feet"})

drop_nodes = [146, 147, 148, 206, 242, 246, 432, 433, 434]
channels1  = channels0[~channels0["upnode"].isin(drop_nodes)]
channels   = channels1[~channels1["downnode"].isin(drop_nodes)].copy()

# ─────────────────────────────────────────────────────────────────────────────
# 4.  Colour palette  (viridis_d 4 colours, matching scale_color_viridis_d)
# ─────────────────────────────────────────────────────────────────────────────
# OMR levels come from this week's scenario manifest (the OMRI file names), not from
# a fixed list. Colour is a function of the OMR value so a given OMR always gets the same
# colour; -2000 .. -6500 maps onto viridis 0 .. 1 (identical to the previous 4-level palette).
try:
    _scen = load_scenarios(Week)
except FileNotFoundError as e:
    print(f"ERROR: {e}")
    sys.exit(1)
OMR_LEVELS = [str(v) for v in sorted((int(d["omri"]) for d in _scen.values()), reverse=True)]
print("Active OMR levels:", OMR_LEVELS)

_viridis = plt.get_cmap("viridis")
_OMR_HI, _OMR_LO = -2000.0, -6500.0

def omr_color(omr):
    x = (float(omr) - _OMR_HI) / (_OMR_LO - _OMR_HI)
    return _viridis(min(max(x, 0.0), 1.0))

COLOR_MAP = {lvl: omr_color(lvl) for lvl in OMR_LEVELS}

# Linetypes: fixed for the standard bins, cycled for any other value
_LINE_STYLES = ["solid", (0, (5, 5)), "dotted", (0, (3, 1, 1, 1))]
_KNOWN_LINES = {"-2000": _LINE_STYLES[0], "-3500": _LINE_STYLES[1],
                "-5000": _LINE_STYLES[2], "-6500": _LINE_STYLES[3]}
LINE_MAP = {lvl: _KNOWN_LINES.get(lvl, _LINE_STYLES[i % len(_LINE_STYLES)])
            for i, lvl in enumerate(OMR_LEVELS)}

# ─────────────────────────────────────────────────────────────────────────────
# 5.  Helper functions
# ─────────────────────────────────────────────────────────────────────────────

def parse_omr_flow(filename: str) -> str | None:
    """Extract OMR flow string from filename (DEZOI_OMR_Neg6500_... -> '-6500')."""
    m = re.search(r"OMR_(Neg)?(\d+)_", filename)
    if not m:
        return None
    return ("-" if m.group(1) else "") + str(int(m.group(2)))


def idw_interpolate(points_xy: np.ndarray, values: np.ndarray,
                    grid_x: np.ndarray, grid_y: np.ndarray,
                    power: float = 2.0) -> np.ndarray:
    """
    Inverse-distance-weighted interpolation (mirrors gstat::idw with idp=2).
    points_xy : (N,2), values : (N,), grid_x/grid_y : (M,M) meshgrids.
    Returns (M,M) array of interpolated values.
    """
    flat_x = grid_x.ravel()[:, None]          # (K,1)
    flat_y = grid_y.ravel()[:, None]
    px     = points_xy[:, 0][None, :]          # (1,N)
    py     = points_xy[:, 1][None, :]

    dist2  = (flat_x - px)**2 + (flat_y - py)**2
    dist2  = np.where(dist2 == 0, 1e-12, dist2)   # avoid /0
    w      = 1.0 / dist2**(power / 2.0)
    z_flat = (w * values[None, :]).sum(axis=1) / w.sum(axis=1)
    return z_flat.reshape(grid_x.shape)


def create_df(zoi_channel_long: gpd.GeoDataFrame,
              nodes_4326: gpd.GeoDataFrame,
              groupname: str, flow: str) -> gpd.GeoDataFrame:
    """
    Mirrors R create_df():
    Joins zoi_channel_long with nodes, filters to groupname+flow,
    drops negative/NA overlap rows.
    """
    cols = ["OMR_Flow", "node", "channel_number", "length_feet",
            "upnode", "downnode", "group", "overlap"]
    group_data = zoi_channel_long[cols].copy()

    merged = nodes_4326.merge(group_data, on="node", how="inner")
    merged["overlap"] = merged["overlap"].where(merged["overlap"] >= 0, np.nan)
    merged = merged.dropna(subset=["overlap"])

    df_filtered = merged[(merged["OMR_Flow"] == flow) &
                         (merged["group"]    == groupname)].copy()
    print(f"  Group: {groupname}  Flow: {flow}  Rows: {len(df_filtered)}")
    return df_filtered


def interp_nodes(df: gpd.GeoDataFrame,
                 delta_mask: gpd.GeoDataFrame,
                 n_pts: int = 50_000) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Mirrors R interp_nodes():
    IDW interpolation of df["overlap"] onto a regular grid,
    then masks outside the delta polygon.
    Returns (grid_x, grid_y, grid_z_masked).
    """
    xs = df.geometry.x.values
    ys = df.geometry.y.values
    zs = df["overlap"].values

    # bounding box of the data points (mirrors spsample on df)
    x0, x1 = xs.min(), xs.max()
    y0, y1 = ys.min(), ys.max()

    side  = int(np.sqrt(n_pts))
    xi    = np.linspace(x0, x1, side)
    yi    = np.linspace(y0, y1, side)
    gx, gy = np.meshgrid(xi, yi)

    gz = idw_interpolate(np.column_stack([xs, ys]), zs, gx, gy, power=2.0)

    # Mask outside delta polygon (mirrors raster::mask)
    from shapely.geometry import Point
    delta_union = delta_mask.union_all() if hasattr(delta_mask, "union_all") \
                  else delta_mask.unary_union
    flat_pts    = [Point(x, y) for x, y in zip(gx.ravel(), gy.ravel())]
    inside      = np.array([delta_union.contains(p) for p in flat_pts])
    mask_2d     = inside.reshape(gx.shape)
    gz_masked   = np.where(mask_2d, gz, np.nan)

    return gx, gy, gz_masked


def extract_contour_lines(gx, gy, gz, level: float,
                          flow_label: str, group_label: str) -> list[dict]:
    """
    Uses matplotlib's contour engine to extract iso-contour paths at `level`,
    returns list of dicts with keys: long, lat, group, flow, contour, group2, OMR_flow.
    Mirrors rasterToContour + fortify in R.
    """
    fig_tmp, ax_tmp = plt.subplots()
    cs = ax_tmp.contour(gx, gy, gz, levels=[level])
    plt.close(fig_tmp)

    rows = []
    seg_idx = 0
    for path in cs.get_paths():
        v = path.vertices
        grp_id = f"{group_label}_{flow_label}_{seg_idx}"
        for x, y in v:
            rows.append({"long": x, "lat": y,
                         "group": grp_id,
                         "flow": flow_label,
                         "contour": level,
                         "group2": group_label,
                         "OMR_flow": flow_label})
        seg_idx += 1
    return rows


def create_contour(
        rasters: list[tuple],
        flow_labels: list[str],
        inflow_group: str
    ) -> pd.DataFrame:

    all_rows = []

    for (gx, gy, gz), flow in zip(rasters, flow_labels):

        for level in (0.75, 0.25):
            all_rows.extend(
                extract_contour_lines(
                    gx,
                    gy,
                    gz,
                    level,
                    flow,
                    inflow_group
                )
            )

    return pd.DataFrame(all_rows)
def split_lines_by_distance(x, y, thresh=0.001):
    x = np.asarray(x)
    y = np.asarray(y)

    # Distance between consecutive points
    dist = np.sqrt(np.diff(x)**2 + np.diff(y)**2)

    # Find break indices
    breaks = np.where(dist > thresh)[0] + 1

    # Split into segments
    segments = np.split(np.column_stack([x, y]), breaks)

    # Keep only valid segments
    line_strings = [LineString(seg) for seg in segments if len(seg) > 1]

    return line_strings    
def plot_broken_lines(ax, x, y, **kwargs):
    x = np.asarray(x)
    y = np.asarray(y)

    # Compute distance between consecutive points
    dist = np.sqrt(np.diff(x)**2 + np.diff(y)**2)

    # Threshold (tune this!)
    thresh = 0.01  # ~1 km-ish in degrees; adjust if needed

    # Find break points
    breaks = np.where(dist > thresh)[0] + 1

    # Split into segments
    segments = np.split(np.column_stack([x, y]), breaks)

    for seg in segments:
        if len(seg) > 1:
            ax.plot(seg[:, 0], seg[:, 1], **kwargs)

# ─────────────────────────────────────────────────────────────────────────────
# 6.  WW_Delta waterway layer  (used for gray background fill)
#     In the R code this comes from deltamapr::WW_Delta; here we reconstruct
#     it from the NHD or a bundled shapefile if present, otherwise skip the
#     fill and use the delta boundary only.
# ─────────────────────────────────────────────────────────────────────────────
WW_Delta_crop = None
ww_candidates = glob.glob(os.path.join(script_dir, "ZOI_Shapefiles", "**", "*.shp"),
                          recursive=True)
ww_candidates = [f for f in ww_candidates if "water" in f.lower() or "WW" in f or
                 "hydro" in f.lower() or "nhd" in f.lower()]
for candidate in ww_candidates:
    try:
        _ww = gpd.read_file(candidate).to_crs(epsg=4326)
        if "HNAME" in _ww.columns:
            _ww = _ww[_ww["HNAME"] != "SAN FRANCISCO BAY"]
        WW_Delta_crop = _ww.cx[-122.2:-121.0, 37.5:38.8]
        print(f"  Loaded waterway layer from {candidate}")
        break
    except Exception:
        pass

if WW_Delta_crop is None:
    print("  WW_Delta waterway shapefile not found – using delta boundary as water fill.")
    WW_Delta_crop = delta.cx[-122.2:-121.0, 37.5:38.8].copy()

# ─────────────────────────────────────────────────────────────────────────────
# 7.  Main loop over weeks  (mirrors `for (i in weeks)` in R)
# ─────────────────────────────────────────────────────────────────────────────
for week_num, week_label in enumerate(["week1", "week2", "week3"], start=1):

    print(f"\n{'='*60}")
    print(f"Processing {week_label}")
    print(f"{'='*60}")

    # ── 7a. Choose per-week parameters ──────────────────────────────────────
    if week_label == "week1":
        inflowbin  = InflowBin_W1
        Week_dates = Week1_dates
        Sac_Flow   = SacramentoFlow_W1
        SanJoan_Flow = SanJoaquinFlow_W1
    elif week_label == "week2":
        inflowbin  = InflowBin_W2
        Week_dates = Week2_dates
        Sac_Flow   = SacramentoFlow_W2
        SanJoan_Flow = SanJoaquinFlow_W2
    else:
        inflowbin  = InflowBin_W3
        Week_dates = Week3_dates
        Sac_Flow   = SacramentoFlow_W3
        SanJoan_Flow = SanJoaquinFlow_W3

    # ── 7b. Read ZOI CSVs for this week ─────────────────────────────────────
    pattern  = os.path.join(ZOI_path, f"DEZOI_OMR*{week_label}*.csv")
    csv_files = glob.glob(pattern)
    if not csv_files:
        print(f"  No CSV files found for {week_label}, skipping.")
        continue

    frames = []
    found_flows = set()
    for f in csv_files:
        flow_f = parse_omr_flow(os.path.basename(f))
        if flow_f not in OMR_LEVELS:
            print(f"  Ignoring file from non-active OMR scenario: {os.path.basename(f)}")
            continue
        df_tmp = pd.read_csv(f)
        df_tmp["file"]     = f
        df_tmp["OMR_Flow"] = flow_f
        frames.append(df_tmp)
        found_flows.add(flow_f)
    missing_flows = [l for l in OMR_LEVELS if l not in found_flows]
    if missing_flows:
        print(f"ERROR: {week_label} has no DEZOI file for active OMR scenario(s) {missing_flows}. "
              f"Did ZOI preprocessing (Section 10) finish for every scenario?")
        sys.exit(1)

    zoi_data = pd.concat(frames, ignore_index=True).drop(columns=["file"])

    # ── 7c. Merge with channels ──────────────────────────────────────────────
    zoi_channel = zoi_data.merge(channels, on="channel_number", how="left")

    # ── 7d. Build zoi_channel_long (mirrors R code) ─────────────────────────
    zoi_channel_long = zoi_channel.copy()
    zoi_channel_long["group"]   = inflowbin
    zoi_channel_long = zoi_channel_long.rename(columns={"Proportion_Ratio": "overlap"})

    print("  Available OMR_Flow values:", zoi_channel_long["OMR_Flow"].unique())
    print("  Available group values:   ", zoi_channel_long["group"].unique())

    # ── 7e. Create spatial data frames  (mirrors create_df) ─────────────────
    available_flows = sorted(
        zoi_channel_long["OMR_Flow"].dropna().unique().tolist()
    )

    print("Available OMR scenarios:", available_flows)

    rasters = []
    flow_labels = []

    for flow in available_flows:

        df_flow = create_df(
            zoi_channel_long,
            nodes,
            inflowbin,
            flow
        )

        if len(df_flow) == 0:
            print(f"Skipping {flow}: no records")
            continue

        try:
            raster = interp_nodes(df_flow, delta)

            rasters.append(raster)
            flow_labels.append(flow)

            print(f"Processed {flow}")

        except Exception as e:
            print(f"Skipping {flow}: {e}")

    if len(rasters) == 0:
        print(f"No valid OMR scenarios found for {week_label}")
        continue

    contours_himed = create_contour(
        rasters,
        flow_labels,
        inflowbin
    )

    contours_all = contours_himed.copy()
        # debug
    print("Columns:", contours_all.columns.tolist())

    print("Shape:", contours_all.shape)

    print(contours_all.head())
    contours_all["OMR_flow"] = pd.Categorical(contours_all["OMR_flow"],
                                               categories=OMR_LEVELS, ordered=True)
    contour_gdfs = {}

    for omr in flow_labels:
        subset = contours_all[
            (contours_all["OMR_flow"] == omr) &
            (contours_all["contour"] == 0.75)
        ]

        if subset.empty:
            continue

        lines = []

        for _, grp in subset.groupby("group"):
            segs = split_lines_by_distance(
                grp["long"].values,
                grp["lat"].values,
                thresh=0.01
            )
            lines.extend(segs)

        if lines:        
            contour_gdfs[omr] = gpd.GeoDataFrame(
                {"OMR_flow": [omr] * len(lines)},
                geometry=lines,
                crs="EPSG:4326"
            )

    # ── 7h. Filter to 0.75 contour group  (mirrors contourGroup2) ───────────
    inflow_order = ["lolo","lomed","lohi","medlo","medmed","medhi","hilo","himed","hihi"]
    contourGroup2 = (contours_all[contours_all["contour"] == 0.75]
                     .copy()
                     .assign(grouper=lambda d: d["group"] + "_" + d["flow"] + d["group2"])
                     .assign(label  =lambda d: d["group2"] + "_" + d["flow"])
                     .rename(columns={"group2": "Inflow"}))
    contourGroup2["Inflow"] = pd.Categorical(contourGroup2["Inflow"],
                                              categories=inflow_order, ordered=True)
    plot_data = contourGroup2[contourGroup2["Inflow"] == inflowbin].copy()

    # ── 7i. Build the map  (mirrors map_75_fhihi ggplot) ────────────────────
    fig, ax = plt.subplots(figsize=(6.077, 7.2), 
                           subplot_kw={"projection": ccrs.PlateCarree()},
                           constrained_layout = True)

    # Water fill (gray90 / gray70)
    WW_Delta_crop.plot(ax=ax, facecolor="#E8E8E8", edgecolor="#B3B3B3",
                       alpha=0.7, transform=ccrs.PlateCarree(), linewidth=0.5)

    # Contour lines
    for omr_level in OMR_LEVELS:
        subset = plot_data[plot_data["flow"] == omr_level]
        if subset.empty:
            continue
        for grp_id, grp_df in subset.groupby("grouper"):
            plot_broken_lines(
                ax,
                grp_df["long"].values,
                grp_df["lat"].values,
                color=COLOR_MAP[omr_level],
                linestyle=LINE_MAP[omr_level],
                linewidth=2.25,
                transform=ccrs.PlateCarree()
            )

    # Map extent (mirrors xlim / ylim in R)
    ax.set_extent([-121.8, -121.25, 37.7, 38.1], crs=ccrs.PlateCarree())

    # Grid lines / lat-lon labels (theme_classic equivalent)
    gl = ax.gridlines(draw_labels=True, linewidth=0, color="none",
                      x_inline=False, y_inline=False)
    gl.top_labels    = False
    gl.right_labels  = False
    gl.xlabel_style  = {"size": 8, "rotation": 45, "ha": "center", "va": "top"}
    gl.ylabel_style  = {"size": 8}
    gl.xformatter    = LONGITUDE_FORMATTER
    gl.yformatter    = LATITUDE_FORMATTER

    # North arrow  (annotation_north_arrow – top right)
    ax.annotate("N", xy=(0.94, 0.95), xycoords="axes fraction",
                fontsize=11, fontweight="bold", ha="center", va="center")
    ax.annotate("", xy=(0.91, 0.97), xycoords="axes fraction",
                xytext=(0.91, 0.88), textcoords="axes fraction",
                arrowprops=dict(arrowstyle="-|>", color="black", lw=1.5))
    # Compass rose N/S/E/W tick marks
    for angle, label, dx, dy in [(0,"",0,0.025),]:
        pass  # simple north arrow above is sufficient

    # Scale bar  (annotation_scale – bottom left, bar_cols = black/white)
    try:
        sb = ScaleBar(1, units="km", dimension="si-length",
                      location="lower left",
                      scale_loc="bottom",
                      box_alpha=0,
                      color="black",
                      font_properties={"size": 9})
        ax.add_artist(sb)
    except Exception:
        pass   # matplotlib-scalebar not installed; skip

    # Legend (top, matches scale_color_viridis_d + scale_linetype_discrete)
    legend_handles = []
    for omr in flow_labels:
        h = mlines.Line2D([], [],
                          color=COLOR_MAP[omr],
                          linestyle=LINE_MAP[omr],
                          linewidth=2.5,
                          label=omr)
        legend_handles.append(h)
    ax.legend(handles=legend_handles,
              title="OMR Flow (cfs)",
              loc="upper center",
              bbox_to_anchor=(0.5, 1.1),
              ncol=4,
              fontsize=9,
              title_fontsize=9,
              frameon=False)

    # Title
    title_str = (f"0.75 Contour\n"
                 f"{Week_dates}\n"
                 f"Sacramento Flow = {Sac_Flow} cfs\n"
                 f"San Joaquin Flow = {SanJoan_Flow} cfs\n"
                 f"Inflow bin = {inflowbin}")
    ax.set_title(title_str, loc="left", fontsize=10, pad=20)

    # Classic theme – remove spines that R's theme_classic removes
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    # ── 7j. Save PNG  (mirrors ggsave) ──────────────────────────────────────
    out_png = os.path.join(week_dir, f"ZOI_0.75Contour_{week_label}.png")
    fig.savefig(out_png, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"  Saved: {out_png}")

    # ── 7k. Export 0.75-contour shapefile  (mirrors st_write block) ─────────
    shp_dir = os.path.join(week_dir, f"0.75Contour_{week_label}_shapefile")
    os.makedirs(shp_dir, exist_ok=True)

    sf_rows = []

    for omr, gdf in contour_gdfs.items():
        geom = MultiLineString(list(gdf.geometry)) if len(gdf) > 1 else gdf.geometry.iloc[0]

        sf_rows.append({
            "geometry":            geom,
            "OMR_flow":            omr,
            "InflowBin":           inflowbin,
            "Week":                week_label,
            "Week_dates":          Week_dates,
            "ContourLevel":        0.75,
            "SacFlow_cfs":         Sac_Flow,
            "SanJoaquinFlow_cfs":  SanJoan_Flow,
        })


    contour_sf = gpd.GeoDataFrame(sf_rows, crs="EPSG:4326")
    out_shp = os.path.join(shp_dir, f"0.75Contour_{week_label}.shp")
    contour_sf.to_file(out_shp)
    print(f"  Saved shapefile: {out_shp}")
    # CARTO basemaps (folium's "cartodbpositron") now need an API key and otherwise
    # render an "API KEY REQUIRED" watermark. Esri Light Gray Canvas needs no key.
    m = folium.Map(
        location=[38.1, -121.6],
        zoom_start=9,
        tiles=None
    )
    folium.TileLayer(
        tiles=("https://server.arcgisonline.com/ArcGIS/rest/services/"
               "Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"),
        attr="Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ",
        name="Esri Light Gray",
        max_native_zoom=16,
        max_zoom=20,
    ).add_to(m)
        
    folium.GeoJson(
        delta,
        name="Delta Boundary",
        style_function=lambda x: {
            "color": "black",
            "weight": 1,
            "fillOpacity": 0.1
        }
    ).add_to(m)

    for omr, gdf in contour_gdfs.items():
        folium.GeoJson(
            gdf,
            name=f"OMR {omr}",
            style_function=lambda x, omr=omr: {
                "color": to_hex(COLOR_MAP[omr]),
                "weight": 2
            }
        ).add_to(m)

    folium.LayerControl(collapsed=False).add_to(m)

    out_html = os.path.join(week_dir, f"ZOI_Map_{week_label}.html")
    m.save(out_html)

    print(f"  Saved HTML map: {out_html}")

print("\nDone.")
