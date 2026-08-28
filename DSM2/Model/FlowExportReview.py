from calendar import week
import code
import sys
from pathlib import Path
import pandas as pd
from datetime import datetime, timedelta
import shutil
import matplotlib.pyplot as plt
#import textwrap
from matplotlib.ticker import FuncFormatter
import matplotlib.dates as mdates
import numpy as np
from matplotlib.ticker import AutoMinorLocator
from matplotlib.ticker import MultipleLocator
import argparse
import requests
from io import StringIO
from plotly.subplots import make_subplots
import plotly.graph_objects as go
import plotly.io as pio

DATA_EXTERNAL = Path.cwd() / "DataExternal"

# ---------------------------------------------------------
# List files in a week folder
# ---------------------------------------------------------
def get_week_folder(week: str):
    folder = DATA_EXTERNAL / week
    if not folder.exists():
        raise FileNotFoundError(f"Week folder not found: {folder}")

    all_csvs = list(folder.glob("*.csv"))
    weeklyflow_data = [
        f for f in all_csvs
        if f.name not in {
            "DSM2_Historic_data.csv",
            "zoi_bins.csv",
            "average_exports_by_week.csv"
        }
]

    return folder, weeklyflow_data

# ---------------------------------------------------------
# Import CDEC Data
# ---------------------------------------------------------

def fetch_measured_daily_flows(forecast_start):
    """
    Fetch 7 days of DAILY flow data from CDEC.

    Returns a DataFrame with columns:
    DATE, CCF, TPP, VNS, FPT

    Designed to be compatible with existing downstream functions
    that expect DATE as a column.
    """

    import pandas as pd
    import requests
    from io import StringIO
    from datetime import datetime, timedelta

    # Ensure datetime
    if not isinstance(forecast_start, datetime):
        forecast_start = pd.to_datetime(forecast_start)

    start_dt = forecast_start
    end_dt = start_dt + timedelta(days=5)

    start_str = start_dt.strftime("%Y-%m-%d")
    end_str = end_dt.strftime("%Y-%m-%d")

    station_map = {
        "CLC": ("CCF", 76),
        "TRP": ("TPP", 70),
        "VNS": ("VNS", 41),
        "FPT": ("FPT", 20),
    }

    dfs = []

    for st, (colname, sensor) in station_map.items():
        url = (
            "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?"
            f"Stations={st}&SensorNums={sensor}&dur_code=D"
            f"&Start={start_str}&End={end_str}"
        )

        print(f"Fetching {st} ({colname})...")

        try:
            r = requests.get(url, timeout=15)
            r.raise_for_status()

            if not r.text.strip():
                print(f"⚠ {st}: empty response")
                continue

            # Read raw CDEC format (no headers)
            df = pd.read_csv(StringIO(r.text), header=None)

            # Drop empty columns
            df = df.dropna(axis=1, how="all")

            if df.shape[1] < 7:
                print(f"⚠ {st}: unexpected format (columns={df.shape[1]})")
                continue

            # Parse using positional columns
            df_parsed = pd.DataFrame({
                "DATE": pd.to_datetime(
                    df.iloc[:, 4],
                    format="%Y%m%d %H%M",
                    errors="coerce"
                ),
                colname: pd.to_numeric(df.iloc[:, 6], errors="coerce"),
            })

            df_parsed = df_parsed.dropna(subset=["DATE"])

            dfs.append(df_parsed)

        except Exception as e:
            print(f"⚠ Could not load {st}: {e}")
            continue

    # If nothing worked, return empty but compatible structure
    if not dfs:
        print("⚠ No CDEC data retrieved — returning empty DataFrame.")
        return pd.DataFrame(columns=["DATE", "CCF", "TPP", "VNS", "FPT"])

    # Merge all stations on DATE
    out = dfs[0]
    for df in dfs[1:]:
        out = pd.merge(out, df, on="DATE", how="outer")

    # Ensure DATE is a column and sorted
    if "DATE" not in out.columns:
        out = out.reset_index()

    out = out.sort_values("DATE").reset_index(drop=True)

    return out

# ---------------------------------------------------------
# Load CSV with flexible header detection
# ---------------------------------------------------------
def load_measured_csv(path: Path):
    # Read first two rows manually
    preview = pd.read_csv(path, nrows=2, header=None)

    # Row 0: ['', 'FPT', 'VNS', 'CCF', 'TPP']
    row0 = preview.iloc[0].tolist()

    # Build correct header row
    header = ["DATE"] + row0[1:]   # ['DATE', 'FPT', 'VNS', 'CCF', 'TPP']

    # Load data starting at row 2
    df = pd.read_csv(path, skiprows=2, header=None, names=header)

    # Convert DATE column
    df["DATE"] = pd.to_datetime(df["DATE"], errors="coerce")

    return df

def load_weeklyflowdata_csv(path: Path):
    preview = pd.read_csv(path, nrows=5, header=None)

    header_row = None
    for i in range(len(preview)):
        first_col = str(preview.iloc[i, 0]).strip()
        if first_col.lower() == "date":
            header_row = i
            break

    if header_row is None:
        raise KeyError(f"No header row with 'Date' found in {path}")

    df = pd.read_csv(path, header=header_row)
    df.columns = df.columns.str.strip().str.upper()

    if "DATE" not in df.columns:
        raise KeyError(f"'DATE' column missing in {path}")

    df["DATE"] = pd.to_datetime(df["DATE"], errors="coerce")
    return df

# ---------------------------------------------------------
# Archive CSV
# ---------------------------------------------------------
def archive_original_csv(csv_path: Path):
    archive_folder = csv_path.parent / "archive"
    archive_folder.mkdir(exist_ok=True)
    backup_path = archive_folder / csv_path.name

    if not backup_path.exists():
        shutil.copy2(csv_path, backup_path)
        print(f"Archived original: {backup_path.name}")
    else:
        print(f"Backup already exists for: {csv_path.name}")

# ---------------------------------------------------------
# Extend to a required end date 22 days
# ---------------------------------------------------------
def extend_to_required_end(df: pd.DataFrame, required_end):
    last_date = df["DATE"].max().date()
    if last_date >= required_end:
        return df

    new_dates = pd.date_range(last_date + timedelta(days=1), required_end)
    last_row = df.iloc[-1].copy()

    new_rows = []
    for d in new_dates:
        row = last_row.copy()
        row["DATE"] = d
        new_rows.append(row)

    return pd.concat([df, pd.DataFrame(new_rows)], ignore_index=True)

# ---------------------------------------------------------
# Plotting functions
# ---------------------------------------------------------
def plot_delta_inflows(flow_csvs, measured, forecast_start, required_end, ax, use_measured_data = True):

    # Date setup
    forecast_start_ts = pd.Timestamp(forecast_start)
    required_end_ts = pd.Timestamp(required_end)

    # Filter data
    if use_measured_data and measured is not None:
        meas = measured[measured["DATE"] >= forecast_start_ts].copy()
    else:
        meas = None 

    # Reference scenario
    scenario_codes = list(flow_csvs.keys())
    ref_code = scenario_codes[0]
    ref_df = flow_csvs[ref_code]
    ref_df = ref_df[ref_df["DATE"] >= forecast_start_ts].copy()

    # Plot measured
    if use_measured_data and meas is not None:
        ax.plot(meas["DATE"], meas["FPT"], color="#215F9A", linestyle="-", linewidth=2,
                label="Sacramento River at Freeport (Measured)")
        ax.plot(meas["DATE"], meas["VNS"], color="#E97132", linestyle="-", linewidth=2,
                label="San Joaquin River at Vernalis (Measured)")

    # Plot forecast
    ax.plot(ref_df["DATE"], ref_df["FPT"], color="#215F9A", linestyle="--", linewidth=2,
            label="Sacramento River at Freeport (Forecast)")
    ax.plot(ref_df["DATE"], ref_df["VNS"], color="#E97132", linestyle="--", linewidth=2,
            label="San Joaquin River at Vernalis (Forecast)")

    # Y-axis formatting
    ymin, ymax = ax.get_ylim()
    ax.set_ylim(0, ymax*1.1)   # more space for 2 rows
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{int(x):,}"))
    ax.tick_params(axis='y', labelsize=12)

    # X-axis ticks
    week_starts = pd.date_range(start=forecast_start_ts, end=required_end_ts, freq="7D")
    ax.set_xticks(week_starts)
    ax.set_xticklabels([d.strftime("%Y-%m-%d") for d in week_starts], rotation=45)
    #ax.set_xticklabels([]) # hide x-axis labels for the top plot
    ax.tick_params(axis='x', labelsize=12)

    ax.xaxis.set_minor_locator(mdates.DayLocator())
    ax.set_xlim(forecast_start_ts, required_end_ts)

    # Grid
    ax.grid(which="major", linestyle="-", linewidth=1.2, alpha=0.5)
    ax.grid(which="minor", linestyle=":", linewidth=0.5, alpha=0.5)

    # Labels
    ax.set_title("Delta Inflow", pad=45, fontsize=16, fontweight="bold")
    #ax.set_xlabel("Date", fontsize=12, fontweight="bold")
    ax.set_xlabel("") # remove x-axis label for top plot
    ax.set_ylabel("Daily Average Flow (cfs)", fontsize=12, fontweight="bold")

    leg = ax.legend(
        loc="upper center",
        #bbox_to_anchor=(0.5, 1.23),
        ncol=2,
        frameon=True,
        fontsize=12,
        bbox_to_anchor=(0.5, 1.335),   # position relative to top of plot
        handletextpad=0.4,   # space between line and text
        columnspacing=0.6,   # space between columns
        handlelength=1.5   # default is ~2.5
    )

    leg.get_frame().set_edgecolor("none")

def plot_cvp_exports(flow_csvs, measured, forecast_start, required_end, ax, use_measured_data = True):

    forecast_start_ts = pd.Timestamp(forecast_start)
    required_end_ts = pd.Timestamp(required_end)

    # handle measured data presence/absence
    if use_measured_data and measured is not None:
        meas = measured[measured["DATE"] >= forecast_start_ts].copy()
    else:
        meas = None

    colors = ["#0B76A0", "#46B1E1", "#00B0F0", "#61CBF4"]
    styles = [
        {"linestyle": "-",  "linewidth": 3},
        {"linestyle": "--", "linewidth": 3, "dashes": (10, 6)},
        {"linestyle": "--", "linewidth": 1.5, "dashes": (4, 3)},
        {"linestyle": "-",  "linewidth": 1.5},
    ]

    for i, (code, df) in enumerate(sorted(flow_csvs.items(), reverse=True)):
        df = df[df["DATE"] >= forecast_start_ts].copy()
        style = styles[i % len(styles)]
        color = colors[i % len(colors)]
        num = int(str(code).split()[-1])

        ax.plot(
            df["DATE"],
            df["TPP"] / 1.98,
            color=color,
            linestyle=style["linestyle"],
            linewidth=style["linewidth"],
            **({"dashes": style["dashes"]} if "dashes" in style else {}),
            label=f"OMR={num:,}"
        )

    if use_measured_data and meas is not None:
        ax.plot(meas["DATE"], meas["TPP"], "-", color="#000000", linewidth=2, label="Measured")
    
    ymin, ymax = ax.get_ylim()
    ax.set_ylim(0, ymax*1.1)   # more space for 2 rows
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{int(x):,}"))
    ax.tick_params(axis='y', labelsize=12)

    week_starts = pd.date_range(start=forecast_start_ts, end=required_end_ts, freq="7D")
    ax.set_xticks(week_starts)
    ax.set_xticklabels([d.strftime("%Y-%m-%d") for d in week_starts], rotation=45)
    #ax.set_xticklabels([]) # hide x-axis labels for the second plot
    ax.tick_params(axis='x', labelsize=12)

    ax.xaxis.set_minor_locator(mdates.DayLocator())
    ax.set_xlim(forecast_start_ts, required_end_ts)

    ax.grid(which="major", axis="y", linestyle="-", linewidth=1.0, alpha=0.3)
    ax.grid(which="minor", axis="y", linestyle=":", linewidth=0.5, alpha=0.3)
    ax.grid(which="major", axis="x", linestyle="-", linewidth=1.0, alpha=0.5)
    ax.grid(which="minor", axis="x", linestyle=":", linewidth=0.5, alpha=0.5)

    ax.set_title("CVP Exports", pad=30, fontsize=16, fontweight="bold")
    #ax.set_xlabel("Date", fontsize=12, fontweight="bold")
    ax.set_xlabel("") # remove x-axis label for second plot
    ax.set_ylabel("Daily Average Flow (cfs)", fontsize=12, fontweight="bold")
    
    # Get handles and labels
    handles, labels = ax.get_legend_handles_labels()

    # Separate scenario items from "Measured"
    scenario_handles = []
    scenario_labels = []
    measured_handle = None
    measured_label = None

    for h, l in zip(handles, labels):
        if l == "Measured":
            measured_handle = h
            measured_label = l
        else:
            scenario_handles.append(h)
            scenario_labels.append(l)

    combined_handles = scenario_handles.copy()
    combined_labels = scenario_labels.copy()

    if measured_handle is not None:
        combined_handles.append(measured_handle)
        combined_labels.append(measured_label)

    leg = ax.legend(
        combined_handles,
        combined_labels,
        loc="upper center",
        bbox_to_anchor=(0.5, 1.2),
        ncol=len(combined_labels),
        frameon=True,
        fontsize=12,
        handletextpad=0.4,
        columnspacing=0.6,
        handlelength=1.5
    )

    leg.get_frame().set_edgecolor("none")

def plot_swp_exports(flow_csvs, measured, forecast_start, required_end, ax, use_measured_data = True):

    forecast_start_ts = pd.Timestamp(forecast_start)
    required_end_ts = pd.Timestamp(required_end)

    # handle measured data presence/absence
    if use_measured_data and measured is not None:
        meas = measured[measured["DATE"] >= forecast_start_ts].copy()
    else:
        meas = None

    colors = ["#13501B", "#47D45A", "#00B050", "#84E291"]
    styles = [
        {"linestyle": "-",  "linewidth": 3},
        {"linestyle": "--", "linewidth": 3, "dashes": (10, 6)},
        {"linestyle": "--", "linewidth": 1.5, "dashes": (4, 3)},
        {"linestyle": "-",  "linewidth": 1.5},
    ]

    for i, (code, df) in enumerate(sorted(flow_csvs.items(), reverse=True)):
        df = df[df["DATE"] >= forecast_start_ts].copy()
        style = styles[i % len(styles)]
        color = colors[i % len(colors)]
        num = int(str(code).split()[-1])

        ax.plot(
            df["DATE"],
            df["CCF"] / 1.98,
            color=color,
            linestyle=style["linestyle"],
            linewidth=style["linewidth"],
            **({"dashes": style["dashes"]} if "dashes" in style else {}),
            label=f"OMR={num:,}"
        )
    if use_measured_data and meas is not None:
        ax.plot(meas["DATE"], meas["CCF"], "-", color="#000000", linewidth=2, label="Measured")

    ymin, ymax = ax.get_ylim()
    ax.set_ylim(0, ymax*1.1)   # more space for 2 rows
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{int(x):,}"))
    ax.tick_params(axis='y', labelsize=12)

    week_starts = pd.date_range(start=forecast_start_ts, end=required_end_ts, freq="7D")
    ax.set_xticks(week_starts)
    ax.set_xticklabels([d.strftime("%Y-%m-%d") for d in week_starts], rotation=45)
    ax.tick_params(axis='x', labelsize=12)

    ax.xaxis.set_minor_locator(mdates.DayLocator())
    ax.set_xlim(forecast_start_ts, required_end_ts)

    ax.grid(which="major", axis="y", linestyle="-", linewidth=1.0, alpha=0.3)
    ax.grid(which="minor", axis="y", linestyle=":", linewidth=0.5, alpha=0.3)
    ax.grid(which="major", axis="x", linestyle="-", linewidth=1.0, alpha=0.5)
    ax.grid(which="minor", axis="x", linestyle=":", linewidth=0.5, alpha=0.5)

    ax.set_title("SWP Exports", pad=30, fontsize=16, fontweight="bold")
    ax.set_xlabel("Date", fontsize=12, fontweight="bold")
    ax.set_ylabel("Daily Average Flow (cfs)", fontsize=12, fontweight="bold")

    # Get handles and labels
    handles, labels = ax.get_legend_handles_labels()

    # Separate scenario items from "Measured"
    scenario_handles = []
    scenario_labels = []
    measured_handle = None
    measured_label = None

    for h, l in zip(handles, labels):
        if l == "Measured":
            measured_handle = h
            measured_label = l
        else:
            scenario_handles.append(h)
            scenario_labels.append(l)

    combined_handles = scenario_handles.copy()
    combined_labels = scenario_labels.copy()

    if measured_handle is not None:
        combined_handles.append(measured_handle)
        combined_labels.append(measured_label)

    leg = ax.legend(
        combined_handles,
        combined_labels,
        loc="upper center",
        bbox_to_anchor=(0.5, 1.2),
        ncol=len(combined_labels),
        frameon=True,
        fontsize=12,
        handletextpad=0.4,
        columnspacing=0.6,
        handlelength=1.5
    )

    leg.get_frame().set_edgecolor("none")
# ---------------------------------------------------------
# Table Preparation Functions
# ---------------------------------------------------------

def average_exports_by_week(flow_csvs: dict, start_date: datetime.date, output_folder: Path):
    # Define weekly windows
    week1_start = start_date
    week1_end   = start_date + timedelta(days=6)

    week2_start = start_date + timedelta(days=7)
    week2_end   = start_date + timedelta(days=13)

    week3_start = start_date + timedelta(days=14)
    week3_end   = start_date + timedelta(days=20)

    week_ranges = [
        ("Week 1", week1_start, week1_end),
        ("Week 2", week2_start, week2_end),
        ("Week 3", week3_start, week3_end),
    ]

    rows = []

    for scenario, df in flow_csvs.items():
        # Skip measured data
        if "measured" in scenario.lower():
            continue

        df = df.copy()
        df["DATE"] = pd.to_datetime(df["DATE"]).dt.date

        for week_name, start, end in week_ranges:
            mask = (df["DATE"] >= start) & (df["DATE"] <= end)
            subset = df.loc[mask]

            # print the raw daily values for this week
            #print(f"\n--- {scenario} | {week_name} ---")
            #print(subset[["DATE", "TPP", "CCF"]])

            # Rounded averages, converted to cfs
            tpp_avg = round(subset["TPP"].mean()/1.98) if "TPP" in subset and not subset["TPP"].empty else None
            ccf_avg = round(subset["CCF"].mean()/1.98) if "CCF" in subset and not subset["CCF"].empty else None

            # Total exports
            total_exports = (
                tpp_avg + ccf_avg
                if tpp_avg is not None and ccf_avg is not None
                else None
            )

            # Percentages
            percent_cvp = (
                round((tpp_avg / total_exports) * 100)
                if total_exports not in (None, 0)
                else None
            )

            percent_swp = (
                round((ccf_avg / total_exports) * 100)
                if total_exports not in (None, 0)
                else None
            )

            rows.append({
                "Week": week_name,
                "OMR Bins": scenario,
                "CVP Exports (cfs)": tpp_avg,
                "SWP Exports (cfs)": ccf_avg,
                "Total Exports (cfs)": total_exports,
                "CVP Exports (%)": percent_cvp,
                "SWP Exports (%)": percent_swp
            })

    # Build DataFrame
    summary_df = pd.DataFrame(rows)

    # Sort: Week1 → Week2 → Week3, scenarios descending
    summary_df = summary_df.sort_values(
        by=["Week", "OMR Bins"],
        ascending=[True, False]
    )

    # Reorder columns
    summary_df = summary_df[
        [
            "Week",
            "OMR Bins",
            "CVP Exports (cfs)",
            "SWP Exports (cfs)",
            "Total Exports (cfs)",
            "CVP Exports (%)",
            "SWP Exports (%)"
        ]
    ]

    # Format numeric columns with commas
    for col in ["CVP Exports (cfs)", "SWP Exports (cfs)", "Total Exports (cfs)"]:
        summary_df[col] = summary_df[col].map(
            lambda x: f"{x:,}" if pd.notnull(x) else x
        )

    # Format OMR Bins with commas
    summary_df["OMR Bins"] = summary_df["OMR Bins"].map(
        lambda x: f"{int(x):,}" if pd.notnull(x) else x
    )

    # Format percent columns with a % symbol
    for col in ["CVP Exports (%)", "SWP Exports (%)"]:
        summary_df[col] = summary_df[col].map(
            lambda x: f"{x}%" if pd.notnull(x) else x
    )

    # Export CSV to the week folder
    csv_path = output_folder / "average_exports_by_week.csv"
    summary_df.to_csv(csv_path, index=False)
    print(f"Saved formatted CSV to: {csv_path}")

    return summary_df

def build_zoi_bins_df(flow_csvs: dict, start_date: datetime.date, output_folder: Path):
    """
    Weekly-averaged dataframe for FPT and VNS only.
    Adds Sac Flow Bin, SJR Flow Bin, and Delta Inflow Bin.
    """

    variables = ["FPT", "VNS"]

    # Weekly windows (3 weeks)
    week_ranges = []
    for i in range(3):
        start = start_date + timedelta(days=7 * i)
        end = start + timedelta(days=6)
        
        week_label = f"Week {i+1} ({start.strftime('%m/%d/%Y')} - {end.strftime('%m/%d/%Y')})"
        week_ranges.append((week_label, start, end))

    # Use the FIRST scenario (all scenarios produce identical weekly averages)
    first_scenario_df = next(iter(flow_csvs.values())).copy()
    first_scenario_df["DATE"] = pd.to_datetime(first_scenario_df["DATE"]).dt.date

    rows = []

    for week_name, start, end in week_ranges:
        mask = (first_scenario_df["DATE"] >= start) & (first_scenario_df["DATE"] <= end)
        subset = first_scenario_df.loc[mask]

        # Compute averages
        avg_values = {}
        for var in variables:
            if var in subset and not subset[var].empty:
                avg_values[var] = round(subset[var].mean())
            else:
                avg_values[var] = None

        sac_flow = avg_values["FPT"]
        sjr_flow = avg_values["VNS"]

        # Sacramento bin logic
        if sac_flow is None:
            sac_bin = None
        elif sac_flow <= 13145:
            sac_bin = "lo"
        elif sac_flow >= 24726:
            sac_bin = "hi"
        else:
            sac_bin = "med"

        # San Joaquin bin logic
        if sjr_flow is None:
            sjr_bin = None
        elif sjr_flow <= 1983:
            sjr_bin = "lo"
        elif sjr_flow >= 4097:
            sjr_bin = "hi"
        else:
            sjr_bin = "med"

        # Delta Inflow Bin
        if sac_bin is None or sjr_bin is None:
            delta_bin = None
        else:
            delta_bin = sac_bin + sjr_bin

        rows.append({
            "Forecast Week": week_name,
            "Sacramento River at Freeport (cfs)": sac_flow,
            "Sac Flow Bin": sac_bin,
            "San Joaquin River at Vernalis (cfs)": sjr_flow,
            "SJR Flow Bin": sjr_bin,
            "Delta Inflow Bin": delta_bin
        })

    # Build numeric dataframe
    df = pd.DataFrame(rows)

    # Create formatted copy for CSV export
    df_formatted = df.copy()
    df_formatted["Sacramento River at Freeport (cfs)"] = df_formatted["Sacramento River at Freeport (cfs)"].map(
        lambda x: f"{x:,}" if pd.notnull(x) else x
    )
    df_formatted["San Joaquin River at Vernalis (cfs)"] = df_formatted["San Joaquin River at Vernalis (cfs)"].map(
        lambda x: f"{x:,}" if pd.notnull(x) else x
    )

    # Export formatted CSV
    csv_path = output_folder / "zoi_bins.csv"
    df_formatted.to_csv(csv_path, index=False)
    print(f"Saved formatted CSV to: {csv_path}")

    return df

# --------------------------------------------------------
# make html
#---------------------------------------------------------
def write_flow_export_html(flow_csvs, measured, forecast_start, required_end, output_folder, use_measured_data=True):
    """
    Build an interactive Plotly HTML version of the Flow Export Review plots.
    Designed to closely resemble the Matplotlib PNG while remaining zoomable/interactable.
    """

    forecast_start_ts = pd.Timestamp(forecast_start)
    required_end_ts = pd.Timestamp(required_end)

    if not flow_csvs:
        raise ValueError("flow_csvs is empty; cannot build HTML plot.")

    week_starts = pd.date_range(
        start=forecast_start_ts,
        end=required_end_ts,
        freq="7D"
    )

    daily_ticks = pd.date_range(
        start=forecast_start_ts,
        end=required_end_ts,
        freq="D"
    )

    fig_html = make_subplots(
        rows=3,
        cols=1,
        shared_xaxes=False,
        vertical_spacing=0.13,
        subplot_titles=(
            "Delta Inflow",
            "CVP Exports",
            "SWP Exports"
        )
    )

    # ---------------------------------------------------------
    # Helpers
    # ---------------------------------------------------------
    def omr_number(code):
        return int(str(code).split()[-1])

    def hover_template(name):
        return (
            f"{name}<br>"
            "Date: %{x|%Y-%m-%d}<br>"
            "Flow: %{y:,.0f} cfs"
            "<extra></extra>"
        )

    def get_measured():
        if use_measured_data and measured is not None and not measured.empty:
            m = measured.copy()
            m["DATE"] = pd.to_datetime(m["DATE"])
            return m[m["DATE"] >= forecast_start_ts].copy()
        return None

    def add_daily_minor_grid(row):
        """
        Plotly minor date-grid support varies by version, so use shapes.
        This gives a Matplotlib-like dotted daily vertical grid.
        """
        yref = f"y{row}" if row > 1 else "y"
        xref = f"x{row}" if row > 1 else "x"

        for d in daily_ticks:
            fig_html.add_shape(
                type="line",
                x0=d,
                x1=d,
                y0=0,
                y1=1,
                xref=xref,
                yref=f"{yref} domain",
                line=dict(
                    color="rgba(0,0,0,0.22)",
                    width=0.5,
                    dash="dot"
                ),
                layer="below"
            )

    def set_y_range(row, values):
        vals = []
        for v in values:
            if v is not None:
                s = pd.Series(v).dropna()
                if not s.empty:
                    vals.append(s.max())

        ymax = max(vals) if vals else 1
        fig_html.update_yaxes(range=[0, ymax * 1.1], row=row, col=1)

    meas = get_measured()

    # =========================================================
    # Row 1: Delta Inflow
    # =========================================================
    scenario_codes = list(flow_csvs.keys())
    ref_code = scenario_codes[0]

    ref_df = flow_csvs[ref_code].copy()
    ref_df["DATE"] = pd.to_datetime(ref_df["DATE"])
    ref_df = ref_df[ref_df["DATE"] >= forecast_start_ts]

    row1_y_values = []

    if meas is not None:
        fig_html.add_trace(
            go.Scatter(
                x=meas["DATE"],
                y=meas["FPT"],
                mode="lines",
                name="Sacramento River at Freeport (Measured)",
                line=dict(color="#215F9A", width=2, dash="solid"),
                hovertemplate=hover_template("Sacramento River at Freeport (Measured)"),
                legend="legend"
            ),
            row=1,
            col=1
        )
        row1_y_values.append(meas["FPT"])

        fig_html.add_trace(
            go.Scatter(
                x=meas["DATE"],
                y=meas["VNS"],
                mode="lines",
                name="San Joaquin River at Vernalis (Measured)",
                line=dict(color="#E97132", width=2, dash="solid"),
                hovertemplate=hover_template("San Joaquin River at Vernalis (Measured)"),
                legend="legend"
            ),
            row=1,
            col=1
        )
        row1_y_values.append(meas["VNS"])

    fig_html.add_trace(
        go.Scatter(
            x=ref_df["DATE"],
            y=ref_df["FPT"],
            mode="lines",
            name="Sacramento River at Freeport (Forecast)",
            line=dict(color="#215F9A", width=2, dash="dash"),
            hovertemplate=hover_template("Sacramento River at Freeport (Forecast)"),
            legend="legend"
        ),
        row=1,
        col=1
    )
    row1_y_values.append(ref_df["FPT"])

    fig_html.add_trace(
        go.Scatter(
            x=ref_df["DATE"],
            y=ref_df["VNS"],
            mode="lines",
            name="San Joaquin River at Vernalis (Forecast)",
            line=dict(color="#E97132", width=2, dash="dash"),
            hovertemplate=hover_template("San Joaquin River at Vernalis (Forecast)"),
            legend="legend"
        ),
        row=1,
        col=1
    )
    row1_y_values.append(ref_df["VNS"])

    # =========================================================
    # Row 2: CVP Exports
    # =========================================================
    cvp_colors = ["#0B76A0", "#46B1E1", "#00B0F0", "#61CBF4"]
    cvp_styles = [
        dict(width=3, dash="solid"),
        dict(width=3, dash="longdash"),
        dict(width=1.5, dash="dash"),
        dict(width=1.5, dash="solid"),
    ]

    row2_y_values = []

    for i, (code, df) in enumerate(sorted(flow_csvs.items(), reverse=True)):
        plot_df = df.copy()
        plot_df["DATE"] = pd.to_datetime(plot_df["DATE"])
        plot_df = plot_df[plot_df["DATE"] >= forecast_start_ts]

        color = cvp_colors[i % len(cvp_colors)]
        style = cvp_styles[i % len(cvp_styles)]
        num = omr_number(code)
        y = plot_df["TPP"] / 1.98

        fig_html.add_trace(
            go.Scatter(
                x=plot_df["DATE"],
                y=y,
                mode="lines",
                name=f"OMR={num:,}",
                line=dict(
                    color=color,
                    width=style["width"],
                    dash=style["dash"]
                ),
                hovertemplate=hover_template(f"CVP Exports | OMR={num:,}"),
                legend="legend2"
            ),
            row=2,
            col=1
        )
        row2_y_values.append(y)

    if meas is not None:
        fig_html.add_trace(
            go.Scatter(
                x=meas["DATE"],
                y=meas["TPP"],
                mode="lines",
                name="Measured",
                line=dict(color="#000000", width=2, dash="solid"),
                hovertemplate=hover_template("CVP Exports Measured"),
                legend="legend2"
            ),
            row=2,
            col=1
        )
        row2_y_values.append(meas["TPP"])

    # =========================================================
    # Row 3: SWP Exports
    # =========================================================
    swp_colors = ["#13501B", "#47D45A", "#00B050", "#84E291"]
    swp_styles = [
        dict(width=3, dash="solid"),
        dict(width=3, dash="longdash"),
        dict(width=1.5, dash="dash"),
        dict(width=1.5, dash="solid"),
    ]

    row3_y_values = []

    for i, (code, df) in enumerate(sorted(flow_csvs.items(), reverse=True)):
        plot_df = df.copy()
        plot_df["DATE"] = pd.to_datetime(plot_df["DATE"])
        plot_df = plot_df[plot_df["DATE"] >= forecast_start_ts]

        color = swp_colors[i % len(swp_colors)]
        style = swp_styles[i % len(swp_styles)]
        num = omr_number(code)
        y = plot_df["CCF"] / 1.98

        fig_html.add_trace(
            go.Scatter(
                x=plot_df["DATE"],
                y=y,
                mode="lines",
                name=f"OMR={num:,}",
                line=dict(
                    color=color,
                    width=style["width"],
                    dash=style["dash"]
                ),
                hovertemplate=hover_template(f"SWP Exports | OMR={num:,}"),
                legend="legend3"
            ),
            row=3,
            col=1
        )
        row3_y_values.append(y)

    if meas is not None:
        fig_html.add_trace(
            go.Scatter(
                x=meas["DATE"],
                y=meas["CCF"],
                mode="lines",
                name="Measured",
                line=dict(color="#000000", width=2, dash="solid"),
                hovertemplate=hover_template("SWP Exports Measured"),
                legend="legend3"
            ),
            row=3,
            col=1
        )
        row3_y_values.append(meas["CCF"])

    # ---------------------------------------------------------
    # Axes and grid formatting
    # ---------------------------------------------------------
    for row in [1, 2, 3]:
        fig_html.update_xaxes(
            row=row,
            col=1,
            range=[forecast_start_ts, required_end_ts],
            tickmode="array",
            tickvals=week_starts,
            ticktext=[d.strftime("%Y-%m-%d") for d in week_starts],
            tickangle=45,
            tickfont=dict(size=12, family="Arial"),
            showgrid=True,
            gridcolor="rgba(0,0,0,0.45)",
            gridwidth=1,
            griddash="solid",
            zeroline=False,
            showline=True,
            linecolor="black",
            linewidth=1,
            mirror=True
        )

        fig_html.update_yaxes(
            row=row,
            col=1,
            title_text="Daily Average Flow (cfs)",
            title_font=dict(size=12, family="Arial"),
            tickfont=dict(size=12, family="Arial"),
            tickformat=",",
            showgrid=True,
            gridcolor="rgba(0,0,0,0.30)",
            gridwidth=1,
            griddash="solid",
            zeroline=False,
            showline=True,
            linecolor="black",
            linewidth=1,
            mirror=True
        )

        add_daily_minor_grid(row)

    set_y_range(1, row1_y_values)
    set_y_range(2, row2_y_values)
    set_y_range(3, row3_y_values)

    fig_html.update_xaxes(title_text="", row=1, col=1)
    fig_html.update_xaxes(title_text="", row=2, col=1)
    fig_html.update_xaxes(
        title_text="Date",
        title_font=dict(size=12, family="Arial"),
        row=3,
        col=1
    )

    # ---------------------------------------------------------
    # Subplot title styling
    # ---------------------------------------------------------
    for ann in fig_html.layout.annotations:
        ann.font = dict(size=16, family="Arial", color="black")
        if ann.text == "Delta Inflow":
            ann.yshift = 40
        else:
            ann.yshift = 20

    # ---------------------------------------------------------
    # Layout aesthetics
    # ---------------------------------------------------------
    fig_html.update_layout(
        width=850,
        height=1100,
        template="plotly_white",
        paper_bgcolor="white",
        plot_bgcolor="white",
        margin=dict(l=100, r=60, t=75, b=90),
        font=dict(
            family="Arial",
            size=12,
            color="black"
        ),
        hovermode="x unified",

        legend=dict(
            orientation="h",
            x=0.5,
            y=1,
            xanchor="center",
            yanchor="bottom",
            bgcolor="rgba(255,255,255,0)",
            borderwidth=0,
            font=dict(size=12, family="Arial"),
            traceorder="normal"
        ),

        legend2=dict(
            orientation="h",
            x=0.5,
            y=0.62,
            xanchor="center",
            yanchor="bottom",
            bgcolor="rgba(255,255,255,0)",
            borderwidth=0,
            font=dict(size=12, family="Arial"),
            traceorder="normal"
        ),

        legend3=dict(
            orientation="h",
            x=0.5,
            y=0.24,
            xanchor="center",
            yanchor="bottom",
            bgcolor="rgba(255,255,255,0)",
            borderwidth=0,
            font=dict(size=12, family="Arial"),
            traceorder="normal"
        )
    )

    config = {
        "displaylogo": False,
        "responsive": True,
        "toImageButtonOptions": {
            "format": "png",
            "filename": "FlowExportReviewPlots",
            "height": 1100,
            "width": 850,
            "scale": 2
        }
    }

    html_path = output_folder / "FlowExportReviewPlots.html"

    pio.write_html(
        fig_html,
        file=str(html_path),
        include_plotlyjs="cdn",
        full_html=True,
        config=config,
        auto_open=False
    )

    print(f"Saved interactive HTML to: {html_path}")

# ---------------------------------------------------------
# Main workflow (ONLY place where functions are called)
# ---------------------------------------------------------
def main():
    # identifies whether to include measured data based on presence of --measured or --no-measured flag, default is to include measured data if no flags are provided
    parser = argparse.ArgumentParser()
    parser.add_argument("date", help="Forecast date, e.g. 20260324")
    parser.add_argument("--measured", dest="use_measured", action="store_true",
                        help="Include measured data")
    parser.add_argument("--no-measured", dest="use_measured", action="store_false",
                        help="Exclude measured data")
    parser.set_defaults(use_measured=True)

    args = parser.parse_args()

    forecast_date = args.date
    use_measured_data = args.use_measured

    # error handling for date argument
    if len(sys.argv) < 2:
        raise ValueError("Usage: python script.py YYYYMMDD")

    week = sys.argv[1]
    if not week.isdigit() or len(week) != 8:
        raise ValueError("Week must be in YYYYMMDD format")

    #get folder and file lists
    folder, weeklyflow_data = get_week_folder(week)
    print(f"Using folder: {folder}")
    print(f"Found {len(weeklyflow_data)} Scenario CSV files")

    forecast_start = datetime.strptime(week, "%Y%m%d").date()
    required_end = forecast_start + timedelta(days=22)

    if use_measured_data:
        measured_data = fetch_measured_daily_flows(forecast_start)
    else:
        measured_data = None

    #archive + extend weekly flow CSVs (single load) 
    flow_csvs = {}
    for f in weeklyflow_data:
        archive_original_csv(f)

        df = load_weeklyflowdata_csv(f)
        df_extended = extend_to_required_end(df, required_end)
        df_extended.to_csv(f, index=False)

        #store extended version in memory
        stem = f.stem
        if len(stem) >= 5 and stem[-5:].lstrip("-").isdigit():
            flow_code = stem[-5:]
            flow_csvs[flow_code] = df_extended

    #generate plots 
    fig, axes = plt.subplots(3, 1, figsize=(8.5, 11))  # portrait-friendly
    fig.subplots_adjust(top=0.92, bottom=0.10, left=0.12, right=0.93, hspace=0.75)
    #for ax in axes:
        #pos = ax.get_position()
        #ax.set_position([0.08, pos.y0, 0.90, pos.height])

    plot_delta_inflows(flow_csvs, measured_data, forecast_start, required_end, axes[0], use_measured_data)
    plot_cvp_exports(flow_csvs, measured_data, forecast_start, required_end, axes[1], use_measured_data)
    plot_swp_exports(flow_csvs, measured_data, forecast_start, required_end, axes[2], use_measured_data)
    
    #plt.tight_layout()
    fig.savefig(folder / "FlowExportReviewPlots.png", dpi=300)

    write_flow_export_html(
        flow_csvs=flow_csvs,
        measured=measured_data,
        forecast_start=forecast_start,
        required_end=required_end,
        output_folder=folder,
        use_measured_data=use_measured_data
        )

    plt.close(fig)

    #print tables
    summary_table = average_exports_by_week(flow_csvs, forecast_start, folder)
    zoi_bins = build_zoi_bins_df(flow_csvs, forecast_start, folder)

if __name__ == "__main__":
    main()