import sys
import re
from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.lines import Line2D
from pyhecdss import DSSFile
import plotly.graph_objects as go
import plotly.io as pio

# INPUT
# Require one command-line argument: the weekly injection date in YYYYMMDD format.
# This date controls input/output folders, plot titles, plotting windows, and summary timestamps.
if len(sys.argv) != 2:
    raise RuntimeError("Usage: python plot_lfs_exports.py YYYYMMDD")

WEEK_STR = sys.argv[1]
INJECTION_DATE = pd.to_datetime(WEEK_STR)

# DIRECTORIES
# Use the script location as the Model directory so paths remain relative to the model workflow.
# PTM_DIR is where this week's post-processed PTM DSS files are expected.
# FIG_DIR is where all PNG, HTML, and CSV outputs for this week will be written.
MODEL_DIR = Path(__file__).resolve().parent
PTM_DIR = MODEL_DIR / "Output" / WEEK_STR / "PTM" / "pp"
FIG_DIR = MODEL_DIR / "Figures" / WEEK_STR / "LFS"
FIG_DIR.mkdir(parents=True, exist_ok=True)

# WEEK DEFINITIONS
# Summary tables sample PTM values at exact weekly timestamps after injection.
# These timestamps must exist in the DSS time series exactly; otherwise the script will assign 0.0.
weeks = {
    "Week1": INJECTION_DATE + pd.Timedelta(days=6, hours=23, minutes=45),
    "Week2": INJECTION_DATE + pd.Timedelta(days=13, hours=23, minutes=45),
    "Week3": INJECTION_DATE + pd.Timedelta(days=20, hours=23, minutes=45),
}

'''# REGION NODES
# Nodes included in detailed node-level plots, organized by plotting/reporting region.
# data_all may contain additional nodes, but only nodes listed here are included in detailed plots.
REGION_NODES = {
    "Sacramento/North Delta": [293, 350, 323, 304],
    "Eastern Delta": [249],
    "Lower San Joaquin": [41, 34, 46],
    "Lower Sacramento": [351, 353],
    "South Delta": [29, 99, 75, 86, 225, 145],
    "Western Delta" : [359],
    "Suisun Marsh" : [227, 329, 359, 365, 420],
}'''
# SLS DATA
# SLS abundance estimates used to convert PTM percentages into estimated entrained fish numbers.
SLS_CSV = (
    MODEL_DIR
    / "SLS"
    / "SLS_abundance_estimates"
    / "2026 LFS Abundances through SLS Survey 6 (Final)_April 21st.csv"
)
# Selects Latest
# Use the most recent survey date available in the SLS CSV.
# This is not tied directly to WEEK_STR unless the CSV itself has been updated accordingly.
sls_df = pd.read_csv(SLS_CSV)
sls_df["survDate"] = pd.to_datetime(sls_df["survDate"])
latest_date = sls_df["survDate"].max()
sls_latest = sls_df[sls_df["survDate"] == latest_date]
SLS_ABUND = dict(zip(sls_latest["SubRegion"], sls_latest["Subregion_Abund"]))

# COLUMN ORDER
COLUMNS = [
    "West Suisun Bay","East Suisun Bay","Grizzly Bay","Montezuma Slough","Honker Bay",
    "Carquinez Strait","Upper Napa River","Lower Napa River",
    "East San Pablo Bay","West San Pablo Bay","Mid San Pablo Bay",
    "Lower Sacramento River Ship Channel","Sacramento River near Ryde",
    "Cache Slough and Liberty Island","Upper Sacramento River",
    "San Joaquin River at Prisoners Pt","San Joaquin River at Twitchell Island",
    "Lower San Joaquin River","Lower Sacramento River",
    "Sacramento River near Rio Vista",
    "San Joaquin River near Stockton","Old River","Middle River",
    "Victoria Canal","Holland Cut","Franks Tract",
    "North and South Forks Mokelumne River"
]

# NODE MAP
# Map each SLS subregion to the DSM2 node whose PTM result is applied to that subregion.
SUBREGION_TO_NODE = {
    "West Suisun Bay":359,"East Suisun Bay":329,"Grizzly Bay":365,
    "Montezuma Slough":420,"Honker Bay":227,"Carquinez Strait":359,
    "Upper Napa River":359,"Lower Napa River":359,
    "East San Pablo Bay":359,"West San Pablo Bay":359,"Mid San Pablo Bay":359,
    "Lower Sacramento River Ship Channel":350,
    "Sacramento River near Ryde":293,
    "Cache Slough and Liberty Island":323,
    "Upper Sacramento River":304,
    "San Joaquin River at Prisoners Pt":34,
    "San Joaquin River at Twitchell Island":41,
    "Lower San Joaquin River":46,
    "Lower Sacramento River":353,
    "Sacramento River near Rio Vista":351,
    "San Joaquin River near Stockton":29,
    "Old River":86,"Middle River":145,
    "Victoria Canal":75,"Holland Cut":99,
    "Franks Tract":225,
    "North and South Forks Mokelumne River":249
}

# GROUPED TABLE 
GROUPS = {
    "Western Delta": ["Carquinez Strait","Upper Napa River","Lower Napa River","East San Pablo Bay","West San Pablo Bay","Mid San Pablo Bay"],
    "Suisun Marsh": ["West Suisun Bay","East Suisun Bay","Grizzly Bay","Montezuma Slough","Honker Bay"],
    "Sacramento/North Delta": ["Lower Sacramento River Ship Channel","Sacramento River near Ryde","Cache Slough and Liberty Island","Upper Sacramento River"],
    "Lower San Joaquin": ["San Joaquin River at Prisoners Pt","San Joaquin River at Twitchell Island","Lower San Joaquin River"],
    "Lower Sacramento": ["Lower Sacramento River","Sacramento River near Rio Vista"],
    "South Delta": ["San Joaquin River near Stockton","Old River","Middle River","Victoria Canal","Holland Cut","Franks Tract"],
    "Eastern Delta": ["North and South Forks Mokelumne River"]
}

# helper function(s)
def grouped_ptm(cols, ptm_dict):
    #Calculate grouped PTM using simple arithmetic mean of all subregions in the group.

    vals = [
        ptm_dict[col]
        for col in cols
    ]

    return sum(vals) / len(vals) if vals else 0.0

# Build REGION_NODES automatically from GROUPS and SUBREGION_TO_NODE
REGION_NODES = {}

for region, cols in GROUPS.items():

    nodes = []

    for col in cols:
        node = SUBREGION_TO_NODE[col]

        if node not in nodes:
            nodes.append(node)

    REGION_NODES[region] = nodes

# COLORS
# Region colors used consistently for both PNG and HTML plots.
# Nodes in the same region share a color and are distinguished by line style.
REGION_COLORS = {
    "Sacramento/North Delta": "#9BBB59",
    "Eastern Delta": "#CE6DCD",
    "Lower San Joaquin": "#558ED5",
    "Lower Sacramento": "#C0504D",
    "South Delta": "#93CDDD",
    "Western Delta" : "#8064A2",
    "Suisun Marsh" : "#F79646",
}

# LINESTYLES
# Node-specific line styles used to distinguish nodes within each region.
# Tuple styles are custom Matplotlib dash patterns and are simplified in the HTML output.
NODE_LINESTYLES = {
    293: "-", 350: "--", 323: "-.", 304: ":",
    249: "-",
    41: "-", 34: "--", 46: "-.",
    351: "-", 353: "--",
    29: "-", 99: "--", 75: ":", 86: "-.", 225: (5, 2), 145: (3, 1, 1, 1),
    359: "-",
    #227: "-", 329: "--", 359: "-.", 365: ":", 420: (5, 2),
    227: "-", 329: "--", 365: ":", 420: (5, 2)
}

# SCENARIOS
# Map PTM scenario letters from DSS filenames to OMR labels used in plot titles and summary rows.
SCENARIO_OMR = {
    "D": "-6,500",
    "A": "-5,000",
    "B": "-3,500",
    "C": "-2,000",
}

# HELPER FUNCTIONS
# Parse scenario ID, scenario letter, and DSM2 node number from the DSS filename.
# Assumes every DSS filename in PTM_DIR follows the expected naming convention.
# If a filename does not match this pattern, the script will fail at match.group().
def parse_dss_filename(dss_path):
    match = re.match(r"(\d{8}-\d+[A-D]).*_(\d+)\.dss", dss_path.name)
    return match.group(1), match.group(1)[-1], int(match.group(2))

# Search all DSS pathnames for the requested flux time series and scenario ID.
# Expected DSS pathname structure is /A/B/C/D/E/F/.
def get_rts_series(dss, flux_name, scenario_id):
    for p in dss.get_pathnames():
        parts = p.strip("/").split("/")
        if len(parts) != 6:
            continue

        a, b, c, d, e, f = parts
        # Match only PTM 15-minute FLUX records for the requested export facility and scenario.
        if (
            a.startswith("PTMV")
            and b == flux_name
            and c == "FLUX"
            and e == "15MIN"
            and f.strip() == scenario_id
        ):
            ts = dss.read_rts(p)
            df = ts.data.copy()
            df.index = pd.to_datetime(df.index)
            return pd.Series(df.iloc[:, 0], index=df.index)

    raise RuntimeError()

# Build and save the interactive Plotly HTML version of a Matplotlib plot.
# This function uses pre-built trace dictionaries so the HTML uses the same data as the PNG.
def save_lfs_html(
    traces,
    title,
    t_start,
    t_end,
    out_html,
    ylabel="Percentage of Particles Entrained"
):

    fig = go.Figure()

    for tr in traces:

        fig.add_trace(
            go.Scatter(
                x=tr["x"],
                y=tr["y"],
                mode="lines",
                name=tr["name"],
                line=dict(
                    color=tr["color"],
                    width=3,
                    dash=tr["dash"]
                ),
                hovertemplate=(
                    "%{fullData.name}<br>"
                    "Date: %{x|%m/%d/%Y %H:%M}<br>"
                    "Value: %{y:.1f}%"
                    "<extra></extra>"
                )
            )
        )

    fig.update_layout(
        title=dict(
            text=title.replace("\n", "<br>"),
            x=0.5
        ),
        template="plotly_white",
        width=850,
        height=700,
        hovermode="x unified",
        margin=dict(
            l=70,
            r=30,
            t=150,
            b=120
        ),
        legend=dict(
            orientation="h",
            y=-0.25,
            x=0.5,
            xanchor="center"
        )
    )

    weekly_ticks = [
        pd.Timestamp(t_start) + pd.Timedelta(days=7 * i)
        for i in range(5)
    ]

    x_axis_end = max(pd.Timestamp(t_end), weekly_ticks[-1]) + pd.Timedelta(days=1)

    fig.update_xaxes(
        range=[
            pd.Timestamp(t_start).strftime("%Y-%m-%d %H:%M:%S"),
            x_axis_end.strftime("%Y-%m-%d %H:%M:%S")
        ],
        tickmode="array",
        tickvals=[
            tick.strftime("%Y-%m-%d %H:%M:%S")
            for tick in weekly_ticks
        ],
        ticktext=[
            tick.strftime("%m/%d/%Y")
            for tick in weekly_ticks
        ],
        tickangle=45,
        ticklabelstep=1,
        ticklabeloverflow="allow",
        automargin=True,
        showgrid=True,
        gridcolor="rgba(0,0,0,0.2)"
    )

    fig.update_yaxes(
        range=[-20, 120],
        title=ylabel,
        showgrid=True,
        gridcolor="rgba(0,0,0,0.2)"
    )

    fig.add_hline(
        y=0,
        line_color="gray",
        line_width=1
    )

    pio.write_html(
        fig,
        file=str(out_html),
        auto_open=False,
        include_plotlyjs="cdn",
        config={
            "displaylogo": False,
            "responsive": True
        }
    )

# LOAD DATA
# data_all stores all successfully loaded node/scenario DSS time series.
# data_plot stores only the subset of nodes included in REGION_NODES for detailed plotting.
data_all = {}
data_plot = {}

# Load each DSS file for the requested week.
# Each file is expected to represent one DSM2 node and one PTM scenario.
for dss_path in sorted(PTM_DIR.glob("*.dss")):
    scenario_id, scen, node = parse_dss_filename(dss_path)

    # Read CVP and SWP export entrainment time series from the DSS file.
    # If either required series is missing, this DSS file is skipped.
    with DSSFile(str(dss_path)) as dss:
        try:
            cvp = get_rts_series(dss, "EXPORT_CVP", scenario_id)
            swp = get_rts_series(dss, "EXPORT_SWP", scenario_id)
        except RuntimeError:
            continue

# Calculate Delta Exports
    df = pd.DataFrame({"EXPORT_CVP": cvp, "EXPORT_SWP": swp})
    df["EXPORT_TOTAL"] = df["EXPORT_CVP"] + df["EXPORT_SWP"]

    # Restrict stored PTM data to the plotting and summary window:
    # 7 days before injection through 21 days after injection.
    t_start = INJECTION_DATE - pd.Timedelta(days=7)
    t_end = INJECTION_DATE + pd.Timedelta(days=21)
    df = df[(df.index >= t_start) & (df.index <= t_end)]

    # Store all successfully loaded node/scenario data for later table calculations.
    data_all[(node, scen)] = df

    # Store only template nodes for detailed node-level plotting.
    # Nodes not listed in REGION_NODES remain available in data_all for table calculations.
    if any(node in nodes for nodes in REGION_NODES.values()):
        data_plot[(node, scen)] = df

# Build scenario labels only from scenarios that were actually loaded from DSS files.
available_scenarios = dict(
    sorted(
        {
            scen: SCENARIO_OMR.get(scen, scen)
            for _, scen in data_all.keys()
        }.items(),
        key=lambda item: int(item[1].replace(",", ""))
    )
)

# PLOT
scenarios = sorted(set(s for (_, s) in data_plot.keys()))

# Generate one detailed node-level PNG and HTML plot for each available scenario.
for scen in scenarios:
    fig, ax = plt.subplots(figsize=(6.5, 6))
    fig.subplots_adjust(bottom=0.42)

    html_traces = []

    # Add each plotted node for the current scenario.
    # These traces are used for both the Matplotlib PNG and the Plotly HTML output.
    for (node, s), df in data_plot.items():
        if s != scen:
            continue

        # Identify the plotting region for this node based on REGION_NODES.
        region = next((r for r, nodes in REGION_NODES.items() if node in nodes), None)
        color = REGION_COLORS[region]
        ls = NODE_LINESTYLES[node]

        line, = ax.plot(df.index, df["EXPORT_TOTAL"], color=color, lw=2)

        dash_lookup = {
            "-": "solid",
            "--": "dash",
            "-.": "dashdot",
            ":": "dot"
        }

        dash = (
            "solid"
            if isinstance(ls, tuple)
            else dash_lookup.get(ls, "solid")
        )

        html_traces.append(
            dict(
                x=df.index,
                y=df["EXPORT_TOTAL"],
                name=f"{region} ({node})",
                color=color,
                dash=dash
            )
        )

        if isinstance(ls, tuple):
            line.set_linestyle("-")
            line.set_dashes(ls)
        else:
            line.set_linestyle(ls)

    ax.set_xlim(t_start, t_end)
    ax.xaxis.set_major_locator(mdates.WeekdayLocator(interval=1))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%m/%d/%Y"))
    plt.setp(ax.get_xticklabels(), rotation=45, ha="right")

    ax.set_ylim(-20, 120)
    ax.set_ylabel("Percentage of Particles Entrained")
    ax.grid(alpha=0.3)
    ax.axhline(0, color="gray", lw=0.8)
    ax.set_title(
        "PTM Results by Injection Region for Position Oriented Particles\n"
        "Entrained at CVP and SWP Export Facilities.\n"
        f"OMR Scenario = {available_scenarios[scen]}. "
        f"Particles Injected {INJECTION_DATE.strftime('%m/%d/%Y')}."
    )

    # Build a fixed legend from REGION_NODES so plot legends remain consistent across scenarios.
    # This legend may include nodes even if data for that node/scenario was not available.
    legend_handles = []
    for region, nodes in REGION_NODES.items():
        for node in nodes:
            handle = Line2D([0], [0], color=REGION_COLORS[region], lw=2)
            ls = NODE_LINESTYLES[node]

            if isinstance(ls, tuple):
                handle.set_linestyle("-")
                handle.set_dashes(ls)
            else:
                handle.set_linestyle(ls)

            handle.set_label(f"{region} ({node})")
            legend_handles.append(handle)

    ax.legend(
        handles=legend_handles,
        loc="upper center",
        bbox_to_anchor=(0.49, -0.28),
        ncol=3,
        frameon=True,
        fontsize=9
    )

    png_file = FIG_DIR / f"LFS_Scenario_{scen}.png"
    html_file = FIG_DIR / f"LFS_Scenario_{scen}.html"

    plt.savefig(png_file, dpi=300)

    save_lfs_html(
        traces=html_traces,
        title=ax.get_title(),
        t_start=t_start,
        t_end=t_end,
        out_html=html_file
    )

    plt.close(fig)

# Builds Full Summary Table

ptm_lookup = {(node, scen): df["EXPORT_TOTAL"] for (node, scen), df in data_all.items()}

for wk, timestamp in weeks.items():

    rows = []

    # First row documents which DSM2 node is assigned to each SLS subregion.
    node_row = {"Metric": "DSM2 Node"}
    for col in COLUMNS:
        node_row[col] = SUBREGION_TO_NODE[col]
    node_row["Total"] = ""
    rows.append(node_row)

    total_abundance = 0
    # Add latest SLS abundance estimates by subregion.
    # If a subregion is missing from the SLS lookup, it is treated as 0.
    abundance_row = {"Metric": "LFS Abundance"}
    for col in COLUMNS:
        val = SLS_ABUND.get(col, 0)
        abundance_row[col] = val
        total_abundance += val

    abundance_row["Total"] = total_abundance
    rows.append(abundance_row)

    ptm_vals = {}

    # Sample PTM total export entrainment percentage at the exact weekly timestamp.
    # If the node/scenario series is missing or the exact timestamp is absent, the value is set to 0.0.
    for scen, omr in available_scenarios.items():
        row = {"Metric": f"PTM (%) {omr}"}
        ptm_vals[scen] = {}

        weighted_numerator = 0.0
        weighted_denominator = 0.0

        for col in COLUMNS:
            node = SUBREGION_TO_NODE[col]
            series = ptm_lookup.get((node, scen), None)

            val = (
                float(series.loc[timestamp])
                if (series is not None and timestamp in series.index)
                else 0.0
            )

            row[col] = round(val, 1)
            ptm_vals[scen][col] = val

            abundance = SLS_ABUND.get(col, 0)

            weighted_numerator += abundance * val
            weighted_denominator += abundance

        row["Total"] = (
            round(weighted_numerator / weighted_denominator, 1)
            if weighted_denominator > 0
            else 0.0
        )

        rows.append(row)

    # Entrained fish counts
    for scen, omr in available_scenarios.items():

        row = {"Metric": f"Entrained (#) {omr}"}

        total_entrained = 0

        for col in COLUMNS:

            abundance = SLS_ABUND.get(col, 0)

            entrained = abundance * ptm_vals[scen][col] / 100.0

            row[col] = round(entrained, 1)

            total_entrained += entrained

        row["Total"] = round(total_entrained, 0)

        rows.append(row)

    df_out = pd.DataFrame(rows)
    df_out.to_csv(FIG_DIR / f"LFS_Summary_{wk}_{WEEK_STR}.csv", index=False)

    print(f"Saved table: {wk}")

def build_grouped_from_existing():
    
    for wk, timestamp in weeks.items():

        ptm_vals = {}
        for scen in available_scenarios:
            ptm_vals[scen] = {}
            for col in COLUMNS:
                node = SUBREGION_TO_NODE[col]
                series = ptm_lookup.get((node, scen), None)
                
                # Critical timestamp check: this requires an exact match to the 15-minute DSS timestamp.
                # No interpolation or nearest-time lookup is performed.
                val = float(series.loc[timestamp]) if (series is not None and timestamp in series.index) else 0.0
                ptm_vals[scen][col] = val

        rows = []

        # --- DSM2 Node Mapping Row ---
        node_row = {"Metric": "DSM2 Nodes"}

        for region, nodes in REGION_NODES.items():
            node_row[region] = ",".join(str(n) for n in nodes)

        node_row["Total"] = ""
        node_row["Total (%)"] = ""

        rows.append(node_row)

        # --- Abundance ---
        # Grouped abundance is calculated as the sum of SLS abundance across subregions in each group.
        abundance_row = {"Metric": "LFS Abundance (Grouped)"}
        html_traces = []

        for group, cols in GROUPS.items():
            abundance_row[group] = sum(SLS_ABUND.get(c, 0) for c in cols)

        abundance_row["Total"] = sum(abundance_row[g] for g in GROUPS)
        grouped_total_abundance = abundance_row["Total"]
        abundance_row["Total (%)"] = ""
        rows.append(abundance_row)

        # --- PTM ---
        for scen, omr in available_scenarios.items():
            row = {"Metric": f"PTM (%) {omr}"}

            for group, cols in GROUPS.items():
                # Grouped PTM percentage is calculated as a simple average across subregions in the group.
                # This is not abundance-weighted.
                row[group] = round(
                    grouped_ptm(cols, ptm_vals[scen]),
                    1
                )

            row["Total"] = round(sum(row[g] for g in GROUPS), 1)
            row["Total (%)"] = ""
            rows.append(row)

        # --- Entrained ---
        for scen, omr in available_scenarios.items():
            row = {"Metric": f"Entrained (#) {omr}"}
            total_ent = 0

            for group, cols in GROUPS.items():
                # Grouped entrained count is calculated at the subregion level first, then summed by group.
                total = sum(
                    SLS_ABUND.get(col, 0) * ptm_vals[scen][col] / 100.0
                    for col in cols
                )
                row[group] = round(total, 1)
                total_ent += total

            row["Total"] = round(total_ent, 0)

            if grouped_total_abundance > 0:
                row["Total (%)"] = round(
                    total_ent / grouped_total_abundance * 100,
                    2
                )
            else:
                row["Total (%)"] = 0

            rows.append(row)

        df_group = pd.DataFrame(rows)
        df_group.to_csv(FIG_DIR / f"LFS_Summary_Grouped_{wk}_{WEEK_STR}.csv", index=False)

        print(f"Saved grouped table: {wk}")

build_grouped_from_existing()

# Generate grouped PTM time-series plots by averaging mapped subregion/node time series within each group.
def plot_grouped_ptm():

    t_start = INJECTION_DATE - pd.Timedelta(days=7)
    t_end = INJECTION_DATE + pd.Timedelta(days=21)

    scenarios = sorted(set(s for (_, s) in data_all.keys()))

    for scen in scenarios:

        fig, ax = plt.subplots(figsize=(6.5, 6))
        fig.subplots_adjust(bottom=0.32)

        html_traces = []

        # --- build grouped time series ---
        for group, cols in GROUPS.items():

            series_list = []

            for col in cols:
                node = SUBREGION_TO_NODE[col]
                series = ptm_lookup.get((node, scen), None)

                if series is not None:
                    series_list.append(series)

            if not series_list:
                continue

            # align + average
            # Align all available subregion/node time series by timestamp before averaging.
            df_concat = pd.concat(series_list, axis=1)
            group_series = df_concat.mean(axis=1)

            group_series = group_series[
                (group_series.index >= t_start) &
                (group_series.index <= t_end)
            ]

            # --- plot ---
            color = REGION_COLORS.get(group, "#000000")

            ax.plot(
                group_series.index,
                group_series,
                color=color,
                lw=3,
                linestyle="--",
                label=group
            )

            html_traces.append(
                dict(
                    x=group_series.index,
                    y=group_series,
                    name=group,
                    color=color,
                    dash="dash"
                )
            )

        # --- formatting ---
        ax.set_xlim(t_start, t_end)
        ax.xaxis.set_major_locator(mdates.WeekdayLocator(interval=1))
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%m/%d/%Y"))
        plt.setp(ax.get_xticklabels(), rotation=45, ha="right")

        ax.set_ylim(-20, 120)
        ax.set_ylabel("Percentage of Particles Entrained")
        ax.grid(alpha=0.3)
        ax.axhline(0, color="gray", lw=0.8)

        # Title matches your reference figure
        ax.set_title(
            "Average PTM Results by Injection Region for Position Oriented Particles\n"
            "Entrained at CVP and SWP Export Facilities.\n"
            f"OMR Scenario = {available_scenarios[scen]}. "
            f"Particles Injected {INJECTION_DATE.strftime('%m/%d/%Y')}."
        )

        ax.legend(
            loc="upper center",
            bbox_to_anchor=(0.5, -0.28),
            ncol=2,
            frameon=True,
            fontsize=9
        )

        png_file = FIG_DIR / f"LFS_Grouped_PTM_Scenario_{scen}.png"
        html_file = FIG_DIR / f"LFS_Grouped_PTM_Scenario_{scen}.html"

        plt.savefig(png_file, dpi=300)

        save_lfs_html(
            traces=html_traces,
            title=ax.get_title(),
            t_start=t_start,
            t_end=t_end,
            out_html=html_file
        )
        plt.close(fig)

        print(f"Saved grouped plot: Scenario {scen}")

plot_grouped_ptm()