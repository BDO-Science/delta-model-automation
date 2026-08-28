import sys
import re
from pathlib import Path
from datetime import timedelta
from collections import defaultdict
import matplotlib as mpl
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from pyhecdss import DSSFile
from plotly.subplots import make_subplots
import plotly.graph_objects as go
import plotly.io as pio

# ==============================================================
# GLOBAL PLOT STYLE
# ==============================================================

mpl.rcParams.update({
    "figure.figsize": (11, 6),
    "axes.titlesize": 14,
    "axes.titleweight": "bold",
    "axes.labelsize": 12,
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "legend.fontsize": 11,
})

# ==============================================================
# COMMAND-LINE ARGUMENT
# ==============================================================

if len(sys.argv) != 2:
    raise RuntimeError(
        "Usage: python plot_ptm_particles.py YYYYMMDD\n"
        "Example: python plot_ptm_particles.py 20260512"
    )

WEEK_STR = sys.argv[1]
if not (WEEK_STR.isdigit() and len(WEEK_STR) == 8):
    raise ValueError("Weekly folder must be YYYYMMDD")

INJECTION_DATE = pd.Timestamp(WEEK_STR)


# ==============================================================
# DIRECTORIES
# ==============================================================

MODEL_DIR = Path(__file__).resolve().parent
OUTPUT_ROOT = MODEL_DIR / "Output"
FIG_ROOT = MODEL_DIR / "Figures"

WEEK_DIR = OUTPUT_ROOT / WEEK_STR
if not WEEK_DIR.exists():
    raise FileNotFoundError(f"Weekly folder not found: {WEEK_DIR}")

PTM_ROOT = WEEK_DIR / "PTM"
if not PTM_ROOT.exists():
    raise FileNotFoundError(f"Missing PTM directory: {PTM_ROOT}")

FIG_WEEK_DIR = FIG_ROOT / WEEK_STR
FIG_WEEK_DIR.mkdir(parents=True, exist_ok=True)

print(f"Processing weekly folder: {WEEK_STR}")


# ==============================================================
# SETTINGS
# ==============================================================

FLUX_NAMES = [
    "PAST_CHIPPS",
    "DECKER",
    "MIDDLERIVER",
    "OLDRIVER",
    "EXPORT_CVP",
    "EXPORT_SWP",
]

REQUIRED_NODES = {99, 350, 465, 469}
REQUIRED_SCENARIOS = {"A", "B", "C", "D"}


# ==============================================================
# LOOKUPS
# ==============================================================

SCENARIO_OMR_LOOKUP = {
    "A": "-5,000",
    "B": "-3,500",
    "C": "-2,000",
    "D": "-6,500",
}

SCENARIO_TO_FLOW = SCENARIO_OMR_LOOKUP.copy()

NODE_NAME_LOOKUP = {
    465: "Chipps",
    350: "Cache Slough",
    469: "Jersey Point",
    99:  "Old River",
}

PARTICLE_TYPES = {
    "NP": {
        "folder": "np",
        "title": "PTM Results for Neutral Particles",
    },
    "PP": {
        "folder": "pp",
        "title": "PTM Results for Surface Particles",
    },
}


# ==============================================================
# HELPER (FIXED ONLY HERE)
# ==============================================================

def parse_dss_filename(dss_path):
    match = re.match(
        r"(?P<scenario>\d{8}-\d+[A-D]).*_(?P<node>\d+)\.dss",
        dss_path.name
    )
    if not match:
        raise ValueError(f"Cannot parse DSS filename: {dss_path.name}")

    scenario_id = match.group("scenario")
    scenario_letter = scenario_id[-1]
    node = int(match.group("node"))

    return scenario_id, scenario_letter, node


def get_rts_series(dss, flux_name, scenario_id):
    for p in dss.get_pathnames():
        parts = p.strip("/").split("/")
        if len(parts) != 6:
            continue

        a, b, c, d, e, f = parts
        if (
            a.startswith("PTMV")
            and b == flux_name
            and c == "FLUX"
            and e == "15MIN"
            and f == scenario_id
        ):
            ts = dss.read_rts(p)

            df = ts.data.copy()

            # ONLY CHANGE: use DSS time index directly
            df.index = pd.to_datetime(df.index)

            values = pd.to_numeric(df.iloc[:, 0], errors="coerce")

            return pd.Series(values, index=df.index)

    raise RuntimeError(f"No RTS record found for {flux_name}")


# ==============================================================
# RULESET
# ==============================================================

def apply_ruleset_common(df, node):
    df = df.copy()

    df["Past Chipps"] = df["PAST_CHIPPS"]
    df["Upstream of Decker"] = df["DECKER"]

    df["Unresolved in OMR Corridor"] = (
        -(df["MIDDLERIVER"] + df["OLDRIVER"])
        - (df["EXPORT_CVP"] + df["EXPORT_SWP"])
    )

    if node == 99:
        df["Unresolved in Central Delta"] = (
            100 - df["PAST_CHIPPS"] + df["MIDDLERIVER"] + df["OLDRIVER"]
        )
    else:
        df["Unresolved in Central Delta"] = (
            100 - df["PAST_CHIPPS"] - df["MIDDLERIVER"] - df["OLDRIVER"]
        )

    df["CVP Entrainment"] = df["EXPORT_CVP"]
    df["SWP Entrainment"] = df["EXPORT_SWP"]

    plot_order = [
        "Past Chipps",
        "Upstream of Decker",
        "Unresolved in Central Delta",
        "Unresolved in OMR Corridor",
        "CVP Entrainment",
        "SWP Entrainment",
    ]

    plot_styles = {
        "Past Chipps": dict(ls="--", color="#000000", lw=2.5),
        "Upstream of Decker": dict(ls="-", color="#CE6DCD"),
        "Unresolved in Central Delta": dict(ls="--", color="#f2b600", lw=2.5),
        "Unresolved in OMR Corridor": dict(ls="--", color="#C00000"),
        "CVP Entrainment": dict(ls="-", color="#0252F2"),
        "SWP Entrainment": dict(ls="--", color="#9BBB59"),
    }

    return df, plot_order, plot_styles


# ==============================================================
# WEEKLY SAMPLING
# ==============================================================

def compute_weekly_points(df, week1_start):
    weeks = {}

    for i in range(3):
            week_start = week1_start + timedelta(days=7 * i)
            ref_time = week_start + timedelta(days=6, hours=23, minutes=45)

            weeks[f"Week {i+1}"] = df.loc[ref_time]

    return weeks

def save_plotly_html(
    df,
    plot_order,
    plot_styles,
    title,
    t_start,
    t_end,
    out_html
):
    fig = go.Figure()

    for name in plot_order:

        style = plot_styles[name]

        dash_lookup = {
            "-": "solid",
            "--": "dash",
            "-.": "dashdot",
            ":": "dot"
        }

        fig.add_trace(
            go.Scatter(
                x=df.index,
                y=df[name],
                mode="lines",
                name=name,
                line=dict(
                    color=style.get("color", "black"),
                    width=style.get("lw", 2),
                    dash=dash_lookup.get(style.get("ls", "-"), "solid")
                ),
                hovertemplate=(
                    f"{name}<br>"
                    "Date: %{x|%m/%d/%Y %H:%M}<br>"
                    "Value: %{y:.1f}%"
                    "<extra></extra>"
                )
            )
        )

    fig.update_layout(
        title=dict(
            text=title,
            x=0.5
        ),
        width=1100,
        height=600,
        template="plotly_white",
        hovermode="x unified",
        margin=dict(l=70, r=30, t=100, b=120),
        legend=dict(
            orientation="h",
            y=-0.28,
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
        title="Percentage of Particles",
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

# ==============================================================
# MAIN DRIVER
# ==============================================================

for ptype, cfg in PARTICLE_TYPES.items():

    print(f"\n=== Processing {ptype} particles ===")

    PTM_DIR = PTM_ROOT / cfg["folder"]
    if not PTM_DIR.exists():
        raise FileNotFoundError(f"Missing PTM folder: {PTM_DIR}")

    FIG_OUT_DIR = FIG_WEEK_DIR / ptype
    FIG_OUT_DIR.mkdir(parents=True, exist_ok=True)

    dss_files = sorted(PTM_DIR.glob("*.dss"))

    found = defaultdict(set)
    for f in dss_files:
        _, s, n = parse_dss_filename(f)
        if n in REQUIRED_NODES:
            found[n].add(s)

    # Warn (rather than abort) if any required node is missing scenarios.
    # Nodes are expected to be identical across scenarios, so we just
    # report what's missing and continue with whatever is available.
    warnings = []
    for node in REQUIRED_NODES:
        missing = REQUIRED_SCENARIOS - found.get(node, set())
        if missing:
            warnings.append(
                f"{ptype}: Node {node} missing scenarios {sorted(missing)}"
            )

    if warnings:
        print("[WARNING] Missing PTM runs (continuing with available data):")
        for w in warnings:
            print(f"  - {w}")
    else:
        print(f"[OK] {ptype}: all required nodes/scenarios present")

    node_weekly_results = defaultdict(list)

    for dss_path in dss_files:
        scenario_id, scenario_letter, node = parse_dss_filename(dss_path)
        if node not in REQUIRED_NODES:
            continue

        with DSSFile(str(dss_path)) as dss:
            data = {
                f: get_rts_series(dss, f, scenario_id)
                for f in FLUX_NAMES
            }

        df = pd.DataFrame(data)

        t_start = INJECTION_DATE - timedelta(days=7)
        t_end = INJECTION_DATE + timedelta(days=21)
        df = df[(df.index >= t_start) & (df.index <= t_end)]

        df, plot_order, plot_styles = apply_ruleset_common(df, node)

        weekly_dfs = compute_weekly_points(df, INJECTION_DATE)
        flow_bin = SCENARIO_TO_FLOW[scenario_letter]

        for wk, wkdf in weekly_dfs.items():
            if wkdf is None:
                continue

            node_weekly_results[node].append({
                "Forecast Week": wk,
                "Week Range": wkdf.name.strftime("%m/%d/%Y %H:%M"),
                "OMR Flow Bin": flow_bin,
                "Past Chipps": round(wkdf["Past Chipps"], 1),
                "Upstream of Decker": round(wkdf["Upstream of Decker"], 1),
                "Unresolved in Central Delta": round(
                    wkdf["Unresolved in Central Delta"], 1
                ),
                "Unresolved in OMR Corridor": round(
                    wkdf["Unresolved in OMR Corridor"], 1
                ),
                "CVP Entrainment": round(wkdf["CVP Entrainment"], 1),
                "SWP Entrainment": round(wkdf["SWP Entrainment"], 1),
            })

        fig, ax = plt.subplots()

        for name in plot_order:
            ax.plot(df.index, df[name], label=name, **plot_styles[name])

        ax.set_xlim(t_start, t_end)
        ax.xaxis.set_major_locator(mdates.WeekdayLocator(interval=1))
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%m/%d/%Y"))

        plt.setp(
            ax.get_xticklabels(),
            rotation=45,
            ha="right",
            va="top",
            rotation_mode="anchor"
        )

        ax.set_ylim(-20, 120)
        ax.set_ylabel("Percentage of Particles")
        ax.axhline(0, color="gray", lw=0.8)
        ax.grid(alpha=0.3)

        title = (
            f"{cfg['title']}. "
            f"OMR Scenario = {SCENARIO_OMR_LOOKUP[scenario_letter]}.\n"
            f"Particles Injected {INJECTION_DATE.strftime('%m/%d/%Y')} "
            f"at DSM2 Node {node} ({NODE_NAME_LOOKUP[node]})."
        )

        ax.set_title(title)

        ax.legend(
            loc="upper center",
            bbox_to_anchor=(0.5, -0.30),
            ncol=2,
            frameon=True
        )

        fig.tight_layout()
        base_name = f"PTM_{ptype}_Node{node}_Scenario{scenario_letter}"

        base_name = f"PTM_{ptype}_Node{node}_Scenario{scenario_letter}"

        out_png = FIG_OUT_DIR / f"{base_name}.png"
        out_html = FIG_OUT_DIR / f"{base_name}.html"

        fig.savefig(out_png, dpi=300)

        save_plotly_html(
            df=df,
            plot_order=plot_order,
            plot_styles=plot_styles,
            title=title,
            t_start=t_start,
            t_end=t_end,
            out_html=out_html
        )

        plt.close(fig)

        print(f"Saved figure: {out_png}")
        print(f"Saved HTML : {out_html}")

        #out_png = (
        #    FIG_OUT_DIR
        #    / f"PTM_{ptype}_Node{node}_Scenario{scenario_letter}.png"
        #)
        #fig.savefig(out_png, dpi=300)
        #plt.close(fig)

        #print(f"Saved figure: {out_png}")

    for node_id, rows in node_weekly_results.items():
        df_out = pd.DataFrame(rows)

        flow_order = ["-6,500", "-5,000", "-3,500", "-2,000"]
        df_out["FlowOrder"] = df_out["OMR Flow Bin"].apply(flow_order.index)
        df_out["WeekOrder"] = df_out["Forecast Week"].str.extract(r"(\d)").astype(int)

        df_out = df_out.sort_values(["WeekOrder", "FlowOrder"])
        df_out = df_out.drop(columns=["FlowOrder", "WeekOrder"])

        out_csv = FIG_OUT_DIR / f"PTM_{ptype}_Node{node_id}_WeeklySummary.csv"
        df_out.to_csv(out_csv, index=False)

        print(f"Saved CSV: {out_csv}")