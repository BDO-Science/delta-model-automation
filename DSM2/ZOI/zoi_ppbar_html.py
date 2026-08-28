"""
Channel Length Bar Plots - Python Version
Replicates the R script channel_length_barplots_clean.R

Last updated: 03/27/2026
Converts R workflow to Python using pandas, matplotlib, and openpyxl
"""


import sys
import os
import glob
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from pathlib import Path
import geopandas as gpd
from openpyxl import Workbook
from openpyxl.styles import Font, Alignment, Border, Side
from openpyxl.utils import get_column_letter


import plotly.graph_objects as go
from plotly.subplots import make_subplots

def parse_omr_flow(filename: str) -> str:
    mapping = {
        "Neg6500": "-6500",
        "Neg5000": "-5000",
        "Neg3500": "-3500",
        "Neg2000": "-2000"
    }
    for key, val in mapping.items():
        if key in filename:
            return val
    raise ValueError(f"Could not parse OMR flow from filename: {filename}")
def main():
    # Parse command line arguments
    args = sys.argv[1:]
    print("Command line arguments:", args)
    
    Week = args[0]
    InflowBin_W1 = args[1]
    InflowBin_W2 = args[2]
    InflowBin_W3 = args[3]
    Week1_dates = args[4]
    Week2_dates = args[5]
    Week3_dates = args[6]
    
    print(f"Week: {Week}")
    
    # Set paths
    script_dir   = os.path.dirname(os.path.abspath(__file__))
    week_dir     = os.path.join(script_dir, Week)
    ZOI_path = os.path.join(week_dir, "output_csvs")
    print(f"Z0I_path: {ZOI_path}")
    os.chdir(Week)
    
    # Define orderings
    alt_order = ["EXP1", "EXP3", "NAA", "ALT1", "ALT2d", "ALT2b", "ALT2c", "ALT2a", "ALT3", "ALT4"]
    alt_order2 = ["NAA", "Alt1", "Alt2woTUCPwoVA", "Alt2woTUCPDeltaVA", "Alt2woTUCPAllVA", "Alt2wTUCPwoVA", "Alt3", "Alt4"]
    inflow_order = ["lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi", "NA"]
    scenario_order = [-6500, -5000, -3500, -2000]

    
    # Create template dataframe
    weeks_list = ["Week 1", "Week 2", "Week 3"]
    omr_bins = [-6500, -5000, -3500, -2000]
    omr_levels = [-2000, -3500, -5000, -6500]
    template_df = pd.DataFrame([
        {"Week": w, "OMR_Bin": omr}
        for w in weeks_list
        for omr in omr_bins
    ])
    
    template_df["Dates"] = None
    template_df["Low_Sum_mi"] = np.nan
    template_df["Low_pct"] = np.nan
    template_df["Medium_Sum_mi"] = np.nan
    template_df["Medium_pct"] = np.nan
    template_df["High_Sum_mi"] = np.nan
    template_df["High_pct"] = np.nan
    
    # Process each week
    weeks = ["week1", "week2", "week3"]
    
    for i, week in enumerate(weeks):
        print(f"\nProcessing {week}...")
        
        # Find CSV files for this week
        pattern = f"DEZOI_OMR.*{week}.*csv$"
        fp = glob.glob(os.path.join("output_csvs", f"*{week}*.csv"))
        if not fp:
            fp = glob.glob(os.path.join("output_csvs", f"DEZOI_OMR*{week}*.csv"))
        
        print(f"Found {len(fp)} files for {week}")
        
        # Set inflow bin based on week
        if week == "week1":
            inflowbin = InflowBin_W1
            Week_dates = Week1_dates
        elif week == "week2":
            inflowbin = InflowBin_W2
            Week_dates = Week2_dates
        else:
            inflowbin = InflowBin_W3
            Week_dates = Week3_dates
        
        # Read all CSV files for this week
        dsm2bins = []
        for file in fp:
            df = pd.read_csv(file, dtype=str)

            df['Sub.group'] = inflowbin

            filetag = parse_omr_flow(os.path.basename(file)).replace("-", "Neg")
            df['FileTag'] = filetag

            dsm2bins.append(df)
        
        # Combine all files
        bins = pd.concat(dsm2bins, keys=range(len(dsm2bins)), names=['id'])
        bins = bins.reset_index()
        
        bins["OMR_bin"] = bins["FileTag"].str.replace("Neg", "-").astype(float)
        
        # Fill NA Sub.group with "NA"
        bins['Sub.group'] = bins['Sub.group'].fillna("NA")
        
        # Remove duplicates
        bins = bins.drop_duplicates()
        
        # Convert to categorical with proper ordering
        bins['OMR_bin'] = pd.Categorical(bins['OMR_bin'], categories=[-2000, -3500, -5000, -6500], ordered=True)
        bins['FileTag'] = pd.Categorical(bins['FileTag'], categories=["Neg2000", "Neg3500", "Neg5000", "Neg6500"], ordered=True)
        bins['Sub.group'] = pd.Categorical(bins['Sub.group'], categories=inflow_order, ordered=True)
        
        # Calculate sample sizes
        n_flow_OMR_Alt = bins.groupby(['FileTag', 'Sub.group', 'OMR_bin'], observed=True).size().reset_index(name='n')
        
        # Calculate percentages within each FileTag
        n_flow_OMR_Alt['percent'] = n_flow_OMR_Alt.groupby('FileTag', observed = False)['n'].transform(lambda x: round(x / x.sum() * 100))
        
        
        

       # Complete missing combinations with zeros
        all_combos = pd.MultiIndex.from_product([
            n_flow_OMR_Alt['Sub.group'].cat.categories,
            n_flow_OMR_Alt['OMR_bin'].cat.categories,
            n_flow_OMR_Alt['FileTag'].cat.categories
        ], names=['Sub.group', 'OMR_bin', 'FileTag'])
        
        n_flow_OMR_Alt = n_flow_OMR_Alt.set_index(['Sub.group', 'OMR_bin', 'FileTag']).reindex(all_combos, fill_value=0).reset_index()
        
        # Calculate n_flow_Alt
        n_flow_Alt = bins.groupby(['FileTag', 'Sub.group'], observed=True).size().reset_index(name='n')
        n_flow_Alt['percent'] = n_flow_Alt.groupby('FileTag')['n'].transform(lambda x: round(x / x.sum() * 100))
        
        # Write sample sizes
        n_flow_OMR_Alt.to_csv("samplesizes_flow_OMR_alt.csv", index=False)
        n_flow_Alt.to_csv("samplesizes_flow_alt.csv", index=False)
        
        # Read channels data
        channels0 = pd.read_csv("../ZOI_Shapefiles/channel_std_delta_grid_NAVD.csv")
        channels0.columns = [c.lower().strip() for c in channels0.columns]   # clean_names()
        channels0 = channels0.rename(columns={"chan_no": "channel_number",
                                               "length":  "length"})

        drop_nodes = [146, 147, 148, 206, 242, 246, 432, 433, 434]
        channels1  = channels0[~channels0["upnode"].isin(drop_nodes)]
        channels   = channels1[~channels1["downnode"].isin(drop_nodes)].copy()
        
        # Read ZOI data for this week
        # Read ZOI data for this week
        pattern  = os.path.join(ZOI_path, f"DEZOI_OMR*{week}*.csv")
        csv_files = glob.glob(pattern)
        if not csv_files:
            print(f"  No CSV files found for {week}, skipping.")
            continue

        frames = []
        for f in csv_files:
            df_tmp = pd.read_csv(f)
            df_tmp["file"]     = f
            df_tmp["OMR_Flow"] = parse_omr_flow(os.path.basename(f))
            frames.append(df_tmp)

        zoi_data = pd.concat(frames, ignore_index=True).drop(columns=["file"])

        # ── 7c. Merge with channels ──────────────────────────────────────────────
        zoi_channel = zoi_data.merge(channels, on="channel_number", how="left")

        # ── 7d. Build zoi_channel_long (mirrors R code) ─────────────────────────
        zoi_channel_long = zoi_channel.copy()
        zoi_channel_long["group"]   = inflowbin
        zoi_channel_long = zoi_channel_long.rename(columns={"Proportion_Ratio": "overlap"})
        # Write data
        zoi_channel_long.to_csv("prop_overlap_data_long.csv", index=False)
        
        # Read the data for plotting
        zoi_channel_group = pd.read_csv("prop_overlap_data_long.csv")
        zoi_channel_group = zoi_channel_group[zoi_channel_group['overlap'] >= 0].copy()

        # ensure numeric
        zoi_channel_group["OMR_Flow"] = pd.to_numeric(zoi_channel_group["OMR_Flow"], errors="coerce")

        # SAFE definition
        available_omrs = sorted(zoi_channel_group["OMR_Flow"].dropna().unique().tolist())

        print("Available OMR scenarios:", available_omrs)
        print("Overlap stats:")
        print(zoi_channel_group['overlap'].describe())
        print("OMR dtype:", zoi_channel_long["OMR_Flow"].dtype)
        print("OMR unique:", zoi_channel_long["OMR_Flow"].unique())
        
        print("\n================ DEBUG WEEK3 -2000 =================")

        debug_2000 = zoi_channel_group[
            zoi_channel_group['OMR_Flow'].astype(str).str.strip() == '-2000'
        ].copy()

        print("Total rows:", len(debug_2000))

        print("\nOverlap summary:")
        print(debug_2000['overlap'].describe())

        print("\nRows in each category:")

        high_ct = (debug_2000['overlap'] < 0.25).sum()

        med_ct = (
            (debug_2000['overlap'] >= 0.25) &
            (debug_2000['overlap'] <= 0.75)
        ).sum()

        low_ct = (debug_2000['overlap'] > 0.75).sum()

        print("HIGH:", high_ct)
        print("MED :", med_ct)
        print("LOW :", low_ct)

        print("\nSmallest overlap values:")
        print(
            debug_2000[['overlap']]
            .sort_values('overlap')
            .head(20)
        )

        print("\nClosest to 0.25:")
        print(
            debug_2000.assign(
                dist=lambda x: abs(x['overlap'] - 0.25)
            )
            .sort_values('dist')[['overlap']]
            .head(20)
        )

        print("\nClosest to 0.75:")
        print(
            debug_2000.assign(
                dist=lambda x: abs(x['overlap'] - 0.75)
            )
            .sort_values('dist')[['overlap']]
            .head(20)
        )

        print("====================================================")

        # Filter to contours of interest
        filtered_dat_high = zoi_channel_group[zoi_channel_group['overlap'] < 0.25].copy()
        filtered_dat_med = zoi_channel_group[(zoi_channel_group['overlap'] >= 0.25) & 
                                              (zoi_channel_group['overlap'] <= 0.75)].copy()
        filtered_dat_low = zoi_channel_group[zoi_channel_group['overlap'] > 0.75].copy()

        print("\n========== CATEGORY VALIDATION ==========")

        total_rows = len(zoi_channel_group)
        high_rows = len(filtered_dat_high)
        med_rows  = len(filtered_dat_med)
        low_rows  = len(filtered_dat_low)

        print(f"Total rows : {total_rows}")
        print(f"High rows  : {high_rows}")
        print(f"Medium rows: {med_rows}")
        print(f"Low rows   : {low_rows}")

        classified_total = high_rows + med_rows + low_rows

        print(f"Classified total: {classified_total}")

        if classified_total == total_rows:
            print("SUCCESS: All rows classified exactly once.")
        else:
            print("WARNING: Row count mismatch!")

        # Check for overlaps between categories
        high_idx = set(filtered_dat_high.index)
        med_idx  = set(filtered_dat_med.index)
        low_idx  = set(filtered_dat_low.index)

        print("\nOverlap checks:")
        print("High & Medium overlap:", len(high_idx & med_idx))
        print("High & Low overlap   :", len(high_idx & low_idx))
        print("Medium & Low overlap :", len(med_idx & low_idx))

        # Find uncategorized rows
        classified_idx = high_idx | med_idx | low_idx
        missing_idx = set(zoi_channel_group.index) - classified_idx

        print("\nUncategorized rows:", len(missing_idx))

        if missing_idx:
            print(
                zoi_channel_group.loc[list(missing_idx),
                ['OMR_Flow', 'overlap']].head(20)
            )

        print("=========================================\n")
        
        # Calculate total channel length
        total_channel_length = (
            zoi_channel_group
            .drop_duplicates(subset='channel_number')
            ['length']
            .sum()
        )
        
        # Process each influence level
        def process_influence(filtered_dat, h_influence_label):
    
            # Step 1: make a copy FIRST
            filtered2 = filtered_dat.copy()

            # Step 2: clean OMR_Flow BEFORE mapping
            filtered2['OMR_Flow'] = filtered2['OMR_Flow'].astype(str).str.strip()

            # Step 3: map FileTag
            filtered2['FileTag'] = filtered2['OMR_Flow'].map({
                '-2000': 'Neg2000',
                '-3500': 'Neg3500',
                '-5000': 'Neg5000',
                '-6500': 'Neg6500'
            })

            # 🚨 SAFETY CHECK (keep this!!)
            if filtered2['FileTag'].isna().any():
                print("WARNING: Missing FileTag mapping!")
                print(filtered2[['OMR_Flow']].drop_duplicates())

            # Step 4: convert to numeric
            filtered2['OMR_Flow_num'] = filtered2['OMR_Flow'].astype(float)

            # Step 5: group
            if "length" not in filtered2.columns:
                raise ValueError("Missing 'length' column — check merge with channels")

            grouped = filtered2.groupby(
                ['group', 'OMR_Flow_num', 'FileTag'],
                as_index=False
            )["length"].sum()

            grouped = grouped.rename(columns={'length': 'sumLength'})
            grouped['OMR_Flow'] = grouped['OMR_Flow_num']

            # Step 6: categorical ordering
            grouped['group'] = pd.Categorical(
                grouped['group'], 
                categories=["lolo", "lomed", "lohi", "medlo", "medmed", "medhi", "hilo", "himed", "hihi"],
                ordered=True
            )

            grouped['FileTag'] = pd.Categorical(
                grouped['FileTag'],
                categories=["Neg2000", "Neg3500", "Neg5000", "Neg6500"],
                ordered=True
            )

            # Step 7: compute proportions
            grouped['pLength'] = grouped['sumLength'] / total_channel_length
            grouped['h_influence'] = h_influence_label

            return grouped
        
        filtered2_high = process_influence(filtered_dat_high, "High hydrologic influence")
        filtered2_med = process_influence(filtered_dat_med, "Medium hydrologic influence")
        filtered2_low = process_influence(filtered_dat_low, "Low hydrologic influence")
        
        # Combine all influence levels
        #filtered_dat = pd.concat([filtered2_high, filtered2_med, filtered2_low], ignore_index=True)

                # NEW: force 0 so that no bins are empty
        filtered_dat = pd.concat(
            [filtered2_high, filtered2_med, filtered2_low],
            ignore_index=True
        )

        # Force all combinations to exist
        all_combos = pd.MultiIndex.from_product(
            [
                [inflowbin],
                available_omrs,
                [
                    "Low hydrologic influence",
                    "Medium hydrologic influence",
                    "High hydrologic influence"
                ]
            ],
            names=["group", "OMR_Flow_num", "h_influence"]
        )

        filtered_dat = (
            filtered_dat
            .set_index(["group", "OMR_Flow_num", "h_influence"])
            .reindex(all_combos)
            .reset_index()
        )

        # Fill missing values with zero
        filtered_dat["sumLength"] = filtered_dat["sumLength"].fillna(0)
        filtered_dat["pLength"] = filtered_dat["pLength"].fillna(0)

        # Restore OMR_Flow column
        filtered_dat["OMR_Flow"] = filtered_dat["OMR_Flow_num"]

        filtered_dat['h_influence'] = pd.Categorical(
            filtered_dat['h_influence'],
            categories=["Low hydrologic influence", "Medium hydrologic influence", "High hydrologic influence"],
            ordered=True
        )
        
               # Sort for consistent ordering
        filtered_dat = filtered_dat.sort_values(['h_influence', 'group', 'OMR_Flow'])
        
        # Debug: print what we have
        print(f"\nFiltered data summary for {week}:")
        print(f"Unique groups: {filtered_dat['group'].unique()}")
        print(f"Inflowbin: {inflowbin}")
        print(f"Number of rows: {len(filtered_dat)}")
        print(f"Sample of data:\n{filtered_dat.head(15)}")
                
        influences = [
            "Low hydrologic influence",
            "Medium hydrologic influence",
            "High hydrologic influence"
        ]

        # Create the plot
        fig = make_subplots(
            rows=1, cols=3,
            shared_yaxes=True,
            subplot_titles=[
                "Low hydrologic influence",
                "Medium hydrologic influence",
                "High hydrologic influence"
            ]
        )

        color_map = {
            -6500: "#440154",
            -5000: "#31688e",
            -3500: "#35b779",
            -2000: "#fde725"
        }

        for col_idx, h_inf in enumerate(influences, start=1):
            plot_data = filtered_dat[
                (filtered_dat["h_influence"] == h_inf) &
                (filtered_dat["group"] == inflowbin)
            ]

            for omr in available_omrs:
                filtered_dat["OMR_Flow"] = filtered_dat["OMR_Flow"].astype(float)
                val_series = plot_data.loc[plot_data["OMR_Flow"] == omr, "pLength"]

                val = plot_data.loc[plot_data["OMR_Flow"] == omr, "pLength"]
                y_val = val.iloc[0] if len(val) > 0 else 0

                fig.add_trace(
                    go.Bar(
                        x=[str(omr)],
                        y=[y_val],
                        name=str(omr),
                        marker_color=color_map[omr],
                        showlegend=(col_idx == 1),
                        hovertemplate=(
                            "OMR: %{x}<br>"
                            "Proportion: %{y:.2%}<extra></extra>"
                        )
                    ),
                    row=1, col=col_idx
                )

        fig.update_layout(
            title_text=f"ZOI Proportional Channel Length<br>{Week_dates}",
            barmode="group",
            yaxis_title="Proportional Channel Length",
            template="plotly_white"
        )

        html_out = f"ZOI_Proportional_ChannelLength_{week}.html"
        fig.write_html(html_out, include_plotlyjs="cdn")

        # ============================
        # PNG export (Matplotlib)
        # ============================

        png_out = f"ZOI_Proportional_ChannelLength_{week}.png"

        # Match Plotly / R-style colors
        color_map = {
            -6500: "#2b0b3f",  # dark purple
            -5000: "#1de9b6",  # teal
            -3500: "#fbc02d",  # yellow
            -2000: "#7f0000",  # dark red
        }

        omr_order = [-6500, -5000, -3500, -2000]

        fig, axes = plt.subplots(
            1, 3,
            figsize=(15, 5),
            sharey=True
        )

        influences = [
            "Low hydrologic influence",
            "Medium hydrologic influence",
            "High hydrologic influence"
        ]

        for ax, h_inf in zip(axes, influences):

            plot_data = filtered_dat[
                (filtered_dat["h_influence"] == h_inf) &
                (filtered_dat["group"] == inflowbin)
            ]

            heights = [
                plot_data.loc[
                    plot_data["OMR_Flow"] == omr, "pLength"
                ].values[0] if not plot_data.loc[
                    plot_data["OMR_Flow"] == omr, "pLength"
                ].empty else 0
                for omr in omr_order
            ]

            ax.bar(
                [str(o) for o in omr_order],
                heights,
                color=[color_map[o] for o in omr_order],
                edgecolor="black",
                linewidth=0.6
            )

            ax.set_title(h_inf, fontsize=14)
            ax.set_xlabel("OMR Flow (cfs)", fontsize=12)
            ax.set_ylim(0, 1.05)
            ax.tick_params(axis="x", labelsize=11)
            ax.tick_params(axis="y", labelsize=11)
            ax.grid(False)

        # Left y-axis label only
        axes[0].set_ylabel(
            f"Proportional Channel Length\nfor {Week_dates}",
            fontsize=12
        )

        # Add overall figure title (keep it high)
        fig.suptitle(
            f"ZOI Proportional Channel Length\n{Week_dates}",
            fontsize=16,
            y=0.98
        )

        # Build legend handles (if not already defined above)
        legend_handles = [
            plt.Rectangle((0, 0), 1, 1, color=color_map[o], ec="black")
            for o in reversed(omr_order)
        ]

        # Use tight_layout but reserve space for title AND legend
        plt.tight_layout(rect=[0, 0, 0.80, 0.93])

        # Place legend on right
        fig.legend(
            legend_handles,
            [str(o) for o in reversed(omr_order)],
            title="factor(OMR_Flow)",
            loc="center left",
            bbox_to_anchor=(0.82, 0.5),
            frameon=True,
            fontsize=11,
            title_fontsize=12
        )

        # Save (important: include bbox_inches)
        plt.savefig(png_out, dpi=300, bbox_inches="tight")
        plt.close()

        print(f"Saved PNG: {png_out}")        
        
        # Update template_df for Excel output
        # The indices in filtered_dat are ordered as: High (1-4), Medium (5-8), Low (9-12)
        week_idx = i + 1  # 1, 2, or 3
        
        if week == "week1":
            row_start = 0
        elif week == "week2":
            row_start = 4
        else:
            row_start = 8
        
        row_end = row_start + 4
        
        # Extract values in correct order
        # filtered_dat is sorted by h_influence, group, OMR_Flow
        # We need to extract values for each influence level separately
        high_data = filtered_dat[filtered_dat['h_influence'] == "High hydrologic influence"].sort_values('OMR_Flow', ascending=False)
        med_data = filtered_dat[filtered_dat['h_influence'] == "Medium hydrologic influence"].sort_values('OMR_Flow', ascending=False)
        low_data = filtered_dat[filtered_dat['h_influence'] == "Low hydrologic influence"].sort_values('OMR_Flow', ascending=False)
        
        # Update template row by row to handle any size mismatches
        template_df.loc[row_start:row_end-1, 'Dates'] = Week_dates
        
        # Match OMR bins and assign values
        for idx, omr_bin in enumerate([-6500, -5000, -3500, -2000]):
            template_row = row_start + idx
            
            # Low data
            low_row = low_data[low_data['OMR_Flow'] == omr_bin]
            if not low_row.empty:
                template_df.loc[template_row, 'Low_Sum_mi'] = low_row['sumLength'].values[0] / 5280
                template_df.loc[template_row, 'Low_pct'] = low_row['pLength'].values[0]
            
            # Medium data
            med_row = med_data[med_data['OMR_Flow'] == omr_bin]
            if not med_row.empty:
                template_df.loc[template_row, 'Medium_Sum_mi'] = med_row['sumLength'].values[0] / 5280
                template_df.loc[template_row, 'Medium_pct'] = med_row['pLength'].values[0]
            
            # High data
            high_row = high_data[high_data['OMR_Flow'] == omr_bin]
            if not high_row.empty:
                template_df.loc[template_row, 'High_Sum_mi'] = high_row['sumLength'].values[0] / 5280
                template_df.loc[template_row, 'High_pct'] = high_row['pLength'].values[0]
    
    # Create Excel workbook
    create_excel_output(template_df, Week1_dates, Week2_dates, Week3_dates)
    
    # Clean up CSV files
    csv_files = glob.glob("*.csv")
    for f in csv_files:
        try:
            os.remove(f)
        except:
            pass
    
    print("\nProcessing complete!")


def create_excel_output(template_df, Week1_dates, Week2_dates, Week3_dates):
    """Create Excel workbook with formatted channel length data"""
    
    wb = Workbook()
    ws = wb.active
    ws.title = "Sheet2"
    
    # Define styles
    border = Border(
        left=Side(style='thin'),
        right=Side(style='thin'),
        top=Side(style='thin'),
        bottom=Side(style='thin')
    )
    
    hdr_big = Font(bold=True)
    hdr_align = Alignment(horizontal='center', vertical='center', wrap_text=True)
    cell_align = Alignment(horizontal='center', vertical='center')
    cell_left_top = Alignment(horizontal='left', vertical='top', wrap_text=True)
    
    # Set column widths
    ws.column_dimensions['A'].width = 22
    ws.column_dimensions['B'].width = 11
    for col in ['C', 'D', 'E', 'F', 'G', 'H']:
        ws.column_dimensions[col].width = 16
    
    # Set row heights
    ws.row_dimensions[2].height = 26
    ws.row_dimensions[3].height = 26
    ws.row_dimensions[4].height = 55
    
    # Header block
    # Row 2-3: Hydrologic Influence / Overlap Range
    ws['A2'] = "Hydrologic Influence\nOverlap Range"
    ws.merge_cells('A2:A3')
    ws['A2'].font = hdr_big
    ws['A2'].alignment = hdr_align
    ws['A2'].border = border
    
    # Low (>0.75)
    ws['C2'] = "Low\n(>0.75)"
    ws.merge_cells('C2:D2')
    ws['C2'].font = hdr_big
    ws['C2'].alignment = hdr_align
    ws['C2'].border = border
    
    # Medium (>= 0.25 & <= 0.75)
    ws['E2'] = "Medium\n(>= 0.25 & <= 0.75)"
    ws.merge_cells('E2:F2')
    ws['E2'].font = hdr_big
    ws['E2'].alignment = hdr_align
    ws['E2'].border = border
    
    # High (<0.25)
    ws['G2'] = "High\n(<0.25)"
    ws.merge_cells('G2:H2')
    ws['G2'].font = hdr_big
    ws['G2'].alignment = hdr_align
    ws['G2'].border = border
    
    # Row 4 headers
    headers_row4 = [
        "Weekly\nModel Run",
        "OMR Bin\n(cfs)",
        "Sum\nChannel\nLength\n(Mile)",
        "%\nChannel\nLength",
        "Sum\nChannel\nLength\n(Mile)",
        "%\nChannel\nLength",
        "Sum\nChannel\nLength\n(Mile)",
        "%\nChannel\nLength"
    ]
    
    for col_idx, header in enumerate(headers_row4, start=1):
        cell = ws.cell(row=4, column=col_idx, value=header)
        cell.font = hdr_big
        cell.alignment = hdr_align
        cell.border = border
    
    # Data rows
    omr_bins = ["-6,500", "-5,000", "-3,500", "-2,000"]
    weeks_data = [
        ("Week 1:", Week1_dates),
        ("Week 2:", Week2_dates),
        ("Week 3:", Week3_dates)
    ]
    
    row = 5
    for week_idx, (week_label, week_dates) in enumerate(weeks_data):
        # Parse dates
        date_range = week_dates.split('(')[1].split(')')[0]
        start_date, end_date = date_range.split(' - ')
        
        week_text = f"{week_label}\n\n{start_date} -\n{end_date}"
        
        # Merged cell for week
        ws.merge_cells(f'A{row}:A{row+3}')
        ws[f'A{row}'] = week_text
        ws[f'A{row}'].alignment = cell_left_top
        ws[f'A{row}'].border = border
        
        # OMR bins and data
        for i in range(4):
            # OMR bin
            ws.cell(row=row+i, column=2, value=omr_bins[i])
            ws.cell(row=row+i, column=2).alignment = cell_align
            ws.cell(row=row+i, column=2).border = border
            
            # Get data from template_df
            df_row = week_idx * 4 + i
            
            # Low columns
            ws.cell(row=row+i, column=3, value=template_df.loc[df_row, 'Low_Sum_mi'])
            ws.cell(row=row+i, column=4, value=template_df.loc[df_row, 'Low_pct'])
            
            # Medium columns
            ws.cell(row=row+i, column=5, value=template_df.loc[df_row, 'Medium_Sum_mi'])
            ws.cell(row=row+i, column=6, value=template_df.loc[df_row, 'Medium_pct'])
            
            # High columns
            ws.cell(row=row+i, column=7, value=template_df.loc[df_row, 'High_Sum_mi'])
            ws.cell(row=row+i, column=8, value=template_df.loc[df_row, 'High_pct'])
            
            # Apply formatting
            for col in range(3, 9):
                ws.cell(row=row+i, column=col).alignment = cell_align
                ws.cell(row=row+i, column=col).border = border
            
            ws.row_dimensions[row+i].height = 20
        
        row += 4
    
    # Save workbook
    wb.save("ChannelLength_Data.xlsx")
    print("Saved Excel file: ChannelLength_Data.xlsx")


if __name__ == "__main__":
    main()
