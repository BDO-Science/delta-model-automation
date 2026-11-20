import argparse
import re
import sys
import shutil
from pathlib import Path
from pyhecdss import DSSFile
import pandas as pd
import os
import stat
import time

# ------------------------------
# Arguments: active scenario letters for this week
# ------------------------------
def valid_letter(c):
    if not re.fullmatch(r"[A-Z]", c):
        raise argparse.ArgumentTypeError(f"Not a single uppercase letter: '{c}'")
    return c

parser = argparse.ArgumentParser(
    description="Create the Model_NOPUMP inputs: copy Model/Input and zero out "
                "the pumping time series for the active scenarios.")
parser.add_argument("--scenarios", "-s", nargs="+", required=True, type=valid_letter,
                    help="Active scenario letters, e.g. -s A B C "
                         "(WeeklyWorkflow.bat passes the discovered scenario list)")
args = parser.parse_args()
scenario_letters = args.scenarios
print(f"Active scenarios: {', '.join(scenario_letters)}")

# ------------------------------
# Paths
# ------------------------------
root_np = Path(__file__).parent.resolve()   # the folder where this script lives
source_lm = root_np.parent / "Model"
input_src = source_lm / "Input"
input_dst = root_np / "Input"

# Step 1: Copy Input folder
def remove_readonly(func, path, _):
    os.chmod(path, stat.S_IWRITE)
    func(path)

def safe_rmtree(path, retries=3, delay=1):
    for i in range(retries):
        try:
            shutil.rmtree(path, onerror=remove_readonly)
            return
        except PermissionError as e:
            print(f"Retry {i+1}/{retries} failed: {e}")
            time.sleep(delay)
    raise

# use it
if input_dst.exists():
    print(f"Removing existing {input_dst}")
    safe_rmtree(input_dst)

print(f"Copying {input_src} to {input_dst}")
shutil.copytree(input_src, input_dst)

# Step 2: Open forecast.dss
forecast_dss_path = input_dst / "timeseries" / "forecast.dss"
if not forecast_dss_path.exists():
    raise FileNotFoundError(f"{forecast_dss_path} not found!")

print(f"Opening DSS file: {forecast_dss_path}")
dss = DSSFile(str(forecast_dss_path))

# Step 3: Read DSS catalog into DataFrame
print("Reading DSS catalog...")
catalog_df = dss.read_catalog()

# Pumping nodes to zero out (physical locations, not scenario-dependent)
nodes = ["CHDMC004", "CHSWP003"]

# Work out the F-part for each active scenario letter from the catalog.
# F-parts look like "<prefix><letter>", e.g. "DSM2-20260616-21A" / "...-21B".
# The prefix is whatever the preprocessor wrote; it is not hard-coded here.
all_f = sorted(catalog_df["F"].unique())
prefixes = {}
for f in all_f:
    if len(f) > 1 and f[-1] in scenario_letters:
        prefixes.setdefault(f[:-1], set()).add(f[-1])
valid_prefixes = [p for p, letters in prefixes.items() if letters == set(scenario_letters)]

if len(valid_prefixes) != 1:
    dss.close()
    raise RuntimeError(
        f"Could not identify scenario F-parts {scenario_letters} in forecast.dss "
        f"(matching prefixes: {valid_prefixes or 'none'}). "
        f"F-parts in file: {all_f}")

f_prefix = valid_prefixes[0]
active_f = [f_prefix + sc for sc in scenario_letters]
print(f"Scenario F-parts: {active_f}")

# Filter catalog for matching paths
df_filtered = catalog_df[
    (catalog_df["B"].isin(nodes)) &    # channel nodes
    (catalog_df["E"] == "1DAY") &      # daily interval
    (catalog_df["F"].isin(active_f))   # active scenarios only
]

paths_to_zero = dss.get_pathnames(df_filtered)
print(f"Found {len(paths_to_zero)} matching entries in catalog.")

# Every active scenario must have something to zero, otherwise the NoPumping
# run would silently be identical to the pumping run for that scenario.
found_f = set(df_filtered["F"])
no_match = [f for f in active_f if f not in found_f]
if no_match:
    dss.close()
    raise RuntimeError(f"No pumping records for nodes {nodes} found for: {no_match}")

# Zero out each timeseries
n_zeroed = 0
n_failed = 0
for path in paths_to_zero:
    print(f"Zeroing timeseries: {path}")
    try:
        # Read regular time series
        df_ts, units, period_type = dss.read_rts(path)

        # Set all values to zero
        df_ts.iloc[:, 0] = 0.0

        # Write back to DSS
        dss.write_rts(path, df_ts, units, period_type)
        n_zeroed += 1

    except Exception as e:
        n_failed += 1
        print(f"WARNING: Could not zero {path}: {e}")

dss.close()
print(f"Zeroed {n_zeroed} of {len(paths_to_zero)} time series for scenarios {scenario_letters}.")
print("Done updating DSS file.")
if n_failed:
    sys.exit(1)
